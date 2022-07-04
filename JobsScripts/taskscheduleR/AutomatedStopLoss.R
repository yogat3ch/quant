HDA::startPkgs(c("dplyr","magrittr","RSelenium","rvest"))
try({load(file = "~/R/Quant/JobsScripts/parameters.Rdata")})
try({load(file = "~/R/Quant/JobsScripts/Positions_new.Rdata")})
try({load(file = "~/R/Quant/Positions_tsl.Rdata")})
names(TSLvars) <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist
# ----------------------- Wed Jun 12 16:40:59 2019 ------------------------#
# Login
shell_out <- shell("docker info", intern = T)
message(shell_out)
# If docker is not yet started start it
if (stringr::str_detect(shell_out, "error during connect")) shell.exec("\"file:///C:/Program Files/Docker Toolbox/start.sh\"")
while(stringr::str_detect(shell_out, "error during connect") | length(shell_out) < 1) {
  Sys.sleep(5)
  shell_out <- shell("docker info", intern = T)
}
shell_out <- shell("docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug", intern = T)
Sys.sleep(5)

remDr <- RSelenium::remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L,browserName = "chrome")
catch <- try({remDr$open(silent=T)
remDr$setImplicitWaitTimeout(milliseconds = 4500)
browserName <- remDr$getSession()[["browserName"]]})
while (catch != "chrome") {
  Sys.sleep(5)
  catch <- try({remDr$open(silent=T)
    remDr$setImplicitWaitTimeout(milliseconds = 4500)
    browserName <- remDr$getSession()[["browserName"]]})
}
remDr$navigate("https://robinhood.com/login")
remDr$findElement("xpath", "//input[@name = 'username']")$sendKeysToElement(list(keyring::key_list("RH")[[2]]))
remDr$findElement("xpath", "//input[@name = 'password']")$sendKeysToElement(list(keyring::key_get("RH", username = keyring::key_list("RH")[[2]])))
remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
# Authenticate via Email
remDr$findElement("xpath", "//span[contains(text(), 'Email Me')]")$clickElement()
gmailr::gmail_auth(secret_file = "~/R/Quant/JobsScripts/sholsen_My Project_GoogleCalendaR.json")
msgs <-  gmailr::messages(search = paste0("from:(notifications@robinhood.com) subject:(Your Email Verification Code) after:",lubridate::today()))

while (is.null(msgs[[1]][["messages"]][[1]][["id"]])) {
  msgs <- gmailr::messages(search = paste0("from:(notifications@robinhood.com) subject:(Your Email Verification Code) after:",lubridate::today()))
}
code <- gmailr::message(msgs[[1]][["messages"]][[1]][["id"]], format = "minimal") %>% .[["snippet"]] %>% stringr::str_match("\\d{6}") 
remDr$findElement("xpath", "//input[@name = 'response']")$sendKeysToElement(list(as.character(code)))
remDr$findElement("xpath", "//span[contains(text(),'Confirm')]")$clickElement()
gmailr::delete_message(msgs[[1]][["messages"]][[1]][["id"]]) # Delete the message after using the confirmation code


# ----------------------- Tue Jun 11 11:50:51 2019 ------------------------#
# Set Trailing Stop Losses
# Get the stocks held

holding <- remDr$findElements("xpath", "//h3[contains(text(),'Stocks')]/ancestor::section/a") 
while (length(holding) < 1 & i < 7) {
  holding <- remDr$findElements("xpath", "//h3[contains(text(),'Stocks')]/ancestor::section/a") 
  Sys.sleep(5)
}
holding <- purrr::map_dfr(holding, function(.x){
  out <- .x$getElementText()[[1]] %>% stringr::str_split("\\\n") %>% .[[1]]
  out[2] %<>% stringr::str_match("\\d+") %>% as.numeric
  out[3] %<>% stringr::str_match("\\d+\\.\\d+") %>% as.numeric
  names(out) <- c("Sym","Shares","Price")
  return(as.list(out))
  }) %>% dplyr::mutate_at(dplyr::vars(Shares,Price), dplyr::funs(as.numeric))
# for each set the stop loss
load(file = "~/R/Quant/Positions_tsl.Rdata")
for (i in 1:nrow(holding)) {
  s <- holding[["Sym"]][i]
  # Load model for Stock to predict gain and mix type
  try({
    fn <- paste0(s,"_cl.Rdata")
    load(file = paste0("~/R/Quant/MdlBkp/",fn))
  })
  mdl <- get0(paste0(s,"_cl"))
  if (is.null(mdl) & is.null(Positions_tsl[[s]][["TSL"]])) {
    
    # If a Trailing Stop Loss Type is not set, notify
    HDA::startPkgs(c("htmltools"))
    gmailr::use_secret_file("~/R/Quant/JobsScripts/sholsen_My Project_GoogleCalendaR.json")
    gmailr::gmail_auth(secret_file = "~/R/Quant/JobsScripts/sholsen_My Project_GoogleCalendaR.json")
    tsl_msg <- gmailr::mime(From="sholsen@alumni.emory.edu",
                            To="7818797492@vtext.com",
                            subject = "R: TSL ") %>%
      gmailr::html_body(body = paste0("No Trailing Stop Loss type or model for: ",s,". Please optimize a TSL and build a model for this position."))
    gmailr::send_message(tsl_msg)
    next
  }
  url <- httr::parse_url("https://robinhood.com")
  url$path <- paste("stocks",s,sep = "/")
  remDr$navigate(httr::build_url(url))
  remDr$mouseMoveToLocation(webElement = remDr$findElement("xpath", "//header[@class = 'card-heading']/div/*[2]"))
  remDr$click()
  remDr$findElement("xpath", "//a[contains(text(),'Stop Loss Order')]")$clickElement()
  remDr$findElement("xpath","//span[@data-testid = 'OrderFormHeading-Sell']")$clickElement()
  price <- remDr$findElement("xpath","//section[@data-testid = 'ChartSection']/header/h1")$getElementText() %>% unlist %>% stringr::str_extract("\\d+\\.\\d+") %>% as.numeric
# ----------------------- Tue Jun 18 18:08:46 2019 ------------------------#
#   # Choose the best Trailing Stop Loss
  if (!is.null(Positions_tsl[[s]][["TSL"]])) {
    if (length(Positions_tsl[[s]][["TSL"]][["rowname"]]) > 1) {
      if (diff(Positions_tsl[[s]][["TSL"]][["Cum.Returns"]]) > 1) {
        # If theres a difference of more than 1 in Cumulative returns, choose the best
        tsl_type <- Positions_tsl[[s]][["TSL"]][["rowname"]][which.max(Positions_tsl[[s]][["TSL"]][["Cum.Returns"]])]
      } else if (!is.null(mdl)) {
        # If not, and a model is present
        all_tsl <- c(Positions_tsl[[s]][["TSL"]][["rowname"]],stats::predict(mdl[["Mix_type"]], newdata = xts::last(Positions_new[[s]])),stats::predict(mdl[["Gain_type"]], newdata = xts::last(Positions_new[[s]])))
        if (any(duplicated(all_tsl))) {
          # If cum.returns are tied, but mix type or gain type confirms either TSL type, then choose the one that is confirmed.
        tsl_type <- HDA::Mode(all_tsl)
        } else {
          tsl_type <- HDA::mode(stats::predict(mdl[["Mix_type"]], newdata = xts::last(Positions_new[[s]])))# If no confirmation from type predictions, then just use mix type
        }
        
      }
    } else {
      tsl_type <- Positions_tsl[[s]][["TSL"]][["rowname"]]
    }
  }
# End Choose the best Trailing Stop Loss
# ----------------------- Tue Jun 18 18:10:14 2019 ------------------------#
# Set the trailing stop loss amount using TSLvars

  if (stringr::str_detect(tsl_type, "tsla")) {
    tsla <- TSLvars[[tsl_type]]
    stoploss <- tsla(Positions_new[[s]][["close"]])
  } else if (stringr::str_detect(tsl_type, "retro")) {
    retro <- TSLvars[[tsl_type]]
    ind <- {length(Positions_new[[s]][["close"]]) - retro[1]}:length(Positions_new[[s]][["close"]])
    stoploss <-  Positions_new[[s]][["close"]][ind] %>% range() %>% diff() %>% {. * retro[2]}
  } else if (stringr::str_detect(tsl_type, "tslp")) {
    stoploss <- xts::last(Positions_new[[s]])[["close"]] * TSLvars[[tsl_type]]
  }
  remDr$findElement("xpath", "//input[@name = 'stop_price']")$sendKeysToElement(list(as.character({price - stoploss} %>% round(2))))
  shares <- remDr$findElement("xpath","//*[@id='react_root']/div/main/div[2]/div/div/div/main/div[2]/div[2]/div/form/div[1]/div[3]")$getElementText() %>% stringr::str_extract("\\d")
  remDr$findElement("xpath", "//input[@data-testid = 'OrderFormRows-Shares']")$sendKeysToElement(list(shares))
  remDr$findElement("css", "span.Select-arrow-zone")$clickElement()
  wE <- remDr$findElement("css", "div.Select-menu-outer")
  wE$findChildElement("xpath", "//*[contains(text(),'Good till Canceled')]")$clickElement()
  remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
  remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
  try({remDr$findElement("xpath", "//span[contains(text(),'Done')]/ancestor::button[@type = 'button']")$clickElement()})
# End  Set the trailing stop loss amount using TSLvars
}

# ----------------------- Tue Jun 18 19:31:34 2019 ------------------------#
# TODO(Need to check account history for executed orders and update the Orders sheet appropriately)


# If length Actions
# Shutdown images
if (lubridate::now() > lubridate::today() %>% paste("17:00:00") %>% lubridate::ymd_hms()) {
shell("docker stop $(docker ps -aq)")
shell("docker rm $(docker ps -aq)")
}

HDA::startPkgs(c("dplyr","magrittr","RSelenium","rvest"))
setwd("~/R/Quant/JobsScripts")
message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
try({load(file = "~/R/Quant/JobsScripts/parameters.Rdata")})
try({load(file = "~/R/Quant/JobsScripts/Positions_new.Rdata")})
try({load(file = "~/R/Quant/Positions_tsl.Rdata")})
# ----------------------- Mon Jun 24 13:28:44 2019 ------------------------#
# Initiate Googlesheets and retrieve necessary data
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=1757293360")
toNum <- function(x){
  x %>% stringr::str_replace_all("\\$|\\,","") %>% as.numeric
}
Hourly <- googlesheets::gs_read(gs, ws = "Hourly") %>% dplyr::mutate_at(dplyr::vars(Time), dplyr::funs(lubridate::mdy_hms))
Orders <- googlesheets::gs_read(gs, ws = "Orders") %>% dplyr::mutate_at(dplyr::vars(Date),dplyr::funs(lubridate::ymd)) %>% dplyr::mutate_at(dplyr::vars(Price,CB,`G/L`), dplyr::funs(toNum))
# ----------------------- Tue Jun 25 17:57:31 2019 ------------------------#
# Check to see if script should run using just Google Sheets

run <- purrr::imap(Positions_tsl, O = Orders, H = Hourly, function(.x, .y, O, H){
  open_orders <- O %>% dplyr::filter(Platform == "RH" & Tick == .y & Order == "B" & Exec == "Exec") %>% dplyr::arrange(dplyr::desc(Date)) %>% dplyr::mutate(Cum.Shares = cumsum(Qty))
  closed_orders <- O %>% dplyr::filter(Platform == "RH" & Tick == .y & Order == "TSL" & Exec == "Exec") %>% dplyr::arrange(dplyr::desc(Date)) %>% dplyr::mutate(Cum.Shares = cumsum(Qty))
  rbind(open_orders,closed_orders)
  
  if (nrow(open_orders) > 0 & nrow(closed_orders) > 0) {
    open_shares <- xts::last(open_orders)[["Qty"]] - xts::last(closed_orders)[["Qty"]]
    } else if (nrow(closed_orders) < 1 & nrow(open_orders) > 0) {
      open_shares <- xts::last(open_orders)[["Qty"]]
  } else open_shares <- NULL
  if (!is.null(open_shares) & HDA::`%n%`(.x[["stopprice"]],T) & HDA::`%n%`(.x[["stoploss"]],T)) {
    if(xts::last(H)[[.y]] - .x[["stoploss"]] > .x[["stopprice"]]) out <- T else if (xts::last(H)[[.y]] < .x[["stopprice"]]) out <- T else out <- F
  } else out <- NULL
  return(out)
}) %>% purrr::keep(.p = ~ ifelse(length(.x) < 1, F, ifelse(.x < 1, F, T)))
if (length(run) < 1) {
  stop("All stop losses are up to date")
} else message(paste0("Stop Losses will be updated for: ", paste0(names(run), collapse = ", ")))


# End Initiate Googlesheets
names(TSLvars) <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist
# ----------------------- Tue Jun 25 16:52:03 2019 ------------------------#
# Add Two Factor Authenticator

source("~/R/Quant/JobsScripts/RunDocker.R")
remDr <- startSelenium()
remDr$navigate("https://robinhood.com/login")

remDr$findElement("xpath", "//input[@name = 'username']")$sendKeysToElement(list(keyring::key_list("RH")[[2]]))
remDr$findElement("xpath", "//input[@name = 'password']")$sendKeysToElement(list(keyring::key_get("RH", username = keyring::key_list("RH")[[2]])))
remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
# Login Notification
# googleAuthR::gar_auth(token = "~/R/Quant/JobsScripts/gmailoauth.rds")
# gmailr::gmail_auth("full", id = getOption("googleAuthR.client_id"), secret = getOption("googleAuthR.client_secret"))
# login_msg <- gmailr::mime(From="sholsen@alumni.emory.edu",
#                           To="7818797492@vtext.com",
#                           subject = "R: Login") %>%
#   gmailr::html_body(body = "Send Two Factor Authentication Code")#%>% gmailr::attach_file("table.png")
# gmailr::send_message(login_msg)


  msgs <-  gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))

while (is.null(msgs[[1]][["messages"]][[1]][["id"]])) {
  Sys.sleep(1)
  msgs <- gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))
}
code <- gmailr::message(msgs[[1]][["messages"]][[1]][["id"]], format = "minimal") %>% .[["snippet"]] %>% stringr::str_match("^\\d{6}$") 
remDr$findElement("xpath", "//input[@name = 'mfa-code']")$sendKeysToElement(list(as.character(code)))
remDr$findElement("xpath", "//button[@type = 'submit')]")$clickElement()
gmailr::delete_message(msgs[[1]][["messages"]][[1]][["id"]]) # Delete the message after using the confirmation code


# ----------------------- Tue Jun 11 11:50:51 2019 ------------------------#
# Set Trailing Stop Losses
# TODO(Needs to be set in reference to the maximum value)

holding <- remDr$findElements("xpath", "//h3[contains(text(),'Stocks')]/ancestor::section/a") 
i <- 1
while (length(holding) < 1 & i < 7) {
  Sys.sleep(5)
  holding <- remDr$findElements("xpath", "//h3[contains(text(),'Stocks')]/ancestor::section/a") 
  message(paste0("Trying to scrape holdings, try number: ",i))
  i <- i + 1
  
}
holding <- purrr::map_dfr(holding, function(.x){
  out <- .x$getElementText()[[1]] %>% stringr::str_split("\\\n") %>% .[[1]]
  out[2] %<>% stringr::str_match("\\d+") %>% as.numeric
  out[3] %<>% stringr::str_match("\\d+\\.\\d+") %>% as.numeric
  names(out) <- c("Sym","Shares","Price")
  return(as.list(out))
}) %>% dplyr::mutate_at(dplyr::vars(Shares,Price), dplyr::funs(as.numeric))

try(load(file = "holding_prev.Rdata"))
if (nrow(holding_prev) > nrow(holding)) {
  holding_sold <- subset(holding_prev, subset = !holding_prev[["Sym"]] %in% holding[["Sym"]])
  for (i in 1:nrow(holding_sold)) {
    s <- holding_sold[["Sym"]][i]
    message(paste0("Trailing stop loss executed for ",s,". Recording results..."))
    url <- httr::parse_url("https://robinhood.com")
    url$path <- paste("stocks",s,sep = "/")
    remDr$navigate(httr::build_url(url))
    stock_name <- remDr$findElement("xpath", "//main/div[@class = 'row']/div[@class = 'col-12']/header/h1")$getElementText() %>% unlist
    remDr$findElement("xpath", paste0("//h2[contains(text(),'History')]/ancestor::section/descendant::h3[contains(text(),'Stop Loss Sell')]/ancestor::div[4]"))$clickElement()
    
  Qty <- remDr$findElement("xpath", "//div[contains(text(), 'Filled Quantity')]/following-sibling::div")$getElementText()[[1]] %>% stringr::str_extract("^\\d{1,4}") %>% as.numeric
  Price <- remDr$findElement("xpath", "//div[contains(text(), 'Filled Quantity')]/following-sibling::div")$getElementText()[[1]] %>% stringr::str_extract("\\d{1,4}\\.\\d{2}$") %>% as.numeric
  # Get the previous buy orders, and create a cumulative sum col to match with the number sold 
  orders_prev  <- Orders %>% dplyr::filter(Platform == "RH" & Tick == s & Order == "B") %>% dplyr::arrange(dplyr::desc(Date)) %>% dplyr::mutate(Cum.Shares = cumsum(Qty))
  # Get the total cost basis
  cb <- sum(orders_prev[1:which(orders_prev[["Cum.Shares"]] == Qty), "CB", drop = T], na.rm = T)
  Type <- ifelse(!is.null(names(Positions_tsl[[s]][["stoploss"]])), names(Positions_tsl[[s]][["stoploss"]]), NA)
  Date <- remDr$findElement("xpath", "//div[contains(text(), 'Filled')]/following-sibling::div")$getElementText()[[1]] %>% stringr::str_extract("\\w{3}\\s\\d{1,2}\\,\\s\\d{4}") %>% lubridate::mdy(tz = Sys.timezone())
  sell_dat <- c(Platform = "RH", Order = "TSL", Type = Type, Tick = s, Date = as.character(Date), Qty = Qty, Price = Price, Exec = "Exec", CB = "", `G/L` = {Qty * Price - cb})
  googlesheets::gs_add_row(gs, ws = "Orders", input = sell_dat)
  
  }
}
holding_prev <- holding
save(holding_prev, file = "holding_prev.Rdata")
# for each set the stop loss
message(paste0("Number of holdings: ", nrow(holding)))
for (i in 1:nrow(holding)) {
  s <- holding[["Sym"]][i]
  message(paste0("Checking Stop loss for ", s))
  
  if (is.null(Positions_tsl[[s]])) {
    
    # If a Trailing Stop Loss Type is not set, notify
    HDA::startPkgs(c("htmltools"))
    
    tsl_msg <- gmailr::mime(From = "sholsen@alumni.emory.edu",
                            To = "7818797492@vtext.com",
                            subject = "R: TSL ") %>%
      gmailr::html_body(body = paste0("No Trailing Stop Loss type or model for: ",s,". Please optimize a TSL and build a model for this position."))
    gmailr::send_message(tsl_msg)
    next
  }
  
  
  # ----------------------- Tue Jun 18 18:08:46 2019 ------------------------#
  #   # Choose the best Trailing Stop Loss
  if (!is.null(Positions_tsl[[s]]) & !is.null(Positions_tsl[[s]][["stoploss"]])) {
    # If a stoploss was recorded from previous iterations use it, or if the type was set from the AutomatedOOTDataTest response SMS, then use that.
    if (is.numeric(Positions_tsl[[s]][["stoploss"]])) stoploss <-  Positions_tsl[[s]][["stoploss"]] else {
      tsl_type <-  Positions_tsl[[s]][["stoploss"]]
    }
  } else if (!is.null(Positions_tsl[[s]]) & is.null(Positions_tsl[[s]][["stoploss"]])) {
    if (length(Positions_tsl[[s]][["TSL"]][["rowname"]]) > 1) {
      if (diff(Positions_tsl[[s]][["TSL"]][["Cum.Returns"]]) > 1) {
        # If theres a difference of more than 1 in Cumulative returns, choose the best
        tsl_type <- Positions_tsl[[s]][["TSL"]][["rowname"]][which.max(Positions_tsl[[s]][["TSL"]][["Cum.Returns"]])]
      } else if (is.null(mdl)) {
        # Load model for Stock to predict gain and mix type
        try({
          fn <- paste0(s,"_cl.Rdata")
          load(file = paste0("~/R/Quant/MdlBkp/",fn))
        })
        mdl <- get0(paste0(s,"_cl"))
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
  
  # End Choose the best Trailing Stop Loss
  # ----------------------- Tue Jun 18 18:10:14 2019 ------------------------#
  # Set the trailing stop loss amount using TSLvars
  
    if (stringr::str_detect(tsl_type, "tsla")) {
      tsla <- TSLvars[[tsl_type]]
      stoploss <- tsla(Positions_new[[s]][["close"]])
      names(stoploss) <- tsl_type
    } else if (stringr::str_detect(tsl_type, "retro")) {
      retro <- TSLvars[[tsl_type]]
      ind <- {length(Positions_new[[s]][["close"]]) - retro[1]}:length(Positions_new[[s]][["close"]])
      stoploss <-  Positions_new[[s]][["close"]][ind] %>% range() %>% diff() %>% {. * retro[2]}
      names(stoploss) <- tsl_type
    } else if (stringr::str_detect(tsl_type, "tslp")) {
      stoploss <- xts::last(Positions_new[[s]])[["close"]] * TSLvars[[tsl_type]]
      names(stoploss) <- tsl_type
    }
    
    
  }  
  # Save the stop loss after creating it
  
  Positions_tsl[[s]][["stoploss"]] <- stoploss
  save(Positions_tsl, file = "~/R/Quant/Positions_tsl.Rdata")
  url <- httr::parse_url("https://robinhood.com")
  url$path <- paste("stocks",s,sep = "/")
  remDr$navigate(httr::build_url(url))
  remDr$findElement("xpath","//span[@data-testid = 'OrderFormHeading-Sell']")$clickElement()
  shares <- remDr$findElement("xpath","//div[contains(text(), 'Available')]")$getElementText() %>% stringr::str_extract("\\d") %>% as.numeric
  if (holding[["Shares"]][i] > shares) {
    message(paste0(s, ": Removing Previous Stop Loss"))
    stock_name <- remDr$findElement("xpath", "//main/div[@class = 'row']/div[@class = 'col-12']/header/h1")$getElementText() %>% unlist
    previous_sl <- remDr$findElement("xpath", paste0("//h2[contains(text(),'History')]/ancestor::section/descendant::h3[contains(text(),'Stop Loss Sell')]/ancestor::div[4]"))
    previous_sl_text <- previous_sl$getElementText()[[1]] %>% stringr::str_split("\\\n") %>% .[[1]]
    remove_sl <- {previous_sl_text[3] == "Placed" | previous_sl_text[3] == "Queued"}
    if (remove_sl) {
     previous_sl$clickElement()
      prev_sp <- remDr$findElement("xpath", "//div[contains(text(), 'Stop Price')]/following-sibling::div")$getElementText()[[1]] %>% stringr::str_replace("\\$|\\,","") %>% as.numeric
      
      orders_s <- Orders %>% dplyr::filter(Platform == "RH" & Tick == s & Order == "B")
      s_max <- Hourly %>% dplyr::filter(Time >= xts::last(orders_s[["Date"]])) %>% .[[s]] %>% max(na.rm = T)
      if (round(s_max - stoploss,2) == prev_sp) {
        next
      } else {
    remDr$findElement("partial link text", "Cancel Order")$clickElement()
        }
    }
    # purrr::map(pending, function(.x){
    #   .x$getElementText()
    # }) %>% unlist %>% purrr::map(.f = function(.x){
    #   out <- stringr::str_split(.x,"\\\n") %>% unlist %>% as.data.frame() %>% t
    #   names(out) <- c("Order","Date","Status")
    #   message(names(out))
    #   return(out)
    #   }) %>% do.call("bind_rows", .)
  }
  wE <- remDr$findElement("xpath", "//header[@class = 'card-heading']/div/*[2]")
  remDr$mouseMoveToLocation(webElement = wE)
  remDr$click()
  remDr$findElement("xpath", "//a[contains(text(),'Stop Loss Order')]")$clickElement()
  remDr$findElement("xpath","//span[@data-testid = 'OrderFormHeading-Sell']")$clickElement()
  shares <- remDr$findElement("xpath","//div[contains(text(), 'Available')]")$getElementText() %>% stringr::str_extract("\\d") %>% as.numeric
  price <- remDr$findElement("xpath","//section[@data-testid = 'ChartSection']/header/h1")$getElementText() %>% unlist %>% stringr::str_extract("\\d+\\.\\d+") %>% as.numeric
  while(!is.numeric(price)){
    Sys.sleep(5)
    price <- remDr$findElement("xpath","//section[@data-testid = 'ChartSection']/header/h1")$getElementText() %>% unlist %>% stringr::str_extract("\\d+\\.\\d+") %>% as.numeric
  }
  orders_s <- Orders %>% dplyr::filter(Platform == "RH" & Tick == s & Order == "B")
  s_max <- Hourly %>% dplyr::filter(Time >= xts::last(orders_s[["Date"]])) %>% .[[s]] %>% max(na.rm = T)
  remDr$findElement("xpath", "//input[@name = 'stop_price']")$sendKeysToElement(list(as.character({s_max - stoploss} %>% round(2))))
  remDr$findElement("xpath", "//input[@data-testid = 'OrderFormRows-Shares']")$sendKeysToElement(list(as.character(shares)))
  remDr$findElement("css", "span.Select-arrow-zone")$clickElement()
  wE <- remDr$findElement("css", "div.Select-menu-outer")
  wE$findChildElement("xpath", "//*[contains(text(),'Good till Canceled')]")$clickElement()
  remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
  remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
  Sys.sleep(4)
  try({remDr$findElement("xpath", "//span[contains(text(),'Done')]/ancestor::button[@type = 'button']")$clickElement()})
  Positions_tsl[[s]][["stopprice"]] <- {s_max - stoploss} %>% round(2)
  save(Positions_tsl, file = "~/R/Quant/Positions_tsl.Rdata")
  # End  Set the trailing stop loss amount using TSLvars
}

# ----------------------- Tue Jun 18 19:31:34 2019 ------------------------#
# TODO(Need to check account history for executed orders and update the Orders sheet appropriately, also remove the previous stoploss from Positions_tsl)


# If length Actions
# Shutdown images
if (lubridate::now() > lubridate::today() %>% paste("17:00:00") %>% lubridate::ymd_hms()) {
  shell_out <- shell("docker ps", intern = T) %>% .[2] %>% stringr::str_split("\\s{2,}") %>% unlist %>% .[1]
  shell(paste0("docker stop ",shell_out))
  shell(paste0("docker rm "), shell_out)
}

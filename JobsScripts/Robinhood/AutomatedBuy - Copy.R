# ----------------------- Wed Jun 12 09:52:14 2019 ------------------------#
message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
source("~/R/Quant/JobsScripts/RunDocker.R")
remDr <- startSelenium()
remDr$navigate("https://robinhood.com/login")

remDr$findElement("xpath", "//input[@name = 'username']")$sendKeysToElement(list(keyring::key_list("RH")[[2]]))
remDr$findElement("xpath", "//input[@name = 'password']")$sendKeysToElement(list(keyring::key_get("RH", username = keyring::key_list("RH")[[2]])))
remDr$findElement("xpath", "//button[@type = 'submit']")$clickElement()
# Login Notification
login_msg <- gmailr::mime(From="sholsen@alumni.emory.edu",
                        To="7818797492@vtext.com",
                        subject = "R: Login") %>%
  gmailr::html_body(body = "Send Two Factor Authentication Code")#%>% gmailr::attach_file("table.png")
gmailr::send_message(buy_msg)
googleAuthR::gar_auth(token = "~/R/Quant/JobsScripts/gmailoauth.rds")
gmailr::gmail_auth("full", id = getOption("googleAuthR.client_id"), secret = getOption("googleAuthR.client_secret"))
msgs <-  gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) after:",lubridate::today()))
auth_code_api <- plumber::plumb("~/R/Quant/JobsScripts/auth_code.R")
auth_code_api$run(port = 8000)
while (is.null(msgs[[1]][["messages"]][[1]][["id"]])) {
  Sys.sleep(1)
  msgs <- gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) after:",lubridate::today()))
}
code <- gmailr::message(msgs[[1]][["messages"]][[1]][["id"]], format = "minimal") %>% .[["snippet"]] %>% stringr::str_match("\\d{6}") 
remDr$findElement("xpath", "//input[@name = 'mfa-code']")$sendKeysToElement(list(as.character(code)))
remDr$findElement("xpath", "//button[@type = 'submit')]")$clickElement()
gmailr::delete_message(msgs[[1]][["messages"]][[1]][["id"]]) # Delete the message after using the confirmation code
orders <- tibble::tribble(~Platform,~Order,~Type,~Tick,~Date,~Qty,~Price,~Exec,~CB,~G.L)
Purchase$price <- vector("numeric",length(Actions))
Purchase$shares <- vector("numeric",length(Actions))
  for (i in seq_along(Actions)) {
    s <- names(Actions)[i]
    url <- httr::parse_url("https://robinhood.com")
    url$path <- paste("stocks",s,sep = "/")
    remDr$navigate(httr::build_url(url))
    Purchase$price[i] <- price <- remDr$findElement("xpath","//section[@data-testid = 'ChartSection']/header/h1")$getElementText() %>% unlist %>% stringr::str_replace("\\$","") %>% as.numeric
    if (!is.null(Purchase$principals)) {
    principal <- Purchase$principals[which(Purchase$buy_syms == s)]
    Purchase$shares[i] <- principal %/% price
    } else {
      Purchase$shares[i] <- Purchase$buy_shares[which(Purchase$buy_syms == s)]
    }
    remDr$findElement("xpath","//input[@data-testid = 'OrderFormRows-Shares']")$sendKeysToElement(list(as.character(Purchase$shares[i])))
    remDr$findElement("xpath","//button[@data-testid = 'OrderFormControls-Review']")$clickElement()
    remDr$findElement("xpath","//button[@data-testid = 'OrderFormControls-Submit']")$clickElement()
    # ----------------------- Mon Jun 17 07:58:55 2019 ------------------------#
    # Record the purchase
    orders[i,] <- c("RH","B","Market",s,lubridate::today(),Purchase$shares[i], price, "Exec", Purchase$shares[i] * Purchase$price[i],"")
  }    
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gsPositions <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=1757293360")
  apply(orders, 1, gs = gsPositions, function(r,gs){
    googlesheets::gs_add_row(gs, ws = "Orders", input = r)
  })
  purrr::map(c("Hourly","Holdings","(Hi)","(Lo)","(Op)","(Vo)","(cP)"),gs = gsPositions, function(.x, gs){
    ticks <- googlesheets::gs_read(gs, ws = .x, range = googlesheets::cell_rows(1),col_names = F) %>% unlist
    if (!any(s %in% ticks)) {
      ticks <- c(ticks,s)
      googlesheets::gs_edit_cells(gs, ws = .x, input = data.frame(ticks, stringsAsFactors = F) %>% t, byrow = T, col_names = F)
    }
  })
  
    # ----------------------- Mon Jun 10 20:16:46 2019 ------------------------#
    # Need to retrieve total amount made on this stock
    
   # For i in Actions - Buy
# Shutdown images
shell(paste0("docker stop ",shell_out))
shell(paste0("docker rm ",shell_out))

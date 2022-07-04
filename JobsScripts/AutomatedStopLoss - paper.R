HDA::startPkgs(c("dplyr","magrittr","AlpacaforR"))
setwd("~/R/Quant/JobsScripts")
args <- (commandArgs(TRUE))
#R CMD BATCH --no-save --no-restore '--args a=1 b=c(2,5,6)' test.R test.out &
##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  live = F
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

message(paste0("Begin AutomatedStopLoss sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," in location: ",getwd()))
try({load(file = "~/R/Quant/JobsScripts/parameters.Rdata")})
try({load(file = "~/R/Quant/JobsScripts/Positions_new.Rdata")})
try({load(file = "~/R/Quant/Positions_tsl.Rdata")})
# ----------------------- Mon Jun 24 13:28:44 2019 ------------------------#
# Initiate Googlesheets and retrieve necessary data
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=1757293360")
historical <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1VG4SSYfxAdyekhSJWlQw4t3S53g_tYCnrL74DfEdGU8/edit#gid=0")
toNum <- function(x){
  x %>% stringr::str_replace_all("\\$|\\,","") %>% as.numeric
}
Orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols) 
# Get Alpaca Orders
all_orders <- AlpacaforR::get_orders(status = "all") 
# ----------------------- Thu Jul 04 15:58:39 2019 ------------------------#
# Add query to orders to get recently filled buy orders and update the sheet 
Orders_unfilled <- Orders %>% dplyr::filter(Platform == "A" & is.na(filled_at) & ( status == "new" | status == "open") & side == "buy")
if (nrow(Orders_unfilled) > 0) {
  Orders_filled <- all_orders %>% dplyr::filter(id %in% Orders_unfilled$id & status == "filled" & side == "buy")
  if (nrow(Orders_filled) > 0) {
    for (i in seq_along(Orders_filled$id)) {
      
      merge_by_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][-c(4:9, 14:15, 22)]
      update_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][c(4:9, 14:15, 22)]
      filled_sales <- Orders_filled[i,] %>% dplyr::left_join(Orders_unfilled[i,] %>% dplyr::select(- update_nms), by = merge_by_nms) %>% dplyr::select(Platform, dplyr::everything()) %>% dplyr::mutate(qty_remain = filled_qty)
      filled_sales[, addtl_cols] <- Orders_unfilled[i,addtl_cols]
      # Replace the row in Google Sheets
      googlesheets::gs_edit_cells(gs, ws = "Orders", input = filled_sales, anchor = paste0("A",which(Orders$id == Orders_filled[i, "id"]) + 1),  col_names = F)
    }
  }

#End update buy orders
#----------------------- Thu Jul 11 15:15:26 2019 ------------------------#

# ----------------------- Thu Jul 11 15:50:52 2019 ------------------------#
#Update recently filled sold orders in Positions_tsl and in the googlesheet
Orders_unfilled <- Orders %>% dplyr::filter(Platform == "A" & is.na(filled_at) & ( status == "new" | status == "open") & side == "sell")
if (nrow(Orders_unfilled) > 0) {
  Orders_filled <- all_orders %>% dplyr::filter(id %in% Orders_unfilled$id & status == "filled" & side == "sell")
  if (nrow(Orders_filled) > 0) {
    for (i in seq_along(Orders_filled$id)) {
      
      merge_by_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][-c(4:9, 14:15, 22)]
      update_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][c(4:9, 14:15, 22)]
      filled_sales <- Orders_filled[i,] %>% dplyr::left_join(Orders_unfilled[i,] %>% dplyr::select(- update_nms), by = merge_by_nms) %>% dplyr::select(Platform, dplyr::everything() )%>% dplyr::mutate(qty_remain = 0)
      filled_sales[, addtl_cols] <- Orders_unfilled[i,addtl_cols]
      # Replace the row in Google Sheets
      googlesheets::gs_edit_cells(gs, ws = "Orders", input = filled_sales, anchor = paste0("A",which(Orders$id == Orders_filled[i, "id"]) + 1),  col_names = F)
    }
  }
}
}
#Update Orders in the global Environment
Orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols)

open_positions <- AlpacaforR::get_positions()


# ----------------------- Mon Aug 05 15:58:06 2019 ------------------------#
# Moved here from Automated stop loss on date above. By minute data is of inconsistent intervals from the Alpaca API. Use Polygon 
#End link buy and sell orders and calculate Gain Loss (GL)
#----------------------- Fri Jul 12 08:46:50 2019 ------------------------#

# ----------------------- Thu Jul 04 10:20:06 2019 ------------------------#
# Get and update the recent prices
# For each position: 
#1. get the recent prices 
#1.5 calculate the changePercent and add that in
#2. write that data to the hd in a cumulative fashion



# recent_prices <- apply(open_positions, 1, function(r){
#   
#   by_min <- get_bars(r[["symbol"]], from = lubridate::today(), timeframe = "minute")[[1]]
#     # Add change percent
#   td_nm <- stringr::str_extract(names(by_min), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
#     by_min[["changePercent"]] <- c(0,zoo::rollapply(by_min, width = 2, function(r){
#       (as.numeric(r[2, "close", drop = T]) - as.numeric(r[1, "close", drop = T])) / as.numeric(r[1, "close", drop = T])
#     }, by.column = F))
#     # Get the previously stored data
#     try({bars <- readr::read_csv(file = paste0("~/R/Quant/PositionData/",r[["symbol"]],"_bymin.csv")) %>% dplyr::mutate_at(dplyr::vars(!!td_nm), ~ lubridate::as_datetime(.))})
#     # If the data exists, then combine it
#     go_bind <- tryCatch({is.data.frame(bars)} ,
#                         error = function(cond) {
#                           message("Here's the original error message:")
#                           message(cond)
#                           return(F)
#                         })
#     
#     if (go_bind) bars <- unique(bind_rows(by_min, bars)) else bars <- by_min
#     readr::write_csv(bars, path = paste0("~/R/Quant/PositionData/",r[["symbol"]],"_bymin.csv"), col_names = T)
#     
#    .cal <- AlpacaforR::get_calendar(from = min(bars[[td_nm]]), to = max(bars[[td_nm]])) 
#    missing_dates <- dplyr::setdiff(.cal[["date"]], bars[[td_nm]] %>% lubridate::as_date() %>% unique) %>% lubridate::as_date()
#    missing_dates <- data.frame(dates = missing_dates, diffs = c(0,missing_dates %>% diff)) %>% dplyr::mutate(cumdiff = cumsum(diffs)) %>% dplyr::mutate(weeks = cumdiff %/% 7) 
#    fill_data  <- missing_dates %>% split(f = .[["weeks"]]) %>% purrr::map(s = r[["symbol"]], function(.x, s){
#      AlpacaforR::get_bars(s, from = min(.x[["dates"]]), to = max(.x[["dates"]]), timeframe = "1Min")[[1]]
#    }) %>% data.table::rbindlist()
#    fill_data[["changePercent"]] <- c(0,zoo::rollapply(by_min, width = 2, function(r){
#      (as.numeric(r[2, "close", drop = T]) - as.numeric(r[1, "close", drop = T])) / as.numeric(r[1, "close", drop = T])
#    }, by.column = F))
#    bars <- rbind.data.frame(bars, fill_data) %>% dplyr::filter(!duplicated(time))
#   return(bars)
# }) %>% setNames(nm = open_positions$symbol)
#End get recent prices and update the repository
#----------------------- Fri Jul 12 09:05:15 2019 ------------------------#


# ----------------------- Thu Jul 04 10:22:44 2019 ------------------------#
# Open orders according to Googlesheets - return the number of shares
open_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "buy" & status == "filled") %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::mutate(Cum.Shares = cumsum(qty)) 
closed_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "sell" & status == "filled") %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::mutate(Cum.Shares = cumsum(qty))
# Get the number of open shares per symbol from the Orders sheet
open_shares <- split(open_orders, open_orders$symbol) %>% purrr::imap(c_o = closed_orders, function(.x, .y, c_o){
  if (any(unique(c_o$symbol) %in% .y)) {
  out <- xts::last(.x)[["qty"]] - c_o %>% dplyr::filter(symbol == .y) %>% xts::last() %>% .[["qty"]]
  } else  out <- xts::last(.x)[["qty"]]
  return(out)
}) %>% purrr::keep(~ .x > 0)


# ----------------------- Thu Jul 04 10:23:53 2019 ------------------------#
# Map over recent prices, the open positions, the Positions_tsl tracking list, and open shares
# 1. 
# 2. 
  #Positions_tsl[open_positions$symbol] <- 
  purrr::pmap(list(.x = recent_prices, .y = split(open_positions, open_positions$symbol), .z = Positions_tsl[open_positions$symbol], .o_s = open_shares[open_positions$symbol]), .f = function(.x, .y, .z, .o_s){
    # Retrieve the most recently set stoploss data from Positions_tsl if possible
    prevTSLtime <- try(purrr::pluck(.z, .y, "prevTSLtime"))
    stoploss <- try(purrr::pluck(.z, .y, "stoploss"))
    stopprice <- try(purrr::pluck(.z, .y, "stopprice"))
    # If successful
     if (HDA::`%n`(prevTSLtime, T) & HDA::`%n%`(stopprice,T) & HDA::`%n%`(stoploss,T)) {
      elapsed_ts <- .x %>% dplyr::filter(Time > prevTSLtime)
      high_price <- max(elapsed_ts[["close"]])
      # If the stock has risen to a point where a new stop loss needs to be set
      if(high_price - stoploss > stopprice) {
        # Then calculate that new stoploss and set it, record that change
        stopprice <- high_price - stoploss
        AlpacaforR::submit_order(.y$symbol, qty = .y$qty, side = "sell", type = "stop", time_in_force = "gtc", stop_price = stopprice, live = live)
      } else if (xts::last(H)[[.y$symbol]] < .z[["stopprice"]]) out <- T else out <- F
    } else out <- NULL
    
    # Instance where that positions is already being tracked
    
    
    
    
    return()
  })
# If there is a discrepancy because an order has been filled, record that filled order in the sheet


if (length(run) < 1) {
  stop("All stop losses are up to date")
} else message(paste0("Stop Losses will be updated for: ", paste0(names(run), collapse = ", ")))



# End Initiate Googlesheets
names(TSLvars) <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist
# ----------------------- Tue Jun 11 11:50:51 2019 ------------------------#
# Set Trailing Stop Losses
# 1. Get alpaca orders
Orders_a <- AlpacaforR::get_orders(status = "all")
Orders_a %<>% dplyr::mutate_at(.vars = dplyr::vars(dplyr::ends_with("at")), dplyr::funs(lubridate::ymd_hms))
Orders_a %<>% dplyr::mutate_at(dplyr::vars(filled_qty, filled_avg_price), dplyr::funs(as.numeric)) 
# Get the orders which are not recorded in the Googlesheet
filled_orders <- dplyr::anti_join(Orders_a %>% dplyr::filter(side == "buy" & status == "filled") %>% dplyr::select(symbol, filled_at, filled_qty),
Orders %>% dplyr::filter(Platform == "A" & Order == "B") %>% dplyr::select(Tick, Date, Qty), by = c(symbol = "Tick", filled_at = "Date", filled_qty = "Qty")) %>% dplyr::left_join(Orders_a, by = c(symbol = "symbol", filled_at = "filled_at", filled_qty = "filled_qty")) %>% dplyr::rename(Order = "side", Type = "order_type", Tick = "symbol", Date = "filled_at", Qty = "filled_qty", Price = "filled_avg_price", Status = "status") %>% dplyr::select(Order, Type, Tick, Date, Qty, Price, Status)

cbind(data.frame(c(rep("A", nrow(filled_orders)))) %>% stats::setNames("Platform"), filled_orders)
# Add the new orders to the Googlesheet for record keeping

googlesheets::gs_add_row(gs, ws = "Orders")

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
# ----------------------- Mon Jun 10 20:16:46 2019 ------------------------#
# Need to retrieve total amount made on this stock

# If length Actions
# Shutdown images
if (lubridate::now() > lubridate::today() %>% paste("17:00:00") %>% lubridate::ymd_hms()) {
  shell_out <- shell("docker ps", intern = T) %>% .[2] %>% stringr::str_split("\\s{2,}") %>% unlist %>% .[1]
  shell(paste0("docker stop ",shell_out))
  shell(paste0("docker rm "), shell_out)
}

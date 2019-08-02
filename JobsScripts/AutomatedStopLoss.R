# ----------------------- Wed Jul 10 13:33:29 2019 ------------------------#
# Packages
HDA::startPkgs(c("dplyr","magrittr","AlpacaforR", "rlang"))
setwd("~/R/Quant/JobsScripts")
# args <- (commandArgs(TRUE))
# #R CMD BATCH --no-save --no-restore '--args a=1 b=c(2,5,6)' test.R test.out &
# ##args is now a list of character vectors
# ## First check to see if arguments are passed.
# ## Then cycle through each element of the list and evaluate the expressions.
# if(length(args)==0){
#   print("No arguments supplied.")
#   ##supply default values
#   live = F
# }else{
#   for(i in 1:length(args)){
#     eval(parse(text=args[[i]]))
#   }
# }
# ----------------------- Thu Jul 11 15:17:19 2019 ------------------------#
#Parameters
try({source(file = "~/R/Quant/JobsScripts/parameters.R")})
tax <- c(`Short-term tax sensitive` = "stts",`First-in first-out` = "fifo", `Last-in first-out` = "lifo",`Max Profit` = "maxp")
tax <- tax[tax == params$taxp]
addtl_cols <- c("Platform", "CB", "GL", "TSL", "live", "SID", "qty_remain")
# ----------------------- Tue Jun 18 19:31:34 2019 ------------------------#
# TODO(Need to check account history for executed orders and update the Orders sheet appropriately, also remove the previous stoploss from Positions_tsl)

message(paste0("Begin AutomatedStopLoss sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," in location: ",getwd()))

# ----------------------- Wed Jul 10 13:33:51 2019 ------------------------#
# Loads
try({load(file = params$paths$Positions_ts)})
try({load(file = params$paths$Positions_tsl)})
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=1757293360")
historical <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1VG4SSYfxAdyekhSJWlQw4t3S53g_tYCnrL74DfEdGU8/edit#gid=0")
# Get Googlesheets orders

Orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols) 

# ----------------------- Thu Jul 04 15:58:39 2019 ------------------------#
# Add query to orders to get recently filled buy orders and update the sheet 
Orders_unfilled <- Orders %>% dplyr::filter(Platform == "A" & is.na(filled_at) & ( status == "new" | status == "open") & side == "buy")
if (nrow(Orders_unfilled) > 0) {
  Orders_filled <- all_orders %>% filter(id %in% Orders_unfilled$id & status == "filled" & side == "buy")
  if (nrow(Orders_filled) > 0) {
    for (i in seq_along(Orders_filled$id)) {
      
      merge_by_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][-c(4:9, 14:15, 22)]
      update_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][c(4:9, 14:15, 22)]
      filled_sales <- Orders_filled[i,] %>% left_join(Orders_unfilled[i,] %>% select(- update_nms), by = merge_by_nms) %>% select(Platform, everything()) %>% mutate(qty_remain = filled_qty)
      filled_sales[, addtl_cols] <- Orders_unfilled[i,addtl_cols]
      # Replace the row in Google Sheets
      googlesheets::gs_edit_cells(gs, ws = "Orders", input = filled_sales, anchor = paste0("A",which(Orders$id == Orders_filled[i, "id"]) + 1),  col_names = F)
    }
  }
}
# ----------------------- Thu Jul 11 15:50:52 2019 ------------------------#
#Update recently filled sold orders in Positions_tsl and in the googlesheet
Orders_unfilled <- Orders %>% dplyr::filter(Platform == "A" & is.na(filled_at) & ( status == "new" | status == "open") & side == "sell")
if (nrow(Orders_unfilled) > 0) {
  Orders_filled <- all_orders %>% filter(id %in% Orders_unfilled$id & status == "filled" & side == "sell")
  if (nrow(Orders_filled) > 0) {
    for (i in seq_along(Orders_filled$id)) {
      
      merge_by_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][-c(4:9, 14:15, 22)]
      update_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][c(4:9, 14:15, 22)]
      filled_sales <- Orders_filled[i,] %>% left_join(Orders_unfilled[i,] %>% select(- update_nms), by = merge_by_nms) %>% select(Platform, everything() )%>% mutate(qty_remain = 0)
      filled_sales[, addtl_cols] <- Orders_unfilled[i,addtl_cols]
      # Replace the row in Google Sheets
      googlesheets::gs_edit_cells(gs, ws = "Orders", input = filled_sales, anchor = paste0("A",which(Orders$id == Orders_filled[i, "id"]) + 1),  col_names = F)
    }
  }
}
#Update Orders in the global Environment
Orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols)
#End update buy orders
#----------------------- Thu Jul 11 15:15:26 2019 ------------------------#
# ----------------------- Thu Jul 04 10:22:44 2019 ------------------------#
# Open orders according to Googlesheets - return the number of shares
open_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "buy" & status == "filled" & qty_remain > 0) %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::summarize(Cum.Shares = sum(filled_qty))
closed_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "sell" & status == "filled" & qty_remain == 0) %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::summarize(Cum.Shares = sum(filled_qty))
# Get the number of open shares per symbol from the Orders sheet
open_shares <- purrr::map2(.x = split(open_orders, open_orders$symbol), .y = split(closed_orders, open_orders$symbol), function(.x, .y){
  if (HDA::go(.y$symbol) & HDA::go(.x$symbol)) {
    out <- .x[["Cum.Shares"]] - .y[["Cum.Shares"]]
  } else  out <- .x[["Cum.Shares"]]
  return(out)
}) %>% purrr::keep(~ .x > 0)
# Get Alpaca Orders
all_orders <- AlpacaforR::get_orders(status = "all") 
# Get the positions from Alpaca
open_positions <- get_positions(live = params$live)
# Get the subset of all Alpaca orders by symbol that googlesheets indicates are open
A_open <- all_orders %>% filter(symbol %in% names(open_shares))

# Run to update all orders from Alpaca
# all_orders %>% filter(status != "canceled") %>% rowwise() %>%  mutate(Platform = "A", CB = CB(filled_qty, filled_avg_price), GL = "", live = F) %>% select(Platform, dplyr::everything()) %>% googlesheets::gs_add_row(gs, ws = "Orders", input = .)




# ----------------------- Thu Jul 11 15:16:12 2019 ------------------------#
# Get all orders from the sheet not already linked to a sale, link them and calculate Gain Loss
Orders_open <- Orders %>% dplyr::filter(Platform == "A" & (!is.character(SID) | qty_remain > 0))
# #For debugging
pmap_test <- list(.o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .Ptsl = Positions_tsl[names(open_shares)], .o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)]) %>% purrr::map(1) %>% list2env()

sell_updates <- purrr::pmap(list(.o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .Ptsl = Positions_tsl[names(open_shares)], .o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)]),addtl_cols = addtl_cols, tax = tax, .f = function(.o_p, .Ptsl, .o_s, .a_o, .o_o, addtl_cols, tax){
  # If the open shares according to google sheets != the open positions according to Alpaca then sell orders must be reconciled
if (.o_s != sum(.o_o$qty)) {
  # Get the previous time at which the stop loss was set
  prev_tsl_time <- .a_o %>% arrange(desc(created_at)) %>% filter(side == "sell" & type == "stop") %>% .[1, "created_at"]
  # Get the alpaca sell orders that were filled since that last TSL was set
  a_o_sell <- .a_o %>% filter(side == "sell" & status == "filled" & created_at >= prev_tsl_time) # Symbol is a given due to mapping
  # Merge the sold order info with the order info already in TSL_placed and add the unique ID to match buy & sell orders
  merge_by_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][-c(4:9, 14:15, 22)]
  update_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][c(4:9, 14:15, 22)]
  filled_tsl <- a_o_sell %>% left_join(.o_o %>% select(-update_nms), by = merge_by_nms) %>% select(Platform, everything())
  filled_tsl$Platform <- "A"
  # account for multiple filled tsl
  for (i in seq_along(filled_tsl$id)) {
    filled_tsl[i, addtl_cols] <- .o_o[.o_o$id == filled_tsl$id[i], addtl_cols]
  }
  
  # ----------------------- Wed Jul 10 10:49:32 2019 ------------------------#
  # First In First Out Gain/Loss Calculation linking buys with sales
  if (tax == "fifo") {
  # get the cumulative sum of unsold bought share orders with it sorted by the oldest create date first, calculate the cum.sum (descending) 
  o_ns_buy <- .o_o %>% filter(side == "buy" & status == "filled") %>% arrange(created_at) %>% mutate(cum.shares = cumsum(qty_remain))
  } else if (tax == "lifo") {
    # if last in first out, sort by created at descending (recent first) and then add the cum.sum (descending)
    o_ns_buy <- .o_o %>% filter(side == "buy" & status == "filled") %>% arrange(desc(created_at)) %>% mutate(cum.shares = cumsum(qty_remain)) 
  } else if (tax == "stts") {
    # if short term tax sensitive, sort desc by the filled_avg_price
    o_ns_buy <- .o_o %>% filter(side == "buy" & status == "filled") %>% arrange(desc(filled_avg_price)) %>% mutate(cum.shares = cumsum(qty_remain)) 
  } else if (tax == "maxp") {
    # if max profit, sort ascending by the filled_avg price
    o_ns_buy <- .o_o %>% filter(side == "buy" & status == "filled") %>% arrange(filled_avg_price) %>% mutate(cum.shares = cumsum(qty_remain))
  }
  # Pre allocate space for results of GL calculation and linking of sells to buys
  sell_update <- list()
  # For each of the sold orders add the ids
  for (i in seq_along(filled_tsl$id)) {
    o_ns_buy %<>% filter((!is.character(SID) | qty_remain > 0))
    # Create an index vector for the previous buy orders sold when this order filled
      soldbuy_orders_ind <- seq(1, which(o_ns_buy$cum.shares >= filled_tsl$filled_qty[i])[1])
    #fill the sid for the buy orders
    for (si in soldbuy_orders_ind) {
      if (!is.character(o_ns_buy[si, "SID"])) {
    o_ns_buy[soldbuy_orders_ind, "SID"] <- filled_tsl$id[i]
      } else {
        o_ns_buy[si, "SID"] <- paste(o_ns_buy[si, "SID", drop = T], filled_tsl$id[i], sep = ",")
      }
    }
    # fill the sid for the sale orders
    filled_tsl[i, "SID"] <- filled_tsl$id[i]
    b <- o_ns_buy %>% filter(stringr::str_detect(SID,stringr::coll(filled_tsl$id[i]))) # Filter for just the sold buy orders and remove the cum.shares 
    # ----------------------- Wed Jul 10 10:59:43 2019 ------------------------#
    # Map over the sold and buy orders by their matching SID
    e <- new.env()
    e$sq <- filled_tsl$filled_qty[i]
    b_df <- purrr::pmap_dfr(.l = list(id = b$id, fq = b$filled_qty, CB = b$CB, qr = b$qty_remain, GL = b$GL), sp = filled_tsl$filled_avg_price[i], e = e, function(id, fq, CB, qr, GL, sp, e){
        # get the GL per by 
      if (!is.numeric(GL) | is.na(GL)) {
        GL <- sp - CB / fq
      } else { GL <- sp - CB / fq + GL # Account for a GL from previously split buy order
      }
      out_qr <- ifelse(qr - e$sq <= 0, 0, qr - e$sq) 
      # Update the sell quantity
      assign("sq", ifelse(e$sq - qr <= 0, 0, e$sq - qr), envir = e)
      return(data.frame(GL = GL, qty_remain = out_qr))
      })
    # update the values in B & filled_tsl for later updating of spreadsheet
    filled_tsl[i, c("GL", "qty_remain")] <- data.frame(GL = sum(b_df$GL), qty_remain = e$sq)
    b[, c("GL", "qty_remain")] <- b_df
    sell_update[[1]] <- rbind.data.frame(filled_tsl, b %>% select(-cum.shares))
    } # for i loop over id
  sell_updates <- bind_rows(sell_update)
} # Close if statement
  
  if (HDA::go(sell_updates)) out <- sell_updates else out <- NULL
return(out)
})
# Update Positions_tsl & googlesheets
for (l in seq_along(sell_updates)) {
  if (HDA::go(sell_updates[[l]])) {
  for (i in 1:nrow(sell_updates[[l]])) {
  # Update Positions_tsl
  Positions_tsl[[unique(sell_updates[[l]]$symbol)]][["orders"]][sell_updates[[l]][i, "id"] == Positions_tsl[[unique(sell_updates[[l]]$symbol)]][["orders"]]$id, ] <- sell_updates[[l]][i, ]
  # Update Googlesheets
  googlesheets::gs_edit_cells(gs, ws = "Orders", input = sell_updates[[l]][i, ], anchor = paste0("A",which(Orders$id == sell_updates[[l]][i, "id", drop = T]) + 1),  col_names = F)
  }
  }
}
#End link buy and sell orders and calculate Gain Loss (GL)
 #----------------------- Fri Jul 12 08:46:50 2019 ------------------------#

# ----------------------- Thu Jul 04 10:20:06 2019 ------------------------#
# Get and update the recent prices
# For each position: 
#1. get the recent prices 
#1.5 calculate the changePercent and add that in
#2. write that data to the hd in a cumulative fashion



recent_prices <- apply(open_positions, 1, function(r){
  by_min <- get_bars(r[["symbol"]], from = lubridate::today(), timeframe = "minute")[[1]]
    # Add change percent
    by_min[["changePercent"]] <- c(0,zoo::rollapply(by_min, width = 2, function(r){
      (as.numeric(r[2, "close", drop = T]) - as.numeric(r[1, "close", drop = T])) / as.numeric(r[1, "close", drop = T])
    }, by.column = F))
    # Get the previously stored data
    try(bars <- readr::read_csv(file = paste0("~/R/Quant/PositionData/",r[["symbol"]],"_bymin.csv")) %>% mutate_at(vars(Time), funs(lubridate::as_datetime)))
    # If the data exists, then combine it
    go_bind <- tryCatch({is.data.frame(bars)} ,
                        error = function(cond) {
                          message("Here's the original error message:")
                          message(cond)
                          return(F)
                        })
    
    if (go_bind) bars <- unique(bind_rows(by_min, bars)) else bars <- by_min
    readr::write_csv(bars, path = paste0("~/R/Quant/PositionData/",r[["symbol"]],"_bymin.csv"), col_names = T)
    # This operation is too expensive. Data repository will need to reside locally.
    # # on Fridays update the google sheet
    # historical <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1VG4SSYfxAdyekhSJWlQw4t3S53g_tYCnrL74DfEdGU8/edit#gid=0")
    # # If no spreadsheet exists to contain the data, create one
    # if (!any(stringr::str_detect(googlesheets::gs_ws_ls(historical), r[["symbol"]]))) {
    #   googlesheets::gs_ws_new(historical, ws_title = r[["symbol"]], col_extent = length(bars))
    #   historical <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1VG4SSYfxAdyekhSJWlQw4t3S53g_tYCnrL74DfEdGU8/edit#gid=0")
    #   anchor <- 1
    # } else {
    #   # otherwise read the existing data
    #   gs_bars <- googlesheets::gs_read(historical, ws = r[["symbol"]])
    #   # Add the data to the sheet
    #   hist_bars <- rbind(gs_bars, bars) # combine existing data to new data
    #   hist_bars <- hist_bars[!duplicated(hist_bars), ] # remove duplicated
    #   anchor <- nrow(gs_bars + 1)
    # }
    # go_add <- try(is.data.frame(hist_bars))
    # if (go_add) input <- hist_bars else input <- bars
    # 
    # googlesheets::gs_edit_cells(historical, ws = r[["symbol"]], input = input, anchor = paste0("A", anchor))
  return(bars)
}) %>% setNames(nm = open_positions$symbol)
#End get recent prices and update the repository
 #----------------------- Fri Jul 12 09:05:15 2019 ------------------------#


# ----------------------- Thu Jul 04 10:23:53 2019 ------------------------#
# Set the trailing stop losses 
# Map over recent prices, the open positions, the Positions_tsl tracking list, open shares, and all orders
# @return The modified Position_tsl 
# for Debugging
list(.r_p = recent_prices[names(open_shares)], .o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .Ptsl = Positions_ts[names(open_shares)]) %>% purrr::map(1) %>% list2env(envir = .GlobalEnv)

purrr::pmap(list(.r_p = recent_prices[names(open_shares)], .o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .Ptsl = Positions_tsl[names(open_shares)]), .f = function(.r_p, .o_p, .Ptsl){
  
  # Retrieve unsold buy positions from Positions_tsl & by the symbol, which API (live or not), and TSL types and respective open shares associated with each
  tsl_orders_cumshares <- .Ptsl[["orders"]] %>% filter(side == "buy" & status == "filled" & qty_remain > 0) %>% group_by(symbol, live, TSL) %>% summarize(TSL_shares = sum(qty_remain)) 
  # Get all previous stop losses
  prev_tsl <- .Ptsl[["orders"]] %>% filter(side == "sell" & stringr::str_detect(status, "new|open") & qty_remain > 0) %>% mutate(cum.shares = cumsum(qty_remain))
  # Get the current price
  price <- AlpacaforR::get_poly_last_price(ticker = .o_p$symbol)
  #TODO 
  if (nrow(prev_tsl) > 0) { # if there are previous stoplosses
    
  }
    set_tsl <- .Ptsl[["orders"]] %>% filter(side == "buy" & status == "filled" & qty_remain > 0) %>% mutate(cum.shares = cumsum(qty_remain))
    
  if (nrow(set_tsl) > 0) { # if there are new buy orders for which tsl need to be set
      for (i in 1:nrow(set_tsl)) { # loop along the rows of set_tsl
      purrr::pmap_dfr(list(.s = set_tsl$symbol[i], .c_a = set_tsl$created_at[i], .q = set_tsl$qty_remain[i], .p = set_tsl$filled_avg_price[i], .tsl = set_tsl$TSL[i], .live = set_tsl$live[i]), r_p = recent_prices, tslv = params$TSLvars, function(.s, .c_a, .q, .p, .tsl, .live, r_p, tslv){
        #TODO calculate stop loss here. Calculation of stop loss will require a pre-allocated price data.frame of at most 84 days prior
      tsl_amt <- attr(tslv, "tsl_amt")
      if (stringr::str_detect(.tsl, "tslsd")) {
        .args <- tslv[[.tsl]]
        stoploss <- tsl_amt()
      } else if (stringr::str_detect(.tsl, "tslret")) {
        retro <- tslv[[.tsl]]
        stoploss <- r_p[[.s]] %>% filter(Time >= {max(r_p[[.s]][["Time"]]) - lubridate::days(retro[1])}) %>% .[["close"]] %>% range() %>% diff() %>% {. * retro[2]}
      } else if (stringr::str_detect(.tsl, "tslp")) {
        stoploss <- .p * tslv[[.tsl]]
      }
      stopprice <- .p - stoploss
        # Place the order
      TSL_placed <- AlpacaforR::submit_order(ticker = .s, qty = .q, side = "sell", type = "stop", time_in_force = "gtc", stop_price = stopprice, live = .live) %>% AlpacatoR_order_mutate()
        
    })
  }
}

  
    
    stoploss <- try(purrr::pluck(.z, .y, "stoploss")) #TODO needs to set these values for multiple open TSL types
    stopprice <- try(purrr::pluck(.z, .y, "stopprice")) #TODO needs to set these values for multiple open TSL types
    # If a previous stoploss has not been set for a given position
    if (!(HDA::go(prevTSLtime) & HDA::go(stopprice) & HDA::go(stoploss))) {
      TSL_placed <- purrr::pmap_dfr(tsl_orders_cumshares, TSLvars = params$TSLvars, price = AlpacaforR::get_poly_last_price(ticker = .y$symbol)$price, r_p = .x, function(.x, TSLvars, price, r_p){
        tsl_type <- .x[["TSL"]] # Get the TSL type
      # Set the stoploss based on the type
      if (stringr::str_detect(tsl_type, "tsla")) {
        tsla <- TSLvars[[tsl_type]]
        stoploss <- tsla(r_p[["close"]])
      } else if (stringr::str_detect(tsl_type, "retro")) {
        retro <- TSLvars[[tsl_type]]
        stoploss <- r_p %>% filter(Time >= {max(r_p[["Time"]]) - lubridate::days(retro[1])}) %>% .[["close"]] %>% range() %>% diff() %>% {. * retro[2]}
      } else if (stringr::str_detect(tsl_type, "tslp")) {
        stoploss <- price * TSLvars[[tsl_type]]
      }
        stopprice <- price - stoploss
      # Place the order
        TSL_placed <- AlpacaforR::submit_order(ticker = .x$symbol, qty = .x$TSL_shares, side = "sell", type = "stop", time_in_force = "gtc", stop_price = stopprice, live = .x[["live"]])
        # Change NULL values to NA for coercion to data.frame
        TSL_placed[TSL_placed %>% purrr::map_lgl(is.null)] <- NA
        # Coerce to data.frame
        TSL_placed %<>% as.data.frame %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("at")), ~ lubridate::ymd_hms(., tz = Sys.timezone())) %>% dplyr::mutate_at(dplyr::vars(qty, filled_qty, filled_avg_price), ~toNum) %>% cbind.data.frame(TSL = tsl_type, live = .x[["live"]])
      return(TSL_placed)
      })
      # If TSL_placed exists
      if (HDA::go(.z[["TSL_placed"]])) {
        # Add the TSL_placed to the existing rows otherwise create a new data.frame
        .z[["TSL_placed"]] <- bind_rows(.z[["TSL_placed"]],TSL_placed)
      } else .z[["TSL_placed"]] <- TSL_placed
    } else if (HDA::go(prevTSLtime) & HDA::go(stopprice) & HDA::go(stoploss)) {
      elapsed_ts <- .x %>% dplyr::filter(Time > prevTSLtime)
      high_price <- max(elapsed_ts[["close"]])
      low_price <- min(elapsed_ts[["close"]])
      # If the stock has risen to a point where a new stop loss needs to be set
      if(high_price - stoploss > stopprice) {
        # Then calculate that new stoploss and set it, record that change
        stopprice <- high_price - stoploss
        placed <- AlpacaforR::submit_order(.y$symbol, qty = .y$qty, side = "sell", type = "stop", time_in_force = "gtc", stop_price = stopprice, live = live)
      } else if (low_price < stopprice) {
        # Instance where a sell is detected
        
      }
     
      .z[["prevTSLtime"]] <- lubridate::now()
    
    }
    
    return(d)
  })



save(Positions_tsl, file = params$paths$Positions_tsl)



# End Initiate Googlesheets
names(params$TSLvars) <- purrr::pmap(.l = list(.x = params$TSLvars, .y = names(params$TSLvars), .z = seq_along(params$TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist



  
  
  # if (is.null(mdl)) {
  #       # Load model for Stock to predict gain and mix type
  #       try({
  #         fn <- paste0(s,"_cl.Rdata")
  #         load(file = paste0("~/R/Quant/MdlBkp/",fn))
  #       })
  #       mdl <- get0(paste0(s,"_cl"))
  #       # If not, and a model is present
  #       all_tsl <- c(Positions_tsl[[s]][["TSL"]][["rowname"]],predict(mdl[["Mix_type"]], newdata = xts::last(Positions_new[[s]])),predict(mdl[["Gain_type"]], newdata = xts::last(Positions_new[[s]])))
  #       if (any(duplicated(all_tsl))) {
  #         # If cum.returns are tied, but mix type or gain type confirms either TSL type, then choose the one that is confirmed.
  #         tsl_type <- HDA::Mode(all_tsl)
  #       } else {
  #         tsl_type <- HDA::mode(predict(mdl[["Mix_type"]], newdata = xts::last(Positions_new[[s]])))# If no confirmation from type predictions, then just use mix type
  #       }
  #       
  #     }
  #   } else {
  #     tsl_type <- Positions_tsl[[s]][["TSL"]][["rowname"]]
  #   }
  # 
  
  
 



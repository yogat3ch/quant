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
# Get the positions from Alpaca. Split it by the open shares according to the sheet, and name it according to those open_shares (such that the pmap below has named list items)
open_positions <- AlpacaforR::get_positions(live = params$live) %>% split(., .[["symbol"]]) %>% .[names(open_shares)] %>% setNames(names(open_shares))
# Get the subset of all Alpaca orders by symbol that googlesheets indicates are open
A_open <- all_orders %>% dplyr::filter(symbol %in% names(open_shares))

# Run to update all orders from Alpaca
# all_orders %>% filter(status != "canceled") %>% rowwise() %>%  mutate(Platform = "A", CB = CB(filled_qty, filled_avg_price), GL = "", live = F) %>% select(Platform, dplyr::everything()) %>% googlesheets::gs_add_row(gs, ws = "Orders", input = .)




# ----------------------- Thu Jul 11 15:16:12 2019 ------------------------#
# Get all orders from the sheet not already linked to a sale, link them and calculate Gain Loss
Orders_open <- Orders %>% dplyr::filter(Platform == "A" & (!is.character(SID) | qty_remain > 0))
# #For debugging
pmap_test <- list(.o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)]) %>% purrr::map(1) %>% list2env(envir = .GlobalEnv)

sell_updates <- purrr::pmap(list(.o_p = open_positions, .o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)]), tax = tax, .f = function(.o_p, .o_s, .a_o, .o_o, tax){
  # If the open shares according to google sheets != the open positions according to Alpaca then sell orders must be reconciled
if (.o_s != sum(.o_p$qty_remain)) {
  # Get the previous time at which the stop loss was set
  prev_tsl_time <- .a_o %>% arrange(desc(created_at)) %>% filter(side == "sell" & type == "stop") %>% .[1, "created_at"]
  # Get the alpaca sell orders that were filled since that last TSL was set
  a_o_sell <- .a_o %>% filter(side == "sell" & status == "filled" & created_at >= prev_tsl_time) # Symbol is a given due to mapping
  # Merge the sold order info with the order info already in TSL_placed and add the unique ID to match buy & sell orders
  merge_by_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][-c(4:9, 14:15, 22)]
  update_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][c(4:9, 14:15, 22)]
  filled_tsl <- a_o_sell %>% left_join(.o_o %>% select(-update_nms), by = merge_by_nms) %>% select(Platform, everything())
  
  
  
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
    # Fill other values
    filled_tsl$Platform <- "A"
    filled_tsl$TSL <- unique(b$TSL)
    filled_tsl$live <- unique(b$live)
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
    # Update CB
    filled_tsl[i, "CB"] <- mean(b$CB)
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
  # Update Googlesheets
  googlesheets::gs_edit_cells(gs, ws = "Orders", input = sell_updates[[l]][i, ], anchor = paste0("A",which(Orders$id == sell_updates[[l]][i, "id", drop = T]) + 1),  col_names = F)
  }
  }
}



# ----------------------- Thu Jul 04 10:23:53 2019 ------------------------#
# Set the trailing stop losses 
# Map over recent prices, the open positions, the Positions_tsl tracking list, open shares, and all orders
# @return The modified Position_tsl 
# for Debugging
.orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols) %>% dplyr::filter(Platform == "A")
list(.o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .orders = split(.orders, .orders$symbol)[names(open_shares)]) %>% purrr::map(1) %>% append(list(TSLvars = params$TSLvars)) %>% list2env(envir = .GlobalEnv)

purrr::pmap(list(.o_p = split(open_positions, open_positions$symbol)[names(open_shares)], .orders = split(.orders, .orders$symbol)[names(open_shares)]), TSLvars = params$TSLvars, gs = gs, .f = function(.o_p, .orders, TSLvars, gs){
  
  
  # Get all previous stop losses
  prev_tsl <- .orders %>% dplyr::filter(side == "sell" & stringr::str_detect(status, "new|open")) %>% dplyr::mutate(cum.shares = cumsum(qty_remain))
  # ----------------------- Wed Aug 14 08:27:38 2019 ------------------------#
  # TODO Cancel all previous stop losses
  
  
  # ----------------------- Mon Aug 05 16:47:38 2019 ------------------------#
  # Update local data
  # Get the Historical Data filename from the HD
  local_fn <- list.files(path = "~/R/Quant/PositionData", pattern = paste0(.o_p$symbol,"\\d{4}\\-\\d{2}\\-\\d{2}\\_\\d{4}\\-\\d{2}\\-\\d{2}\\.csv"), full.names = T)
  if (HDA::go(local_fn)) {
  # Get the last bar recorded
  last_bar <- stringr::str_extract_all(local_fn, "\\d{4}\\-\\d{2}\\-\\d{2}")[[1]] %>% lubridate::ymd() %>% max() 
  # Load the historical data
  .data <- readr::read_csv(file = local_fn)
  # Get the column with the date or time
  td_nm <- stringr::str_extract(colnames(.data), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
  # If it's uppercase from earlier versions convert it to lower
  if (grepl("T", td_nm)) {
    n.td_nm <- tolower(td_nm)
    .data %<>% dplyr::rename(!!n.td_nm := !!td_nm)
  }
  # Get the columns corresponding to the get_bars call
  .data %<>% dplyr::select("time", "open", "high", "low", "close", "volume")
  
  } else last_bar <- lubridate::today() - lubridate::weeks(12) # If theres no data then set the previous date to 1 quarter ago
  # Retrieve the updated data
  new_bars <- AlpacaforR::get_bars(ticker = .o_p$symbol, from = last_bar, to = lubridate::today())[[1]]
  if (HDA::go(.data)) { # if the data exists on the HD
    # Combine it with the new data
    new_data <- rbind.data.frame(.data[ - nrow(.data), ], new_bars)
  } else new_data <- new_bars # If it doesn't just make the new_data
  # Save the new data
  readr::write_csv(new_data,  path = paste0("~/R/Quant/PositionData/",.o_p$symbol, min(new_data[["time"]]),"_", max(new_data[["time"]]), ".csv"))
  # Delete the old data
  if (HDA::go(local_fn)) file.remove(local_fn)
  # End update local data
   #----------------------- Mon Aug 05 16:48:04 2019 ------------------------#
   
  # Retrieve unsold buy positions from Orders & by the symbol, which API (live or not), and TSL types and respective open shares associated with each
  tsl_orders_cumshares <- .orders %>% dplyr::filter(side == "buy" & status == "filled" & qty_remain > 0) %>% dplyr::group_by(symbol, live, TSL) %>% dplyr::summarize(TSL_shares = sum(qty_remain)) 
  
  
    
  if (nrow(tsl_orders_cumshares) > 0) { # if there are new buy orders for which tsl need to be set
    for (i in 1:nrow(tsl_orders_cumshares)) {
    # Retrieve the orders themselves (such that the data can be filtered according to when the purchase was made)
    tsl_orders <- .orders %>% dplyr::filter(side == "buy" & status == "filled" & qty_remain > 0 & TSL == dplyr::ungroup(tsl_orders_cumshares)[i, "TSL", drop = T])
    tsl_orders$peaks <- purrr::map_dbl(tsl_orders$filled_at, .new_data = new_data, function(.x, .new_data) {
      .new_data %>% dplyr::filter(time >= .x) %>% .[["high"]] %>% max
    })
    # group by tsl type, live, and the peaks
    tsl_orders %<>% dplyr::group_by(TSL, live, peaks) %>% dplyr::summarise(qty = sum(qty)) %>% dplyr::ungroup()
    if (any(stringr::str_detect(tsl_orders$TSL, "^tslp"))) {
    # find the peaks over the timeframes since the purchase (for tslp)
    
    tslp <- function(TSL, peaks) {
      stringr::str_extract(TSL, "[\\d\\.]+") %>% as.numeric %>% {peaks - . * peaks}
    }
     tsl_orders %<>% dplyr::mutate(SL = tslp(TSL,peaks))
      
    } else {
    # calculate tsl based on other types
      tsl_orders %<>% dplyr::group_by(TSL, live) %>% dplyr::summarise(qty = sum(qty)) %>% dplyr::ungroup()
      # Subtract the stop loss amounts from the peaks
      tsl_orders %<>% dplyr::mutate(SL = purrr::pmap_dbl(list(TSL, live, qty, peaks, filled_at), .data = new_data, TSLvars = TSLvars, function(TSL, live, qty, peaks, filled_at, .data, TSLvars) {
        tsl_amt <- attr(TSLvars, "tsl_amt")
        .args <- TSLvars[TSL]
        .args[[1]]$dtref <- filled_at
        amt <- tsl_amt(.data = .data, .args = .args[1])
        out <- peaks - amt
        return(out)
      })
      )
    tsl_final <- tsl_orders %>% dplyr::group_by(symbol, SL, live, TSL) %>% dplyr::summarise(qty = sum(qty))
    submitted_orders <- purrr::pmap_dfr(list(symbol = tsl_final$symbol, SL = tsl_final$SL, live = tsl_final[["live"]], qty = tsl_final$qty, TSL = tsl_final$TSL), function(symbol, SL, live, qty, TSL) {
      submitted_order <- AlpacaforR::submit_order(ticker = symbol, qty = qty, side = "sell", type = "stop", time_in_force = "gtc", stop_price = SL, live = live) %>% params$AlpacatoR_order_mutate(live = live, tsl = TSL)
      })
      }
    }
    # ----------------------- Mon Aug 12 10:59:30 2019 ------------------------#
    # Add Orders to Googlesheet
    apply(submitted_orders, 1, gs = gs, function(r, gs){
      googlesheets::gs_add_row(gs, ws = "Orders", input = r)
    })
  }
})





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
  
  
 



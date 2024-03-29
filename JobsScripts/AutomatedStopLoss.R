# ----------------------- Wed Jul 10 13:33:29 2019 ------------------------#
# Packages
HDA::startPkgs(c("dplyr","magrittr","AlpacaforR", "rlang"))
setwd("~/R/Quant/JobsScripts")
params$gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
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
try({load(file = params$paths$best_tsl)})



# Get Googlesheets orders
Orders <- googlesheets::gs_read(params$gs, ws = "Orders", col_types = params$Orders_cols) 
# Get Alpaca Orders
all_orders <- AlpacaforR::get_orders(status = "all") 

# ----------------------- Thu Jul 04 15:58:39 2019 ------------------------#
# Add query to orders to get recently filled buy orders and update the sheet 
Orders_unfilled <- Orders %>% dplyr::filter(Platform == "A" & is.na(filled_at) & ( status == "new" | status == "open") & side == "buy")
if (nrow(Orders_unfilled) > 0) {
  Orders_filled <- all_orders %>% dplyr::filter(id %in% Orders_unfilled$id & status == "filled" & side == "buy")
  if (nrow(Orders_filled) > 0) {
    for (i in seq_along(Orders_filled$id)) {
      # Get the names of the common values to be joined on
      merge_by_nms <- names(Orders_filled)[purrr::map2_lgl(Orders_filled,{Orders_unfilled[1, - c(names(Orders_unfilled) %in% dplyr::setdiff(names(Orders_unfilled),names(Orders_filled)) %>% which)] %>% unclass},~ .x == .y) %>% which]
      update_nms <- names(Orders_filled)[names(Orders_filled) %in% names(Orders_unfilled)][- {purrr::map2_lgl(Orders_filled,{Orders_unfilled[1, - c(names(Orders_unfilled) %in% dplyr::setdiff(names(Orders_unfilled),names(Orders_filled)) %>% which)] %>% unclass},~ .x == .y) %>% which}]
      filled_sales <- Orders_filled[i,] %>% dplyr::left_join(Orders_unfilled[i,] %>% dplyr::select(- update_nms), by = merge_by_nms) %>% dplyr::select(Platform, dplyr::everything()) %>% dplyr::mutate(qty_remain = filled_qty)
      filled_sales[, addtl_cols] <- Orders_unfilled[i,addtl_cols]
      # Replace the row in Google Sheets
      googlesheets::gs_edit_cells(params$gs, ws = "Orders", input = filled_sales, anchor = paste0("A",which(Orders$id == Orders_filled[i, "id"]) + 1),  col_names = F)
    }
  }
}


#Update Orders in the global Environment
Orders <- googlesheets::gs_read(params$gs, ws = "Orders", col_types = params$Orders_cols)
#End update buy orders
#----------------------- Thu Jul 11 15:15:26 2019 ------------------------#
# ----------------------- Thu Jul 04 10:22:44 2019 ------------------------#
# Open orders according to Googlesheets - return the number of shares
open_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "buy" & status == "filled" & qty_remain > 0) %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::summarize(Cum.Shares = sum(filled_qty))
closed_orders <- Orders %>% dplyr::filter(Platform == "A" & side == "sell" & status == "filled" & qty_remain == 0) %>% dplyr::group_by(symbol) %>% dplyr::arrange(dplyr::desc(filled_at), .by_group = T) %>% dplyr::summarize(Cum.Shares = sum(filled_qty))
# Get the number of open shares per symbol from the Orders sheet
open_shares <- purrr::map2(.x = split(open_orders, open_orders$symbol), .y = split(closed_orders, open_orders$symbol), function(.x, .y){
  if (HDA::go(".y$symbol") & HDA::go(".x$symbol")) {
    out <- .x[["Cum.Shares"]] - .y[["Cum.Shares"]]
  } else  out <- .x[["Cum.Shares"]]
  return(out)
}) %>% purrr::keep(~ .x > 0)

if (!HDA::go(open_shares)) stop(paste0(lubridate::now(), ": No open shares. Stopping."))
# Get the positions from Alpaca. Split it by the open shares according to the sheet, and name it according to those open_shares (such that the pmap below has named list items)
open_positions <- AlpacaforR::get_positions(live = params$live) 
if (HDA::go(open_positions)) {
  open_positions %<>% split(., .[["symbol"]]) %>% .[names(open_shares)] %>% stats::setNames(names(open_shares))
}  
# Get the subset of all Alpaca orders by symbol that googlesheets indicates are open
A_open <- all_orders %>% dplyr::filter(symbol %in% names(open_shares)) %>% dplyr::select(- replaced_at, - replaced_by, - replaces)

# Run to update all orders from Alpaca
# all_orders %>% filter(status != "canceled") %>% rowwise() %>%  mutate(Platform = "A", CB = CB(filled_qty, filled_avg_price), GL = "", live = F) %>% select(Platform, dplyr::everything()) %>% googlesheets::gs_add_row(gs, ws = "Orders", input = .)

# ----------------------- Thu Jul 11 15:16:12 2019 ------------------------#
# Get all orders from the sheet not already linked to a sale, link them and calculate Gain Loss
#TODO 2019-08-18 0805 Revise to make use of Active sheet where trailing stop losses will be tracked
Orders_open <- Orders %>% dplyr::filter(Platform == "A" & (!is.character(SID) | qty_remain > 0))
# #For debugging
#pmap_test <- list(.o_p = open_positions, .o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)],.o_p = open_positions[names(open_shares)]) %>% purrr::map(1) %>% append(tax) %>% append(list(.o_p = open_positions[names(open_shares)])) %>% list2env(envir = .GlobalEnv)

sell_updates <- purrr::pmap(list(.o_s = open_shares, .a_o = split(A_open, A_open$symbol)[names(open_shares)], .o_o = split(Orders_open, Orders_open$symbol)[names(open_shares)]), .o_p = open_positions[names(open_shares)],  tax = tax, .f = function(.o_s, .a_o, .o_o, .o_p, tax){
  # If the open shares according to google sheets != the open positions according to Alpaca then sell orders must be reconciled
  o_p.lgl <- vector()
  o_p.lgl[1] <- isTRUE(try({.o_p[[unique(.o_o$symbol)]]$qty_remain != .o_s}))
  o_p.lgl[2] <- !HDA::go(".o_p")
if (any(o_p.lgl)) {
  message("Updating orders...")
  # Get the previous time at which the stop loss was set
  prev_tsl_time <- .a_o %>% dplyr::arrange(dplyr::desc(created_at)) %>% dplyr::filter(side == "sell" & type == "stop") %>% .[1, "created_at"]
  # Get the alpaca sell orders that were filled since that last TSL was set
  a_o_sell <- .a_o %>% dplyr::filter(side == "sell" & status == "filled" & created_at >= prev_tsl_time) # Symbol is a given due to mapping
  # Merge the sold order info with the order info already in TSL_placed and add the unique ID to match buy & sell orders
  merge_by_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][- {names(a_o_sell) %in% .m_n %>% which}]
  update_nms <- names(a_o_sell)[names(a_o_sell) %in% names(.o_o)][- {names(a_o_sell) %in% .m_n %>% which}]
  filled_tsl <- a_o_sell %>% dplyr::left_join(.o_o %>% dplyr::select(- .u_n), by = .m_n) %>% dplyr::select(Platform, dplyr::everything()) %>% dplyr::mutate_at(dplyr::vars(Platform), ~ {.  <-  "A"})
  
  # ----------------------- Sun Aug 18 07:05:12 2019 ------------------------#
  #TODO Buy orders need to be matched to sell orders by the trailing stop loss type
  
  # ----------------------- Wed Jul 10 10:49:32 2019 ------------------------#
  # First In First Out Gain/Loss Calculation linking buys with sales
  if (tax == "fifo") {
  # get the cumulative sum of unsold bought share orders with it sorted by the oldest create date first, calculate the cum.sum (descending) 
  o_ns_buy <- .o_o %>% dplyr::filter(side == "buy" & status == "filled") %>% dplyr::arrange(created_at) %>% dplyr::mutate(cum.shares = cumsum(qty_remain))
  } else if (tax == "lifo") {
    # if last in first out, sort by created at descending (recent first) and then add the cum.sum (descending)
    o_ns_buy <- .o_o %>% dplyr::filter(side == "buy" & status == "filled") %>% dplyr::arrange(dplyr::desc(created_at)) %>% dplyr::mutate(cum.shares = cumsum(qty_remain)) 
  } else if (tax == "stts") {
    # if short term tax sensitive, sort desc by the filled_avg_price
    o_ns_buy <- .o_o %>% dplyr::filter(side == "buy" & status == "filled") %>% dplyr::arrange(dplyr::desc(filled_avg_price)) %>% dplyr::mutate(cum.shares = cumsum(qty_remain)) 
  } else if (tax == "maxp") {
    # if max profit, sort ascending by the filled_avg price
    o_ns_buy <- .o_o %>% dplyr::filter(side == "buy" & status == "filled") %>% dplyr::arrange(filled_avg_price) %>% dplyr::mutate(cum.shares = cumsum(qty_remain))
  }
  # Pre allocate space for results of GL calculation and linking of sells to buys
  sell_update <- list()
  # For each of the sold orders add the ids
  for (i in seq_along(filled_tsl$id)) {
    o_ns_buy %<>% dplyr::filter((!is.character(SID) | qty_remain > 0))
    # Create an index vector for the previous buy orders sold when this order filled
      soldbuy_orders_ind <- seq(1, which(o_ns_buy$cum.shares >= filled_tsl$filled_qty[i])[1])
    #fill the sid for the buy orders
    for (si in 1:soldbuy_orders_ind) {
      if (!is.character(o_ns_buy[si, "SID"])) {
    o_ns_buy[soldbuy_orders_ind, "SID"] <- filled_tsl$id[i]
      } else {
        o_ns_buy[si, "SID"] <- paste(o_ns_buy[si, "SID", drop = T], filled_tsl$id[i], sep = ",")
      }
    }
    # fill the sid for the sale orders
    filled_tsl[i, "SID"] <- filled_tsl$id[i]
    b <- o_ns_buy %>% dplyr::filter(stringr::str_detect(SID,stringr::coll(filled_tsl$id[i]))) # Filter for just the sold buy orders and remove the cum.shares
    # Fill other values
    # TODO 2019-08-18 0706 TSL should not be simply filled, must be matched between sell & buy orders.
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
    sell_update[[1]] <- rbind.data.frame(filled_tsl, b %>% dplyr::select(-cum.shares))
    } # for i loop over id
  sell_updates <- dplyr::bind_rows(sell_update)
} # Close if statement
  
  if (HDA::go("sell_updates")) out <- sell_updates else out <- NULL
return(out)
})
# Update Positions_tsl & googlesheets
if (any(purrr::map_lgl(sell_updates, ~ HDA::go(.x)))) {
  sell_updates %<>% subset(subset = purrr::map_lgl(sell_updates, ~ HDA::go(.x)))
  for (l in seq_along(sell_updates)) {
    for (i in 1:nrow(sell_updates[[l]])) {
  # Update Googlesheets
      if (any(sell_updates[[l]][i, "id"] == purrr::keep(Orders$id, ~!is.na(.)))) { # if the ID of the order is already recorded in the sheet
  googlesheets::gs_edit_cells(params$gs, ws = "Orders", input = sell_updates[[l]][i, ], anchor = paste0("A",which(Orders$id == sell_updates[[l]][i, "id", drop = T]) + 1),  col_names = F)
      } else { # If the order is not recorded in the sheet, add it
  googlesheets::gs_add_row(params$gs, ws = "Orders", input = sell_updates[[l]][i, ])        
      }
    }
  }
}

# Load Positions_new
Positions_new <- params$getPositions_new(names(open_shares), .retrieveAll = T)

# ----------------------- Thu Jul 04 10:23:53 2019 ------------------------#
# Set the trailing stop losses 
# Map open positions from Alpaca (already split), open shares from the sheet, and all orders from Alpaca
# TODO Use Active sheet to record active stop losses
Orders <- googlesheets::gs_read(params$gs, ws = "Orders", col_types = params$Orders_cols)

# for Debugging
list(.o_p = open_positions[names(open_shares)], .orders = split(Orders %>% dplyr::filter(Platform == "A"), Orders %>% dplyr::filter(Platform == "A") %>% .[["symbol"]])[names(open_shares)], .o_s = open_shares, .p_n = Positions_new) %>% purrr::map(1) %>% append(list(TSLvars = params$TSLvars)) %>% list2env(envir = .GlobalEnv)





purrr::pwalk(list(.o_p = open_positions[names(open_shares)], .orders = split(Orders, Orders$symbol)[names(open_shares)], .o_s = open_shares, .p_n = Positions_new), TSLvars = params$TSLvars, gs = params$gs, Orders = Orders, .f = function(.o_p, .orders, .o_s, TSLvars, gs, Orders){
  
  if(!HDA::go(".o_p")) {
    message(paste0(unique(.o_p$symbol),": returning NULL"))
    return(NULL)
    }
  message(unique(.o_p$symbol))
  

  
  # Setting stop losses
  # Get all previous stop losses recorded in the sheet
  # There should not be discrepancies between these 
  prev_tsl.orders <- .orders %>% dplyr::filter(side == "sell" & stringr::str_detect(status, "new|open")) %>% dplyr::mutate(cum.shares = cumsum(qty))
  prev_tsl.alpaca <- AlpacaforR::get_orders(ticker = unique(.o_p$symbol), status = "all")
  if (HDA::go(prev_tsl.alpaca)) {
    prev_tsl.alpaca %<>% dplyr::filter(side == "sell") %>% dplyr::filter(is.na(filled_at))
  }
   
  
  # Retrieve the filled buy orders themselves (such that the data can be filtered according to when the purchase was made)
  tsl_orders <- .orders %>% dplyr::filter(side == "buy" & status == "filled" & qty_remain > 0) %>% dplyr::mutate(cum.shares = cumsum(qty_remain))
  tsl_orders$peaks <- purrr::map_dbl(tsl_orders$filled_at, .new_data = .p_n, function(.x, .new_data) {
    .new_data %>% dplyr::filter(time >= .x) %>% .[["high"]] %>% max
  })
  # group by tsl type, live, and the peaks
  tsl_sum <- tsl_orders %>% dplyr::group_by(TSL, live, peaks) %>% dplyr::summarise(qty = sum(qty)) %>% dplyr::ungroup()
  if (any(stringr::str_detect(tsl_sum$TSL, "^tslp"))) {
    # find the peaks over the timeframes since the purchase (for tslp)
    
    tslp <- function(TSL, peaks) {
      stringr::str_extract(TSL, "[\\d\\.]+") %>% as.numeric %>% {peaks - . * peaks}
    }
    tsl_sum %<>% dplyr::mutate(SL = tslp(TSL,peaks))
    
  } else {
    # calculate tsl based on other types
    tsl_sum %<>% dplyr::group_by(TSL, live) %>% dplyr::summarise(qty = sum(qty)) %>% dplyr::ungroup()
    # Subtract the stop loss amounts from the peaks
    tsl_sum %<>% dplyr::mutate(SL = purrr::pmap_dbl(list(TSL, live, qty, peaks, filled_at), .dat = .p_n, TSLvars = TSLvars, function(TSL, live, qty, peaks, filled_at, .dat, TSLvars) {
      .args <- TSLvars[TSL]
      .args[[1]]$dtref <- filled_at
      amt <- qf::tsl_amt(.data = .dat, .args = .args[1])
      out <- peaks - amt
      return(out)
    })
    )
  }
  
  # submitted_orders <- AlpacaforR::submit_order(ticker = "CGC", qty = 1, side = "sell", type = "stop", time_in_force = "gtc", stop_price = 31, live = F) %>% params$AlpacatoR_order_mutate(live = F, tsl = "tslp_0.24")
  # canceled_orders <- AlpacaforR::cancel_order(ticker_id = "CGC", live = F)
  # ----------------------- Wed Aug 14 08:27:38 2019 ------------------------#
  # TODO Cancel all previous stop losses
    message("Canceling previous stop loss orders")
    if (nrow(prev_tsl.orders) > 0) {
    #Cancel the previous orders in the Alpaca API
    canceled_orders <- apply(prev_tsl.orders, 1, function(r){
      AlpacaforR::cancel_order(ticker_id = r[["id"]])
    })  
    }
    # Set new orders
    message("Creating new orders...")
    tsl_final <- tsl_sum %>% dplyr::group_by(SL, live, TSL) %>% dplyr::summarise(qty = sum(qty)) %>% dplyr::mutate(symbol = unique(.o_p$symbol))
    submitted_orders <- purrr::pmap_dfr(list(symbol = tsl_final$symbol, SL = tsl_final$SL, live = tsl_final[["live"]], qty = tsl_final$qty, TSL = tsl_final$TSL), function(symbol, SL, live, qty, TSL) {
      submitted_order <- AlpacaforR::submit_order(ticker = symbol, qty = qty, side = "sell", type = "stop", time_in_force = "gtc", stop_price = SL, live = live) %>% params$AlpacatoR_order_mutate(live = live, tsl = TSL)
    })
    # If there were previous stop losses recorded
    if (nrow(prev_tsl.orders) > 0) {
      for (i in 1:nrow(prev_tsl.orders)) { 
    # Loop through and replace those orders with new orders
        googlesheets::gs_edit_cells(ss = gs, ws = "Orders", input = submitted_order[i, ], anchor = paste0("A",which(Orders$id == prev_tsl.orders[i, "id", drop = T]) + 1), byrow = T) 
      }
    }
    # If there are new orders in excess of the previously set stop losses
    if (nrow(prev_tsl.orders) < nrow(submitted_orders)) {
      for (i in {nrow(prev_tsl.orders) + 1}:nrow(submitted_orders)) {
    # ----------------------- Mon Aug 12 10:59:30 2019 ------------------------#
    # Add Orders to Googlesheet
    message("Adding new orders to googlesheet...")
    googlesheets::gs_add_row(ss = gs, ws = "Orders", input = submitted_orders[i, ])
      }
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
  
  
 



# ----------------------- Wed Jun 12 09:52:14 2019 ------------------------#
HDA::startPkgs(c("magrittr", "AlpacaforR", "dplyr"))
message(paste0("Begin AutomatedBuy sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))


# ----------------------- Wed Jul 10 13:32:46 2019 ------------------------#
# Loads
try({source(file = "~/R/Quant/JobsScripts/parameters.R")})
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=1757293360")
Orders <- googlesheets::gs_read(gs, ws = "Orders", col_types = params$Orders_cols)

# ----------------------- Wed Jul 10 13:32:38 2019 ------------------------#
# Pre-allocation
Purchase$price <- vector("numeric",length(Purchase$buy_syms))
if(HDA::go(Purchase$buy_shares)) Purchase$buy_shares <- vector("numeric",length(Purchase$buy_syms))
Purchase$orders <- data.frame()
# ----------------------- Wed Jul 10 13:33:13 2019 ------------------------#
# For each of the buy instructions

  for (i in seq_along(Purchase$buy_syms)) {
    # Get the symbol as character
    s <- Purchase$buy_syms[i]
    # Get the last price
    price <- AlpacaforR::get_poly_last_price(s)
    # If a Principal is provided
    if (HDA::go("Purchase$principals")) {
    Purchase$buy_shares[i] <- Purchase$principals[i] %/% price$price
    } 
    # ----------------------- Mon Jul 15 13:54:30 2019 ------------------------#
    # Account for wash sale rule - See parameters.R AlpacatoR_order_mutate for rest of code
    #TODO Need to link wash sale buys with losing sales to prevent double counting via the same order_type id concatenation.
    Orders_ws <- Orders %>% filter(symbol == s & side == "sell" & GL < 0 & filled_at >= {lubridate::now() - lubridate::days(30)} & !stringr::str_detect(order_type, "ws"))
    if (nrow(Orders_ws) > 0) {
      ws <- abs(sum(Orders_ws$GL))
      wso <- paste0(Orders_ws$id, collapse = ",")
    } else {
      ws <- 0
      wso <- ''
      }
    #End Wash sale rule
     #----------------------- Mon Jul 15 16:40:59 2019 ------------------------#
    # ----------------------- Fri Jun 28 18:56:36 2019 ------------------------#
    # Make the purchase
    Purchase$placed <- AlpacaforR::submit_order(Purchase$buy_syms[i], qty = Purchase$buy_shares[i], side = "buy", type = "market", time_in_force = "gtc", live = Purchase$live[i]) %>% AlpacatoR_order_mutate(tsl = Purchase$tsl[i], live = Purchase$live[i], ws = ws, wso = wso)
    
  # ----------------------- Tue Jul 02 15:27:43 2019 -----------------------#
  # Update the googlesheet meta-record
  googlesheets::gs_add_row(gs, ws = "Orders", input = Purchase$placed)
  }  
# Add any symbols not currently present in the tracking sheets to the sheets.
purrr::map(c("Hourly","Holdings","(Hi)","(Lo)","(Op)","(Vo)","(cP)"), gs = gs, s = Purchase$buy_syms, function(.x, gs, s){
  symbols <- googlesheets::gs_read(gs, ws = .x, range = googlesheets::cell_rows(1),col_names = F) %>% unlist
  if (!any(s %in% symbols)) {
    symbols <- c(symbols,s[!s %in% symbols])
    googlesheets::gs_edit_cells(gs, ws = .x, input = data.frame(symbols, stringsAsFactors = F) %>% t, byrow = T, col_names = F)
  }
})
   
    
   

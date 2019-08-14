HDA::startPkgs(c("magrittr","tidyverse"))
setwd("~/R/Quant/JobsScripts")
message(paste0("Begin AutomatedStopLoss sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
# TODO(could use optimization to only retrieve positions_new once per day)
try({load("~/R/Quant/JobsScripts/parameters.Rdata")}) # principal and TSLvars
try({load(params$paths$Positions_new)})
# ----------------------- Mon Jun 10 15:37:01 2019 ------------------------#
# Apply Models to the new data
message(paste0("Load: ",params$paths$Positions_ts))
load(file = params$paths$Positions_ts)
if (xts::is.xts(Positions_ts[[1]])) {
  OOTbegin <- lubridate::ymd({Positions_ts[[1]] %>% time %>% max})
} else {
  OOTbegin <- lubridate::ymd({Positions_ts[[1]] %>% tibbletime::get_index_col() %>% max})
}
message("Load Positions_tsl")
load(file = params$paths$Positions_tsl)
if (HDA::go("Positions_new")) {
  if (xts::is.xts(Positions_new[[1]])) {
    loadP_new <- time(Positions_new[[1]]) %>% max(na.rm = T) < {lubridate::today()} 
  }else {
    loadP_new <- tibbletime::get_index_col(Positions_new[[1]]) %>% max(na.rm = T) < {lubridate::today()}
  }
} else loadP_new <- T
if (loadP_new) {
# ----------------------- Mon Jun 10 13:56:31 2019 ------------------------#
# Get New Positions
gsPositions <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
shts <- purrr::map(.x = c('Holdings', '(Hi)' , '(Lo)', '(Op)', '(Vo)', '(cP)'), gs = gsPositions, .f =  function(.x, gs){
  out <- googlesheets::gs_read(gs, ws = .x, trim_ws = T)
  out$Date %<>% lubridate::ymd_hms()
  Sys.sleep(1.5)
  return(out)
})

Positions_v <- names(Positions_ts)
names(Positions_v) <- names(Positions_ts)
Positions_new <- purrr::map(Positions_v, shts = shts, function(.x, shts){
  clm <- purrr::map(shts, purrr::pluck, .x)
  clm <- do.call('cbind', clm) %>% as.data.frame
  names(clm) <- c("close","high","low","open","volume","changePercent")
  toNum <- function(x){
    x %>% stringr::str_replace_all("\\$|\\,","") %>% as.numeric
  }  
  clm %<>% dplyr::mutate_if(.pred = funs(is.factor),funs(toNum))
  out <- cbind.data.frame(Time = shts[[1]][["Date"]], clm, stringsAsFactors = F) 
  out %<>% na.omit
  #Fill dates for proper windowing
  # First create a vector with omitted days (weekends) added in ymd_hms format
  times <- c(out$Time,  format(seq(out$Time %>% min(na.rm = T),out$Time %>% max(na.rm = T), "1 days"), "%Y-%m-%d")[!format(seq(out$Time %>% min(na.rm = T),out$Time %>% max(na.rm = T), "1 days"), "%Y-%m-%d") %in% format(out$Time, "%Y-%m-%d")] %>% stringr::str_replace("$", " 17:00:00 UTC") %>% lubridate::ymd_hms()) %>% sort
  
  out <- dplyr::left_join(data.frame(Time = times), out, by = "Time") %>% zoo::na.locf() %>% tibbletime::tbl_time(index = "Time")
  out$Dec_date <- tibbletime::get_index_col(out) %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))}
  attr(out,"Sym") <- .x
  return(out)
})
}
# ----------------------- Mon Jun 10 14:29:11 2019 ------------------------#
# Get the most recent data from googlesheets every hour
gsPositions <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
ss <- googlesheets::gs_read(gsPositions, ws = "Personal", range = cellranger::cell_cols("A:L")) %>% cbind.data.frame(Time = lubridate::today(), .) %>% select(Time, Symbol,high = Hi,low = Lo, close = Price, open = Open, volume = Volume, changePercent) %>% mutate_at(vars(Time), funs(lubridate::as_date))

toNum <- function(x){
  x %>% stringr::str_replace_all("\\$|\\,","") %>% as.numeric
}
ss %<>% mutate_at(vars(c("high","low","close","open","volume","changePercent")),~toNum)
ss %<>% na.omit
#Fill dates for proper windowing
ss$Dec_date <- ss$Time %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))}
# Combine the most recent data with the historical daily data
dat <- purrr::map2(Positions_new, names(Positions_new), ss = ss, function(.x, .y, ss){
  # Remove the last row of Positions_new if it's an earlier synchronization of the same day, and rbind the most recent data minus the symbol column
  out <- rbind.data.frame(.x[.x$Time != ss$Time[1], ], ss[ss$Symbol == .y,] %>% dplyr::select(-Symbol))
  return(out)
})
dat <- Positions_new
save(Positions_new, file = params$paths$Positions_new)
# Set the signal windows desired
wind  <-  c(weeks = 7, moonphase = 7*2, mooncycle = 7*4, quarters = 7*4*3)
# ----------------------- Mon Jun 10 13:57:35 2019 ------------------------#
# Add Independent variables
source("~/R/Quant/JobsScripts/AddIVstoData.R", local = T)
# Add Dependent variables
source("~/R/Quant/JobsScripts/AddRVstoNewData.R", local = T)

best_tsl <- Positions_tsl
source("~/R/Quant/JobsScripts/ApplyModelstoNewData.R", local = T)
Actions <- purrr::map(dat, function(.x){
  out <- xts::last(.x) %>% dplyr::select(dplyr::ends_with("pred")) %>% unlist %>% as.character %>% as.numeric
  names(out) <- xts::last(.x) %>% dplyr::select(dplyr::ends_with("pred")) %>% unlist %>% names
  
  return(out)
}) %>% purrr::map2(.y = Positions_tsl[names(dat)], function(.x, .y){
  tsls <- .y[["TSL"]][["rowname"]]
  pcts <- .y[["TSL"]][["pct"]]
  out <- list()
  for (i in seq_along(tsls)) {
      out[[i]] <- c(Pred = .x[paste0(tsls[i],"_pred")], Pct = pcts[i])
     
    
  }
  if (out[[i]] > 0) return(out) else return(NULL)
   
}) %>% purrr::keep(.p = ~ !is.null(.x))

# Send Buy Options via Text
try({load(file = "~/R/Quant/JobsScripts/prev_buyoption.Rdata")})
if (all(class(prev_buyoption) != "try-error")) {
buy_option <- {lubridate::now() - prev_buyoption} %>% lubridate::as.period() > lubridate::hours(1)
prev_buyoption <- lubridate::now()
save(prev_buyoption, file = "~/R/Quant/JobsScripts/prev_buyoption.Rdata")
} else {
  prev_buyoption <- lubridate::now()
  save(prev_buyoption, file = "~/R/Quant/JobsScripts/prev_buyoption.Rdata")
  buy_option <- T
}

if (buy_option) {


if (length(Actions) > 0) {
  HDA::startPkgs(c("htmltools"))
  googleAuthR::gar_auth(token = "~/R/Quant/JobsScripts/gmailoauth.rds")
  gmailr::gmail_auth("full", id = getOption("googleAuthR.client_id"), secret = getOption("googleAuthR.client_secret"))
  
  bdy <- paste0(tags$table(
    purrr::map2(Actions, names(Actions), dat = dat, function(.x, .y, dat){
      out <- purrr::map(.x, tick = .y, function(.x, tick) {
        out <- rbind(names(.x) %>% stringr::str_replace("^Pred\\.","") %>% stringr::str_replace("_rv_pred", ""),.x %>% round(3)) %>% apply(2, paste, collapse = ": ")
        out <- c(Sym = tick,out)}) %>% do.call(rbind,.)  %>% cbind(.,Price = paste0("Price: ", xts::last(dat[[.y]])[["close"]]))
    }) %>% do.call(rbind,.) %>% apply(1, function(r){
      if (stringr::str_extract(r[2],"\\d{1,3}\\.\\d{1,3}$") %>% as.numeric > stringr::str_extract(r[3],"\\d{1,3}\\.\\d{1,3}$") %>% as.numeric) {
        r <- purrr::modify(r,function(.x)paste0("<strong>",.x,"</strong>")) %>% unlist
        
      }
      return(r)
    }) %>% t %>% htmlTable::htmlTable() %>% htmltools::HTML()
  ))
  # For later use to send via Twilio
  # cat(bdy, file = "~/R/Quant/JobsScripts/table.html")
  # webshot::webshot("table.html", file = "table.png", selector = "table")
  buy_msg <- gmailr::mime(From="sholsen@alumni.emory.edu",
             To="7818797492@vtext.com;sholsen@alumni.emory.edu",
             subject = "R: Buy Options") %>%
    gmailr::html_body(body = bdy)#%>% gmailr::attach_file("table.png")
  gmailr::send_message(buy_msg)

  # Give a 30 min window to solicit a response, checking each minute
  timer_start <- lubridate::now()
  timer_end <- timer_start + lubridate::duration(30, "minutes")
  response <- gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))
  while (lubridate::now() < timer_end & is.null(response[[1]][["messages"]][[1]][["id"]])) {
    response <- gmailr::messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))
    Sys.sleep(60)
  }
  if (!is.null(response[[1]][["messages"]][[1]][["id"]])) {
  # Retrieve Instructions
  response <- gmailr::message(response[[1]][["messages"]][[1]][["id"]])
  quant_lbl <- gmailr::labels()[[1]] %>% .[[purrr::map(., function(.x)stringr::str_detect(.x,"Quant") %>% any) %>% unlist %>% which]] %>% purrr::pluck("id")
   Purchase <- list()
    if (response %>% purrr::pluck("snippet") %>% stringr::str_detect("None")) {
      gmailr::modify_message(response$id,add_labels = quant_lbl, remove_labels = "INBOX")
      stop("No Buy Orders Issued", .call = F)
    } else {
    Purchase$buy_syms <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=Syms?\\:\\s?)([A-Za-z\\,]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both")
    Purchase$principals <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=Amt\\:\\s?)([0-9\\,]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    #Purchase$principals <- ifelse(!is.na(principals), principals, NULL)
    Purchase$buy_shares <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=Shares\\:\\s?)([0-9\\,]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    # Get the Trailing Stop Loss types
    Purchase$tsl <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=TSL\\:\\s?)([A-Za-z\\_\\-\\.0-9\\,]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    # If tsl is not specified, then find the best option
    if (!HDA::go(Purchase$tsl)){
      Purchase$tsl <- purrr::map_chr(Purchase$buy_sms, Actns = Actions, P_tsl = Positions_tsl, function(.x, Actns, P_tsl){
        message(paste0("Finding best TSL for ",.x))
        tsl <- Actns[[.x]]
        if (HDA::go(tsl)) {
          out <- stringr::str_extract(names(tsl[[which.max(purrr::map_dbl(tsl, ~ purrr::pluck(.x, 1)))]])[1],"(?<=Pred\\.)([\\w\\d\\_\\.\\-]+)(?=\\_rv\\_pred)")
        } else {
        tsl <- P_tsl[.x][["TSL"]]
          out <- tsl[which.max(tsl$Cum.Returns), "rowname"]
        }
        return(out)
      })
        
    } else if (any(is.na(purrr::map_lgl(Purchase$tsl)))) {
      Purchase$tsl <- purrr::map_chr(Purchase$buy_sms[is.na(purrr::map_lgl(Purchase$tsl))], Actns = Actions, P_tsl = Positions_tsl, function(.x, Actns, P_tsl){
        message(paste0("Finding best TSL for ",.x))
        tsl <- Actns[[.x]]
        if (HDA::go(tsl)) {
          out <- stringr::str_extract(names(tsl[[which.max(purrr::map_dbl(tsl, ~ purrr::pluck(.x, 1)))]])[1],"(?<=Pred\\.)([\\w\\d\\_\\.\\-]+)(?=\\_rv\\_pred)")
        } else {
          tsl <- P_tsl[.x][["TSL"]]
          out <- tsl[which.max(tsl$Cum.Returns), "rowname"]
        }
        return(out)
      })
    }
    # Get whether the order is for the live account or for the paper account.
    Purchase$live <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=Live\\:\\s?)([A-Za-z\\_\\-\\.0-9\\,]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    # If live is not specified, set all to F (paper)
    if (!HDA::go(Purchase$live)){
    Purchase$live <- rep(F, length(Purchase$buy_syms))  
    } else if (any(is.na(purrr::map_lgl(Purchase$live)))) {
      #Otherwise if some values are set and others are not, set those as F
      Purchase$live[is.na(purrr::map_lgl(Purchase$live))] <- F
    }
  }
    # Move out of inbox to quant folder
    gmailr::modify_message(response$id,add_labels = quant_lbl, remove_labels = "INBOX")
    
  } else {
    message("Response Timer Timed Out: No Buys issued")
  }
} else { message(paste0("No predicted gains as of ",lubridate::now()))}
# If instructions 
if (length(Purchase$buy_syms) > 0) {
  Actions <- Actions[Purchase$buy_syms]
  source("~/R/Quant/JobsScripts/AutomatedBuy.R", local = T)
}
} # If previous buy options was offered more than 6 hours ago
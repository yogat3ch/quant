HDA::startPkgs(c("magrittr","tidyverse","rlang"))
setwd("~/R/Quant/JobsScripts")
message(paste0("Begin AutomatedStopLoss sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
params$gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")# ----------------------- Sun Aug 18 08:53:47 2019 ------------------------#
# Load Files
# TODO(could use optimization to only retrieve positions_new once per day)
try({source("~/R/Quant/JobsScripts/parameters.R")}) # principal and TSLvars
Personal <- googlesheets::gs_read(params$gs, ws = "Personal")
Positions_v <- Personal$Symbol[- c(1,2)]
names(Positions_v) <- Positions_v
load(file = params$paths$Positions_tsl) # Positions tsl for optimal TSL
#TODO 2019-08-18 0902 Rework for new format of Positions_tsl 
#TODO Load PositionData and form into Position_ts save Positions_ts to dat
# ----------------------- Mon Aug 05 16:47:38 2019 ------------------------#
# Update local data
dat <- params$getPositions_new(Positions_v, params)
# End update local data
#----------------------- Mon Aug 05 16:48:04 2019 ------------------------#
best_tsl <- purrr::map2(Positions_tsl[names(dat)], .y = dat, tslv = params$TSLvars, function(.x, .y, tslv){
  .x %<>% dplyr::mutate_at(dplyr::vars(tsl),~ stringr::str_replace(.,"\\_rv$",""))
  # Get the TSL with the most cumulative returns and with the most possible limit gain for a given period
  out <- list(tsl_types = rbind.data.frame(dplyr::arrange(.x, dplyr::desc(Cum.Returns)) %>% .[1, c("tsl", "pct", "75%max", "100%max", "Cum.Returns")],
                                           dplyr::arrange(.x, dplyr::desc(`100%max`)) %>% .[1, c("tsl", "pct", "75%max", "100%max", "Cum.Returns")]))
  # Are there tsl not already represented in the data?
  existing_tsl <- {out$tsl_types$tsl[out$tsl_types$tsl %in% {names(.y)[stringr::str_which(names(.y),"rv$")] %>% stringr::str_replace(.,"\\_rv$","")}] %>% length} > 0
  # If there are and add is true (defaults to true) then add only those that don't already exist
  if (existing_tsl){
    # filter the table for those tsl not already in the data
    out$tsl_types %<>% dplyr::filter(!tsl == existing_tsl) }
  # Return the TSLvars parameters for each of those TSL types
  out$tslv <- tslv[out$tsl_types[["tsl"]]]
  return(out)
})
# ----------------------- Mon Jun 10 13:57:35 2019 ------------------------#
# Add Independent variables
source("~/R/Quant/JobsScripts/AddIVstoData.R", local = T)
# Add Dependent variables
source("~/R/Quant/JobsScripts/AddRVstoNewData.R", local = T)
# ----------------------- Mon Jun 10 15:37:01 2019 ------------------------#
# Apply Models to the new data
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
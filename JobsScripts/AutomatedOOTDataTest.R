HDA::startPkgs(c("magrittr","tidyverse","rlang"))
setwd("~/R/Quant/JobsScripts")
message(paste0("Begin AutomatedStopLoss sourced from ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
# ----------------------- Sun Aug 18 08:53:47 2019 ------------------------#
# Load Files
# TODO(could use optimization to only retrieve positions_new once per day)
try({source("~/R/Quant/JobsScripts/parameters.R")}) # principal and TSLvars
Personal <- googlesheets::gs_read(params$gs, ws = "Personal")
Positions_v <- Personal$Symbol[- c(1,2)]
names(Positions_v) <- Positions_v
load(file = params$paths$best_tsl) # Positions tsl for optimal TSL
# ----------------------- Mon Aug 05 16:47:38 2019 ------------------------#
# Update local data
.retrieveAll <- T
dat <- params$getPositions_new(Positions_v, params)
# End update local data
# ----------------------- Mon Jun 10 13:57:35 2019 ------------------------#
# Add Independent variables
source("~/R/Quant/JobsScripts/AddIVstoData.R", local = T)
# Add dependent variables because dummyVars depends on it.
source("~/R/Quant/JobsScripts/AddRVstoNewData.R", local = T)
# ----------------------- Mon Jun 10 15:37:01 2019 ------------------------#
# Apply Models to the new data
source("~/R/Quant/JobsScripts/ApplyModelstoNewData.R", local = T)
library(htmltools)
preds <- purrr::map(dat, function(.x){
  .out <- utils::head(.x, 10) %>% dplyr::select(dplyr::ends_with("pred")) %>% dplyr::mutate_if(.pred = ~ is.numeric(.), ~ round(.,4))})

# Create logical subset of those stocks where the prediction exceeds the percentage gain threshold
.thresh <- purrr::map2(dat, best_tsl[names(dat)], function(.x, .y){
  .df <- utils::head(.x, 10)
  .lgl <- purrr::map2_lgl(.y[['tsl_types']][['pct']], .y[['tsl_types']][['tsl']], .df = .df, function(.x, .y, .df){
    any(xts::last(.df)[[paste0(.y,"_rv_pred")]] > .x)
  }) 
  return(.lgl)
}) %>% purrr::map(~ any(.)) %>% unlist
# Create a vector of RMSE values of the predictions vs the actual tsl values
.rmse <- purrr::map2(dat[.thresh], best_tsl[names(dat[.thresh])], function(.x, .y){
  .df <- utils::head(.x, 10)
  .rmse <- purrr::map_dbl(.y[['tsl_types']][['tsl']], .df = .df, function(.x, .df){
    caret::RMSE(.df[[paste0(.x,"_rv_pred")]], .df[[paste0(.x, "_rv")]])
  }) 
  return(.rmse)
})
.sort <- .rmse %>% purrr::map(~ dplyr::pluck(., 1)) %>% unlist %>% sort() %>% names

if (length(.sort) > 0) {
  HDA::startPkgs(c("htmltools"))
  
  bdy <- tags$body(
    tags$table(
      tags$tr(tags$thead(purrr::map(names(preds[.sort]), ~ tags$td(.,colspan = 2)))),
      tags$tr(purrr::map(best_tsl[.sort], ~ .x[['tsl_types']] %>% dplyr::select(tsl, pct, `75%max`)) %>% purrr::map2(.rmse[.sort], ~ htmltools::tagList(tags$td(paste0(.x[1,1], ": ", .x[1,2], "\n 75%: ", round(.[1,3],2),"\n RMSE: ",round(.y[1],3))), tags$td(paste0(.[2,1], ": ", .[2,2], "\n75%: ", round(.[2,3],2),"\n RMSE: ",round(.y[2],3))))))
      ,
      tags$tr(preds[.sort] %>% purrr::map(~ utils::head(.x, 10)) %>% purrr::map(~ tags$td(htmltools::HTML(knitr::kable(.x, format = "html")), colspan = 2)))
    )
  )
  # For later use to send via Twilio
  # cat(bdy, file = "~/R/Quant/JobsScripts/table.html")
  # webshot::webshot("table.html", file = "table.png", selector = "table")
  gmailr::gm_auth_configure(path = "C:/Users/Administrator/Documents/R/gmailr/credentials.json")
  buy_msg <- gmailr::mime(From="sholsen@alumni.emory.edu",
             To="7818797492@vtext.com;sholsen@alumni.emory.edu",
             subject = "R: Buy Options") %>%
    gmailr::html_body(body = htmltools::doRenderTags(bdy))#%>% gmailr::attach_file("table.png")
  gmailr::send_message(buy_msg)

  # Give a 30 min window to solicit a response, checking each minute
  timer_start <- lubridate::now()
  timer_end <- timer_start + lubridate::duration(30, "minutes")
  response <- gmailr::gm_messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))
  while (lubridate::now() < timer_end & is.null(response[[1]][["messages"]][[1]][["id"]])) {
    response <- gmailr::gm_messages(search = paste0("from:(7818797492@vzwpix.com) label:(INBOX) after:",lubridate::today()))
    Sys.sleep(60)
  }
  if (!is.null(response[[1]][["messages"]][[1]][["id"]])) {
  # Retrieve Instructions
  response <- gmailr::gm_message(response[[1]][["messages"]][[1]][["id"]])
  quant_lbl <- gmailr::gm_labels()[[1]] %>% .[[purrr::map(.,.f = function(.x)stringr::str_detect(.x,"Quant") %>% any) %>% unlist %>% which]] %>% purrr::pluck("id")
   Purchase <- list()
    if (response %>% purrr::pluck("snippet") %>% stringr::str_detect("None")) {
      gmailr::gm_modify_message(response$id,add_labels = quant_lbl, remove_labels = "INBOX")
      stop("No Buy Orders Issued", .call = F)
    } else {
    Purchase$buy_syms <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=[Ss]yms?\\:\\s?)([A-Za-z\\,\\s]+)(?=[Ss]h)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both")
    Purchase$principals <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=[Aa]mt\\:\\s?)([0-9\\,\\s]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    #Purchase$principals <- ifelse(!is.na(principals), principals, NULL)
    Purchase$buy_shares <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=[Ss]hares\\:\\s?)([0-9\\,\\s]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    # Get the Trailing Stop Loss types
    Purchase$tsl <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=[Tt][Ss][Ll]\\:\\s?)([A-Za-z\\_\\-\\.0-9\\,\\s]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.numeric
    # Get a boolean as to whether a purchase should be live or not
    Purchase$live <- response %>% purrr::pluck("snippet") %>% stringr::str_match(stringr::regex("(?<=[Ll]ive\\:\\s?)([A-Za-z\\_\\-\\.0-9\\,\\s]+)", ignore_case = T)) %>% as.character %>% .[!duplicated(.)] %>% stringr::str_split(",") %>% unlist %>% trimws(which = "both") %>% as.logical()
    # If no tsl are specified, then use the first for all
    if (!HDA::go(Purchase$tsl)) {
      Purchase$tsl <- purrr::map2_chr(Purchase$buy_syms, Purchase$tsl, .ptsl = best_tsl, .f = function(.x, .y, .ptsl){
        message(paste0("Getting first TSL for ",.x))
        .out <- .ptsl[[.x]][['tsl_types']][['tsl']][1]
        return(.out)
      })
    } else if (any(purrr::map_lgl(Purchase$tsl, ~ is.na(.x) | is.numeric(.x)))) {
      # # For debugging:
      # list(.ptsl = best_tsl, .x = Purchase$buy_syms[1], .y = Purchase$tsl[1]) %>% list2env(envir = .GlobalEnv)
       # rm(.ptsl, .x, .y)
      Purchase$tsl <- purrr::map2_chr(.x = Purchase$buy_syms, .y = Purchase$tsl, .ptsl = best_tsl, .f = function(.x, .y, .ptsl){
        message(paste0("Finding best TSL for ",.x))
        .tsl <- .ptsl[[.x]]
        # If a best_tsl exists and a TSL is not specified
        if (HDA::go(.tsl) & is.na(.y)) {
          # Use the first - the one with the highest cumulative returns
          .out <- .tsl[['tsl_types']][['tsl']][1]
        } else {
          # Otherwise use the tsl specified
          .out <- .tsl[["tsl_types"]][['tsl']][.y]
        }
        return(.out)
      })
      
    } 
    # Get whether the order is for the live account or for the paper account.
    
    # If live is not specified, set all to F (paper)
    if (!HDA::go(Purchase$live)){
    Purchase$live <- rep(F, length(Purchase$buy_syms))  
    } else if (any(is.na(Purchase$live))) {
      #Otherwise if some values are set and others are not, set those as F
      Purchase$live[is.na(Purchase$live)] <- F
    }
  }
    # Move out of inbox to quant folder
    gmailr::gm_modify_message(response$id,add_labels = quant_lbl, remove_labels = "INBOX")
    
  } else {
    message("Response Timer Timed Out: No Buys issued")
  }
} else { message(paste0("No predicted gains as of ",lubridate::now()))}
# If instructions 
if (length(Purchase$buy_syms) > 0) {
  message("Initializing Automated Buy Script...")
  source("~/R/Quant/JobsScripts/AutomatedBuy.R", local = T)
}

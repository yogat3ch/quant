params <- new.env()
library(magrittr)
params$taxp <- "stts"
params$live <- F

# The Portfolio google sheet
#googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
#params$gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
# Get the time index for a data.frame or xts
params$time_index <- function(.dat){ 
  .out <- stringr::str_extract(colnames(.dat), stringr::regex("^time$|^date$", ignore_case = TRUE)) 
  .out <- purrr::keep(.out, ~!is.na(.x))
  return(.out)
}
params$start_cluster <- function(ac, timeout = 60*60*24*30, outfile){
  # Remove the previous outfile if it exists
  if (file.exists(outfile)) {
    file.remove(outfile)  
  }
  # create the cluster with timout and outfile
  cl <- parallel::makeCluster(ac, timeout = timeout, outfile = outfile)
  # Function that handles conditions and writes them to file
  write_to_file <- function(cond) {
    cond_class <- class(cond)[1]
    msg <- paste(cond_class, ":", cond$message)
    write(msg, file = outfile, append=TRUE)
  }
  
  catch <- catchr::make_catch_fn(
    warning = c(write_to_file, muffle),
    message = c(write_to_file, muffle),
    error   = c(write_to_file, catchr::exit_with("Returned error!"))
  )
  
  return(list(cl = cl, catch = catch))
}


#' @title timewindows
#' @description Create dynamic windows based on input data
#' @param .dat A single data.frame timeseries
#' @param .wind A list of \code{\link[lubridate]{duration}}s for which to determine the number of data points elapsed
timewindows <- function(.dat, .wind) {
  `!!` <- rlang::`!!`
  `%>%` <- dplyr::`%>%`
  # get the time index column name
  .t_ind <- params$time_index(.dat)
  # determine the number of datapoints in a day
  .day_length <- dplyr::mutate(.dat, day = lubridate::as_date(!!rlang::sym(.t_ind))) %>% 
    dplyr::group_by(day) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::pull(n) %>% 
    HDA::Mode()
  # .r <- range(.dat[[.t_ind]])
  # .seq <- 1:{lubridate::interval(.r[1], .r[2]) %/% lubridate::days(1)}
  # purrr::accumulate(.seq, .f = ~{
  #   browser()
  #   if (class(.x) == "Period") {
  #     .out <- .x + (.y - 1) * lubridate::days(1)
  #   } else {
  #     lubridate::days(1)
  #   }
  #   })
  # create the windows
  .wind %>% 
    purrr::map(rlang::expr) %>% 
  purrr::map_dbl(~{
    purrr::map_dbl(.day_length, .f = .x)
  })
}
# Expressions that create interval multiples of one day

params$wind  <-  c(day = 1, week = 5, moonphase = 10, mooncycle = 20)
params$wind_dur <- list(day = lubridate::days(1), week = lubridate::weeks(1), moonphase = lubridate::weeks(2), mooncycle = lubridate::weeks(4))
# Paths to the most recent data saves
params$paths <- list(Positions_tsl = "~/R/Quant/Positions_tsl2019-09-02.Rdata", 
                     Positions_ts_rv_iv = "~/R/Quant/Positions_ts_rv_iv2015-07-27_2019-07-16.Rdata",
                     Positions_new = "~/R/Quant/Positions_new.Rdata",
                     best_tsl = "~/R/Quant/best_tsl.Rdata")

# Read the columns from the Orders sheet in their appropriate classes
params$Orders_cols <- c(Platform= 'c',id= 'c',client_order_id= 'c',created_at= 'T' ,updated_at= 'T',submitted_at= 'T',filled_at= 'T',expired_at= 'T',canceled_at= 'T' ,failed_at= 'T',asset_id= 'c',symbol= 'c',asset_class= 'c',qty= 'd',filled_qty='d' ,filled_avg_price= 'd',order_type= 'c',type= 'c',side= 'c',time_in_force= 'c',limit_price= 'd',stop_price= 'd',status= 'c',extended_hours = 'l', CB= 'd',GL= 'd',TSL= 'c',live= 'l',SID ='c')

#Create Trailing stop loss types
`!!` <- rlang::`!!`
`%<>%` <- magrittr::`%<>%`
params$TSLvars <-
  list(
    tidyr::expand_grid(.retro = params$wind_dur, .hilop = seq(.5, .9, .1)) %>%
      purrr::pmap(~{
        .retro <- ..1
        nm <- paste(
          "tslret",
          paste0("x", ..2),
          paste0("day",stringr::str_extract(as.character(.retro), "^\\d+")),
          sep = "_"
        )
        rlang::list2(!!nm := list(retro = .retro, hilop = ..2))
      }),
    tidyr::expand_grid(.retro = params$wind_dur, .m = c(1, 1.5, 2, 2.5)) %>%
      purrr::pmap(~{
        .retro <- ..1
        nm <- paste(
          "tslsd",
          paste0("x", ..2),
          paste0("day",stringr::str_extract(as.character(.retro), "^\\d+")),
          sep = "_"
        )
        rlang::list2(!!nm := list(retro = .retro, m = ..2))
      }),
    seq(.04, .24, .02) %>% setNames(paste0(rep("tslp", length(.)),"_",.)) %>% as.list() %>%
      purrr::map( ~ c(tslp = .x)))
params$TSLvars[1:2] %<>% purrr::map_depth(.depth = 1, .f = unlist, recursive = FALSE)
params$TSLvars <- unlist(params$TSLvars, recursive = F) %>% setNames(nm = stringr::str_remove(names(.), ".*(?=tsl)"))

# Function for calculating the amount of the trailing stop loss given data and TSL arguments which includes the TSLvar item and the date of the current point in time
attr(params$TSLvars, "tsl_amt") <- function(.dat, tsl, .dtref, cl_nm){
  # Make the names simple for following if statements
  tsl_type <- stringr::str_extract(names(tsl), stringr::regex("^[A-Za-z]+"))
  .tsl <- tsl[[1]]
  .dbg <- get0(".dbg", envir = .GlobalEnv)
  td_nm <- rlang::sym(cl_nm["t"])
  if (isTRUE(.dbg)) message(tsl_type)
  if (grepl("sd|ret", tsl_type, ignore.case = T)) {
    #message(paste0("Cols: ", colnames(.data)))
    # if its a tbl_time or data.frame
    if (inherits(.dat, c("tbl_time","tbl_ts","data.frame"))) { 
      # get the min date
      min_d <- min(.dat[, cl_nm["t"], drop = T])
    } else {
      # if it's a ts object
      min_d <- min(time(.dat))
    }
    # if the retro time frame goes back farther than the minimum date
    if ({.dtref - .tsl$retro} < min_d) {
      if(isTRUE(.dbg)) message(paste0("Historical date reference is: ",{.dtref - .tsl$retro}, " data only goes back as far as ", min_d, ". "))
      d <- min_d
    } else d <- .dtref - .tsl$retro
    
    # Subset the data based on the class
    if (inherits(.dat, c("tbl_time", "tbl_ts", "data.frame"))) {
      # get the name of the time date columns as a symbol
      
      # subset the data accordingly
      new_df <- dplyr::filter(.dat, d <= (!!td_nm) & (!!td_nm) <= .dtref) 
      #%>% dplyr::mutate_at(dplyr::vars(!!td_nm), ~ as.POSIXct(., origin = lubridate::origin))
    } else {
      # if it's a ts object
      new_df <- window(.dat, start = d, end = .dtref)
    }
    # based on the tsl type, calculate the tsl
    if (grepl("sd", tsl_type, ignore.case = TRUE)) {
      out <- sd(new_df[, cl_nm[c("h","l","c")]] %>% unlist, na.rm = TRUE) * .tsl$m
    } else if (grepl("ret", tsl_type, ignore.case = TRUE)) {
      out <- diff(range(new_df[, cl_nm[c("h","l","c")]] %>% unlist)) * .tsl$hilop
    }
  } else if (grepl("tslp", tsl_type, ignore.case = TRUE)) {
    out <- {.dat %>% dplyr::filter((!!td_nm) >= .dtref) %>% dplyr::pull(cl_nm["h"]) %>% max()} * .tsl[1]
  } 
  return(out)
}

# ----------------------- Wed Jul 10 13:32:33 2019 ------------------------#
# Functions
# ws = wash sale amout
# wso = wash sale order id
params$AlpacatoR_order_mutate <- function(l, tsl = '', live = '', ws = 0, wso = '') {
  library("magrittr")
  # toNum <- function(x){
  #   as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
  # }
  l[l %>% purrr::map_lgl(is.null)] <- NA
  l %<>% as.data.frame(stringsAsFactors = F) %>% cbind.data.frame(data.frame(Platform = "A"), ., stringsAsFactors = F)
  df <- dplyr::mutate_if(l, .predicate = ~is.character(.), ~trimws(.)) %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("at")), ~ lubridate::as_datetime(., tz = "EST"))
  
  if (df$side == "buy" & !is.na(df$filled_qty)) {
    df %<>% mutate(CB = filled_qty * filled_avg_price + ws)
    if (ws > 0) {
      df %<>% dplyr::mutate_at(dplyr::vars(order_type), ~paste0(., ' ws:', wso))
    }
    out <- cbind.data.frame(df, data.frame(GL = "", TSL = tsl, live = live, SID = "", stringsAsFactors = F), stringsAsFactors = F) 
  } else if (df$side == "buy" & is.na(df$filled_at)) {
    out <- cbind.data.frame(df, data.frame(CB = '', GL = '', TSL = tsl, live = live, SID = ""), stringsAsFactors = F) %>% dplyr::mutate(qty_remain = df$qty)
  } else if (df$side == "sell") {
    out <- cbind.data.frame(df, data.frame(CB = '', GL = '', TSL = tsl, live = live, SID = ""), stringsAsFactors = F) %>% dplyr::mutate(qty_remain = 0)
  } 
  return(out)
}
params$AlpacatoR_position_mutate <- function(df) {
  df <- dplyr::mutate_at(df, tidyselect::vars_select(names(df), "qty", "market_value", "cost_basis", "change_today", dplyr::starts_with("unr"), dplyr::ends_with("price")), ~as.numeric)
  out <- dplyr::mutate_if(df, .predicate = ~is.character, ~trimws)
  return(out)
}

params$AlpacatoR_bars_mutate <- function(l) {
  out <- purrr::imap(l, function(.x, .y){
    out <- dplyr::rename(dplyr::select(.x, d,o,h,l,c,v), Time = "d", open = "o", high = "h", low = "l", close = "c", volume = "v")
    return(out)  
  })
  return(out)
}

# Function for loading data quickly
params$dataUtil <- function(reg = NULL, as.suffix = F, object = NULL, name = NULL, where = NULL) {
  if (is.null(reg) & is.null(object)) {
    reg <- ".Rdata$"
    files <- list.files(recursive = T)[stringr::str_which(list.files(recursive = T), reg)]
    print(files)
    i <- readline(prompt = "Select the file you would like to load:")
    message(paste0("Loading: ",files[as.numeric(i)]))
    load(files[as.numeric(i)], envir = .GlobalEnv)
  }else if (is.null(object) & !is.null(reg)) {
    if (as.suffix) reg <- paste0("Positions_", reg)
    #print(reg)
    files <- list.files(recursive = T)[stringr::str_which(list.files(recursive = T), reg)]
    print(files)
    i <- readline(prompt = "Select the file you would like to load:")
    message(paste0("Loading: ",files[as.numeric(i)]))
    load(files[as.numeric(i)], envir = .GlobalEnv)
  } else if (!is.null(object)) {
    if (!is.null(name)) assign(name, object) else {
      name <- deparse(match.call()$object)
    }
    fn <- paste0(name,".Rdata")
    if (tibble::is_tibble(object[[1]])) {
      td_nm <- stringr::str_extract(names(object[[1]]), "^time$|^date$") %>% subset(subset = !is.na(.)) %>% .[1]
      tsrange <- object[[1]][[td_nm]] %>% lubridate::as_date() %>% range
    } else tsrange <- object[[1]] %>% time %>% lubridate::as_date() %>% range
    fn <- append(fn, paste0(name,paste0(tsrange, collapse = "_"),".Rdata"))
    fn <- append(fn, paste0(name,lubridate::today(),".Rdata"))
    print(fn)
    i <- readline(prompt = "Choose the file name:")
    save(list = name, file = paste0(where,fn[as.numeric(i)]), compress = "bzip2", safe = T)
  }
}





# Function for retrieving new data
# For Debugging:
#rm(list = c(".dat", "last_bar", "local_fn", "date_history", "td_nm", "nms", ".cal", "cal", "lgl", "from_weeks", "new_data"))
params$getPositions_new <- function(Positions_v, .retrieveAll = NULL){
  .bgJob <- any(stringr::str_detect(deparse(sys.calls()), "sourceEnv"))
  if (.bgJob) message(paste0("Running getPositions_new as Background Task"))
  
  dat <- purrr::map(Positions_v, .f = ~{
    .sym <- .x
    # Get the Historical Data filename from the HD
    # local_fn <- list.files(path = "~/R/Quant/PositionData", pattern = paste0(.sym,"\\d{4}\\-\\d{2}\\-\\d{2}\\_\\d{4}\\-\\d{2}\\-\\d{2}\\.csv"), full.names = T)
    message(local_fn)
    date_history <- stringr::str_extract_all(local_fn, "\\d{4}\\-\\d{2}\\-\\d{2}") %>% do.call("c", .) %>% lubridate::ymd()
    # Get the last bar recorded
    last_bar <-  date_history %>% max()
    message(paste0("Last bar: ",last_bar))
    message(paste0("Filename: ",basename(local_fn)))
    message(HDA::go(local_fn))
    #TODO HDA::go is not working for this character vector
    if (HDA::go(local_fn)) {
      # Load the historical data
      .dat <- readr::read_csv(file = local_fn[stringr::str_which(local_fn, as.character(last_bar))])
      # Get the column with the date or time
      td_nm <- stringr::str_extract(colnames(.dat), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      # If it's uppercase from earlier versions convert it to lower
      if (grepl("T", td_nm, fixed = T)) {
        message("Renaming")
        n.td_nm <- tolower(td_nm)
        .dat %<>% dplyr::rename(!!n.td_nm := !!td_nm)
        td_nm <- n.td_nm
      }
      # Get the columns corresponding to the get_bars call
      nms <- c(td_nm, "open","high","low", "close", "volume")
      nms <- rlang::syms(nms)
      .dat %<>% dplyr::select(!!! nms)
    } else {
      # If no data, get 241 data points previous
      from_weeks <- 241/5 + 2 
      cal <- AlpacaforR::get_calendar(lubridate::today() - lubridate::weeks(from_weeks), lubridate::today())
      while (nrow(cal) < 241) {
        cal <- AlpacaforR::get_calendar(lubridate::today() - lubridate::weeks(from_weeks), lubridate::today())
        from_weeks <- {241 - nrow(cal)} %/% 5 + 1 + from_weeks
      }
      last_bar <- cal[1, 1]
    }
    .cal <- AlpacaforR::get_calendar(from = lubridate::today() - lubridate::weeks(2), to = lubridate::today() + lubridate::weeks(2))
    .cal <- purrr::pmap(.cal, function(date, open, close){
      lubridate::interval(start = lubridate::ymd_hm(paste0(date, " ", open), tz = Sys.timezone()), end = lubridate::ymd_hm(paste0(date, " ", close), tz = Sys.timezone()))
    })
    lgl <- vector()
    # If it is NOT during market hours
    lgl[1]<- {!purrr::map_lgl(.cal, now = lubridate::now(), function(.x, now) lubridate::`%within%`(now,.x)) %>% any}
    # AND the last_bar is equal to the last market day 
    lgl[2] <- {.cal[[max(which(purrr::map_lgl(.cal, ~ lubridate::int_end(.x) < lubridate::now() | lubridate::`%within%`(lubridate::now(), .x))))]] %>% lubridate::int_end() %>% lubridate::as_date() == last_bar}
    # AND there is data for .dat
    lgl[3] <- HDA::go(.dat)
    message(lgl)
    if ( lgl[1] & lgl[2] | lgl[2] & lgl[3] ) {
      attr(.dat, "Sym") <- .sym
      return(.dat)
    }
    if (!HDA::go(.retrieveAll)) { # if .retrieveAll is not in the current env
      if (HDA::go(".retrieveAll", env = .GlobalEnv)) { # if its in the global environment
        .retrieveAll <- get0(".retrieveAll", envir = .GlobalEnv) # get it
        
      } else .retrieveAll <- F # otherwise set it as false.
    }
    message(paste0("Background job: ", .bgJob, " Retrieve all?: ", .retrieveAll))
    if (!.bgJob & !.retrieveAll){
      a <- readline(paste0("1. Retrieve data for ",.sym,"? \n 2. Skip ", .sym,"\n 3. Retrieve data for all? \n 4. Skip all"))
      if (a == 4) stop("Quitting.") else if (a == 2) return(.dat) else if (a == 1) message(paste0("Retrieving data for ",.sym)) else if (a == 3) {
        .retrieveAll <<- T
      }
    }
    message(paste0("Retrieving data for ",.sym))
    # Retrieve the updated data
    new_bars <- AlpacaforR::get_bars(ticker = .sym, from = last_bar, to = lubridate::today())[[1]]
    if (HDA::go(.dat)) { # if the data exists on the HD
      # Combine it with the new data
      keep_ind <- .dat[[td_nm]] %>% lubridate::as_date() != new_bars[["time"]] %>% lubridate::as_date()
      new_data <- rbind.data.frame(.dat[keep_ind, ], new_bars)
    } else new_data <- new_bars # If it doesn't just make the new_data
    # Save the new data
    
    readr::write_csv(new_data,  path = paste0("~/R/Quant/PositionData/",.sym, lubridate::as_date(min(new_data[["time"]])),"_", lubridate::as_date(max(new_data[["time"]])), ".csv"))
    # If previous files exist
    if (HDA::go(local_fn) & last_bar != lubridate::today()) {
      
      purrr::walk(local_fn[stringr::str_detect(local_fn, as.character(last_bar))],file.remove) 
    }
    attr(new_data, "Sym") <- .sym
    return(new_data)
  })
  names(dat) <- Positions_v
  return(dat)
}



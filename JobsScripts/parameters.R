params <- new.env()
HDA::startPkgs(c("magrittr"))
params$taxp <- "stts"
params$live <- F

# The Portfolio google sheet
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")

# Fixed windows
params$wind  <-  c(weeks = 5, moonphase = 5 * 2, mooncycle = 5 * 4, quarters = 5 * 12)

# Paths to the most recent data saves
params$paths <- list(Positions_tsl = "~/R/Quant/Positions_tsl2019-07-18.Rdata", 
                     Positions_ts = "~/R/Quant/Positions_ts_rv_iv2015-07-27_2019-07-16.Rdata",
                     Positions_new = "~/R/Quant/Positions_new.Rdata")

# Read the columns from the Orders sheet in their appropriate classes
params$Orders_cols <- c(Platform= 'c',id= 'c',client_order_id= 'c',created_at= 'T' ,updated_at= 'T',submitted_at= 'T',filled_at= 'T',expired_at= 'T',canceled_at= 'T' ,failed_at= 'T',asset_id= 'c',symbol= 'c',asset_class= 'c',qty= 'd',filled_qty='d' ,filled_avg_price= 'd',order_type= 'c',type= 'c',side= 'c',time_in_force= 'c',limit_price= 'd',stop_price= 'd',status= 'c',extended_hours = 'l', CB= 'd',GL= 'd',TSL= 'c',live= 'l',SID ='c')

#Create Trailing stop loss types
params$TSLvars <- append(expand.grid(.retro = c(1,2,4,12), .hilop = seq(.5,.9,.1)) %>% purrr::pmap( function(.retro,.hilop){list(tslret = list(retro = lubridate::weeks(.retro), hilop = .hilop))}),expand.grid(.retro = c(1,2,4,12), .m = c(1,1.5,2,2.5)) %>% purrr::pmap( function(.retro,.m){list(tslsd = list(retro = lubridate::weeks(.retro), m = .m))})) %>% append(seq(.04,.24,.02) %>% setNames(rep("tslp",length(.))) %>% as.list %>% purrr::map(~c(tslp = .x)))
names(params$TSLvars) <- purrr::map(params$TSLvars, .f = function(.x){
  if (names(.x) == "tslsd") nm <- paste("tslsd",paste0("x",.x[[1]][[2]]),stringr::str_match_all(.x[[1]][1], "(day|year|month|hour|minute)\\s\\=\\s([1-9]{1,2})") %>% .[[1]] %>% .[, -1] %>% paste0(collapse = ""), sep = "_") else if (names(.x) == "tslret") nm <- paste("tslret",paste0("px",.x[[1]][[2]]),stringr::str_match_all(.x[[1]][1], "(day|year|month|hour|minute)\\s\\=\\s([1-9]{1,2})") %>% .[[1]] %>% .[, -1] %>% paste0(collapse = ""), sep = "_") else if (names(.x) == "tslp") nm <- paste("tslp",.x, sep = "_")
  return(nm)
}) %>% unlist



# Function for calculating the amount of the trailing stop loss given data and TSL arguments which includes the TSLvar item and the date of the current point in time
attr(params$TSLvars, "tsl_amt") <- function(.data, .args, verbose = F){
  # Make the names simple for following if statements
  tsl_type <- stringr::str_extract(names(.args)[1], stringr::regex("^[A-Za-z]+"))
  .args <- .args[[1]]
  if (verbose) message(tsl_type)
  # Create a character index vector
  cl_nm <- c(high = stringr::str_extract(colnames(.data), stringr::regex("^high", ignore_case = T)) %>% subset(subset = !is.na(.)),
             low =   lo_cl <- stringr::str_extract(colnames(.data), stringr::regex("^low", ignore_case = F)) %>% subset(subset = !is.na(.)),
             close =   stringr::str_extract(colnames(.data), stringr::regex("^close", ignore_case = F)) %>% subset(subset = !is.na(.)))
  # if it's a tbl_time object
  if (any(stringr::str_detect(class(.data), stringr::regex("tbl_time|data.frame")))){
    # add the date/time column
    cl_nm <- c(time = stringr::str_extract(colnames(.data), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1], cl_nm)
    # subset the data
    .data <- .data[, cl_nm]
  } else {.data <- .data[,cl_nm]}# if it's a ts object subset the data
  
  if (tsl_type == "tslsd" | tsl_type == "tslret") {
  #message(paste0("Cols: ", colnames(.data)))
    # if its a tbl_time or data.frame
    if (any(stringr::str_detect(class(.data),"tbl_time|data\\.frame"))) { 
      # get the min date
      min_d <- min(.data[, cl_nm[["time"]], drop = T])
    } else {
      # if it's a ts object
      min_d <- min(time(.data))
    }
     # if the retro time frame goes back farther than the minimum date
    if ({.args$dtref - .args$retro} < min_d) {
      if(verbose) message(paste0("Historical date reference is: ",{.args$dtref - .args$retro}, " data only goes back as far as ", min_d, ". "))
      d <- min_d
    } else d <- .args$dtref - .args$retro
    
    # Subset the data based on the class
  if (any(stringr::str_detect(class(.data),"tbl_time|data.frame"))) {
        # get the name of the time date columns as a symbol
    cl <- cl_nm["time"]
    td_nm <- rlang::sym(cl)
    # subset the data accordingly
    new_df <- dplyr::filter(.data,(!!td_nm) <= .args$dtref & (!!td_nm) >= d) %>% dplyr::mutate_at(dplyr::vars(!!td_nm), ~as.POSIXct(.,origin = lubridate::origin))
  } else {
    # if it's a ts object
      new_df <- .data[paste0(d %>% lubridate::as_date(),"/")]
  }
    # based on the tsl type, calculate the tsl
    if (tsl_type == "tslsd") {
  out <- sd(new_df[,cl_nm[c("high","low","close")]] %>% unlist) * .args$m
    } else if (tsl_type == "tslret") {
        out <- range(new_df[,cl_nm[c("high","low","close")]] %>% unlist) %>% diff %>% {. * .args$hilop}
      }
    
  } else if (tsl_type == "tslp") {
    out <- {{.data %>% dplyr::filter(time >= .args$dtref) %>% .[["high"]] %>% max()} * .args$tslp}
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
  
  if (df$side == "buy") {
    df %<>% mutate(CB = filled_qty * filled_avg_price + ws)
    if (ws > 0) {
      df %<>% dplyr::mutate_at(dplyr::vars(order_type), ~paste0(., ' ws:', wso))
    }
    out <- cbind.data.frame(df, data.frame(GL = "", TSL = tsl, live = live, SID = "", stringsAsFactors = F), stringsAsFactors = F) 
  } else {
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
params$dataUtil <- function(reg = NULL, as.suffix = F, object = NULL, name = NULL) {
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
    save(list = name, file = fn[as.numeric(i)])
  }
}

# Function for retrieving new data
# For Debugging:
#rm(list = c(".dat", "last_bar", "local_fn", "date_history", "td_nm", "nms", ".cal", "cal", "lgl", "from_weeks", "new_data"))
params$getPositions_new <- function(Positions_v, params){
  .bgJob <- any(stringr::str_detect(deparse(sys.calls()), "sourceEnv"))
  if (.bgJob) message(paste0("Running getPositions_new as Background Task"))
   
dat <- purrr::map(Positions_v, params = params, .f = function(.sym, params){
  # Get the Historical Data filename from the HD
  local_fn <- list.files(path = "~/R/Quant/PositionData", pattern = paste0(.sym,"\\d{4}\\-\\d{2}\\-\\d{2}\\_\\d{4}\\-\\d{2}\\-\\d{2}\\.csv"), full.names = T)
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
    lubridate::interval(start = lubridate::ymd_hm(paste0(date, " ", open), tz = "EDT"), end = lubridate::ymd_hm(paste0(date, " ", close), tz = "EDT"))
  })
  lgl <- vector()
  # If it is NOT during market hours
  lgl[1]<- {!purrr::map_lgl(.cal, now = lubridate::now(), function(.x, now) lubridate::`%within%`(now,.x)) %>% any}
  # AND the last_bar is equal to the last market day 
  lgl[2] <- {.cal[[max(which(purrr::map_lgl(.cal, ~ lubridate::int_end(.x) < lubridate::now())))]] %>% lubridate::int_end() %>% lubridate::as_date() == last_bar}
  # AND there is data for .dat
  lgl[3] <- HDA::go(.dat)
  message(lgl)
  if ( lgl[1] & lgl[3] | lgl[2] & lgl[3] ) {
    attr(.dat, "Sym") <- .sym
    return(.dat)
  }
  
  if (HDA::go(".retrieveAll", env = .GlobalEnv)) .retrieveAll <- get0(".retrieveAll", envir = .GlobalEnv) else .retrieveAll <- F
  message(paste0("Background job: ", .bgJob, " Retrieve all?: ", .retrieveAll))
  if (!.bgJob & !.retrieveAll){
    a <- readline(paste0("1. Retrieve data for ",.sym,"? \n 2. Skip ", .sym,"\n 3. Retrieve data for all? \n 4. Skip all"))
    if (a == 4) stop("Quitting.") else if (a == 2) return(.dat) else if (a == 1) message(paste0("Retrieving data for ",.sym)) else if (a == 3) {
      .retrieveAll <<- T
      }
  }
  if (.retrieveAll) message(paste0("Retrieving data for ",.sym))
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


# Get the time index for a data.frame or xts
params$getTimeIndex <- function(.dat){ 
  stringr::str_extract(colnames(.dat), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
}
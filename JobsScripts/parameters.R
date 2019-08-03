params <- new.env()
HDA::startPkgs(c("magrittr"))
params$taxp <- "stts"
params$live <- F
params$wind  <-  list(weeks = lubridate::days(7), moonphase = lubridate::weeks(2), mooncycle = lubridate::weeks(4), quarters = lubridate::weeks(12))

params$paths <- list(Positions_tsl = "~/R/Quant/Positions_tsl2019-07-18.Rdata", 
                     Positions_ts = "~/R/Quant/Positions_ts_rv_iv2015-07-27_2019-07-16.Rdata",
                     Positions_new = "~/R/Quant/Positions_new.Rdata")

params$Orders_cols <- c(Platform= 'c',id= 'c',client_order_id= 'c',created_at= 'T' ,updated_at= 'T',submitted_at= 'T',filled_at= 'T',expired_at= 'T',canceled_at= 'T' ,failed_at= 'T',asset_id= 'c',symbol= 'c',asset_class= 'c',qty= 'd',filled_qty='d' ,filled_avg_price= 'd',order_type= 'c',type= 'c',side= 'c',time_in_force= 'c',limit_price= 'd',stop_price= 'd',status= 'c',extended_hours = 'l', CB= 'd',GL= 'd',TSL= 'c',live= 'l',SID ='c')

params$TSLvars <- append(expand.grid(.retro = c(1,2,4,12), .hilop = seq(.5,.9,.1)) %>% purrr::pmap( function(.retro,.hilop){list(tslret = list(retro = lubridate::weeks(.retro), hilop = .hilop))}),expand.grid(.retro = c(1,2,4,12), .m = c(1,1.5,2,2.5)) %>% purrr::pmap( function(.retro,.m){list(tslsd = list(retro = lubridate::weeks(.retro), m = .m))})) %>% append(seq(.04,.24,.02) %>% setNames(rep("tslp",length(.))) %>% as.list %>% purrr::map(~c(tslp = .x)))
names(params$TSLvars) <- purrr::map(params$TSLvars, .f = function(.x){
  if (names(.x) == "tslsd") nm <- paste("tslsd",paste0("x",.x[[1]][[2]]),stringr::str_match_all(.x[[1]][1], "(day|year|month|hour|minute)\\s\\=\\s([1-9]{1,2})") %>% .[[1]] %>% .[, -1] %>% paste0(collapse = ""), sep = "_") else if (names(.x) == "tslret") nm <- paste("tslret",paste0("px",.x[[1]][[2]]),stringr::str_match_all(.x[[1]][1], "(day|year|month|hour|minute)\\s\\=\\s([1-9]{1,2})") %>% .[[1]] %>% .[, -1] %>% paste0(collapse = ""), sep = "_") else if (names(.x) == "tslp") nm <- paste("tslp",.x, sep = "_")
  return(nm)
}) %>% unlist




attr(params$TSLvars, "tsl_amt") <- function(.data, .args, verbose = F){
  # Make the names simple for following if statements
  tsl_type <- stringr::str_extract(names(.args)[1], stringr::regex("^\\w+"))
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
    
  } 
  return(out)
}

# ----------------------- Wed Jul 10 13:32:33 2019 ------------------------#
# Functions
params$AlpacatoR_order_mutate <- function(l, tsl = '', live = '', ws = 0, wso = '') {
  library("magrittr")
  # toNum <- function(x){
  #   as.numeric(stringr::str_replace_all(x, "\\$|\\,", ""))
  # }
  l[l %>% purrr::map_lgl(is.null)] <- NA
  l %<>% as.data.frame %>% cbind.data.frame(data.frame(Platform = "A"), .)
  # Deprecated as of commit 83dacb1 on AlpacaforR
  # df <- dplyr::mutate_at(l, dplyr::vars(dplyr::ends_with("at")),~lubridate::ymd_hms(., tz = Sys.timezone()))
  # df <- dplyr::mutate_at(df, dplyr::vars(qty, filled_qty, filled_avg_price, limit_price, stop_price), ~toNum)
  df <- dplyr::mutate_if(df, .predicate = ~is.character, ~trimws)
  if (df$side == "buy") {
    df %<>% mutate(CB = filled_qty * filled_avg_price + ws)
    if (ws > 0) {
      df %<>% dplyr::mutate_at(dplyr::vars(order_type), list(~paste0(' ws:', wso)))
    }
    out <- cbind.data.frame(df, data.frame(GL = "", TSL = tsl, live = live, SID = "")) 
  } else {
    out <- cbind.data.frame(df, data.frame(CB = '', GL = '', TSL = tsl, live = live, SID = "")) %>% dplyr::mutate(qty_remain = filled_qty)
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
#$
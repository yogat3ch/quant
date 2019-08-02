# ----------------------- Sat May 18 22:58:45 2019 ------------------------#
#' TSL: Trailing Stop Loss function
#' @param .args \code{(list)} A list with a single trailing stop loss type: tslsd - a standard deviationn based TSL, tslret - a retrograde window based TSL, and tslp - percentage of price based TSL
#'    \itemize{
#'  \item{\code{tslsd}}{\code{(list)} A list with two variables: \code{retro} a lubridate duration for traversing a retrograde window and \code{m} a multiplier for the sd of the high-low range}
#'  \item{\code{tslret}}{\code{(list)} A list with two variables: \code{retro} a lubridate duration  for traversing a retrograde window and \code{hilop} a percentage of the high-low range for that window}
#'  \item{\code{tslp}}{\code{(double)} A double vector with the percent as decimal of the peak to set as the stop loss}
#'    }
#' @export
TSL <- function(v, .args, verbose = F) {
  if (verbose) print(names(.args)[1])
  tsl_amt <- .args$tsl_amt
  if (!quantmod::is.HLC(v) & !all(tibble::is_tibble(v) & any(stringr::str_detect(names(v),stringr::regex("^date$|^time$", ignore_case = T))) & any(stringr::str_detect(names(v),stringr::regex("high", ignore_case = T))) & any(stringr::str_detect(names(v),stringr::regex("low", ignore_case = T)))) ) stop("Requires an HLC timeseries object (xts) or a tibbletime with a date or time and hlc columns")
  # Check that the object supplied is either an XTS or tibbletime with appropriately labelled index and high/low columns
  
  cl_nm <- c(high = stringr::str_extract(colnames(v), stringr::regex("^high", ignore_case = T)) %>% subset(subset = !is.na(.)),
    low =   lo_cl <- stringr::str_extract(colnames(v), stringr::regex("^low", ignore_case = F)) %>% subset(subset = !is.na(.)),
    close =   stringr::str_extract(colnames(v), stringr::regex("^close", ignore_case = F)) %>% subset(subset = !is.na(.)))
  if (any(stringr::str_detect(class(v), stringr::regex("tbl_time|data.frame")))){
  cl_nm <- c(time = stringr::str_extract(colnames(v), stringr::regex("^time|date", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1], cl_nm)
  # Get the column indexes and subset
  v <- v[,cl_nm]
  } else {v <- v[,cl_nm]}
  # Primary Loop starts here
  s <- attr(v, "Sym")
  out <- purrr::map(1:nrow(v), verbose = verbose, s = s, .f = function(.x, verbose, s) {
    i <- dayi <- .x # Save as the increment i and initial index day
    if (xts::is.xts(v)) .args[[1]]$dtref <- time(v)[i] else .args[[1]]$dtref <- v[i, cl_nm[["time"]], drop = T]
    
    price <- as.numeric(v[i, cl_nm[["close"]], drop = T])
    peak <- as.numeric(v[i, cl_nm[["high"]], drop = T])
    tsl <- peak - price + 1
    if (verbose)  message(paste0(s," fn(TSL) row#: ", .x))
    while (price >= peak - tsl) {
      # Get the datetime reference
      if (xts::is.xts(v)) .args[[1]]$dtref <- time(v)[i] else .args[[1]]$dtref <- v[i, cl_nm[["time"]], drop = T]
      # if (verbose) message(paste0("dtref: ",.args[[1]]$dtref," next date: ",.args[[1]]$dtref - .args[[1]]$retro))
      # Get the tsl amount
      # if its tslsd
      if (!is.null(.args$tslsd)) tsl <- tsl_amt(.data = v, .args[1])
      # if its tslret 
      if (!is.null(.args$tslret)) tsl <- tsl_amt(.data = v, .args[1])
      # if its tslp
      if (!is.null(.args$tslp)) tsl <- peak * .args$tslp[[1]]
      #if (verbose) message(paste0("tsl: ",tsl))
      
      #if (verbose) print(c(.x = .x, tsl = tsl,price = price, peak = peak, i = i))
      price <- as.numeric(v[i, cl_nm[["low"]], drop = T])
      peak <- as.numeric(max(v[dayi:i, cl_nm[["high"]], drop = T]))
      
      if (i == nrow(v)) break
      i <- i + 1
    }
    pct_gain <- (mean(as.numeric(v[i, cl_nm[c("high","low","close")], drop = T])) - as.numeric(v[dayi, cl_nm[["close"]], drop = T])) / as.numeric(v[dayi, cl_nm[["close"]], drop = T])
  # return the percent change of the original price and the date on which the TSL is triggered.
      out <- c(pct_gain, v[i, cl_nm[["time"]], drop = T])
      
    return(out)
  })
  if (verbose) message(paste0(lubridate::now(), "fn TSL result: ", paste0(out, collapse = ", ")))
  return(do.call("rbind",out))
}


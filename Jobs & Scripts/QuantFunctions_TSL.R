# ----------------------- Sat May 18 22:58:45 2019 ------------------------#
#' TSL: Trailing Stop Loss function
#' @param tsla \code{(numeric)} An amount for a trailing stop loss or a function to be used on a timeseries (xts,zoo) or tbl_time OHLC object to compute the trailing stop loss amount
#' @param tslp \code{(proportion)} A percent as decimal to calculate the trailing stop loss amount
#' @param retro \code{(numeric)} A vector with two values:
#'    \itemize{
#'  \item{n}{ A retrograde window period over which the max high and min low will be observed}
#'  \item{hilop}{ A percent as a decimal to multiple by the observed hi-lo range difference to set as the trailing stop loss amount}
#'    }
#' @export
TSL <- function(v, tsla = NULL, tslp = NULL, retro = c(n = NULL, hilop = NULL), verbose = F) {
  if (verbose) print(match.call())
  if (!quantmod::is.HLC(v) & !all(tibble::is_tibble(v) & any(stringr::str_detect(names(v),stringr::regex("Date|Time", ignore_case = T))) & any(stringr::str_detect(names(v),stringr::regex("high", ignore_case = T))) & any(stringr::str_detect(names(v),stringr::regex("low", ignore_case = T)))) ) stop("Requires an HLC timeseries object (xts) or a tibbletime with a date or time and hlc columns")
  # Check that the object supplied is either an XTS or tibbletime with appropriately labelled index and high/low columns
  
  dt_cl <- stringr::str_which(colnames(v), stringr::regex("date|time", ignore_case = T))
  hi_cl <- stringr::str_which(colnames(v), stringr::regex("high", ignore_case = T))
  lo_cl <- stringr::str_which(colnames(v), stringr::regex("low", ignore_case = T))
  cl_cl <- stringr::str_which(colnames(v), stringr::regex("close", ignore_case = T))
  if(tibble::is_tibble(v)) v <- v[,c(dt_cl, hi_cl, lo_cl, cl_cl)]
  # Get the column indexes and subset a tibble time if provided
  if (!is.null(tsla) & is.function(tsla)) tsla <- tsla(v)
  # Primary Loop starts here
  out <- purrr::map(1:nrow(v), .f = function(.x, env = parent.frame()) {
    i <- day <- .x # Save as the increment i and initial index ii
    tsl <- 0
    price <- v[i, cl_cl]
    peak <- v[i, cl_cl]
    # Set initial values
    while (price >= peak - tsl) {
      if (all(!is.null(retro))) {  
        if (retro[[1]] > nrow(v)) message("Retrograde window is greater than the number of observations. Max number of observations will be used to calculate the range.")
        # Set tsl (trailing stop loss) based on which type is provided
        if (retro[[1]] > i) tpi <- day else tpi <- i - retro[[1]] # If the retrograde window from which the trailing stop loss is calculated is will create a negative index, then just start the tpi (timepoint index) at the index of the day from which the iteration starts
        #if (verbose) tryCatch({v[tpi:i, hi_cl]}, error = function(e) print(c(tpi = tpi, i = i)))
        tsl <- {max(v[tpi:i, hi_cl]) - min(v[tpi:i, lo_cl])} * retro[[2]] # TSL is set at the proportion of the range of hi/los over a given retrograde period
      } else if (!is.null(tsla) & is.numeric(tsla)) tsl <- tsla else tsl <- tslp * peak
      #if (verbose) print(c(.x = .x, tsl = tsl,price = price, peak = peak, i = i))
      price <- v[i, cl_cl]
      peak <- max(v[day:i, hi_cl])
      if (i == nrow(v)) break
      i <- i + 1
    }
    #if (verbose) print(c(peak = peak, price = price, GL = price - v[day, cl_cl]))
    out <- as.numeric(price) - as.numeric(v[day, cl_cl]) #Get the gain or loss and add it to out vector
    return(out)
  })
  return(unlist(out))
}
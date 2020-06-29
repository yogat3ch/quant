# ----------------------- Sat May 18 22:58:45 2019 ------------------------#
#' TSL: Trailing Stop Loss function
#' @param .args \code{(list)} A list with a single trailing stop loss type: tslsd - a standard deviationn based TSL, tslret - a retrograde window based TSL, and tslp - percentage of price based TSL
#'    \itemize{
#'  \item{\code{tslsd}}{\code{(list)} A list with two variables: \code{retro} a lubridate duration for traversing a retrograde window and \code{m} a multiplier for the sd of the high-low range}
#'  \item{\code{tslret}}{\code{(list)} A list with two variables: \code{retro} a lubridate duration  for traversing a retrograde window and \code{hilop} a percentage of the high-low range for that window}
#'  \item{\code{tslp}}{\code{(double)} A double vector with the percent as decimal of the peak to set as the stop loss}
#'    }
#' @export
TSL <- function(v, tsl = NULL, tsl_amt = NULL, time_index = NULL) {
  if (!quantmod::is.HLC(v) &
      !all(
        tibble::is_tibble(v) &
        any(stringr::str_detect(
          names(v), stringr::regex("^date$|^time$", ignore_case = T)
        )) &
        any(stringr::str_detect(
          names(v), stringr::regex("high", ignore_case = T)
        )) &
        any(stringr::str_detect(
          names(v), stringr::regex("low", ignore_case = T)
        ))
      )) {
    # Check that the object supplied is either an XTS or tibbletime with appropriately labelled index and high/low columns
    stop("Requires an HLC timeseries object (xts) or a tibbletime with a date or time and hlc columns")
  }
  verbose <- isTRUE(get0(".dbg", envir = .GlobalEnv))
  cl_nm <- c(
    t = time_index(v),
    o = purrr::keep(stringr::str_extract(colnames(v), stringr::regex("^open$", ignore_case = T)), ~ !is.na(.x)),
    h = purrr::keep(stringr::str_extract(colnames(v), stringr::regex("^high$", ignore_case = T)), ~ !is.na(.x)),
    l = purrr::keep(stringr::str_extract(colnames(v), stringr::regex("^low$", ignore_case = T)), ~ !is.na(.x)),
    c = purrr::keep(stringr::str_extract(colnames(v), stringr::regex("^close$", ignore_case = T)), ~ !is.na(.x))
  )
  v <- v[, cl_nm]
  # Primary Loop starts here
  s <- attr(v, "Sym")
  out <- purrr::map_dfr(1:nrow(v), ~{
    i <- dayi <- .x # Save as the increment i and initial index day
    price <- as.numeric(v[i, cl_nm["c"], drop = T])
    peak <- as.numeric(v[i, cl_nm["h"], drop = T])
    .tsl <- peak - price + .1
    .open <- FALSE
    if (verbose)  message(paste0(s," fn(TSL) row#: ", .x))
    while (price >= peak - .tsl) {
      if (i > nrow(v)) break
      # Get the datetime reference
      if (xts::is.xts(v))
        .dtref <- time(v)[i]
      else
        .dtref <- v[i, cl_nm["t"], drop = T]
      # Get the tsl amount
      #2019-09-20 Because these are recalculated as the TSL moves along the dataset, tslsd & tslret will make it such that during periods of little variance the position is likely to be sold. This is because the tsl is recalculated and subtracted from the peak at each time point as it moves along the dataset. 
      if (i == .x) { # Added to address the above 2019-09-20 
        .tsl <- tsl_amt(v, tsl = tsl, .dtref = .dtref, cl_nm = cl_nm)
      }

      price <- as.numeric(v[i, cl_nm["l"], drop = T])
      peak <- as.numeric(max(v[dayi:i, cl_nm["h"], drop = T], na.rm = TRUE))
      
      if (i == nrow(v)) {
        # Add a variable to flag for an open order that does not sell by the end of the data
        if (verbose) message("End of data reached, no sale")
        .open <- T
      }
      i <- i + 1
    }
    .buy_price <- mean(as.numeric(v[dayi, cl_nm[c("h","l","c")], drop = T]), na.rm = TRUE)
    pct_gain <- ((as.numeric(v[i, cl_nm[c("h")], drop = T]) - .tsl) - .buy_price) / .buy_price
  # return the percent change of the original price and the date on which the TSL is triggered.
    if (length(pct_gain) != 1) stop("Length of pct_gain is not 1")
    if (.open) {
      out <- data.frame(p = pct_gain, d = NA)
    } else {
      out <- data.frame(p = pct_gain, d = v[i, cl_nm["t"], drop = T])
    }
    return(out)
  })
  if (verbose) message(paste0(lubridate::now(), "fn TSL result: ", paste0(out, collapse = ", ")))
  return(out)
}


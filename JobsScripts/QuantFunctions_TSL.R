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
  .ohlc <- quantmod::OHLC(v)
  .h <- unlist(v[, cl_nm["h"]])
  .l <- unlist(v[, cl_nm["l"]])
  .nrv <- nrow(v)
  # Primary Loop starts here
  s <- attr(v, "Sym")
  out <- purrr::map_dfr(1:nrow(v), ~{
    e <- new.env()
    e$peak <- .buy_price <- mean(unlist(.ohlc[.x,]), na.rm = TRUE)
    i <-  .x
    e$li_max <- .nrv
    while (i <= length(.h)) {
      if (.h[i] > e$peak[length(e$peak)]) {
        if (i == .x) {e$peak <- NULL; e$pi <- NULL}
        e$peak <- append(e$peak, .h[i])
        e$pi <- append(e$pi, i)
        i <- suppressWarnings(which(.h[(i+1):.nrv] > .h[i])[1] + i)
        if (length(i) < 1 || is.na(i)) break
      } else {
        i <- i + 1
      }
    }
    i <- 1
    
    if (xts::is.xts(v)) {
      .dtref <- time(v)[.x]
    } else {
      .dtref <- v[[cl_nm["t"]]][.x]
    }
    if (verbose) print(.x)
    .tsl <- tsl_amt(v, tsl = tsl, .dtref = .dtref, cl_nm = cl_nm)
    while (i <= length(e$pi)) {
      if (i == .x) {
        .e <- ifelse((i + 1) <= length(e$pi), i + 1, length(e$pi))
        # Determine the range prior to the first peak
        .gap <- any(.l[.x:e$pi[.e]] <= e$peak[i] - .tsl)
      } else if (i < length(e$pi)) {
        .e <- ifelse((i + 1) <= length(e$pi), i + 1, length(e$pi))
        # Determine the range between the present peak and the next peak
        .gap <- any(.l[e$pi[i]:e$pi[.e]] <= e$peak[i] - .tsl)
      } else if (i == length(e$pi)) {
        .e <- .nrv
        # Determine the range between the present peak and the next peak
        .gap <- any(.l[e$pi[i]:.e] <= e$peak[i] - .tsl)
      }
      
      # If the range exceeds the trailing stop loss
      if (.gap) {
        if (i == .x) {
          .open <- .x
          .close <- e$pi[.e]
        } else if (i < length(e$pi)) {
          .open <- e$pi[i]
          .close <- e$pi[.e]
        } else {
          .open <- e$pi[i]
          .close <- .e
        }
       .sp <- suppressWarnings(which(.l[.open:.close] <= (e$peak[i] - .tsl))[1] - 1)
       if (is.na(.sp) || length(.sp) < 1) break
       .sp <- ifelse(.open + .sp > .nrv, .nrv, .open + .sp)
       browser(expr = verbose && .x %in% 44:47)
       pct_gain <- ((e$peak[i] - .tsl) - .buy_price) / .buy_price
       .out <- data.frame(pct_gain = pct_gain, d = v[[cl_nm["t"]]][.sp])
       return(.out)
      }
      i <- i + 1  
    }
    pct_gain <- ((e$peak[length(e$peak)] - .tsl) - .buy_price) / .buy_price
    .out <- data.frame(pct_gain = pct_gain, d = NA, row.names = NULL)
    return(.out)
  })
  if (verbose) message(paste0(lubridate::now(), "fn TSL result: ", paste0(out, collapse = ", ")))
  return(out)
}



# # Set the starting point at the first low
# .i <-  e$pi[i]
# # Determine where the sale happens
# 
# while (.i <= .nrv) {
#   # Check for the sale on each day between the two peaks 
#   .s_l <- abs(purrr::map_dbl(unlist(.ohlc[.i,]), ~{diff(c(e$peak[i], .x))})) > .tsl
#   if (any(.s_l)) {
#     pct_gain <- ((e$peak[i] - .tsl) - .buy_price) / .buy_price
#     .out <- data.frame(pct_gain = pct_gain, d = v[[cl_nm["t"]]][.open + .i])
#     return(.out)
#   }
#   # if we reached the last peak, set the first low and li_max
#   if (.open == 1) {
#     # Re-create the vector of lows from the peak forward to circumvent lows prior to the peak influencing the threshold for lows after the peak
#     e$li <- .ii <- e$pi[i]
#     e$low <- .l[.ii]
#     while (.ii <= length(.l)) {
#       if (.l[.ii] < e$low[length(e$low)]) {
#         e$low <- append(e$low, .l[.ii])
#         e$li <- append(e$li, .ii)
#       } 
#       .ii <- .ii + 1
#     }
#     .open <- 2
#     # get the first low
#     .first_low <- try(suppressWarnings({e$li[min(which(e$li >= .i))]}), silent = TRUE)
#     # if there isn't one return the gain if the TSL executes assuming no higher peaks
#     if (inherits(.first_low, "try-error") || is.na(.first_low)) {
#       pct_gain <- ((e$peak[i] - .tsl) - .buy_price) / .buy_price
#       .out <- data.frame(pct_gain = pct_gain, d = v[[cl_nm["t"]]][.i])
#       return(.out)
#     } else {
#       
#       # if there are lows, get the index of the lowest low
#       .li_max <- try(suppressWarnings({e$li[max(which(e$li >= .i))]}), silent = TRUE)
#       e$li_max <- ifelse(inherits(.li_max, "try-error") || is.na(.li_max), e$li_max, .li_max)
#     }
#     # Otherwise set first low to the end of the timeseries to skip the conditional below and increment .i normally
#   }
#   # when the index is after the last peak, first_low has been allocated, and the index is below the last low index (the lowest low)
#   
#   if (.open > 1 && exists(".first_low", inherits = FALSE) && .i < e$li_max) {
#     #increment .i to the new low (skipping all values that will not  cause the TSL to execute)
#     .o <- .i
#     .i <- try(suppressWarnings({e$li[min(which(e$li > .i))]}), silent = TRUE)
#     .i <- ifelse(inherits(.i, "try-error") || is.na(.i), .o + 1, .i)
#   } else {
#     .i <- .i + 1
#   }
# }

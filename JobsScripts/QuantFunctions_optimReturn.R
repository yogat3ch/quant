#' Optimize returns
#' 
#' Allows for optimization of returns from an xts, matrix, data.frame or tibbletime object given the column with returns threshold as percent and the target percentage of returns expected
#' @param .dat \code{[xts, data.frame, tibble_time]} Data to be used **Required**
#' @param returns.clm \code{[character]} The name of the column with returns (in percents) **Required**
#' @param percent \code{[numeric]} Percent value as proportion to used as a threshold for buy. Defaults to 0.05
#' @param tslindex.clm \code{[character]} The name or number of the column with the index of days following the setting of a trailing stop loss before the sale executes. If not supplied, will be \code{paste0(returns.clm,"_ind")}
#' @param .opts \code{[list (logical)]} \itemize{
#' \item{\code{bs.v}: Return a vector of the buy/sell positions as a factor 1 for buy, -1 for sell, the rest 0.}
#' \item{\code{with.gains}:  Return a vector of the corresponding percent gains with each purchase.}
#' \item{\code{max.gain}:  Return a vector of the maximum gain possible with each trailing stop loss holding period.}
#' }
#' @return \itemize \code{[numeric]} A vector indicating buy points as 1, sell points as -1, and all other values 0 with the following attributes:
#' \itemize{
#'  \item{\code{Returns}}{A percent indicating the total percent of returns expected }
#'  \item{\code{Actions}}{ The number of actions (if commission fees are charged, useful for calculating total charges) }
#' }

# For debugging:
#list(.dat = Positions_ts_rv$AMD, percent = .1, returns.clm = "tslret_px0.5_day7_rv", tslindex.clm = NULL, .opts = list(bs.v = T, with.gains = F, max.gain = T)) %>% list2env(envir = .GlobalEnv)
test <- optimReturn(.dat = Positions_ts_rv$AMD, returns.clm = "tslret_px0.5_day7_rv", percent = .1, .opts = list(bs.v = T, with.gains = T, max.gain = T))
 #filter_all(.vars_predicate = dplyr::all_vars({. != 0})) %>% View
optimReturn <- function(.dat, percent = 0.05, returns.clm = NULL, tslindex.clm = NULL, .opts = list(bs.v = F, with.gains = F, max.gain = F), .debug = F) {
  # Set default values to avoid errors on if statements
  if (is.null(.opts$bs.v)) .opts$bs.v <- F 
  if (is.null(.opts$with.gains)) .opts$with.gains <- F 
  if (is.null(.opts$max.gain)) .opts$max.gain <- F
  tslindex.clm <- ifelse(!is.null(tslindex.clm),tslindex.clm,paste0(returns.clm,"_ind"))
  td_nm <- stringr::str_extract(names(.dat), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
  .tsl_i <- rlang::enquo(tslindex.clm)
  v <- .dat[, c(td_nm, returns.clm, tslindex.clm)] %>% dplyr::mutate_at(dplyr::vars((!! .tsl_i)), dplyr::funs(as.POSIXct, .args = list(origin = lubridate::origin)))
  lgl.v <- .dat[, returns.clm, drop = T] > percent
  # if there are no positive values at all
  if (sum(lgl.v) == 0) {
    return(NULL)
  }
  rle.df <- HDA::rleIndex(rle(lgl.v))
  v.ind <- v[, tslindex.clm, drop = T]
  cl_nm <- c(high = stringr::str_extract(colnames(.dat), stringr::regex("^high", ignore_case = T)) %>% subset(subset = !is.na(.)),
             low =  stringr::str_extract(colnames(.dat), stringr::regex("^low", ignore_case = F)) %>% subset(subset = !is.na(.)),
             close = stringr::str_extract(colnames(.dat), stringr::regex("^close", ignore_case = F)) %>% subset(subset = !is.na(.)))
  # if it's a tbl_time object
  if (any(stringr::str_detect(class(.dat), stringr::regex("tbl_time|data.frame")))){
    # add the date/time column
    cl_nm <- c(time = stringr::str_extract(colnames(.dat), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1], cl_nm)
  }
  out.res <- purrr::map(1:20, .pf = sys.frame(sys.nframe()), .opts = .opts, function(.x, .pf, .opts){
    # Compute the first transaction
    i <- which(.pf$lgl.v)[.x] # The index of the first buy
    sell_dt <- .pf$v.ind[i] # Get the date the TSL is sold
    
    # If the order is just open and doesn't sell
    if (is.na(sell_dt)) {
      if (.opts$bs.v) {
        bs.v <- rep.int(0, length(.pf$lgl.v))
        bs.v[i] <- 1
      }
      # If the gains for each buy point are requested
      if (.opts$with.gains) {
        with.gains <- rep.int(0, length(.pf$lgl.v))
        with.gains[i] <- NA_complex_
      }
      # if the range for each buy-sell date range are requested
      if (.opts$max.gain) {
        max.gain <- rep.int(0, length(.pf$lgl.v))
        max.gain[i] <- NA_complex_
      }
      out <- list()
      if (.opts$bs.v | .opts$with.gains | .opts$max.gain){
        if (.opts$bs.v) out$bs.v <- bs.v
        if (.opts$with.gains)  out$with.gains <- with.gains
        if (.opts$max.gain) out$max.gain <- max.gain
      } 
      out$returns <- c(Returns = 0, Cum.Returns = 0, shares = 1)
      return(out)
    }
    i_sell <- which(.pf$v[, .pf$cl_nm["time"], drop = T] == sell_dt) # Get the index of the day it's sold
    # Start with initial parameters
    total.returns <- 0  #The running investment & initial
    total.value <- init.inv <- as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T]) # the amt of initial investment
    shares <- 1 # The number of shares
    # If a buy sell factor is requested
    if (.opts$bs.v) {
      bs.v <- rep.int(0, length(.pf$lgl.v))
      bs.v[i] <- 1
      bs.v[i_sell] <- -1 # add sell to out.vector
    }
    # If the gains for each buy point are requested
    if (.opts$with.gains) {
      with.gains <- rep.int(0, length(.pf$lgl.v))
      with.gains[i] <- .pf$v[i, returns.clm, drop = T]
    }
    # if the range for each buy-sell date range are requested
    if (.opts$max.gain) {
      max.gain <- rep.int(0, length(.pf$lgl.v))
      max.gain[i] <- (max(.pf$.dat[i:i_sell, cl_nm[c("high")], drop = T] %>% unlist) - .pf$.dat[i, cl_nm[c("close")], drop = T]) / .pf$.dat[i, cl_nm[c("close")], drop = T]
    }
    # Calculate running values
    total.value <- as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) * shares # as total position value
    rem <- total.value %% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])
    total.returns <- (as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) - as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])) / init.inv + total.returns # total returns as numeric
    
    # While loop?
    i <- i_sell + 1
    while (i <= length(.pf$lgl.v)) {
      if (.pf$lgl.v[i]) { # if the immediate next value is a positive gain
        sell_dt <- .pf$v.ind[i] # Get the date the TSL is sold
        i_sell <- which(.pf$v[, .pf$cl_nm["time"], drop = T] == sell_dt) # Get the index of the day it's sold
        #if(!HDA::go(i_sell)) browser()
        # If the order is just open and doesn't sell
        if (is.na(sell_dt)) {
          if (.opts$bs.v) {
            bs.v[i] <- 1
          }
          # If the gains for each buy point are requested
          if (.opts$with.gains) {
            with.gains[i] <- NA_complex_
          }
          # if the range for each buy-sell date range are requested
          if (.opts$max.gain) {
            max.gain[i] <- NA_complex_
          }
          out <- list()
          if (.opts$bs.v | .opts$with.gains | .opts$max.gain){
            if (.opts$bs.v) out$bs.v <- bs.v
            if (.opts$with.gains)  out$with.gains <- with.gains
            if (.opts$max.gain) out$max.gain <- max.gain
          } 
          out$returns <- c(Returns = total.returns, Cum.Returns = (total.value - init.inv) / init.inv, shares = shares)
          return(out)
        }
        if (.opts$bs.v) {
          bs.v[i] <- 1
          bs.v[i_sell] <- -1 # add sell to out.vector
        }
        if (.opts$with.gains) { # tracking the gains
          with.gains[i] <- .pf$v[i, returns.clm, drop = T] # add the returns
        }
        if (.opts$max.gain) {
          if ({!HDA::go("i") | !HDA::go("i_sell")} & .pf$.debug) message(paste0("i:",i,"i_sell:",i_sell,"i:i_sell", paste0(i:i_sell, collapse = "_")))
          max.gain[i] <- (max(.pf$.dat[i:i_sell, cl_nm[c("high")], drop = T] %>% unlist) - .pf$.dat[i, cl_nm[c("close")], drop = T]) / .pf$.dat[i, cl_nm[c("close")], drop = T]
        }
        if (total.value %/% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T]) < 1 ) break # Break if the value drops below being able to buy a share
        shares <- {total.value %/% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])} #Update shares
        rem <- total.value %% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T]) # calculate the remainder left when purchasing additional shares
        # Calculate running values
        total.value <- as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) * shares + rem # as total position value
        
        total.returns <- (as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) - as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])) / init.inv + total.returns # total returns as numeric
        
      } else { # if the immediate next value is a loss
        # Get the index of beginning of the next section of positive gains
        i <- try(.pf$rle.df %>% dplyr::filter(values == T & start > i) %>% dplyr::select(start) %>% .[1,,drop = T])
        if (!HDA::go("i")) break
        sell_dt <- .pf$v.ind[i] # Get the date the TSL is sold
        i_sell <- which(.pf$v[, .pf$cl_nm["time"], drop = T] == sell_dt) # Get the index of the day it's sold
        #if(!HDA::go(i_sell)) browser()
        # If the order is just open and doesn't sell
        if (is.na(sell_dt)) {
          if (.opts$bs.v) {
            bs.v[i] <- 1
          }
          # If the gains for each buy point are requested
          if (.opts$with.gains) {
            with.gains[i] <- NA_complex_
          }
          # if the range for each buy-sell date range are requested
          if (.opts$max.gain) {
            max.gain[i] <- NA_complex_
          }
          out <- list()
          if (.opts$bs.v | .opts$with.gains | .opts$max.gain){
            if (.opts$bs.v) out$bs.v <- bs.v
            if (.opts$with.gains)  out$with.gains <- with.gains
            if (.opts$max.gain) out$max.gain <- max.gain
          } 
          out$returns <- c(Returns = total.returns, Cum.Returns = (total.value - init.inv) / init.inv, shares = shares)
          return(out)
        }
        if (.opts$bs.v) {
          bs.v[i] <- 1
          bs.v[i_sell] <- -1 # add sell to out.vector
        }
        if (.opts$with.gains) {
          with.gains[i] <- .pf$v[i, returns.clm, drop = T]
        }
        if (.opts$max.gain) {
          if ({!HDA::go("i") | !HDA::go("i_sell")} & .pf$.debug) message(paste0("i:",i,"i_sell:",i_sell,"i:i_sell", paste0(i:i_sell, collapse = "_")))
          max.gain[i] <- (max(.pf$.dat[i:i_sell, cl_nm[c("high")], drop = T] %>% unlist) - .pf$.dat[i, cl_nm[c("close")], drop = T]) / .pf$.dat[i, cl_nm[c("close")], drop = T]
        }
        if (total.value %/% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T]) < 1 ) break # Break if the value drops below being able to buy a share
        shares <- {total.value %/% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])} #Update shares
        rem <- total.value %% as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T]) # calculate the remainder left when purchasing additional shares
        # Calculate running values
        total.value <- as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) * shares + rem # as total position value
        
        total.returns <- (as.numeric(.pf$.dat[i_sell, .pf$cl_nm["close"], drop = T]) - as.numeric(.pf$.dat[i, .pf$cl_nm["close"], drop = T])) / init.inv + total.returns # total returns as numeric
      }
    i <- i_sell + 1
    }
    out <- list()
    if (.opts$bs.v | .opts$with.gains | .opts$max.gain){
      if (.opts$bs.v) out$bs.v <- bs.v
      if (.opts$with.gains)  out$with.gains <- with.gains
      if (.opts$max.gain) out$max.gain <- max.gain
    } 
      out$returns <- c(Returns = total.returns, Cum.Returns = (total.value - init.inv) / init.inv, shares = shares)
    
  return(out)    
  })
  out.res <- purrr::compact(out.res)
  # Create final aggregate data structure
  out <- list()
  if (.opts$bs.v | .opts$with.gains | .opts$max.gain) {
    out$.opts <- purrr::map_dfc(out.res, .opts = .opts, function(.x, .opts){
      out <- data.frame(rem = rep(NA, length(.x$bs.v)))
      if (.opts$bs.v) out <- cbind(out,bs.v = .x$bs.v)
      if (.opts$with.gains) out <- cbind(out, with.gains = .x$with.gains)
      if (.opts$max.gain) out <- cbind(out, max.gain = .x$max.gain)
      out <- out %>% dplyr::select( - rem)
      return(out) 
    })
  }
    out$returns <- do.call("rbind", purrr::map(out.res, `[[`, "returns"))
  return(out)
}

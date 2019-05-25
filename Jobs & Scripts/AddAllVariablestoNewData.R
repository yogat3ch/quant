# TODO( - Will the decision tree work without all of the variables? Some variables cannot be created because not enough data exists
# - Rework the functions to create variables for each window that is not longer than the length of the data [Complete])
HDA::startPkgs(c("magrittr", "xts", "tidyverse"))
# Load New Data
gsPositions <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
close <- googlesheets::gs_read(gsPositions, ws = 'Holdings')
high <- googlesheets::gs_read(gsPositions, ws = '(Hi)')
low <- googlesheets::gs_read(gsPositions, ws = '(Lo)')
close$Date %<>% lubridate::mdy_hms() %>% format("%m/%d/%Y") %>% lubridate::mdy()
high$Date %<>% lubridate::mdy_hms() %>% format("%m/%d/%Y") %>% lubridate::mdy()
low$Date %<>% lubridate::mdy_hms() %>% format("%m/%d/%Y") %>% lubridate::mdy()
Positions_hlc <- left_join(close, high, by = "Date", suffix = c(".close",".high")) %>% left_join(low, by = "Date")
toNum <- function(x){
  x %>% stringr::str_replace_all("\\$|\\,","") %>% as.numeric
}

load("Positions_ts2014-05-08_2019-05-07.Rdata")
tickers <- names(close)[-1]
names(tickers) <- names(close)[-1]
Positions_new <-  purrr::map(.x = tickers, dat = Positions_hlc %>% filter(Date > {(time(Positions_ts[[1]]) %>% max) - 200}), toNum = toNum, .f = function(.x, dat, toNum){
  ind <- c(paste0(.x,".close"), paste0(.x, ".high"), .x)
  xts.dat <- dat[, ind] %>% mutate_all(funs(toNum))
  out <- xts(xts.dat, order.by = dat[["Date"]])
  xtsAttributes(out) <- list(Sym = .x)
  colnames(out) <- c("close","high","low")
  return(out)
})
#Fill dates for proper windowing
Positions_new %<>% lapply(function(l){ # Actual Time for measuring exact weeks, quarters and approx. months
  out <- xts(order.by = seq(min(time(l)), max(time(l)), by = "1 days")) %>% merge.xts(l) %>% na.locf()
  
})
rm(Positions_ts, Positions_hlc)
# Set Windows
wind  <-  c(weeks = 7, moonphase = 7*2, mooncycle = 7*4, quarters = 7*4*3)
# Add Simple Moving Averages
Positions_new %<>% lapply(wind = c(wind,200) , function(l, wind){
  if (all(!is.na(l[,c("high","low","close")])) ) {
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
    out_sma <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], l = l, function(.x, l){
      out <- TTR::SMA(l[,c("close")], .x)
      for (i in which(is.na(out))) {
        out[i] <- mean(l[1:i,"close", drop = T])
      }
      names(out) <- paste(names(out), .x, sep = ".") 
      return(out)
    }) 
    out_sma <- do.call("cbind", out_sma)
    if (stringr::str_which(names(l), "SMA\\.\\d{1,2}") %>% length > 0) l <- l[,-stringr::str_which(names(l), "SMA\\.\\d{1,2}")]
    out <- cbind(l, out_sma) 
    } else out <- l
  return(out)
}) 
# Add ADX
Positions_new %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    if (verbose) print(xtsAttributes(l))
    adx <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], l = l, env = parent.env(), function(.x, l, env){
      if (verbose) print(.x)
      out <- TTR::ADX(l[,c("high","low","close")],n = .x)
      names(out) <- paste(names(out), .x, sep = ".") 
      return(out)
    }) 
    adx <- do.call("cbind", adx)
    out <- cbind(l, adx) %>% na.locf(fromLast = T)
  }
  else out <- l
  return(out)
}) 

# Add ADX signal
Positions_new %<>% lapply(threshold1 = 20, threshold2 = 25, wind = wind, verbose = F, function(l, threshold1, threshold2, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")]))  ) {
    out <- list()
    for (i in seq_along(wind[sum(!is.na(l[,"close"])) > wind])) {
      cls <- c(paste0(c("ADX.","DIp.","DIn.","DX.","SMA."), wind[i]),"close")
      if (verbose) print(cls)
      out[[i]] <- apply(l[,cls], 1, function(r, env = parent.frame()){
        if (any(is.na(r))) {return("0")} # Check for NA and return none if present
        if (r[[cls[1]]] > threshold1 & r[[cls[2]]] > r[[cls[3]]] & r[["close"]] > r[[cls[4]]]) {
          return("1") # Moderate uptrend
        }else if (r[[cls[1]]] > threshold2 & r[[cls[2]]] > r[[cls[3]]] & r[["close"]] > r[[cls[4]]]) {
          return("2") # Strong uptrend
        }else if (r[[cls[1]]] > threshold1 & r[[cls[2]]] < r[[cls[3]]] & r[["close"]] < r[[cls[4]]]) {
          return("-1") # Moderate Downtrend
        }else if (r[[cls[1]]] > threshold2 & r[[cls[2]]] < r[[cls[3]]] & r[["close"]] < r[[cls[4]]]) {
          return("-2") # Strong downtrend
        } else return("0")
        if (verbose == T) print(time(r))
      })
    }
  } else return(l)
  nms <- paste0("ADX.", wind[sum(!is.na(l[,"close"])) > wind], "_i")
  out <- do.call("cbind", out) %>% xts(order.by = time(l))
  colnames(out) <- nms
  if (verbose == T) print(identical(time(out), time(l)))
  out <- cbind.xts(l, out)
  if (verbose == T) print(nrow(out))
  return(out)
  
})
# Compress ADX
Positions_new %<>% lapply(wind = wind, compress = T, function(l, wind, compress){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    adx <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      out <- TTR::ADX(l[,c("high","low","close")], .x) %>% na.locf(fromLast = T)
      if (compress) {
        out <- apply(out, 1, function(r){
          out <- r[["DIp"]] - r[["DIn"]] * log(ifelse(is.na(r[["ADX"]]),r[["DX"]],r[["ADX"]]))
        }) %>% xts::as.xts(order.by = time(l))
        colnames(out) <- paste("ADXc", .x, sep = ".")
      }else {colnames(out) <- paste(colnames(out), .x, sep = ".")}
      return(out)
    }) 
    adx <- do.call("cbind", adx)
    l <- l[,-grep("^ADX|DI\\w.*\\d$",names(l))]
    out <- cbind(l, adx) 
  }
  else out <- l
  return(out)
}) 

# Add Williams Percent R
Positions_new %<>% lapply(wind = c(7, 7*4, 7*4*3), verbose = F, function(l, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    WpR <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], function(.x, env = parent.frame()){
      out <- TTR::WPR(l[,c("high","low","close")],n = .x)
      for (i in which(is.na(out))) {
        hmax <- max(l[1:i,"high"])
        if (verbose) print(hmax)
        lmin <- min(l[1:i,"low"])
        if (verbose) print(lmin)
        pctR <- ifelse({(hmax - l[i,"close"])/(hmax - lmin)} %>% is.nan, 0.5, (hmax - l[i,"close"])/(hmax - lmin))
        out[i] <- pctR
      }
      return(out)
    })
    if (verbose) print(WpR)
    WpR <- do.call("cbind", WpR)
    colnames(WpR) <- paste("WpR", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
    if (stringr::str_which(names(l),"^WpR\\.\\d{1,2}") %>% length > 0) l <- l[,-c(stringr::str_which(names(l),"^WpR\\.\\d{1,2}"))]
    out <- cbind(l,WpR) }
  else out <- l
  return(out)
})
# Add RSI
rsi_wind <- c(7, 14)
Positions_new %<>% lapply(wind = rsi_wind, verbose = F, function(l, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    rsic <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], l = l, function(.x, l){
      out <- TTR::RSI(l[,c("close")], n = .x)
      out <- na.locf(out, fromLast = T)
      return(out)
    })
    if (verbose == T) any(is.na(rsic)) %>% print
    rsic <- do.call("cbind", rsic)
    colnames(rsic) <- paste("rsi", wind, sep = ".")
    if (grep("^rsi\\.\\d{1,2}$",names(l)) %>% length > 0) l <- l[, -grep("^rsi\\.\\d{1,2}$",names(l))]
    out <- cbind(l,rsic)
  } else out <- l
  return(out)
})

# Add RSI Indicator
Positions_new %<>% lapply(wind = rsi_wind, threshold1 = c(high = 70, low = 30), threshold2 = c(high = 80, low = 20), verbose = F, function(l, wind, threshold1, threshold2, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    out <- list()
    for (i in seq_along(wind[sum(!is.na(l[,"close"])) > wind])) { # Up to the second to last window
      cls <- c(paste0(c("rsi."), wind[i]), "SMA.200","close")
      out[[i]] <- apply(l[,cls], 1, function(r, env = parent.frame()){
        if (any(is.na(r))) {return("0")} # Check for NA and return none if present
        if (r[[cls[1]]] < threshold1["low"] & r[[cls[2]]] < r[["close"]] ) {
          return("-1") # Moderate oversold
        }else if (r[[cls[1]]] < threshold2["low"] & r[[cls[2]]] < r[["close"]]) {
          return("-2") # Strong oversold
        }else if (r[[cls[1]]] > threshold1["high"] & r[[cls[2]]] > r[["close"]]) {
          return("1") # Moderate overbought
        }else if (r[[cls[1]]] > threshold2["high"] & r[[cls[2]]] > r[["close"]]) {
          return("2") # Strong overbought
        } else return("0")
        if (verbose == T) print(time(r))
      })
    }
    nms <- paste0("rsi.", wind, "_i")
    out <- do.call("cbind", out) %>% xts(order.by = time(l))
    colnames(out) <- nms
    if (verbose == T) print(identical(time(out), time(l)))
    if (grep("^rsi.*i$",names(l)) %>% length > 0) l <- l[,-grep("^rsi.*i$",names(l))]
    out <- cbind.xts(l, out)
    if (verbose == T) print(nrow(out))
  }else return(l)
  return(out)
})
# Add ROC And Momentum
Positions_new %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) ) {
    # ROC
    roc <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(),function(.x, env){
      roc <- TTR::ROC(l[,c("close")],n = .x)
      for (i in which(is.na(roc))) {
        roc[i + 1] <- diff(log(l[,c("close")]),i)[i + 1]
      }
      roc[1] <- 0
      return(roc)
    })
    # momentum
    mom <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      mom <- TTR::momentum(l[,c("close")], n = .x)
      for (i in which(is.na(mom))) {
        mom[i + 1] <- diff(l[,c("close")],i)[i + 1]
      }
      mom[1] <- 0
      return(mom)
    })
    # create  out object
    roc <- do.call("cbind", roc)
    mom <- do.call("cbind", mom)
    if (verbose) print(c(nrow(roc),nrow(mom)))
    colnames(roc) <- paste("roc", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
    colnames(mom) <- paste("mom", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
    if (grep("^roc|^mom",names(l)) %>% length > 0) l <- l[,-grep("^rsi.*i$",names(l))]
    out <- cbind.xts(l, roc, mom)
  } else out <- l
  
  
  if (verbose == T) print(nrow(out))
  return(out)
})
# Add ATR
Positions_new %<>% lapply(wind = sort(c(wind,14)), verbose = F, function(l, wind, verbose){
  if (nrow(l) > max(wind)) message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(quantmod::HLC(l))) ) {
    atrs <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      atr <- TTR::ATR(quantmod::HLC(l), n = .x)
      atr <- na.locf(atr, fromLast = T)
      names(atr) <- names(atr) %>% paste(.x, sep = ".")
      return(atr)
    })
    if (verbose == T) any(is.na(atrs)) %>% print
    atrs <- do.call("cbind", atrs)
    if (grep("^atr|^trueLow",names(l)) %>% length > 0) l <- l[,-grep("^rsi.*i$",names(l))]
    out <- cbind(l,atrs[,c(atrs %>% names %>% grep("atr",.), atrs %>% names %>% grep("trueLow.14",.))])
  } else out <- l
  return(out)
})
# Add SAR
Positions_new %<>% lapply(verbose = F, function(l, verbose){
  if (all(!is.na(quantmod::HLC(l))) ) {
    sar <- TTR::SAR(quantmod::HLC(l), accel = c(0.05, 0.2))
    if (verbose) any(is.na(sar)) %>% print
    if (grep("sar", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("sar", names(l), ignore.case = T)]
    out <- cbind(l,sar)
  } else out <- l
  return(out)
})
# Add SAR Indicator
Positions_new %<>% lapply(verbose = F, function(l, verbose){
  
  if (all(!is.na(quantmod::HLC(l))) ) {
    out <- rollapply(l[,grep("high|low|close|sar", names(l), ignore.case = T)], width = 2, align = "right", by.column = F, env = parent.frame(), function(r, env){
      if (verbose == T) print(r)
      # Names of each of the columns
      sar_nm <- grep("sar", names(r), ignore.case = T)
      low_nm <- grep("low", names(r), ignore.case = T)[1]
      high_nm <- grep("high", names(r), ignore.case = T)[1]
      close_nm <- grep("close", names(r), ignore.case = T)[1]
      if (verbose == T) print(c(SAR = sar_nm, Low = low_nm, Hi = high_nm, Cl = close_nm))
      # SAR values
      p_sar <- as.numeric(r[1,sar_nm])
      n_sar <- as.numeric(r[2,sar_nm])
      p_hi <- as.numeric(r[1,high_nm])
      p_lo <- as.numeric(r[1,low_nm])
      n_hi <- as.numeric(r[2,high_nm])
      n_lo <- as.numeric(r[2,low_nm])
      if (p_hi < p_sar & n_hi > n_sar) {return(-1)
      }else if (p_lo > p_sar & n_lo < n_sar) {return(1)
      }else {return(0)} 
    })
    out <- na.locf(out, fromLast = T)
    colnames(out) <- paste0("sar", "_i")
    if (verbose) any(is.na(out)) %>% print
    if (grep("sar_i\\.?\\d?", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("sar_i\\.?\\d?", names(l), ignore.case = T)]
    out <- cbind(l,out)
  } else out <- l
  return(out)
})
# Add MACD Compressed
Positions_new %<>% purrr::map(wind = wind, function(l, wind){
  if (nrow(l) > max(wind)) {message(paste0("Number of observations in", xtsAttributes(l, user = T)," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))}
  wind <- wind[sum(!is.na(l[,"close"])) > wind]
  if (length(wind) < 2) stop("At least two window values must be fewer than the number of observations in the data")
  
  if (all(!is.na(quantmod::HLC(l))) ) {
    macd <- TTR::MACD(quantmod::Cl(l), nFast = wind[1] * 2, nSlow = wind[2], nSig = wind[1], maType = TTR::EMA)
    macd[,"macd"] %<>%  na.locf(fromLast = T)
    macd[,"signal"] %<>% na.locf(fromLast = T)
    macd_dif <- macd[,"macd"] - macd[,"signal"]
    if (grep("macd\\.?\\d?", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("macd\\.?\\d?", names(l), ignore.case = T)]
    out <- cbind(l, macd_dif)
  } else out <- l
  return(out)
})
# Add trailing stop loss response variables
source("Jobs & Scripts/QuantFunctions_TSL.R")
Positions_new %<>% lapply(TSLvars = list(tsla = function(x){
  cl_cl <- stringr::str_which(colnames(x),"close")
  sd(x[,cl_cl]) * .5
}, tslp = .15, tslp = .2, retro = c(n = 7, hilop = .9), retro = c(n = 14, hilop = .9), retro = c(n = 28, hilop = .9)), verbose = T, function(l, TSLvars, verbose){
  if (all(!is.na(quantmod::HLC(l))) ) {
    rvs <- purrr::map2(.x = TSLvars, .y = names(TSLvars), env = parent.frame(), function(.x, .y, env){
      if (verbose) print(.x)
      args <- list(quantmod::HLC(l), .x)
      names(args) <- c("v", .y)
      out <- eval(rlang::call2("TSL", !!!args))
      return(out)
    })
    if (verbose) any(is.na(rvs)) %>% print
    rvs <- xts(do.call("cbind",rvs), order.by = time(l))
    nms <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
      if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
      return(nm)
    }) %>% unlist
    colnames(rvs) <- nms
    if (grep("rv",names(l)) %>% length > 0) l  <- l[, - grep("rv",names(l))]
    out <- cbind.xts(rvs, l)
  } else out <- l
  return(out)
})


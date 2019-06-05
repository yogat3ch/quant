# TODO( - Created to add all signals to loaded data called dat)
HDA::startPkgs(c("magrittr", "xts", "tidyverse"))
# Load New Data
load("dat.Rdata")
## @knitr Add attributes for debugging purposes
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
 attr(.x, "Sym") <- .y
  return(.x)
 })
## @knitr Add Simple Moving Averages
dat %<>% lapply(wind = c(wind,200) , function(l, wind){
  att <- attr(l, "Sym")
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", attr(l, "Sym")," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
    out_sma <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], l = l, function(.x, l){
      out <- TTR::SMA(l[,c("close")], .x)
      for (i in which(is.na(out))) {
        out[i] <- mean(l[1:i,"close", drop = T])
      }
      return(out)
    })
    out_sma <- do.call("cbind", out_sma)
    colnames(out_sma) <- paste("SMA", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
    if (stringr::str_which(names(l), "SMA\\.\\d{1,2}") %>% length > 0) l <- l[,-stringr::str_which(names(l), "SMA\\.\\d{1,2}")]
    
 message(paste0(attr(l, "Sym"),": SMA"))
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out_sma) else out <- tibbletime::tbl_time(cbind.data.frame(l, out_sma), index = "Time")
    attr(out, "Sym") <- att
    } else out <- l
  return(out)
})
## @knitr Add ADX
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    if (verbose) print(att)
    adx <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], l = l, env = parent.env(), function(.x, l, env){
      if (verbose) print(.x)
      out <- TTR::ADX(l[,c("high","low","close")],n = .x)
      return(out)
    })
    adx <- do.call("cbind", adx) %>% na.locf(fromLast = T)
    colnames(adx) <- paste(colnames(adx), rep(wind[sum(!is.na(l[,"close"])) > wind], each =4 ), sep = ".")
    if (stringr::str_which(names(l),"ADX|DI\\w?|DX|DX\\w.*\\d{1,2}?\\.?\\d{1,2}?$") %>% length > 0) l <- l[,-c(stringr::str_which(names(l),"ADX|DI\\w?|DX|DX\\w.*\\d{1,2}?\\.?\\d{1,2}?$"))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, adx) else out <- tibbletime::tbl_time(cbind.data.frame(l, adx), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ADX"))
  }
  else out <- l
  return(out)
})

## @knitr Add ADX signal
dat %<>% lapply(threshold1 = 20, threshold2 = 25, wind = wind, verbose = F, function(l, threshold1, threshold2, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (magrittr::not(all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) & any(stringr::str_detect(names(l),"^DX")))) return(l)
    out <- list()
    for (i in seq_along(wind[sum(!is.na(l[,"close"])) > wind])) {
      cls <- c(paste0(c("ADX.","DIp.","DIn.","DX.","SMA."), wind[i]),"close")
      if (verbose) print(cls)
      out[[i]] <- apply(l[,cls], 1, function(r, env = parent.frame()){
        if (any(is.na(r))) {return(1)} # Check for NA and return none if present
        if (r[[cls[1]]] > threshold1 & r[[cls[2]]] > r[[cls[3]]] & r[["close"]] > r[[cls[4]]]) {
          return(1) # Moderate uptrend
        }else if (r[[cls[1]]] > threshold2 & r[[cls[2]]] > r[[cls[3]]] & r[["close"]] > r[[cls[4]]]) {
          return(2) # Strong uptrend
        }else if (r[[cls[1]]] > threshold1 & r[[cls[2]]] < r[[cls[3]]] & r[["close"]] < r[[cls[4]]]) {
          return(-1) # Moderate Downtrend
        }else if (r[[cls[1]]] > threshold2 & r[[cls[2]]] < r[[cls[3]]] & r[["close"]] < r[[cls[4]]]) {
          return(-2) # Strong downtrend
        } else return(0)
        if (verbose) print(time(r))
      })
    }
  
  if (length(wind[sum(!is.na(l[,"close"])) > wind]) > 1) nms <- paste0("ADX.", wind[sum(!is.na(l[,"close"])) > wind], "_i") else nms <- "ADX_i"
  if (xts::is.xts(l)) {
  out <- do.call("cbind", out) %>% xts(order.by = time(l))
  if (verbose) print(list(xtsAttributes(l), Data = out))
  colnames(out) <- nms
  out <- cbind.xts(l, out)} else {
    out <- do.call("cbind", out)
    colnames(out) <- nms
    out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = "Time")
  }
  attr(out, "Sym") <- att
  message(paste0(attr(l, "Sym"),": ADX_i"))
 
  if (verbose) print(nrow(out))
  return(out)

})
## @knitr Add Compressed ADX
dat %<>% lapply(wind = wind, compress = T, function(l, wind, compress){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) & any(stringr::str_detect(names(l),"ADX")) ) {
    adx <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      out <- TTR::ADX(l[,c("high","low","close")], .x) %>% na.locf(fromLast = T)
      if (compress) {
        out <- apply(out, 1, function(r){
          out <- r[["DIp"]] - r[["DIn"]] * log(ifelse(is.na(r[["ADX"]]),r[["DX"]],r[["ADX"]]))
        }) 
        if (xts::is.xts(l)) {out %<>% xts::as.xts(order.by = time(l))} else out %<>% as.data.frame
        
        colnames(out) <- paste("ADXc", .x, sep = ".")
      }else {colnames(out) <- paste(colnames(out), .x, sep = ".")}
      return(out)
    })
    adx <- do.call("cbind", adx)
    if (stringr::str_which(names(l),"\\w?DI|\\w?DX\\w?\\.?\\d{1,2}?\\.?\\d{1,2}?$") %>% length > 0 | stringr::str_which(names(l),"\\w?DI|\\w?DX\\w?\\.?\\d{1,2}?$") %>% length > 0) {
      l <- l[,-c(stringr::str_which(names(l),"\\w?DI|\\w?DX\\w?\\.?\\d{1,2}?\\.?\\d{1,2}?$"))]
      l <- l[,-c(stringr::str_which(names(l),"\\w?DI|\\w?DX\\w?\\.?\\d{1,2}?$"))]
    }
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, adx) else out <- tibbletime::tbl_time(cbind.data.frame(l, adx), index = "Time")
    attr(out, "Sym") <- att
    message(paste0(attr(l, "Sym"),": ADXc"))
  }
  else out <- l
  return(out)
})

## @knitr Add Williams Percent R
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (magrittr::not(all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind)) ) return(l)
    WpR <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], function(.x, env = parent.frame()){
      out <- TTR::WPR(l[,c("high","low","close")],n = .x)
      for (i in which(is.na(out))) {
        hmax <- max(l[1:i,"high", drop = T])
        if (verbose) print(hmax)
        lmin <- min(l[1:i,"low", drop = T])
        if (verbose) print(lmin)
        pctR <- ifelse({(hmax - l[i,"close", drop = T])/(hmax - lmin)} %>% is.nan, 0.5, (hmax - l[i,"close", drop = T])/(hmax - lmin))
        out[i] <- pctR
      }
      return(out)
    })
    if (verbose) print(WpR)
    WpR <- do.call("cbind", WpR)
    colnames(WpR) <- paste("WpR", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
    if (stringr::str_which(names(l),"^WpR\\.\\d{1,2}") %>% length > 0) l <- l[,-c(stringr::str_which(names(l),"^WpR\\.\\d{1,2}"))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, WpR) else out <- tibbletime::tbl_time(cbind.data.frame(l, WpR), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": WpR"))
    
  return(out)
})
## @knitr Add RSI
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    rsic <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], l = l, function(.x, l){
      out <- TTR::RSI(l[,c("close")], n = .x)
      out <- na.locf(out, fromLast = T)
      return(out)
    })
    if (verbose == T) any(is.na(rsic)) %>% print
    rsic <- do.call("cbind", rsic)
    colnames(rsic) <- paste("rsi", wind, sep = ".")
    if (grep("^rsi\\.\\d{1,2}$",names(l)) %>% length > 0) l <- l[, -grep("^rsi\\.\\d{1,2}$",names(l))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,rsic) else out <- tibbletime::tbl_time(cbind.data.frame(l,rsic), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": RSI"))
  } else out <- l
  return(out)
})

## @knitr Add RSI Indicator
dat %<>% lapply(wind = wind, threshold1 = c(high = 70, low = 30), threshold2 = c(high = 80, low = 20), verbose = F, function(l, wind, threshold1, threshold2, verbose){
  att <- attr(l, "Sym") 
 if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (magrittr::not(all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind)) ) return(l)
    out <- list()
    for (i in seq_along(wind[sum(!is.na(l[,"close"])) > wind])) { # Up to the second to last window
      cls <- c(paste0(c("rsi."), wind[i]), "SMA.200","close")
      out[[i]] <- apply(l[,cls], 1, function(r, env = parent.frame()){
        if (any(is.na(r))) {return(0)} # Check for NA and return none if present
        if (r[[cls[1]]] < threshold1["low"] & r[[cls[2]]] < r[["close"]] ) {
          return(1) # Moderate oversold
        }else if (r[[cls[1]]] < threshold2["low"] & r[[cls[2]]] < r[["close"]]) {
          return(2) # Strong oversold
        }else if (r[[cls[1]]] > threshold1["high"] & r[[cls[2]]] > r[["close"]]) {
          return(-1) # Moderate overbought
        }else if (r[[cls[1]]] > threshold2["high"] & r[[cls[2]]] > r[["close"]]) {
          return(-2) # Strong overbought
        } else return(0)
        if (verbose == T) print(time(r))
      })
    }
    nms <- paste0("rsi.", wind, "_i")
    out <- do.call("cbind", out)
    colnames(out) <- nms
    if (verbose == T) print(identical(time(out), time(l)))
    if (grep("^rsi.*i$",names(l)) %>% length > 0) l <- l[,-grep("^rsi.*i$",names(l))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out) else out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": RSI_i"))
    if (verbose == T) print(nrow(out))
  return(out)
})
## @knitr Add ROC And Momentum
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym") 
 if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    # ROC
    roc <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(),function(.x, env){
      roc <- TTR::ROC(l[,c("close")],n = .x)
      for (i in which(is.na(roc))) {
        roc[i + 1] <- diff(log(l[,c("close"), drop = T]),i)[i + 1]
      }
      roc[1] <- 0
      return(roc)
    })
    # momentum
    mom <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      mom <- TTR::momentum(l[,c("close")], n = .x)
      for (i in which(is.na(mom))) {
        mom[i + 1] <- diff(l[,c("close"), drop = T],i)[i + 1]
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
    if (grep("^roc|^mom",names(l)) %>% length > 0) l <- l[,-grep("^roc|^mom",names(l))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, roc, mom) else out <- tibbletime::tbl_time(cbind.data.frame(l, roc, mom), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ROC & MOM"))
  } else out <- l

  if (verbose == T) print(nrow(out))
  return(out)
})
## @knitr Add ATR
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym") 
 if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (!all(!is.na(quantmod::HLC(l)) & length(wind[sum(!is.na(l[,"close"])) > wind]) > 0) ) return(l)
    atrs <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      atr <- TTR::ATR(quantmod::HLC(l), n = .x)
      atr <- na.locf(atr, fromLast = T)
      colnames(atr) <- colnames(atr) %>% paste(.x, sep = ".")
      return(atr)
    })
    if (verbose == T) any(is.na(atrs)) %>% print
    atrs <- do.call("cbind", atrs)
    out <- cbind(atrs[,c(atrs %>% colnames %>% grep("atr",.), atrs %>% colnames %>% grep("trueLow\\.\\d{1,3}?",.))])
    if (grep("atr|trueLow",names(l)) %>% length > 0) l <- l[,-grep("atr|trueLow",names(l))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out) else out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ATR"))
  
  return(out)
})
## @knitr Add SAR
dat %<>% lapply(verbose = F, function(l, verbose){
  att <- attr(l, "Sym")
  if (all(!is.na(quantmod::HLC(l))) ) {
    if (verbose) print(xtsAttributes(l))
    sar <- TTR::SAR(quantmod::HLC(l), accel = c(0.05, 0.2))
    if (verbose) any(is.na(sar)) %>% print
    if (grep("sar", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("sar", names(l), ignore.case = T)]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,sar) else out <- tibbletime::tbl_time(cbind.data.frame(l,sar), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": SAR"))
  } else out <- l
  return(out)
})
## @knitr Add SAR Indicator
dat %<>% lapply(verbose = F, function(l, verbose){
  att <- attr(l,"Sym")
  if (all(!is.na(quantmod::HLC(l))) ) {
    out <- zoo::rollapply(l[,grep("high|low|close|sar", names(l), ignore.case = T)], na.pad = T, width = 2, align = "right", by.column = F, env = parent.frame(), function(r, env){
      if (verbose) print(r %>% colnames)
      # Names of each of the columns
      sar_nm <- stringr::str_which(colnames(r),"sar")
      low_nm <- stringr::str_which(colnames(r), "low")
      high_nm <- stringr::str_which(colnames(r), "high")
      close_nm <- stringr::str_which(colnames(r), "close")
      if (verbose) print(c(SAR = sar_nm, Low = low_nm, Hi = high_nm, Cl = close_nm))
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
    if (verbose) any(is.na(out)) %>% print
    if (grep("sar_i\\.?\\d?", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("sar_i\\.?\\d?", names(l), ignore.case = T)]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,sar_i = out) else out <- tibbletime::tbl_time(cbind.data.frame(l,sar_i = out), index = "Time")
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": sar_i"))
    
  } else out <- l
  return(out)
})
## @knitr Add MACD Compressed
dat %<>% purrr::map(wind = wind, function(.x, wind){
  att <- attr(.x, "Sym")
  if (length(wind[sum(!is.na(.x[,"close"])) > wind]) < 1) {
    warning(paste0("At least one window value must be fewer than the number of observations in the data for ", att,", returning as is"))
    return(.x)
  }
  if (nrow(.x) < max(wind)) {message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))}
    if (!all(!is.na(quantmod::HLC(.x)))) {
    warning("NAs present in high low close, returning as is")
    return(.x)
    }
  wind <- wind[sum(!is.na(.x[,"close"])) > wind]
  message(wind)
macd_dif <- purrr::map(wind, env = parent.frame(), l = .x, function(.x, l, env){
  macd <- TTR::MACD(quantmod::Cl(l), nFast = round(.x / 2, 0), nSlow = .x, nSig = round(.x / 2 * .75, 0), maType = TTR::EMA)
  macd[,"macd"] %<>%  na.locf(fromLast = T)
  macd[,"signal"] %<>% na.locf(fromLast = T)
  macd_dif <- macd[,"macd"] - macd[,"signal"]
})
  macd_dif <- do.call("cbind",macd_dif)
  message(ncol(macd_dif))
  colnames(macd_dif) <- paste0("macd.",wind,".",round(wind / 2, 0),".",round(wind / 2 * .75, 0))
  #Remove old columns
  if (stringr::str_which(names(.x), "^macd|^macd\\.?\\d{1,3}?\\.?\\d{1,3}?\\.?\\d{1,3}?") %>% length > 0) .x <- .x[, -grep("^macd|^macd\\.?\\d{1,3}?\\.?\\d{1,3}\\.?\\d{1,3}?", names(.x), ignore.case = T)]
  
    if (xts::is.xts(.x)) macd_dif <- xts::as.xts(macd_dif) else macd_dif <- as.data.frame(macd_dif)
     
    if (xts::is.xts(.x)) out <- xts::cbind.xts(.x, macd_dif) else out <- tibbletime::tbl_time(cbind.data.frame(.x, macd_dif), index = "Time")
    attr(out, "Sym") <- att
    message(paste0(att,": MACD"))
  return(out)
})
## @knitr Add Decimal Date
dat %<>% purrr::map(function(.x){
  if (xts::is.xts(.x)) .x$Dec_date <- time(.x) %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))} else { 
    .x$Dec_date <- tibbletime::get_index_col(.x) %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))}
  }
    return(.x)
})

# Name check, needs a paste function with all of the naming conventions to really be accurate
nms <- c("high", "low", "volume", "open", "close", "changePercent", paste0(rep(c("SMA"),each = length(wind)),".", c(wind,200)), paste0(rep(c("ADX"),each = length(wind)),".", wind, "_i"), paste0(rep(c("ADXc"),each = length(wind)),".", wind), paste0(rep(c("WpR"),each = length(wind)),".", wind), paste0(rep(c("rsi"),each = length(wind)),".", wind), paste0(rep(c("rsi"),each = length(wind)),".", wind, "_i"), paste0(rep(c("roc", "mom"),each = length(wind)),".", wind), paste0(rep(c("atr", "trueLow"),each = length(wind)),".", wind), "sar", "sar_i", paste0("macd.",wind,".",round(wind / 2, 0),".",round(wind / 2 * .75, 0)), "Dec_date")
purrr::map(dat, nms = nms, function(.x, nms){
  att <- attr(l, "Sym")
  num <- which(!c(nms, paste0(nms,"_ind")) %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", c(nms, paste0(nms,"_ind"))[num]))}
})
## @knitr Add Ephemeris Data
# Re-add Attributes
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})

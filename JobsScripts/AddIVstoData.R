# TODO( - Created to add all signals to loaded data called dat)
message("Script: Add All Independent Variables to data")
HDA::startPkgs(c("magrittr", "xts", "tidyverse"))
# Load New Data if calling from a jobRunScript - allows for these files to be sourced by the automation and use the data from the automation calling environment
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Loaded from BG Job, loading dependencies...")
  load("dat.Rdata")}
## @knitr Add attributes for debugging purposes  
message("Add attributes for debugging purposes ")
 message("Add attributes for debugging purposes")
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
 attr(.x, "Sym") <- .y
  return(.x)
 })
source("~/R/Quant/JobsScripts/parameters.R")
# Add wind to this environment
wind <- params$wind
sma.wind <- c(wind, sma = 200)
if (!any(names(dat[[1]]) %in% "changePercent")) { 
dat <- purrr::map(dat, function(.x){
  x <- with(
    .x,{
  x <- quantmod::Delt(close, close, k = 1)
  x[is.na(x)] <- 0
  x
  })
  
  .x$changePercent <- as.vector(x)
  return(.x)
  })
}
## @knitr Add Ephemeris Data 
message("Add Ephemeris Data")
ephData <- readr::read_csv("~/R/Quant/ephData.csv")
ephData %<>% select(time, dplyr::ends_with("SI"))#, - dplyr::starts_with("NEP"), - dplyr::starts_with("PLU"),  - dplyr::starts_with("URA"))
dat <- purrr::map(dat, edat = ephData, function(.x, edat){
  s <- attr(.x, "Sym")
  td_nm <- stringr::str_extract(names(.x), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
   if (any(stringr::str_detect(names(.x), "SI$"))) .x <- .x[, - stringr::str_which(names(.x), "SI$")]
  out <- cbind.data.frame(.x, purrr::map_dfr(.x[[td_nm]], edat = edat, function(.x, edat){
    edat[which(edat$time < .x) %>% max, - c(1)] %>% purrr::map_dfc(function(.x){
      factor(.x,levels = c("CP","AQ","PI","AR", "TA", "GE", "CN", "LE", "VI","LI","SC","SG"))
    })
  }))
  attr(out, "Sym") <- s
  return(out)
})
## @knitr Add Simple Moving Averages 
message("Add Simple Moving Averages")
dat %<>% lapply(wind = sma.wind, function(l, wind){
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
    #Remove previous SMA columns
    if (stringr::str_which(names(l), "SMA\\.\\d{1,2}") %>% length > 0) l <- l[,-stringr::str_which(names(l), "SMA\\.\\d{1,2}")]
    out_sma <- do.call("cbind", out_sma)
    colnames(out_sma) <- paste("SMA", wind[sum(!is.na(l[,"close"])) > wind], sep = ".")
 message(paste0(att,": SMA"))
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out_sma) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, out_sma), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att
    } else out <- l
  return(out)
})
## @knitr Add ADX 
message("Add ADX")
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind <- wind[sum(!is.na(l[,"close"])) > wind * 2]
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    if (verbose) print(att)
    adx <- purrr::map(.x = wind, l = l, env = parent.env(), function(.x, l, env){
      if (verbose) print(.x)
      out <- TTR::ADX(l[,c("high","low","close")], n = .x)
      return(out)
    })
    adx <- do.call("cbind", adx) %>% na.locf(fromLast = T)
    colnames(adx) <- paste(colnames(adx), rep(wind, each = 4), sep = ".")
    if (stringr::str_which(names(l),"ADX|DI\\w?|DX|DX\\w.*\\d{1,2}?\\.?\\d{1,2}?$") %>% length > 0) l <- l[,-c(stringr::str_which(names(l),"ADX|DI\\w?|DX|DX\\w.*\\d{1,2}?\\.?\\d{1,2}?$"))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, adx) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, adx), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ADX"))
  }
  else out <- l
  return(out)
})

## @knitr Add ADX signal 
message("Add ADX signal")
dat %<>% lapply(threshold1 = 20, threshold2 = 25, wind = wind, verbose = F, function(l, threshold1, threshold2, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind <- wind[sum(!is.na(l[,"close"])) > wind * 2]
  if (magrittr::not(all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) & any(stringr::str_detect(names(l),"^DX")))) return(l)
    out <- list()
    for (i in seq_along(wind)) {
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
  
  if (length(wind) > 1) nms <- paste0("ADX.", wind, "_i") else nms <- "ADX_i"
  if (xts::is.xts(l)) {
  out <- do.call("cbind", out) %>% xts(order.by = time(l))
  if (verbose) print(list(xtsAttributes(l), Data = out))
  colnames(out) <- nms
  out <- cbind.xts(l, out)} else {
    out <- do.call("cbind", out)
    colnames(out) <- nms
    td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
    out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = !!rlang::sym(td_nm))
  }
  attr(out, "Sym") <- att
  message(paste0(attr(l, "Sym"),": ADX_i"))
 
  if (verbose) print(nrow(out))
  return(out)

})
## @knitr Add Compressed ADX 
message("Add Compressed ADX")
dat %<>% lapply(wind = wind, compress = T, function(l, wind, compress){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind <- wind[sum(!is.na(l[,"close"])) > wind * 2]
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) & any(stringr::str_detect(names(l),"ADX")) ) {
    adx <- purrr::map(.x = wind, env = parent.frame(), function(.x, env){
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, adx) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, adx), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att
    message(paste0(attr(l, "Sym"),": ADXc"))
  }
  else out <- l
  return(out)
})

## @knitr Add Williams Percent R 
message("Add Williams Percent R")
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, WpR) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, WpR), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": WpR"))
    
  return(out)
})
## @knitr Add RSI 
message("Add RSI")
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym")
  if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind <- wind[sum(!is.na(l[,"close"])) > wind]
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    rsic <- purrr::map(wind, l = l, function(.x, l){
      out <- TTR::RSI(l[,c("close")], n = .x)
      out <- na.locf(out, fromLast = T)
      return(out)
    })
    if (verbose == T) any(is.na(rsic)) %>% print
    rsic <- do.call("cbind", rsic)
    colnames(rsic) <- paste("rsi", wind, sep = ".")
    if (grep("^rsi\\.\\d{1,2}$",names(l)) %>% length > 0) l <- l[, -grep("^rsi\\.\\d{1,2}$",names(l))]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,rsic) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l,rsic), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": RSI"))
  } else out <- l
  return(out)
})

## @knitr Add RSI Indicator 
message("Add RSI Indicator")
dat %<>% lapply(wind = wind, threshold1 = c(high = 70, low = 30), threshold2 = c(high = 80, low = 20), verbose = F, sma.wind = sma.wind, function(l, wind, threshold1, threshold2, sma.wind, verbose){
  att <- attr(l, "Sym") 
 if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind <- wind[sum(!is.na(l[,"close"])) > wind]
  if (magrittr::not(all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind)) ) return(l)
    out <- list()
    
    for (i in seq_along(wind)) { # Up to the second to last window
      if(any(stringr::str_detect(names(l), paste0("SMA.",sma.wind[length(sma.wind)])))) {
        cls <- c(paste0(c("rsi."), wind[i]), paste0("SMA.",sma.wind[length(sma.wind)]),"close")
      } else {
        cls <- c(paste0(c("rsi."), wind[i]),"close")
      }
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": RSI_i"))
    if (verbose == T) print(nrow(out))
  return(out)
})
## @knitr Add ROC And Momentum 
message("Add ROC And Momentum")
dat %<>% lapply(wind = wind, verbose = F, function(l, wind, verbose){
  att <- attr(l, "Sym") 
 if (nrow(l) < max(wind)) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > wind) ) {
    # ROC
    roc <- purrr::map(.x = wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(),function(.x, env){
      roc <- TTR::ROC(l[,c("close")],n = .x)
      for (i in which(is.na(roc))) {
        roc[i + 1] <- TTR::ROC(l[,c("close"), drop = T],i)[i + 1]
      }
      roc[1] <- 0
      return(roc)
    })
    # momentum
    mom <- purrr::map(wind[sum(!is.na(l[,"close"])) > wind], env = parent.frame(), function(.x, env){
      mom <- TTR::momentum(l[,c("close")], n = .x)
      for (i in which(is.na(mom))) {
        mom[i + 1] <- TTR::momentum(l[,c("close"), drop = T],i)[i + 1]
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, roc, mom) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, roc, mom), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ROC & MOM"))
  } else out <- l

  if (verbose == T) print(nrow(out))
  return(out)
})
## @knitr Add ATR 
message("Add ATR")
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l, out) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l, out), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": ATR"))
  
  return(out)
})
## @knitr Add SAR 
message("Add SAR")
dat %<>% lapply(verbose = F, function(l, verbose){
  att <- attr(l, "Sym")
  if (all(!is.na(quantmod::HLC(l))) ) {
    if (verbose) print(xtsAttributes(l))
    sar <- TTR::SAR(quantmod::HLC(l), accel = c(0.05, 0.2))
    if (verbose) any(is.na(sar)) %>% print
    if (grep("sar", names(l), ignore.case = T) %>% length > 0) l <- l[, -grep("sar", names(l), ignore.case = T)]
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,sar) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l,sar), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": SAR"))
  } else out <- l
  return(out)
})
## @knitr Add SAR Indicator 
message("Add SAR Indicator")
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
    if (xts::is.xts(l)) out <- xts::cbind.xts(l,sar_i = out) else {
      td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(l,sar_i = out), index = !!rlang::sym(td_nm))
      }
    attr(out, "Sym") <- att 
 message(paste0(attr(l, "Sym"),": sar_i"))
    
  } else out <- l
  return(out)
})
## @knitr Add MACD Compressed 
message("Add MACD Compressed")
dat %<>% purrr::map(wind = wind, function(.x, wind){
  att <- attr(.x, "Sym")
  if (length(wind[sum(!is.na(.x[,"close"])) > wind * 2]) < 1) {
    warning(paste0("At least one window value must be fewer than the number of observations in the data for ", att,", returning as is"))
    return(.x)
  }
  if (nrow(.x) < max(wind)) {message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))}
    if (!all(!is.na(quantmod::HLC(.x)))) {
    warning("NAs present in high low close, returning as is")
    return(.x)
    }
  wind <- wind[sum(!is.na(.x[,"close"])) > wind * 2]
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
     
    if (xts::is.xts(.x)) out <- xts::cbind.xts(.x, macd_dif) else {
      td_nm <- stringr::str_extract(names(.x), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
      out <- tibbletime::tbl_time(cbind.data.frame(.x, macd_dif), index = !!rlang::sym(td_nm))}
    attr(out, "Sym") <- att
    message(paste0(att,": MACD"))
  return(out)
})
## @knitr Add Decimal Date 
message("Add Decimal Date")
dat %<>% purrr::map(function(.x){
  if (xts::is.xts(.x)) .x$Dec_date <- time(.x) %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))} else { 
    .x$Dec_date <- tibbletime::get_index_col(.x) %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_match(., "^\\d+"))}
  }
    return(.x)
})
## @knitr Add Ultimate Oscillator 
message("Add Ultimate Oscillator")
dat %<>% purrr::map(wind = wind, function(.x, wind){
  att <- attr(.x, "Sym")
  if (nrow(.x) < {max(wind) * 4}) message(paste0("Number of observations in ", att," is fewer than maximum window. Outcome will only be calculated for those windows that do not exceed the max number of observations."))
  wind.df <- purrr::map_dfc(wind, function(.x){
    out <- c(.x, .x * 2, .x * 4)
  })
  uo <- try({
    uo <- purrr::map(.x = wind.df, l = .x, function(.x, l){
      if (all(!is.na(l[,c("high","low","close")])) & any(sum(!is.na(l[,"close"])) > max(.x)) ) {
        out <- TTR::ultimateOscillator(l[,c("high","low","close")], n = .x)
      } else out <- NULL
      return(out)
    })
  }) %>% purrr::keep(~!is.null(.))
  if (length(uo) < 1) return(.x)
  uo <- do.call("cbind", uo) %>% na.locf(fromLast = T)
  colnames(uo) <- paste("UO", wind[purrr::map_lgl(wind.df, l = .x, function(.x, l){
    all(nrow(l) > .x)
  })], sep = ".")
  if (stringr::str_which(names(.x),"UO\\.\\d{1,3}") %>% length > 0) .x <- .x[,-c(stringr::str_which(names(.x),"UO\\.\\d{1,3}"))]
  if (xts::is.xts(.x)) out <- xts::cbind.xts(.x, uo) else {
    td_nm <- stringr::str_extract(names(.x), stringr::regex("^time$|^date$", ignore_case = T)) %>% subset(subset = !is.na(.)) %>% .[1]
    out <- tibbletime::tbl_time(cbind.data.frame(.x, uo), index = !!rlang::sym(td_nm))
  }
  attr(out, "Sym") <- att 
  message(paste0(att,": UO"))
  return(out)
})

#TODO If duplicates from na.locf are greater than 5% of the total data then take the columns with < 5% duplicated and remove duplicated rows.

# Name check, needs a paste function with all of the naming conventions to really be accurate
nms <- c("high", "low", "volume", "open", "close", "changePercent", paste0(rep(c("SMA"),each = length(sma.wind)),".", sma.wind), paste0(rep(c("ADX"),each = length(wind)),".", wind, "_i"), paste0(rep(c("ADXc"),each = length(wind)),".", wind), paste0(rep(c("WpR"),each = length(wind)),".", wind), paste0(rep(c("rsi"),each = length(wind)),".", wind), paste0(rep(c("rsi"),each = length(wind)),".", wind, "_i"), paste0(rep(c("roc", "mom"),each = length(wind)),".", wind), paste0(rep(c("atr", "trueLow"),each = length(wind)),".", wind), "sar", "sar_i", paste0("macd.",wind,".",round(wind / 2, 0),".",round(wind / 2 * .75, 0)), "Dec_date")
purrr::map(dat, nms = nms, function(.x, nms){
  att <- attr(.x, "Sym")
  num <- which(!c(nms) %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", paste0(c(nms)[num], collapse = ", ")))}
  })
## @knitr Add Ephemeris Data 
message("Add Ephemeris Data")
# Re-add Attributes
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})

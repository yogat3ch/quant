load(file = "Positions_ts2014-05-08_2019-05-07.Rdata")
HDA::startPkgs(c("magrittr","xts"))
source("Jobs & Scripts/QuantFunctions_TSL.R")
test <- Positions_ts %>% lapply(TSLvars = list(tsla = function(x){
  cl_cl <- stringr::str_which(colnames(x),"close")
  sd(x[,cl_cl]) * .5
}, tslp = .15, tslp = .2, retro = c(n = 7, hilop = .9), retro = c(n = 14, hilop = .9), retro = c(n = 28, hilop = .9)), verbose = T, function(l, TSLvars, verbose){
  if (stringr::str_detect(names(TSLvars),"retro") %>% any) max_n <- lapply(TSLvars[stringr::str_which(names(TSLvars),"retro")],purrr::pluck,"n") %>% unlist %>% max else max_n <- nrow(l) + 1 # Check what the max value for n is if retro is used, if not used set max_n to pass the next if statement
  if (nrow(l) > max_n & all(!is.na(quantmod::HLC(l))) ){
    rvs <- purrr::map2(.x = TSLvars, .y = names(TSLvars), env = parent.frame(), function(.x, .y, env){
      if(verbose) print(.x)
      args <- list(quantmod::OHLC(l), .x)
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
    out <- cbind.xts(rvs, l[, - c(l %>% names %>% grep("rv",.))])
  } else out <- l
  return(out)
})
try({save(test, file = "test.Rdata")})

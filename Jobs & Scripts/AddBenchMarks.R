HDA::startPkgs(c("magrittr", "doParallel"))
load(file = "dat.Rdata")
cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)
dat <- foreach(x = dat, y = best_tsl, .inorder = F, .multicombine = T, .packages = c("magrittr")) %dopar% {
  if (magrittr::not(all(!is.na(x[,c("high","low","close")])) & any(stringr::str_detect(names(x),"^rsi|^sma")))) return(y)
  y$Goldcross <- NULL
  y$RSI <- NULL
  y$Buy.Hold <- NULL
  att <- attr(x, "Sym")
  message(paste0(att,": Benchmarks"))
  # ----------------------- Tue Jun 04 13:54:09 2019 ------------------------#
  #  Add Gold Cross
  smas <- colnames(x)[stringr::str_which(colnames(x), "^SMA")]
  # Loop over each combination of SMA columns
  smas <- combn(smas, 2)
  colnames(smas) <- apply(smas, 2, FUN = paste0, collapse = "_")
  sma.ret <- apply(smas, 2, l = x, function(clms, l){
    # Create buy sell vector
    bs.v <- l[,clms] %>% apply(1,function(r){
      if(r[[1]] > r[[2]]) return(1) else if (r[[2]] > r[[1]]) return(-1) else return(0)
    })
    # Get the returns as percents
    pct.returns <- rle(bs.v) %>% unclass() %>% as.data.frame() %>% dplyr::mutate(end = cumsum(lengths), start = c(1, dplyr::lag(end)[-1] + 1)) %>% dplyr::select(c(1,2,4,3))
    pct.returns <- apply(pct.returns, 1, l2 = quantmod::HLC(l), function(r2, l2){
        cl_cl <- stringr::str_which(colnames(l2), stringr::regex("^close", ignore_case = T))
        if (r2[["values"]] == 0) out <- 0
        if (r2[["values"]] == -1) out <- 0
        e <- as.numeric(l2[r2[["end"]] + 1, cl_cl, drop = T])
        s <- as.numeric(l2[r2[["start"]], cl_cl, drop = T])
        if (r2[["values"]] == 1) out <- {(e - s) / s}
        return(out)
      }) %>% unlist %>% subset(subset = . != 0)
    # Roll them up into a single value
    total.returns <- 1
    for (i in seq_along(pct.returns)) {
      total.returns <- total.returns * pct.returns[i] + total.returns
    }
    return(total.returns)
  }) %>% unlist
  y$Goldcross <-  sma.ret[[which.max(sma.ret)]]
  # ----------------------- Tue Jun 04 13:54:23 2019 ------------------------#
  # Add RSI returns
  rsis <- colnames(x)[stringr::str_which(colnames(x), "^rsi.*i$")]
  names(rsis) <- rsis
  rsi.ret <- purrr::map(rsis, l = x, function(.x, l){
    # Get the returns as percents
    pct.returns <- rle(l[[.x]]) %>% unclass() %>% as.data.frame() %>% dplyr::mutate(end = cumsum(lengths), start = c(1, dplyr::lag(end)[-1] + 1)) %>% dplyr::select(c(1,2,4,3))
    pct.returns <- apply(pct.returns, 1, l2 = quantmod::HLC(l), function(r2, l2){
      cl_cl <- stringr::str_which(colnames(l2), stringr::regex("^close", ignore_case = T))
      if (r2[["values"]] == 0) out <- 0
      if (r2[["values"]] == -1) out <- 0
      e <- as.numeric(l2[r2[["end"]] + 1, cl_cl, drop = T])
      s <- as.numeric(l2[r2[["start"]], cl_cl, drop = T])
      if (r2[["values"]] == 1) out <- {(e - s) / s}
      return(out)
    }) %>% unlist %>% subset(subset = . != 0)
    # Roll them up into a single value
    total.returns <- 1
    for (i in seq_along(pct.returns)) {
      total.returns <- total.returns * pct.returns[i] + total.returns
    }
    return(total.returns)
  }) %>% unlist
  y$RSI <- rsi.ret[[which.max(rsi.ret)]]
  y$Buy.Hold <- (xts::last(x[,"close", drop = T]) - xts::first(x[,"close", drop = T])) / xts::first(x[,"close", drop = T])
  attr(y, "Sym") <- att
  y
}
names(dat) <- purrr::map(.x = dat, attr, "Sym")
parallel::stopCluster(cl)
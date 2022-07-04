#TODO Fix RSI
HDA::startPkgs(c("magrittr", "doParallel"))
load(file = "dat.Rdata")
cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl, outfile = "~/R/Quant/dopar.log")
#  purrr::map2(.x = dat, .y = best_tsl, function(.x, .y)
ret <- foreach::foreach(x = dat, y = best_tsl, .inorder = F, .multicombine = T, .packages = c("magrittr"), .verbose = T) %dopar% {
  # x <- .x
  # y <- .y
  if (magrittr::not(all(!is.na(x[,c("high","low","close")])) & any(stringr::str_detect(names(x),"^rsi|^sma")))) return(y)
  y$Goldcross <- NULL
  y$RSI <- NULL
  y$Buy.Hold <- NULL
  if (is.data.frame(y)) y <- list(TSL = y)
     # Remove previous iterations
  att <- attr(x, "Sym")
  message(paste0(att,": Benchmarks"))
  # ----------------------- Tue Jun 04 13:54:09 2019 ------------------------#
  #  Add Gold Cross
  smas <- colnames(x)[stringr::str_which(colnames(x), "^SMA")]
  # Loop over each combination of SMA columns
  smas <- utils::combn(smas, 2)
  colnames(smas) <- apply(smas, 2, FUN = paste0, collapse = "_")
  sma.ret <- apply(smas, 2, l = x, function(clms, l){
    # Create buy sell vector
    bs.v <- l[,clms] %>% apply(1,function(r){
      if(r[[1]] > r[[2]]) return(1) else if (r[[2]] > r[[1]]) return(-1) else return(0)
    })
    # Get the returns as percents
    bs.rle <- rle(bs.v) %>% unclass() %>% as.data.frame() %>% dplyr::mutate(end = cumsum(lengths), start = c(1, dplyr::lag(end)[-1] + 1)) %>% dplyr::select(c(1,2,4,3))
    pct.returns <- apply(bs.rle, 1, l2 = quantmod::HLC(l), function(r2, l2){
        cl_cl <- stringr::str_which(colnames(l2), stringr::regex("^close", ignore_case = T))
        if (r2[["values"]] == 0) out <- 0
        if (r2[["values"]] == -1) out <- 0
        if (r2[["values"]] == 1) {
          e <- as.numeric(l2[r2[["end"]] + 1, cl_cl, drop = T])
          s <- as.numeric(l2[r2[["start"]], cl_cl, drop = T])
          out <- (e - s) / s
        }
        return(out)
      }) %>% unlist %>% subset(subset = . != 0)
    # Roll them up into returns and cumulative returns
    cum.returns <- 0
    shares <- 1
    if (!is.na(which(bs.rle[["values"]] == 1)[1])) {
      init <- total.returns <- inv <- x[bs.rle[which(bs.rle[["values"]] == 1)[1], "start", drop = T], "close", drop = T]
      for (i in 1:nrow(bs.rle)) {
        if (bs.rle[i, "start", drop = T] != 1) next
        if (inv %/% x[bs.rle[i, "start", drop = T], "close", drop = T] > 0) {
          shares <- inv %/% x[bs.rle[i, "start", drop = T], "close", drop = T]
          inv <- x[bs.rle[i, "end", drop = T] + 1, "close", drop = T] * shares - x[bs.rle[i, "start", drop = T], "close", drop = T] * shares + inv
        } 
        if (total.returns %/% as.numeric(x[bs.rle[i, "start", drop = T], "close", drop = T]) > 0) {
          total.returns <- x[bs.rle[i, "end", drop = T] + 1, "close", drop = T] - x[bs.rle[i, "start", drop = T], "close", drop = T] + total.returns
        }
      }
      cum.returns <- (inv - init) / init
      total.returns <- (total.returns - init) / init
    } else total.returns <- cum.returns <- 0
    return(c(Returns = total.returns, Cum.Returns = cum.returns))
  })
  if (is.list(sma.ret)) {
    sma.ret <- sma.ret[sma.ret %>% purrr::map(function(.x){sum(.x, na.rm = T)}) != 0] %>% do.call("rbind", .)
    d <- sma.ret %>% as.data.frame
  } else d <- sma.ret %>% t %>% as.data.frame
    
  d <-  sma.ret %>% t %>% as.data.frame
  d %<>% tibble::rownames_to_column()
  if (identical(d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,2],d %>% dplyr::arrange(dplyr::desc(Cum.Returns)) %>% .[1,2])){
    out <- d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,]
  } else {
    out <-  rbind.data.frame(d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,],d %>% dplyr::arrange(dplyr::desc(Cum.Returns)) %>% .[1,])
  }
  print(out)
  y$Goldcross <-  out
  # ----------------------- Tue Jun 04 13:54:23 2019 ------------------------#
  # Add RSI returns
  rsis <- colnames(x)[stringr::str_which(colnames(x), "^rsi.*i$")]
  names(rsis) <- rsis
  rsi.ret <- purrr::map(rsis, l = x, function(.x, l){
    # Get the returns as percents
    bs.rle <- rle(l[[.x]]) %>% unclass() %>% as.data.frame() %>% dplyr::mutate(end = cumsum(lengths), start = c(1, dplyr::lag(end)[-1] + 1)) %>% dplyr::select(c(1,2,4,3))
    pct.returns <- apply(bs.rle, 1, l2 = quantmod::HLC(l), function(r2, l2){
      cl_cl <- stringr::str_which(colnames(l2), stringr::regex("^close", ignore_case = T))
      if (r2[["values"]] == 0) out <- 0
      if (r2[["values"]] == -1) out <- 0
      
      if (r2[["values"]] == 1) {
        e <- as.numeric(l2[r2[["end"]] + 1, cl_cl, drop = T])
        s <- as.numeric(l2[r2[["start"]], cl_cl, drop = T])
        out <- (e - s) / s
        }
      return(out)
    }) %>% unlist %>% subset(subset = . != 0)
    # Roll them up into a single value
    
    cum.returns <- 0
    shares <- 1
    if (!is.na(which(bs.rle[["values"]] == 1)[1])) {
    init <- total.returns <- inv <- x[bs.rle[which(bs.rle[["values"]] == 1)[1], "start", drop = T], "close", drop = T]
    for (i in 1:nrow(bs.rle)) {
      if (bs.rle[i, "start", drop = T] != 1) next
      if (inv %/% x[bs.rle[i, "start", drop = T], "close", drop = T] > 0) {
        shares <- inv %/% x[bs.rle[i, "start", drop = T], "close", drop = T]
        inv <- x[bs.rle[i, "end", drop = T] + 1, "close", drop = T] * shares - x[bs.rle[i, "start", drop = T], "close", drop = T] * shares + inv
      } 
      if (total.returns %/% as.numeric(x[bs.rle[i, "start", drop = T], "close", drop = T]) > 0) {
        total.returns <- x[bs.rle[i, "end", drop = T] + 1, "close", drop = T] - x[bs.rle[i, "start", drop = T], "close", drop = T] + total.returns
      }
    }
    cum.returns <- (inv - init) / init
    total.returns <- (total.returns - init) / init
    } else total.returns <- cum.returns <- 0
    return(c(Returns = total.returns, Cum.Returns = cum.returns))
  }) 
  if (is.list(rsi.ret)) {
    rsi.ret <- rsi.ret[rsi.ret %>% purrr::map(function(.x){sum(.x, na.rm = T)}) != 0] %>% do.call("rbind", .)
    d <- rsi.ret %>% as.data.frame
    } else d <- rsi.ret %>% t %>% as.data.frame
  d %<>% tibble::rownames_to_column()
  if (identical(d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,2],d %>% dplyr::arrange(dplyr::desc(Cum.Returns)) %>% .[1,2])){
    out <- d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,]
  } else {
    out <-  rbind.data.frame(d %>% dplyr::arrange(dplyr::desc(Returns)) %>% .[1,],d %>% dplyr::arrange(dplyr::desc(Cum.Returns)) %>% .[1,])
  }
  print(out)
  y$RSI <- out
  y$Buy.Hold <- (xts::last(x[,"close", drop = T]) - xts::first(x[,"close", drop = T])) / xts::first(x[,"close", drop = T])
  attr(y, "Sym") <- att
  y
}
names(ret) <- purrr::map(.x = ret, attr, "Sym")
parallel::stopCluster(cl)

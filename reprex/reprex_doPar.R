x <- iris[which(iris[,5] != "setosa"), c(1,5)]
 trials <- 10000
 (ptime <- system.time({
   r <- foreach::foreach(iterators::icount(trials), .combine=cbind) %dopar% {
     ind <- sample(100, 100, replace=TRUE)
     result1 <- stats::glm(x[ind,2]~x[ind,1], family=stats::binomial(logit))
     stats::coefficients(result1)
     }
   }))[3]

library(doParallel)

l <- Positions_ts$TLRY
doParallel::registerDoParallel(6, cores = 6)
(pout_time <- system.time({
  rvs <- foreach::foreach(x = TSLvars, y = names(TSLvars),  l = purrr::map(1:18, l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = T, .packages = c("quantmod","xts","rlang","magrittr")) %dopar%  {
    source("JobsScripts/QuantFunctions_TSL.R")
    args <- list(quantmod::HLC(l), x)
    names(args) <- c("v", y)
    out <- eval(rlang::call2("TSL", !!!args))
  }
}))[3]
foreach::registerDoSEQ()
(out_time<- system.time({out <- foreach::foreach(x = TSLvars, y = names(TSLvars),  l = purrr::map(1:18, l = l, function(.x,l)return(l)), .inorder = T, .combine = cbind, .multicombine = T, .packages = c("quantmod","xts","rlang","magrittr")) %do%  {
  paste0(xts::xtsAttributes(l), ": ", y) %>%  message() 
  args <- list(quantmod::HLC(l), x)
  names(args) <- c("v", y)
  out <- eval(rlang::call2("TSL", !!!args))
}
}))[3]

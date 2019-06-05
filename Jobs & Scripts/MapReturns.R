HDA::startPkgs(c("doParallel","magrittr"))
load(file = "dat.Rdata")
cl <- makePSOCKcluster(6)
doParallel::registerDoParallel(cl, cores = 6)
dat <- foreach(l = dat, pct = purrr::map(1:length(dat), pct = pct, function(.x, pct){return(pct)}), .multicombine = T, .packages = c("magrittr"), .verbose = T) %dopar% {
  source("Jobs & Scripts/QuantFunctions_optimReturn.R")
  att <- attr(l, "Sym") %>% unlist
  returns.clms <- colnames(l)[stringr::str_which(colnames(l), "(.*rv$)")]
  names(returns.clms) <- returns.clms
  out <- purrr::map(returns.clms, env = parent.frame(), function(.x, env){
    out <- purrr::map(pct, y = .x, env = parent.frame(), function(.x, y, env){
       test <- optimReturn(l, percent = .x, returns.clm = y)
      return(test)
    })
    names(out) <- names(pct)
    
    return(out)
  })
  attr(out, "Sym") <- att
  out
}
names(dat) <- purrr::map(dat, function(l){
  attr(l,"Sym")
}) %>% unlist
parallel::stopCluster(cl)
save(dat,file = "dat.Rdata")

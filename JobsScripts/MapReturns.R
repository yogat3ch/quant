HDA::startPkgs(c("doParallel","magrittr","foreach"))
load(file = "~/R/Quant/dat.Rdata")
cl <- makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log")
doParallel::registerDoParallel(cl, cores = 6)
# # For Debugging
#list(l = Positions_ts_rv$AMD, .x = .1, y = "tslret_px0.5_day7_rv") %>% list2env(envir = .GlobalEnv)
# env <- new.env()
# list(opts = .opts, att = "AMD") %>% list2env(envir = env)
#ret <- purrr::pmap(list(l = dat, pct = purrr::map(1:length(dat), pct = pct, function(.x, pct){return(pct)}), opts = purrr::map(1:length(dat), .opts = .opts, function(.x, .opts){return(.opts)})), function(l, pct, opts)
ret <- foreach(l = dat, pct = purrr::map(1:length(dat), pct = pct, function(.x, pct){return(pct)}), opts = purrr::map(1:length(dat), .opts = .opts, function(.x, .opts){return(.opts)}), .multicombine = T, .packages = c("magrittr"), .verbose = T, .errorhandling = 'stop') %dopar% {
  source("JobsScripts/QuantFunctions_optimReturn.R")
  att <- attr(l, "Sym")
  message(paste0(lubridate::now(), " Begin:", att))
  st  <- system.time({
  returns.clms <- colnames(l)[stringr::str_which(colnames(l), "rv$")]
  names(returns.clms) <- returns.clms
  message(paste0("Returns Clms Created - mapping returns..."))
  out <- purrr::map(returns.clms, env = sys.frame(sys.nframe()), function(.x, env){
    out <- purrr::map(pct, y = .x, env = env, function(.x, y, env){
      message(paste0("Begin -", env$att,": Pct:",.x," Clm: ",y))
      out <- list()
       test <- optimReturn(l, percent = .x, returns.clm = y, .opts = env$opts)
       if (HDA::go("test")){
       a <- test[[".opts"]] %>% purrr::compact() %>% dplyr::filter_all(.vars_predicate = dplyr::any_vars({. != 0})) %>%  purrr::imap(function(.x, .y) {
         if (stringr::str_detect(.y, "bs")) out <- sum(.x[.x > 0]) else if (stringr::str_detect(.y, "gain")) out <- quantile(.x[.x != 0])
         return(out)
       })
       out$trades <- data.frame(rem = rep(0, a %>% purrr::keep(~ length(.) == 1) %>% length))
       if (env$opts$bs.v) out$trades <- cbind(out$trades,bs.v = a %>% purrr::keep(~ length(.) == 1) %>% do.call("rbind",.))
       if (env$opts$with.gains) out$trades <- cbind(out$trades, a %>% magrittr::extract(stringr::str_detect(names(a), "with")) %>% do.call("rbind",.) %>% as.data.frame%>% setNames(paste0(colnames(.),"w.g")))
       if (env$opts$max.gain) out$trades <- cbind(out$trades, a %>% magrittr::extract(stringr::str_detect(names(a), "max")) %>% do.call("rbind",.) %>% as.data.frame %>% setNames(paste0(colnames(.),"max")))
     out$trades <- out$trades[, colnames(out$trades) != "rem"]
     out$returns <- test$returns
     } else out <- NULL
       message(paste0("End -", env$att,": Pct:",.x," Clm: ",y))
       return(out)
    })
    
    names(out) <- names(pct)
    
    return(out)
  })
  attr(out, "Sym") <- att
  message(paste0(lubridate::now()," End:", att))
  })
  message(paste0(lubridate::now()," End:", att," Elapsed:", lubridate::as.duration(st[[3]])))
  out
}
names(ret) <- purrr::map(dat, function(.x){
  attr(.x,"Sym")
}) %>% unlist
parallel::stopCluster(cl)


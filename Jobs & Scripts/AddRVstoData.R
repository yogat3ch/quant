#TODO(Add nested foreach loop for dat, and TSLvars)
## @knitr Add trailing stop loss response variables
HDA::startPkgs(c("magrittr", "xts", "tidyverse", "doParallel"))
# Load New Data
load("dat.Rdata")
if (!exists("add", mode = "logical")) add <- T
cl <- makePSOCKcluster(6) 
doParallel::registerDoParallel(cl, cores = 6)
dat %<>% lapply(TSLvars = TSLvars, add = add, verbose = F, function(l, TSLvars, add, verbose){
  if (all(!is.na(quantmod::HLC(l))) ) {
    message(paste0("Begin: ", attr(l, "Sym")))
    nms <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
      if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
      return(nm)
    }) %>% unlist
    if (add) TSLvars <- TSLvars[!nms %in% names(l)[stringr::str_which(names(l),"rv$")] ]
    run.time <- system.time({
      rvs <- foreach(x = TSLvars, y = names(TSLvars),  l = purrr::map(1:18, l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = T, .packages = c("quantmod","xts","rlang","magrittr")) %dopar%  {
      source("Jobs & Scripts/QuantFunctions_TSL.R")
      args <- list(quantmod::HLC(l), x)
      names(args) <- c("v", y)
      out <- eval(rlang::call2("TSL", !!!args))
    }
    })
    message(paste0("End: ", attr(l, "Sym"), " Elapsed: ",lubridate::as.duration(run.time[3])," Time: ",lubridate::now()))
    if (verbose) any(is.na(rvs)) %>% print
    nms_ind <- nms %>% paste0(., "_ind")
    clm.nms <- c(rbind(nms, nms_ind))
    if (grep("rv",names(l)) %>% length > 0 & !add) l  <- l[, - grep("rv",names(l))] # Remove columns if add = F
    if (xts::is.xts(l)) { 
      rvs <- xts(rvs, order.by = time(l))
      colnames(rvs) <-  clm.nms
      out <- cbind.xts(rvs, l)
    } else {
      Time <-  tibbletime::get_index_col(l)
      l <- l[!names(l) %in% tibbletime::get_index_char(l)]
        rvs <- cbind(Time = Time, rvs, l)
        colnames(rvs) <- c("Time",clm.nms, names(l))
        out <- tibbletime::tbl_time(rvs, index = "Time")
        }
    
    
    
    
  } else out <- l
  return(out)
})
# Re add attributes
parallel::stopCluster(cl)
# Check Names
nms <- purrr::pmap(.l = list(.x = TSLvars, .y = names(TSLvars), .z = seq_along(TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist
purrr::map(dat, nms = nms, function(.x, nms){
  att <- attr(l, "Sym")
  num <- which(!c(nms, paste0(nms,"_ind")) %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", c(nms, paste0(nms,"_ind"))[num]))}
})
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})
save(dat,file = "dat.Rdata")
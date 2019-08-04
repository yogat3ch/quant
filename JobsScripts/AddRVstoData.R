#TODO(Add nested foreach loop for dat, and TSLvars)
## @knitr Add trailing stop loss response variables
HDA::startPkgs(c("magrittr", "xts", "doParallel"))
calling.script <- basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+") %>% .[!is.na(.)])
message(paste0("Begin AddRVstoData sourced from ",calling.script, " at ", lubridate::now()," in location: ",getwd()))
message("Script: Add All Response Variables to data")

# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/dat.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
if (!exists("add", mode = "logical")) add <- F
nms <- paste0(names(params$TSLvars),"_rv")
nms_ind <- nms %>% paste0(., "_ind")
clm.nms <- c(rbind(nms, nms_ind))
cl <- parallel:::makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log")
doParallel::registerDoParallel(cl, cores = 6)
dat %>% lapply(TSLvars = params$TSLvars, add = add, clm.nms = clm.nms, verbose = F, function(l, TSLvars, add, clm.nms, verbose){
  if (all(!is.na(quantmod::HLC(l))) ) {
    s <- attr(l, "Sym")
    message(paste0("Begin: ", attr(l, "Sym")))
    nms <- paste0(names(TSLvars),"_rv")
    nms_ind <- nms %>% paste0(., "_ind")
    clm.nms <- c(rbind(nms, nms_ind))
    if (any(names(l) %in% clm.nms)) {
      l  <- l[, !names(l) %in% clm.nms]
    }# Remove columns if add = F
     tsl_amt <- attr(TSLvars,"tsl_amt")
    if (add) TSLvars <- TSLvars[!nms %in% names(l)[stringr::str_which(names(l),"rv$")] ]
    run.time <- system.time({
      # rvs <- foreach(x = TSLvars, tsl_amt = purrr::map(1:length(TSLvars), tsl_amt = tsl_amt, function(.x,tsl_amt)return(tsl_amt)), l = purrr::map(1:length(TSLvars), l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = T, .packages = c("quantmod","xts","rlang","magrittr", "stringr", "lubridate")) %dopar%
      # rvs <- purrr::pmap(list(x = TSLvars), tsl_amt = tsl_amt, l = l,function(x,tsl_amt,l)
      # rvs <- foreach(x = TSLvars, tsl_amt = purrr::map(1:length(TSLvars), tsl_amt = tsl_amt, function(.x,tsl_amt)return(tsl_amt)), l = purrr::map(1:length(TSLvars), l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = T, .packages = c("quantmod","xts","rlang","magrittr", "stringr", "lubridate")) %dopar% 
      rvs <- foreach(x = TSLvars, y = nms, tsl_amt = purrr::map(1:length(TSLvars), tsl_amt = tsl_amt, function(.x,tsl_amt)return(tsl_amt)), l = purrr::map(1:length(TSLvars), l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = F, .packages = c("rlang","magrittr", "stringr", "dplyr", "lubridate")) %dopar% {
        s <- attr(l, "Sym")
        message(paste0(lubridate::now()," ",s," - Begin: ",y))
          x$tsl_amt <- tsl_amt
          source("~/R/Quant/JobsScripts/QuantFunctions_TSL.R")
          .args <- list(v = l[, stringr::str_which(names(l), stringr::regex("date|time|open|high|low|close", ignore_case = T))], .args = x)
          out <- eval(rlang::call2("TSL", !!!.args))
        message(paste0(lubridate::now(), " "," - End: ",y))
      }#)
    })
    message(paste0("End: ", attr(l, "Sym"), " Elapsed: ",lubridate::as.duration(run.time[3])," Time: ",lubridate::now()))
    if (verbose) any(is.na(rvs)) %>% print
    if (xts::is.xts(l)) { 
      rvs <- xts::xts(rvs, order.by = time(l))
      colnames(rvs) <-  clm.nms
      out <- xts::cbind.xts(rvs, l)
      dates <- time(out) %>% range()
    } else {
        td_nm <- stringr::str_extract(names(l), "^time$|^date$") %>% subset(subset = !is.na(.)) %>% .[1]
        message(paste0("ncolrv: ",ncol(rvs), "col.nms: ",length(clm.nms)))
        colnames(rvs) <- clm.nms
        rvs <- cbind.data.frame(l, rvs)
        out <- tibbletime::tbl_time(rvs, index = !!td_nm) %>% dplyr::select(!!td_nm, dplyr::everything())
        dates <- tibbletime::get_index_col(out) %>% range()
        }
    
    
    
    
  } else out <- l
  attr(out, "Sym") <- s
    .a_f <- list.files("~/R/Quant/PositionData", pattern = s, full.names = T)
    .rvs <- .a_f[.a_f %>% stringr::str_which("rv\\.csv$")]
    if (HDA::go(.rvs)){
      .rv_data <- purrr::map(.rvs, ~ readr::read_csv(.x))
      purrr::map(.rvs, ~ file.remove(.x))
      .rv_data <- append(.rv_data, out)
      out <- data.table::rbindlist(.rv_data) %>% subset(subset = !duplicated(.[["time"]]))
    } else out <- NULL
    if (HDA::go(out)) { 
      #.a_f[.a_f %>% stringr::str_which("rv\\.csv$")] %>% purrr::walk( ~ file.remove(.x))
      write_csv(out, path = paste0("~/R/Quant/PositionData/",s , min(out[[td_nm]]) %>% lubridate::as_date(), "_", max(out[[td_nm]]) %>% lubridate::as_date(), "_rv.csv"))
    }
  
  return(out)
})
# Re add attributes
parallel::stopCluster(cl)
purrr::map(dat, nms = clm.nms, function(.x, nms){
  att <- attr(.x, "Sym")
  num <- which(!nms %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", nms[num]))}
})
dat <- purrr::map2(.x = dat, .y = names(dat), function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat,file = "dat.Rdata")
HDA::unloadPkgs(c("doParallel","iterators","parallel","foreach"))
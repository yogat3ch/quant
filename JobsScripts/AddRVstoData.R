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
      rvs <- foreach(x = TSLvars, y = nms, tsl_amt = purrr::map(1:length(TSLvars), tsl_amt = tsl_amt, function(.x,tsl_amt)return(tsl_amt)), l = purrr::map(1:length(TSLvars), l = l, function(.x,l)return(l)), .inorder = T, .multicombine = T, .combine = cbind, .verbose = T, .errorhandling = "pass", .packages = c("rlang","magrittr", "stringr", "dplyr", "lubridate")) %dopar% {
        s <- attr(l, "Sym")
        message(paste0(lubridate::now()," ",s," - Begin: ",y))
          x$tsl_amt <- tsl_amt
          source("~/R/Quant/JobsScripts/QuantFunctions_TSL.R")
          .args <- list(v = l[, stringr::str_which(names(l), stringr::regex("date|time|open|high|low|close", ignore_case = T))], .args = x)
          out <- eval(rlang::call2("TSL", !!!.args))
        message(paste0(lubridate::now(), " "," - End: ",y))
        out
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
        td_nm <- stringr::str_extract(names(l), stringr::regex("^time$|^date$", ignore_case = T)) %>% purrr::keep(~ !is.na(.x)) %>% .[1]
        message(paste0("ncolrv: ",ncol(rvs), "col.nms: ",length(clm.nms)))
        colnames(rvs) <- clm.nms
        rvs <- cbind.data.frame(l, rvs)
        out <- tibbletime::tbl_time(rvs, index = !!td_nm) %>% dplyr::select(!!td_nm, dplyr::everything())
        }
    
    
    
    
  } else out <- l
  attr(out, "Sym") <- s
  message(paste0("Finding files: ", s))
  
    .a_f <- list.files("~/R/Quant/PositionData", pattern = s, full.names = T)
    if (s == "GOOG") .rvs <- .a_f[.a_f %>% stringr::str_which("GOOG(?!L).*rv\\.csv$")] else .rvs <- .a_f[.a_f %>% stringr::str_which("rv\\.csv$")]
    if (HDA::go(.rvs)){
      message(paste0("Reading data: ", s))
      .rv_data <- readr::read_csv(.rvs)
      purrr::map(.rvs, ~ file.remove(.x))
      message(paste0("If duplicates: ", s))
      if (length(setdiff(names(.rv_data), names(out))) == 0) .rv_data <- rbind.data.frame(out, .rv_data)
      message(paste0("Removing duplicates: ", s))
      td_nm <- rlang::sym(td_nm)
      out <- .rv_data %>% dplyr::mutate(date = lubridate::as_date(!!td_nm)) %>% subset(subset = !duplicated(.[["date"]])) %>% dplyr::select(- date) %>% dplyr::arrange(!!td_nm)
    }
    
      message(paste0("Saving ", s))
      readr::write_csv(out, path = paste0("~/R/Quant/PositionData/",s , min(out[[td_nm]]) %>% lubridate::as_date(), "_", max(out[[td_nm]]) %>% lubridate::as_date(), "_rv.csv"))
    
  
  return(out)
})
# Re add attributes
parallel::stopCluster(cl)
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat,file = "dat.Rdata")
HDA::unloadPkgs(c("doParallel","iterators","parallel","foreach"))
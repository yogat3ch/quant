HDA::startPkgs(c("magrittr", "xts", "tidyverse", "doParallel","foreach"))
message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
message("Script: Add All Response Variables to new data")
## @knitr Add trailing stop loss response variables

# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/dat.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
try(load(file = params$paths$Positions_tsl))
if (!exists("add", mode = "logical")) add <- F
nms <- names(dat)
# # For debugging
# list(l = dat[[1]], this_tsl = best_tsl[names(dat)][[1]], tsl_amt = purrr::map(1:length(dat), tsl_amt = attr(params$TSLvars, "tsl_amt"), function(.x, tsl_amt) {return(tsl_amt)}) %>% .[[1]]) %>% list2env(envir = .GlobalEnv)
cl <- parallel:::makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log") 
doParallel::registerDoParallel(cl, cores = 6)
run.time <- system.time({
  dat <- foreach::foreach(x = dat, this_tsl = best_tsl[names(dat)], tsl_amt = purrr::map(1:length(dat), tsl_amt = attr(params$TSLvars, "tsl_amt"), function(.x, tsl_amt) {return(tsl_amt)}), .verbose = T,.inorder = T, .multicombine = T, .packages = c("quantmod","xts","rlang","magrittr","foreach")) %dopar% {
    sym <- attr(x, "Sym")
    if (all(!is.na(quantmod::HLC(x))) ) {
#TODO Rearrange based on new best_tsl input 2019-07-20 1059
      this_tsl$tslv <- this_tsl[["tslv"]][!duplicated(names(this_tsl$tslv))]
      rvs <- purrr::imap(this_tsl$tslv, .pf = sys.frame(sys.nframe()), function(.x, .y, .pf){
        message(paste0(lubridate::now()," ",.pf$sym," - Begin: ",.y))
        .x$tsl_amt <- .pf$tsl_amt
        source("~/R/Quant/JobsScripts/QuantFunctions_TSL.R")
        .args <- list(v = .pf$x[, stringr::str_which(names(.pf$x), stringr::regex("date|time|open|high|low|close", ignore_case = T))], .args = .x)
        out <- eval(rlang::call2("TSL", !!!.args))
        message(paste0(lubridate::now(), " ",.pf$sym," - End: ",.y))
        return(out)
        })
      rvs %<>% do.call("cbind", .)
      nms <- names(this_tsl$tslv) %>% stringr::str_replace("$", "_rv")
      nms_ind <- nms %>% paste0(., "_ind")
      clm.nms <- c(rbind(nms, nms_ind))
      if (grep("rv",names(x)) %>% length > 0 & !add) x  <- x[, - grep("rv",names(x))] # Remove columns if add = F
      if (xts::is.xts(x)) {
        rvs <- xts::xts(rvs, order.by = stats::time(x))
        colnames(rvs) <-  clm.nms
        out <- xts::cbind.xts(rvs, x)
      } else {
        Time <-  tibbletime::get_index_col(x)
        x <- x[!names(x) %in% tibbletime::get_index_char(x)]
        rvs <- cbind(Time = Time, rvs, x)
        colnames(rvs) <- c("time",clm.nms, names(x))
        out <- tibbletime::tbl_time(rvs, index = "time")
      }
      attr(out, "Sym") <- sym
      out
    } else out <- x
    out
  }
})
message(paste0(" Elapsed: ",lubridate::as.duration(run.time[3])," Time: ",lubridate::now()))
parallel::stopCluster(cl)
# End Add RVs
#----------------------- Sun Aug 18 15:59:31 2019 ------------------------#
# Re-add attributes
names(dat) <- nms
dat <- purrr::imap(.x = dat, function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})
# Check Names
purrr::walk(dat, .b_tsl = best_tsl, function(.x, .b_tsl){
  att <- attr(.x, "Sym")
  nms <- .b_tsl[[att]][["tsl_types"]][["tsl"]]
  rv_nms <- c(paste0(nms,"_rv"), paste0(nms,"_rv_ind"))
  num <- which(!rv_nms %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", rv_nms[num]))}
})
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat,file = "dat.Rdata")
HDA::unloadPkgs(c("doParallel","iterators","parallel","foreach"))

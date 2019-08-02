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

best_tsl <- purrr::map2(Positions_tsl[names(dat)], .y = dat, tslv = params$TSLvars, function(.x, .y, tslv){
  .x %<>% dplyr::mutate_at(dplyr::vars(tsl),~ stringr::str_replace(.,"\\_rv$",""))
  # Get the TSL with the most cumulative returns and with the most possible limit gain for a given period
  out <- list(tsl_types = rbind.data.frame(dplyr::arrange(.x, dplyr::desc(Cum.Returns)) %>% .[1, c("tsl", "pct", "75%max", "100%max", "Cum.Returns")],
  dplyr::arrange(.x, dplyr::desc(`100%max`)) %>% .[1, c("tsl", "pct", "75%max", "100%max", "Cum.Returns")]))
  # Are there tsl not already represented in the data?
  existing_tsl <- {out$tsl_types$tsl[out$tsl_types$tsl %in% {names(.y)[stringr::str_which(names(.y),"rv$")] %>% stringr::str_replace(.,"\\_rv$","")}] %>% length} > 0
  # If there are and add is true (defaults to true) then add only those that don't already exist
  if (existing_tsl){
  # filter the table for those tsl not already in the data
  out$tsl_types %<>% dplyr::filter(!tsl == existing_tsl) }
  # Return the TSLvars parameters for each of those TSL types
  out$tslv <- tslv[out$tsl_types[["tsl"]]]
  return(out)
  })
# # For debugging
# list(l = dat[[1]], this_tsl = best_tsl[names(dat)][[1]], tsl_amt = purrr::map(1:length(dat), tsl_amt = attr(params$TSLvars, "tsl_amt"), function(.x, tsl_amt) {return(tsl_amt)}) %>% .[[1]]) %>% list2env(envir = .GlobalEnv)
cl <- parallel:::makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log") 
doParallel::registerDoParallel(cl, cores = 6)
run.time <- system.time({
  dat <- foreach(x = dat, this_tsl = best_tsl[names(dat)], tsl_amt = purrr::map(1:length(dat), tsl_amt = attr(params$TSLvars, "tsl_amt"), function(.x, tsl_amt) {return(tsl_amt)}), .verbose = T,.inorder = T, .multicombine = T, .packages = c("quantmod","xts","rlang","magrittr","foreach")) %dopar% {
    sym <- attr(x, "Sym")
    if (all(!is.na(quantmod::HLC(x))) ) {
#TODO Rearrange based on new best_tsl input 2019-07-20 1059
      rvs <- purrr::imap(this_tsl$tslv, pf = sys.frame(sys.nframe()), function(.x, .y, pf){
        message(paste0(lubridate::now()," ",pf$sym," - Begin: ",.y))
        .x$tsl_amt <- pf$tsl_amt
        source("~/R/Quant/JobsScripts/QuantFunctions_TSL.R")
        .args <- list(v = pf$x[, stringr::str_which(names(pf$x), stringr::regex("date|time|open|high|low|close", ignore_case = T))], .args = .x)
        out <- eval(rlang::call2("TSL", !!!.args))
        message(paste0(lubridate::now(), " ",pf$sym," - End: ",.y))
        return(out)
        })
      rvs %<>% do.call("cbind", .)
      nms <- names(this_tsl$tslv) %>% stringr::str_replace("$", "_rv")
      nms_ind <- nms %>% paste0(., "_ind")
      clm.nms <- c(rbind(nms, nms_ind))
      if (grep("rv",names(x)) %>% length > 0 & !add) x  <- x[, - grep("rv",names(x))] # Remove columns if add = F
      if (xts::is.xts(x)) {
        rvs <- xts(rvs, order.by = time(x))
        colnames(rvs) <-  clm.nms
        out <- cbind.xts(rvs, x)
      } else {
        Time <-  tibbletime::get_index_col(x)
        x <- x[!names(x) %in% tibbletime::get_index_char(x)]
        rvs <- cbind(Time = Time, rvs, x)
        colnames(rvs) <- c("Time",clm.nms, names(x))
        out <- tibbletime::tbl_time(rvs, index = "Time")
      }
      attr(out, "Sym") <- sym
      out
    } else out <- x
    out
  }
})

# Re add attributes
message(paste0(" Elapsed: ",lubridate::as.duration(run.time[3])," Time: ",lubridate::now()))
parallel::stopCluster(cl)
# Check Names
tsl_nms <- purrr::pmap(.l = list(.x = params$TSLvars, .y = names(params$TSLvars), .z = seq_along(params$TSLvars)), .f = function(.x, .y, .z){
  if (is.function(.x)) nm <- paste0(.y, .z, "_rv") else nm <- paste0(.y, paste0(.x, collapse = "-"),"_rv")
  return(nm)
}) %>% unlist
dat <- purrr::map2(.x = dat, .y = nms, function(.x, .y){
  attr(.x, "Sym") <- .y
  return(.x)
})
names(dat) <- nms
purrr::map(dat, nms = tsl_nms, Positions_tsl = Positions_tsl, function(.x, nms, Positions_tsl){
  att <- attr(.x, "Sym")
  nms <- Positions_tsl[[att]][["TSL"]][["rowname"]]
  num <- which(!c(nms, paste0(nms,"_ind")) %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", c(nms, paste0(nms,"_ind"))[num]))}
})
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat,file = "dat.Rdata")
HDA::unloadPkgs(c("doParallel","iterators","parallel","foreach"))
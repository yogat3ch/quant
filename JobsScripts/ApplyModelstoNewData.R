message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+")) %>% .[!is.na(.)], " at ", lubridate::now()," From location: ",getwd()))
HDA::startPkgs(c("magrittr"))
source("~/R/Quant/JobsScripts/QuantFunctions_optimReturn.R")
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  load("dat.Rdata")
  message("Loaded as background job, loading dependencies...")
  }
if (!exists("OOTbegin", mode = "numeric")) OOTbegin <- NULL
# For debugging:
list(.sym = names(dat), .b_tsl = best_tsl, .dat = dat) %>% purrr::map(2) %>% append(list(OOTbegin = OOTbegin)) %>% list2env(envir = .GlobalEnv)
.pf <- parent.frame()
#rm(list = c(".fn", ".b_tsl",".dat", "ob_chr"))
dat <- purrr::pmap(list(.dat = dat, .sym = names(dat), .b_tsl = best_tsl), OOTbegin = OOTbegin, function(.sym, .b_tsl, .dat, OOTbegin){
  # get the path to the corresponding object
  .fn <- list.files(path = "~/R/Quant/MdlBkp", pattern = paste0(.sym,"_cl.Rdata"), full.names = T)
  # Get the name of the object
  ob_chr <- paste0(.sym, "_cl")
  message(paste0("Loading: ", .sym))
  # Load the data
  try({load(.fn)})
  
  .b_tsl$tsl_types %<>% dplyr::mutate_at(dplyr::vars(tsl),~ paste0(., "_rv"))
  ob <- get0(ob_chr)
  rm(list = ob_chr)
  # For Debugging:
  #list(.tsl = .b_tsl$tsl_types$tsl, .pct = .b_tsl$tsl_types$pct) %>% purrr::map(1) %>% list2env(envir = .GlobalEnv)
  .preds <- purrr::pmap(list(.tsl = .b_tsl$tsl_types$tsl, .pct = .b_tsl$tsl_types$pct), .pf = parent.frame(), function(.tsl, .pct, .pf) {
    # Preprocess data
    .frm <- formula(paste(paste0("`",.tsl,"`"), "~ ."))
    .td_nm <- params$getTimeIndex(.pf$.dat)
    # ----------------------- Fri Jul 26 14:19:16 2019 ------------------------#
    # PreProcessing Data
    # 1. Make model frame
    .out <- model.frame(.frm, data = .pf$.dat)
    # 2. remove any columns that may be matrices
    .out <- dplyr::mutate_if(.out, .predicate = is.matrix, ~ as.vector(.))
    # 3. Create dummyVars from factors
    library(caret)
    .out <- caret::dummyVars(.frm, data = .out) %>% predict(., newdata = .out)
    HDA::unloadPkgs("caret")
    .frm <- formula(paste(paste0("`",.tsl,"`"), "~ ", paste(.pf$ob[[.tsl]][[1]][["coefnames"]], collapse = " + ")))
    .out <- cbind.data.frame(.pf$.dat[, .tsl], .out)
    .out <- model.frame(.frm, data = as.data.frame(.out))
    if (xts::is.xts(.pf$.dat)) {
      # Add Prediction column
      .out <- data.frame(v = rowMeans(predict(.pf$ob[[.tsl]], newdata = .out)))
      colnames(pred)[1] <- paste0(.tsl, "_pred")
    } else {
      # Add Prediction column
      .out <- data.frame(v = rowMeans(predict(.pf$ob[[.tsl]], newdata = .out)))
      colnames(pred)[1] <- paste0(.tsl, "_pred")
      
    }
    return(.out)
})
  #Name the output list
  names(.preds) <- .b_tsl$tsl_types$tsl
  .out <- cbind.data.frame(do.call("cbind.data.frame", preds), .dat) %>% dplyr::select(!!td_nm, dplyr::everything())
  attr(.out, "Sym") <- .sym
  rm(ob)
  return(.out)
})
names(dat) <- purrr::map(dat,attr,"Sym") %>% unlist
if (any(stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv"))) save(dat, file = "dat.Rdata")

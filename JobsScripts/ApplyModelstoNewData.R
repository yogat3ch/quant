message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+")) %>% .[!is.na(.)], " at ", lubridate::now()," From location: ",getwd()))
HDA::startPkgs(c("magrittr"))
source("~/R/Quant/JobsScripts/QuantFunctions_optimReturn.R")
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  load("dat.Rdata")
  message("Loaded as background job, loading dependencies...")
  }
if (!exists("OOTbegin", mode = "numeric")) OOTbegin <- NULL
# For debugging:
list(.fn = fns, .s = names(dat), .b_tsl = best_tsl, .dat = dat) %>% purrr::map(2) %>% append(list(OOTbegin = OOTbegin)) %>% list2env(envir = .GlobalEnv)
pf <- parent.frame()
#rm(list = c(".fn", ".b_tsl",".dat", "ob_chr"))
dat <- purrr::pmap(list = list(.fn = fns, .s = names(dat), .b_tsl = best_tsl, .dat = dat), OOTbegin = OOTbegin, function(.fn, .s, .b_tsl, .dat, OOTbegin){
  # Get the name of the object
  ob_chr <- stringr::str_match(.fn, "\\/MdlBkp\\/([\\w\\_]+)")[,2]
  message(paste0("Loading: ", .s))
  # Load the data
  try({load(.fn)})
  
  .b_tsl$tsl_types %<>% dplyr::mutate_at(dplyr::vars(tsl),~ paste0(., "_rv"))
  ob <- get0(ob_chr)
  rm(list = ob_chr)
  # For Debugging:
  #list(.tsl = .b_tsl$tsl_types$tsl, .pct = .b_tsl$tsl_types$pct) %>% purrr::map(1) %>% list2env(envir = .GlobalEnv)
  preds <- purrr::pmap(list(.tsl = .b_tsl$tsl_types$tsl, .pct = .b_tsl$tsl_types$pct), .pf = parent.frame(), function(.tsl, .pct, .pf) {
    # Preprocess data
    .frm <- formula(paste(paste0("`",.tsl,"`"), "~ ."))
    .td_nm <- .pf$params$getTimeIndex(.pf$.dat)
    # ----------------------- Fri Jul 26 14:19:16 2019 ------------------------#
    # PreProcessing Data
    # 1. Make model frame
    .out <- model.frame(.frm, .pf$.dat)
    # 2. remove any columns that may be matrices
    .out <- dplyr::mutate_if(.out, .predicate = is.matrix, ~ as.vector(.))
    # 3. Create dummyVars from factors
    library(caret)
    .out <- caret::dummyVars(.frm, data = .out) %>% predict(., newdata = .out)
    HDA::unloadPkgs("caret")
    .frm <- formula(paste(paste0("`",.tsl,"`"), "~ ", paste(.pf$ob[[.tsl]][[1]][["coefnames"]], collapse = " + ")))
    .out <- cbind.data.frame(.pf$.dat[, .tsl], .out)
    .out <- model.frame(.frm, as.data.frame(.out))
    if (xts::is.xts(.out)) {
      # Add Prediction column
      pred <- xts::cbind.xts(rowMeans(predict(.pf$ob[[.tsl]], newdata = .out)), .pf$.dat[, c(.td_nm,"open","high","low","close", paste0(.tsl, "_ind"))], .out)
      colnames(pred)[1] <- paste0(.tsl, "_pred")
    } else {
      # Add Prediction column
      pred <- cbind.data.frame(rowMeans(predict(.pf$ob[[.tsl]], newdata = .out)), .pf$.dat[, c(.td_nm,"open","high","low","close", paste0(.tsl, "_ind"))], .out)
      colnames(pred)[1] <- paste0(.tsl, "_pred")
      
    }
    # Create returns matrix
    ret <- .pf$optimReturn(pred, percent = .pct, returns.clm = paste0(.tsl, "_pred"), tslindex.clm = paste0(.tsl, "_ind"), .opts = list(bs.v = T))
    .out <- cbind.data.frame(ret$.opts[, which.max(ret$returns[,"Cum.Returns"])], pred)
    colnames(.out)[1] <- paste0(.tsl, "_action")
    attr(.out, "Returns") <- ret$returns
    return(.out)
})
  #Name the output list
  names(preds) <- .b_tsl$tsl_types$tsl
  out <- cbind.data.frame(do.call("cbind.data.frame", preds), .dat) %>% dplyr::select(!!td_nm, dplyr::everything())
  attr(out,"Cum.Returns") <- Cum.Returns
  attr(out,"Returns") <- Returns
  attr(out, "Sym") <- .s
  rm(ob_chr)
  return(out)
})
names(dat) <- purrr::map(dat,attr,"Sym") %>% unlist
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat, file = "dat.Rdata")

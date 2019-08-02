#TODO Rewrite to accommodate new AddRVStoNewData with changes to best_tsl 2019-07-22 1840
#DONE Rewrote, now need to debug.
message(paste0("Begin ",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\:\\.]+")) %>% .[!is.na(.)], " at ", lubridate::now()," From location: ",getwd()))
HDA::startPkgs(c("magrittr"))
source("~/R/Quant/JobsScripts/QuantFunctions_optimReturn.R")
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  load("dat.Rdata")
  message("Loaded as background job, loading dependencies...")
  }
if (!exists("OOTbegin", mode = "numeric")) OOTbegin <- NULL
fns <- list.files("~/R/Quant/MdlBkp", full.names = T, pattern = ".Rdata$")
names(fns) <- stringr::str_match(fns, "\\/MdlBkp\\/([A-Z]+)")[,2]
dat <- purrr::imap(fns, best_tsl = best_tsl, newdata = dat, OOTbegin = OOTbegin, function(.x, .y, best_tsl, newdata, OOTbegin){
  try({load(.x)})
  s <- .y
  message(s)
  ob_chr <- stringr::str_match(.x, "\\/MdlBkp\\/([\\w\\_]+)")[,2]
  ob <- get0(ob_chr)
  newdata <- purrr::pluck(newdata, s)
  if (tibble::is_tibble(newdata) & !is.null(OOTbegin)) newdata <- newdata[newdata %>% tibbletime::get_index_col() > OOTbegin,] else if (xts::is.xts(newdata) & !is.null(OOTbegin)) newdata <- newdata[time(newdata) > OOTbegin,]

  tsl <- purrr::pluck(best_tsl, s)
  bestTSL_chr <- tsl[["tsl_types"]][["tsl"]]
  tsl.param_chr <- tsl[["tsl_types"]][["pct"]] # Get the TSL threshold parameter
  preds <- list()
  Cum.Returns <- vector("numeric")
  Returns <- vector("numeric")
  for (i in seq_along(bestTSL_chr)) {
    if (xts::is.xts(newdata)) {
      pred <- xts::cbind.xts(rowMeans(predict(ob[[bestTSL_chr[i]]], newdata = newdata)), newdata)
      colnames(pred)[1] <- paste0(bestTSL_chr[i], "_pred")
    } else {
      pred.name <- paste0(bestTSL_chr[i],"_pred")
      pred <- rowMeans(predict(ob[[bestTSL_chr[i]]], newdata = newdata))
      td_nm <- stringr::str_extract(names(newdata), "^time$|^date$") %>% purrr::discard(~ is.na(.))
      pred <- cbind.data.frame(pred, newdata) %>% dplyr::rename_at(1, ~ pred.name) %>% dplyr::select(!!td_nm, dplyr::everything())
      ret <- optimReturn(pred, percent = tsl.param_chr[i], returns.clm = paste0(bestTSL_chr[i], "_pred"), paste0(bestTSL_chr[i], "_ind"))
      ret.name <- paste0(pred.name, "_action")
      Cum.Returns[i] <- attr(ret,"Cum.Returns")
      Returns[i] <- attr(ret, "Returns")
      preds[[i]] <- cbind.data.frame(ret, pred[,2, drop = F]) %>% dplyr::rename_at(1, ~ ret.name)
    }
  }
  out <- cbind.data.frame(do.call("cbind.data.frame", preds), newdata) %>% dplyr::select(!!td_nm, dplyr::everything())
  attr(out,"Cum.Returns") <- Cum.Returns
  attr(out,"Returns") <- Returns
  attr(out, "Sym") <- s
  rm(ob_chr)
  return(out)
})
names(dat) <- purrr::map(dat,attr,"Sym") %>% unlist
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat, file = "dat.Rdata")

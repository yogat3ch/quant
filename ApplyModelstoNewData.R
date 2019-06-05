load("dat.Rdata")
if (!exists("OOTbegin", mode = "numeric")) OOTbegin <- NULL
dat <- purrr::map(list.files("MdlBkp", full.names = T), best_tsl = best_tsl, newdata = dat, OOTbegin = OOTbegin, function(.x, best_tsl, newdata, OOTbegin){
  load(.x)
  s <- stringr::str_match(.x, "\\/([A-Z]+)")[,2]
  message(s)
  ob_chr <- stringr::str_match(.x, "\\/([\\w\\_]+)")[,2]
  ob <- get0(ob_chr)
  newdata <- purrr::pluck(newdata, s)
  if (tibble::is_tibble(newdata) & !is.null(OOTbegin)) newdata <- newdata[newdata %>% tibbletime::get_index_col() > OOTbegin,] else if (xts::is.xts(newdata) & !is.null(OOTbegin)) newdata <- newdata[time(newdata) > OOTbegin,]

  tsl <- purrr::pluck(best_tsl, s)
  bestTSL_chr <- tsl[["rowname"]]
  tsl.param_chr <- tsl[["pct"]] # Get the TSL threshold parameter
  # ----------------------- Wed Jun 05 07:48:10 2019 ------------------------#
  # Needs edit to accomodate multiple best tsl
  
  if (xts::is.xts(newdata)) {
    pred <- xts::cbind.xts(rowMeans(do.call("cbind",purrr::map(predict(ob, newdata = newdata),rowMeans))), newdata)
    colnames(pred)[1] <- paste0(bestTSL_chr, "_pred")
  } else {
        pred <- cbind.data.frame(rowMeans(do.call("cbind",purrr::map(predict(ob, newdata = newdata),rowMeans))), newdata) %>% dplyr::rename(1 = paste0(bestTSL_chr,"_pred")) %>% dplyr::select(Time, dplyr::everything())
        }
  
  out <- optimReturn(pred, percent = tsl.param_chr, returns.clm = paste0(bestTSL_chr, "_pred"), paste0(bestTSL_chr, "_ind"))
  rm(ob_chr)
  return(out)
})
save(dat, file = "dat.Rdata")
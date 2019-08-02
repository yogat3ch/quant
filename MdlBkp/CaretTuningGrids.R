mod.packages <- c("doParallel","parallel","foreach","iterators")
HDA::startPkgs(mod.packages)
library(magrittr)
train.time <- system.time({
fns <- list.files("~/R/Quant/MdlBkp", pattern = "Rdata$", full.names = T)
names(fns) <- list.files("~/R/Quant/MdlBkp", pattern = "Rdata$", full.names = T) %>% basename() %>%  stringr::str_replace("\\_cl\\.Rdata", "")
cl <- parallel:::makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log")
doParallel::registerDoParallel(cl)
overall.performance <- foreach::foreach(fn = fns,.inorder = T, .multicombine = T, .verbose = T, .packages = c("magrittr"), .final = function(fn) setNames(fn, names(fns)) ) %dopar% {
  message(fn)
  load(fn) 
  ob <- get0(basename(fn) %>% stringr::str_replace("\\.Rdata", ""))
  rm(list = basename(fn) %>% stringr::str_replace("\\.Rdata", ""))
  rv_nms <- stringr::str_extract(names(ob),".*\\_rv") %>% subset(subset=!is.na(.))
  res_list <- purrr::map_depth(ob[rv_nms], .depth = 2, function(.x){
    res <- purrr::pluck(.x, "results")
    res %<>% dplyr::arrange(RMSE) %>% head(3)
    return(res)
  })
  
  res_df <- purrr::map(res_list, ~ data.table::rbindlist(.x, idcol = T, fill = T))
  out <- data.table::rbindlist(res_df, idcol = T, fill = T)
  names(out)[1:2] <- c("tsl", "method")
  out %<>% dplyr::mutate_at(dplyr::vars(method), ~ stringr::str_extract(., "^\\w+"))
  mthds <- unique(out$method)
  names(mthds) <- unique(out$method)
  methods.tG <- purrr::map(mthds, out = out, function(.x, out){
    par <- as.character(caret::modelLookup(.x)$parameter)
    out %<>% dplyr::filter(method == .x) %>% dplyr::select(!!par) %>% purrr::map(~ na.omit(unique(.x))) %>% expand.grid
  })
  
  return(methods.tG)
}

parallel::stopCluster(cl)
})

message(paste0(names(train.time)[3],": ",lubridate::as.duration(train.time[3])," / Time: ",lubridate::now()))
HDA::unloadPkgs(mod.packages)
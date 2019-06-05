HDA::startPkgs("magrittr")
files_to_compress <- tools::checkRdaFiles(list.files(recursive = T, full.names = T) %>% .[stringr::str_which(., stringr::regex(ignore_case = T, "\\.Rdata"))]) %>% tibble::rownames_to_column() %>% dplyr::filter(compress != "xz") %>% .[["rowname"]]
purrr::map(files_to_compress, compress = "xz", function(.x, compress){
  st <- system.time({
    message(paste("Compressing file:",.x, "Begin:", lubridate::now()))
    tools::resaveRdaFiles(.x, compress = compress)
  })
  message(paste("Completed", .x, "End:", lubridate::now(), "Elapsed:", lubridate::as.duration(st[3])))
})

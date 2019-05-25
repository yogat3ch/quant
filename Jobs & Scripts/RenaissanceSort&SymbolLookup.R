Renpos %<>% mutate(Price = `Value(k)`*1000/Held)
Renpos$Change.Pct[{Renpos$Change.Pct == "New"}]  <- 0
Renpos$Change.Pct %<>% stringr::str_replace(",", "") %>% as.numeric
Renpos %<>% filter(!is.na(`Change.Pct`))
Renpos %<>% arrange(desc(`Change.Pct`))
url <- httr::parse_url("https://www.marketwatch.com/tools/quotes/lookup.asp?siteID=mktw&Lookup=CYCLERION+THERAPEUTICS+INC&Country=us&Type=All")
symlookup <- purrr::map(Renpos$Company, url = url, function(com, url){
  url$query$Lookup <-  stringr::str_replace_all(com,"\\s","+")
  url <- httr::build_url(url)
  htm <- xml2::read_html(url)
  out <- tryCatch({ htm %>% rvest::html_node("div.results > table") %>% rvest::html_table()}, error = function(e) return(e))
  return(out)
})
symlookup %<>% keep(.x = ., .p = ~ is.data.frame(.x))
Renstocks <- rbindlist(symlookup)
Renstocks <- Renstocks[!duplicated(Renstocks),]
RenNYSNAS <- Renstocks %>% filter(str_detect(Exchange,"NYS|NAS"))
googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
gs_Ren <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1tFvrMstMNunq0bHoteXe1Kbs4CtDGemVJgYpyVGZ3Cc/edit#gid=0")
googlesheets::gs_edit_cells(gs_Ren, input = RenNYSNAS)

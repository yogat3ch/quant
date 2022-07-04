tracking <- c(lubridate::now())
tracking <- append(tracking, lubridate::now())
save(tracking, file = "timetracking.Rdata")
load("timetracking.Rdata")
tracking <- append(tracking, lubridate::ymd_hms("2019-06-05 15:32:00", tz = Sys.timezone(location = TRUE)))
purrr::map(c(50,52), tracking = tracking,function(.x, tracking){
  tracking[.x] <<- tracking[.x] + lubridate::duration(4,"hours")
})
tracking[31] <- lubridate::ymd_hms("2019-06-03 09:32:00", tz = Sys.timezone(location = TRUE))
tracking %>% split(rep(1:{length(.)/2},each = 2, length.out = length(.))) %>% purrr::map(function(.x){
  
  {lubridate::today() - lubridate::as_date(.x[2])} %>% paste0("plus! -",.,"d ",{{diff(.x) %>% lubridate::as.period(unit = "hours")} / lubridate::period(1, "hours")} %>% round(2), "/", {{diff(.x) %>% lubridate::as.period(unit = "hours")} / lubridate::period(1, "hours")} %>% round(2)) 
}) %>% unlist %>% cat(sep = "\n")


`%>%` <- magrittr::`%>%`
# list with large DFs of named arguments (hyperparameters actually)
.in <- purrr::map_dfc(1:9, ~stats::rnorm(5)) %>% 
  stats::setNames(nm = paste0("col", 1:9)) %>% 
{list(
  .,
  .
)}
cl <- parallel::makeCluster(2)
future::plan(future::cluster, workers = cl)
furrr::future_map(.in, ~{
  .d <- .x
  purrr::map(1:nrow(.d), ~{
    
    # Want to grab all the hyperparameters as a list to pass to do.call
    .args <- .d[.x,]
    do.call(cbind, .args) # ML training fn called here instead
  })
})
parallel::stopCluster(cl)

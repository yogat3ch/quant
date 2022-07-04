write_to_file <- function(cond) {
  cond_class <- class(cond)[1]
  msg <- paste(cond_class, ":", cond$message)
  write(msg, file = "outlog", append=TRUE)
}

catch <- catchr::make_catch_fn(
  warning = c(write_to_file, muffle),
  message = c(write_to_file, muffle),
  error   = c(write_to_file, catchr::exit_with("Returned error!"))
)
cl <- parallel::makeCluster(2)
future::plan(future::cluster, workers = cl)

.d <- data.frame(time = stats::rnorm(5), t2 = stats::rnorm(5))
`!!` <- rlang::`!!`
`%>%` <- magrittr::`%>%`
# With normal furrr it's fine
furrr::future_imap(1:2, ~{
  .t <- "time"
  .ts <- rlang::sym(.t)
  .d %>% 
    dplyr::select(!!.ts)
})
# But with catchr the bang bang causes an error
furrr::future_imap(1:2, ~promises::catch({
  .t <- "time"
  .ts <- rlang::sym(.t)
  .d %>% 
    dplyr::select(!!.ts)
}))

parallel::stopCluster(cl)

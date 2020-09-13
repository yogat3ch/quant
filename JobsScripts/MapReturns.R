
`%>%` <- magrittr::`%>%`
`!!!` <- rlang::`!!!`
`!!` <- rlang::`!!`
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
message(paste0(calling.script, ": ", lubridate::now(),"\nlocation: ",getwd()))
# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/data/input_mapret.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
cl <- qf::start_cluster(outfile = TRUE)
future::plan(future::cluster, workers = cl$cl)
# # For Debugging
#list(l = Positions_ts_rv$AMD, .x = .1, y = "tslret_px0.5_day7_rv") %>% list2env(envir = .GlobalEnv)
# env <- new.env()
# list(opts = .opts, att = "AMD") %>% list2env(envir = env)
#ret <- purrr::pmap(list(l = dat, pct = purrr::map(1:length(dat), pct = pct, function(.x, pct){return(pct)}), opts = purrr::map(1:length(dat), .opts = .opts, function(.x, .opts){return(.opts)})), function(l, pct, opts)
ret <- furrr::future_imap(dat, ~cl$catch({
#ret <- purrr::imap(dat, ~{
  .d <- .x
  .sym <- .y
  message(paste0(lubridate::now(), ": Begin ", .sym))
  st  <- system.time({
    .tsls <- purrr::keep(stringr::str_extract(colnames(.d), ".*(?=\\_rv$)"), ~!is.na(.x)) %>% stats::setNames(nm = .)
    #out <- furrr::future_map(.tsls, ~cl$catch({
    out <- purrr::map(.tsls, ~{
      .tsl <- .x
      rv <- .d[[paste0(.x,"_rv")]]
      ind <- .d[[paste0(.x, "_ind")]]
      # get 10% of the time points with positive gains
      .rp <- range(rv[rv > 0] %>% .[1:(length(.) %/% 10)])
      if (length(.rp) == 0) return(NULL)
      .qt <- .rp %>%
        # create ten even gaps for a total of 11 thresholds 
        {seq(.[1], .[2], by = abs(diff(.)) / 10)} %>% 
        setNames(nm = paste0("p",.))
      qf::iMessage(paste0(lubridate::now(), ": Begin ", .sym,":",.tsl))
      out <- purrr::map(.qt, ~{
        .out <- qf::optimReturn(.d, tsl_name = .tsl, .p = .x)
        return(.out)
      })
      message(paste0(lubridate::now(), ": End ", .sym,":",.tsl))
      return(out)
    }
    #)
    )
    attr(out, "Sym") <- .sym
  })
  message(paste0(lubridate::now()," End:", .sym," Elapsed:", lubridate::as.duration(st[[3]])))
  out
}
)
)
save(ret, file = "~/R/Quant/data/output_mapret.Rdata")
parallel::stopCluster(cl$cl)


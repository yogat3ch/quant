qf::iMessage("Script: Add All Independent Variables to data")
HDA::startPkgs(c("magrittr"))
# Load New Data if calling from a jobRunScript - allows for these files to be sourced by the automation and use the data from the automation calling environment
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  qf::iMessage("Loaded from BG Job, loading dependencies...")
  load("~/R/Quant/data/input_AddIVstoData.Rdata")
  }
library(qf)
source(qf::wd("JobsScripts/parameters.R"))
# Add wind to this environment
.t <- qf::time_index(dat[[1]])
wind <- qf::time_windows(dat[[1]], params$wind$multipliers)
.nms <- names(dat[[1]]) %>% .[!. %in% .t]


cl <- qf::start_cluster(outfile = "out_log.log")
future::plan(future::cluster, workers = cl$cl)
# @knitr Add Moving Averages ----
# Sat Jul 04 08:50:05 2020
qf::iMessage("Add & Cross Moving Averages")
sma.wind <- c(wind, sma = 200)
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Moving Averages"))
  sma <- qf::add_indicator(.x, sma.wind, TTR::SMA)
  ema <- qf::add_indicator(.x, sma.wind, TTR::EMA, wilder = T)
  dplyr::bind_cols(.x, sma, ema)
}
)
)

## @knitr Cross Moving Averages ----
# Sat Jul 04 08:49:19 2020
# crosses to add
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Cross Averages"))
  .out <- qf::cross_ma(.x, sma.wind, params$IV$cross, replace = c("SMA_day", "EMA_day"))
}
)
)

qf::check_dat(dat)

# @knitr Add ADX  ----
# Sat Jul 04 08:50:19 2020
qf::iMessage("Add ADX & ADXi")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": ADX & ADXi"))
  adx <- qf::add_indicator(.x, wind, TTR::ADX, bind_orig = TRUE)
  adx <- qf::adxc(adx, wind, .args = params$IV$adx)
  dplyr::bind_cols(.x, adx)
}
)
)

qf::check_dat(dat)

## @knitr Add Williams Percent R 
qf::iMessage("Add Williams Percent R")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": WPR"))
  wpr <- qf::add_indicator(.x, wind, TTR::WPR, bind_orig = TRUE)
}
)
)

qf::check_dat(dat)

## @knitr Add RSI 
qf::iMessage("Add RSI & Indicator")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": RSI"))
  rsi <- qf::add_indicator(.x, wind, TTR::RSI, bind_orig = TRUE)
  rsi <- qf::rsic(rsi, sma.wind, .args = params$IV$rsi)
  dplyr::bind_cols(.x, rsi)
}
)
)

qf::check_dat(dat)

## @knitr Add ROC And Momentum 
qf::iMessage("Add ROC")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y,": ROC"))
  roc <- qf::add_indicator(.x, wind, .f = TTR::ROC, bind_orig = TRUE)
}
)
)

qf::check_dat(dat)

## @knitr Add SAR 
qf::iMessage("Add SAR and SAR Indicator")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y,": SAR"))
  sar <- qf::add_indicator(.x, wind, .f = TTR::SAR, accel = params$IV$sar$accel, bind_orig = TRUE)
  sar <- qf::sarc(sar, wind, .args = params$IV$sar)
  dplyr::bind_cols(.x, sar)
}
)
)

qf::check_dat(dat)

## @knitr Add MACD
qf::iMessage("Add MACD")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": MACD"))
  .d <- .x
  macd <- purrr::imap(wind, ~{
    .out <- as.data.frame(
      qf::add_indicator(
        .d,
        .x,
        TTR::MACD,
        bc = F,
        nFast = round(.x / 2, 0),
        nSlow = .x,
        nSig = round(.x / 2 * .75, 0),
        maType = TTR::EMA
      )
    ) %>% {magrittr::set_colnames(., paste0(names(.),"_",.y))}
    
    .wi <- which(names(sma.wind) %in% .y) + 2
    .wi <- ifelse(.wi > length(sma.wind), length(sma.wind), .wi)
    .sma_nm <- grep(paste0("SMA_", names(sma.wind[.wi]),"$"), names(.d), ignore.case = TRUE, value = TRUE)
    .macd <- dplyr::bind_cols(.out, tail(.d[,c(.sma_nm, qf::ohlcvt(.d)[4])], nrow(.out)))
    macd <- qf::macc(.macd, .x, .args = params$IV$macd)
    return(macd)
  })
  dplyr::bind_cols(.d[!grepl("^macd", names(.d), ignore.case = TRUE)], do.call(cbind, macd))
}
)
)

qf::check_dat(dat)

## @knitr Add Ultimate Oscillator
qf::iMessage("Add Ultimate Oscillator")
#dat_t <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Ultimate Oscillator"))
  uo <- qf::add_indicator(.x, list(day.mp = wind[1] * c(1, 2, 4), week.mc = wind[2] * c(1, 2, 4)), TTR::ultimateOscillator, bind_orig = F) %>% {stats::setNames(., stringr::str_replace(names(.), "ultimateOscillator", "UO"))}
  .uoc <- qf::uoc(uo, wind = params$wind, .args = params$IV$uo) 
  dplyr::bind_cols(.x[!names(.x) %in% c(names(uo), names(.uoc))], uo)
}
)
)

qf::check_dat(dat)

qf::iMessage("Add Rolling Linear Regression")
#dat_t <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": roLM"))
  .out <- qf::add_indicator(.x, .ns = wind, .f = qf::roLM, x = .x[[.t]])
  dplyr::bind_cols(.x, as.data.frame(.out))
}
)
)

qf::check_dat(dat)

## @knitr Add Date Based Variables 
qf::iMessage("Add Date Based Variables")
dat <- furrr::future_map(dat, qf::addTK)

qf::check_dat(dat)

parallel::stopCluster(cl$cl)
future::plan(future::sequential)
.ti <- qf::time_int(dat[[1]])
.ts <- rlang::sym(.t)
.time <- qf::time_index(dat[[1]], name = F)
db <- get_data("db", .ti$t)
purrr::iwalk(dat, ~{
  .in <- .x[!names(.x) %in% .nms]
  .tbl <- paste0(.y,"_iv")
  .trange <- as.numeric(range(.time))
  if (RSQLite::dbExistsTable(db, .tbl)) {
  .o <- RSQLite::dbGetQuery(db, glue::glue("SELECT * FROM {.tbl} WHERE {.t} BETWEEN {.trange[1]} AND {.trange[2]}"))
  # Check if table should be overwritten
  if (all(names(.in) %in% names(.o))) {
    .do_overwrite  <- any(purrr::map2_lgl(dplyr::mutate_all(.in, as.numeric), .o[names(.in)], ~{
      all(dplyr::near(.x, .y, 1.1))
    }))
  } else {
    .do_overwrite <- TRUE
  }
  if (.do_overwrite) {
    qf::iMessage(paste0("Updating ", .tbl))
    #Just overwrite the whole damn thing bc SQL blows
    RSQLite::dbWriteTable(db, .tbl, .in, overwrite = TRUE)
  } else {
    # otherwise append
    .lt <- RSQLite::dbGetQuery(db, glue::glue("SELECT MAX({.t_ind}) FROM {.tbl}"))[[1]]
    if (.ti$t == "hours") {
      .lt <- lubridate::as_datetime(.lt, tz = "America/New_York")
    } else if (.ti$t == "days") {
      .lt <- lubridate::as_date(.lt)
    }
    .ain <- dplyr::filter(.in, !!.ts > .lt)
    if (nrow(.in) > 0) {
      .append <- TRUE
      RSQLite::dbWriteTable(db, .tbl, .ain, append = .append)
    }
  }
  # purrr::imap(dplyr::mutate_all(.in[!grepl("RSI", names(.in))][.l], as.numeric)[.l], ~{
  #   .q <- RSQLite::dbExecute(db, glue::glue("UPDATE {.tbl} SET {.y} = :time"), bind.data = tibble::tibble(!!.y := .x))
  #   browser()
  #   RSQLite::db(db, .q, bind.data = tibble::tibble(!!.y :=.x))
  #   .col <- RSQLite::dbGetQuery(db, glue::glue("SELECT {.y} FROM {.tbl}"))
  #   identical(.col, .x)
  # })
  
    
  } else {
    RSQLite::dbWriteTable(db, .tbl, .in)
  }
})
RSQLite::dbDisconnect(db)
qf::iMessage("Script: Add All Independent Variables to data - ", Sys.Date())
HDA::startPkgs(c("magrittr"))
# Load New Data if calling from a jobRunScript - allows for these files to be sourced by the automation and use the data from the automation calling environment
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  qf::iMessage("Loaded from BG Job, loading dependencies...")
  load("~/R/Quant/data/input_AddIVstoData.Rdata")
}


cl <- qf::start_cluster(outfile = "out_log.log")
future::plan(future::cluster, workers = cl$cl)
# @knitr Add Moving Averages ----
# Sat Jul 04 08:50:05 2020
qf::iMessage("Add & Cross Moving Averages")
library(qf)
source("JobsScripts/parameters.R")
.nms <- names(dat[[1]]) %>% .[!. %in% qf::time_index(dat[[1]])]
# Add wind to this environment
sma.wind <- qf::time_windows(dat[[1]], c(params$wind$multipliers, sma = 200))
wind <- sma.wind[-length(sma.wind)]

#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Moving Averages"))
  sma <- qf::add_indicator(.x, sma.wind, TTR::SMA)
  ema <- qf::add_indicator(.x, sma.wind, TTR::EMA, wilder = T)
  col_bind(.x, sma, ema)
}
)
)

## @knitr Cross Moving Averages ----
# Sat Jul 04 08:49:19 2020
# crosses to add
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Cross Averages"))
  .out <- qf::cross_ma(.x, sma.wind, params$IV$cross)
}
)
)

dat <- check_dat(dat)

# @knitr Add ADX  ----
# Sat Jul 04 08:50:19 2020
qf::iMessage("Add ADX & ADXi")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": ADX & ADXi"))
  adx <- qf::add_indicator(.x, wind, TTR::ADX, bind_orig = TRUE)
  adx <- qf::adxc(adx, wind, .args = params$IV$adx)
  col_bind(.x, adx)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add Williams Percent R 
qf::iMessage("Add Williams Percent R")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": WPR"))
  wpr <- qf::add_indicator(.x, wind, TTR::WPR, bind_orig = TRUE)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add RSI 
qf::iMessage("Add RSI & Indicator")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": RSI"))
  rsi <- qf::add_indicator(.x, wind, TTR::RSI, bind_orig = TRUE)
  rsi <- qf::rsic(rsi, sma.wind, .args = params$IV$rsi)
  col_bind(.x, rsi)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add ROC And Momentum 
qf::iMessage("Add ROC")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y,": ROC"))
  roc <- qf::add_indicator(.x, wind, .f = TTR::ROC, bind_orig = TRUE)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add SAR 
qf::iMessage("Add SAR and SAR Indicator")
#dat <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y,": SAR"))
  sar <- qf::add_indicator(.x, wind, .f = TTR::SAR, accel = params$IV$sar$accel, bind_orig = TRUE)
  sar <- qf::sarc(sar, wind, .args = params$IV$sar)
  col_bind(.x, sar)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add MACD
qf::iMessage("Add MACD")
#dat_t <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": MACD"))
  # Check for sufficient SMA for at least one MACD, otherwise return data
  if (length(qf::sma_names(.x)) > 1) {
    .d <- .x
    macd <- purrr::imap(wind, ~{
      .out <- as.data.frame(
        qf::add_indicator(
          .d,
          .x,
          TTR::MACD,
          nFast = round(.x / 2, 0),
          nSlow = .x,
          nSig = round(.x / 2 * .75, 0),
          maType = TTR::EMA
        )
      ) %>% {magrittr::set_colnames(., paste0(names(.),"_",.y))}
      
      macd <- macc(.d, .out, setNames(.x, .y), .args = params$IV$macd)
      return(macd)
    })
    .out <- col_bind(.d[!grepl("^macd", names(.d), ignore.case = TRUE)], do.call(cbind, macd))
  } else {
    .out <- .x
  }
   .out
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add Ultimate Oscillator
qf::iMessage("Add Ultimate Oscillator")
#dat_t <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": Ultimate Oscillator"))
  uo <- qf::add_indicator(.x, list(day.mp = wind[1] * c(1, 2, 4), week.mc = wind[2] * c(1, 2, 4)), TTR::ultimateOscillator, bind_orig = F) %>% {stats::setNames(., stringr::str_replace(names(.), "ultimateOscillator", "UO"))}
  .uoc <- qf::uoc(uo, wind = params$wind, .args = params$IV$uo) 
  col_bind(.x[!names(.x) %in% c(names(uo), names(.uoc))], .uoc)
}
)
)

dat <- qf::check_dat(dat)

qf::iMessage("Add Rolling Linear Regression")
#dat_t <- purrr::imap(dat, ~{
dat <- furrr::future_imap(dat, ~cl$catch({
  qf::iMessage(paste0(.y, ": roLM"))
  .out <- qf::add_indicator(.x, .ns = wind, .f = roLM, x = qf::time_index(.x, "v"))
  col_bind(.x, .out)
}
)
)

dat <- qf::check_dat(dat)

## @knitr Add Date Based Variables 
qf::iMessage("Add Date Based Variables")
#dat_t <- purrr::map(dat, qf::addTK)
dat <- furrr::future_map(dat, qf::addTK)

dat <- qf::check_dat(dat)

parallel::stopCluster(cl$cl)
future::plan(future::sequential)

.ti <- qf::time_int(dat[[1]])
.ts <- qf::time_index(dat[[1]], "l")
db <- qf::get_data("db", db = .ti$t)

purrr::iwalk(dat, ~{
  .in <- .x[!names(.x) %in% .nms] %>% setNames(nm = make.names(names(.)))
  .tbl <- paste0(.y,"_iv")
  .trange <- as.numeric(range(.x[[.ts]]))
  if (RSQLite::dbExistsTable(db, .tbl)) {
    # Check the existing data
    #.o <- RSQLite::dbGetQuery(db, glue::glue("SELECT * FROM {.tbl} WHERE {rlang::as_string(.ts)} BETWEEN {.trange[1]} AND {.trange[2]}"))
    .existing <- RSQLite::dbReadTable(db, .tbl)
    .o <- dplyr::filter(.existing, between(!!.ts, !!!as.list(.trange)))
    # Check if table should be overwritten
    .new_nms <- setdiff(names(.in), names(.o))
    if (rlang::is_empty(.new_nms)) {
       # Does the table have equivalent values?
      .values <- any(purrr::map2_lgl(dplyr::mutate_all(.in, as.numeric), .o[names(.in)], ~{
        all(dplyr::near(.x, .y, 1.1))
      }))
      
      # If not all values are equivalent, then overwrite
      .do_overwrite  <-  !.values
    } else {
      # if there are new columns present, just overwrite
      .do_overwrite <- TRUE
    }
    # Does the table cover the same time range?
    .range <- all(range(.o[[.ts]]) == .trange)
    if (.do_overwrite) {
      qf::iMessage(paste0("Overwriting ", .tbl, " with new cols:\n", paste0(names(.in)[!names(.in) %in% names(.o)], collapse = ", ")))
      #Just overwrite the whole damn thing bc SQL blows
      RSQLite::dbWriteTable(db, .tbl, .in, overwrite = TRUE)
    } else if (!.range) {
      
      # otherwise append
      .lt <- RSQLite::dbGetQuery(db, glue::glue("SELECT MAX({rlang::as_string(.ts)}) FROM {.tbl}"))[[1]]
      if (.ti$t == "hours") {
        .lt <- lubridate::as_datetime(.lt, tz = "America/New_York")
      } else if (.ti$t == "days") {
        .lt <- lubridate::as_date(.lt)
      }
      .ain <- dplyr::filter(.in, !!.ts > .lt)
      if (nrow(.in) > 0) {
        .append <- TRUE
        qf::iMessage(paste0("Appending ", .tbl, " with data from:\n", paste0(range(.ain[[.ts]], collapse = ", "))))
        RSQLite::dbWriteTable(db, .tbl, .ain, append = .append)
      }
    } else {
      qf::iMessage("No update needed for ", .tbl)
    }
    
    # DO NOTHING if the data is the same and the range matches
    
  # purrr::imap(dplyr::mutate_all(.in[!grepl("RSI", names(.in))][.l], as.numeric)[.l], ~{
  #   .q <- RSQLite::dbExecute(db, glue::glue("UPDATE {.tbl} SET {.y} = :time"), bind.data = tibble::tibble(!!.y := .x))
  #   browser()
  #   RSQLite::db(db, .q, bind.data = tibble::tibble(!!.y :=.x))
  #   .col <- RSQLite::dbGetQuery(db, glue::glue("SELECT {.y} FROM {.tbl}"))
  #   identical(.col, .x)
  # })
  
    
  } else {
    qf::iMessage("Adding table ", .tbl)
    # if no table exists, create it and add the data
    RSQLite::dbWriteTable(db, .tbl, .in)
  }
})
RSQLite::dbDisconnect(db)

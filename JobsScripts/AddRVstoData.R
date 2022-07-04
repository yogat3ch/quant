## @knitr Add trailing stop loss response variables
#  Thu Jun 25 17:18:44 2020
# starting procedures ----
library(qf)
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
qf::iMessage(paste0(calling.script, ": ", lubridate::now(),"\nlocation: ",getwd()))
# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  qf::iMessage("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/data/input_rv.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
if (!exists(".add", mode = "logical")) .add <- F
TSLvars <- params$TSLvars
db <- qf::get_data("db")

# start the cluster
cl <- qf::start_cluster(outfile = T)
future::plan(future::cluster, workers = cl$cl)
#.out <- furrr::future_imap(dat, ~cl$catch({
purrr::iwalk(dat, ~{
  .d <- .x
  .sym <- attr(.d, "Sym") %||% .y
  .ohlcvt <- qf::ohlcvt(.d)
  .runtime <- system.time({
  if (any(is.na(.d[.ohlcvt[c("h","l","c","o")]]))) {
    .miceFast <- miceFast::mice(.d[.ohlcvt[c("h","l","c","o")]], m = 1, threshold = 1)
    .d[, .ohlcvt[c("h","l","c","o")]] <- miceFast::complete(.mice)
  }
    qf::iMessage(paste0(lubridate::now(), ": Begin ", .sym))
    .has_rvs <- any(purrr::imap_lgl(names(TSLvars), ~{any(grepl(.y, names(.d), fixed = TRUE))}))
    if (.has_rvs && !.add) {
      # Remove columns if .add = F
      .rv_nms <- c(paste0(names(TSLvars),"_rv"), paste0(names(TSLvars),"_ind"))
      .d  <- .d[, !names(.d) %in% .rv_nms]
    }
    .out <- furrr::future_imap_dfc(TSLvars, ~cl$catch({
    #.out <- purrr::imap_dfc(TSLvars[1:3], ~{
      qf::iMessage(paste0(lubridate::now(), ": ", .sym, " ", .y," begin"))
      #interpolate names
      tsl <- rlang::list2(!!.y := .x)
      .nms <- c(
        paste0(.y,"_rv"), # response variable name
        paste0(.y,"_ind") # index column name
      )
      .args <- list(.d = .d[.ohlcvt],
                    tsl = tsl)
        .rvs <- stats::setNames(do.call(qf::TSL, .args), .nms)
      qf::iMessage(paste0(lubridate::now(), ": ", .sym, " ", .y," complete"))
      .rvs
    }
    )
    )
    # Add results to DB ----
    # Sat Jun 27 13:32:04 2020
    .tbl <- paste0(.sym,"_rv")
    qf::iMessage(paste0("Adding RVs to DB: ",.tbl))
    .t <- .ohlcvt[c("t")]
    .ts <- rlang::sym(.t)
    .out <- dplyr::bind_cols(.d[.t],.out)
    if (DBI::dbExistsTable(db, .tbl)) {
      # retrieve data already in the db
      .in_p <- dplyr::mutate_at(DBI::dbReadTable(db, .tbl), dplyr::vars(!!.ts, dplyr::ends_with("_ind")), lubridate::as_datetime, tz = "America/New_York")
      if (!all(names(.in_p) %in% names(.out)) || .in_p[[.t]][1] > .out[[.t]][1]) {
        # if the columns aren't the same or out has data preceding that in the database
        .overwrite <- TRUE
        .append <- FALSE
        .in <- .out
      } else if (isFALSE(identical(range(.in_p[[.t]]), range(.out[[.t]])))) {
        .append <- TRUE
        .overwrite <- FALSE
        .na_1 <- min(which(slider::slide_lgl(.in_p, ~any(is.na(.x)))))
        .t_del <- paste0(as.numeric(.in_p[[.t]][.na_1:nrow(.in_p)]), collapse = ", ")
        .q <- glue::glue("DELETE FROM {.tbl} WHERE {.t} IN ({.t_del})")
        DBI::dbExecute(db, .q)
        .in <- dplyr::filter(.out, !!.ts >= !!.in_p[[.t]][.na_1])
      } 
    } else {
      .in <- .out
      .append <- FALSE
      .overwrite = TRUE
      
    } 
    # Update rows
      DBI::dbWriteTable(db, .tbl, as.data.frame(.in), append = .append, overwrite = .overwrite)
      qf::iMessage(paste0("RVs added to: ", basename(db@dbname)," ", .tbl))
})
  qf::iMessage(paste0(.sym, " Elapsed: ",lubridate::as.duration(.runtime[3])))
}
#)
)
  
parallel::stopCluster(cl$cl)



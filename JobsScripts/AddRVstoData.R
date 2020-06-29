#TODO(Add nested foreach loop for dat, and TSLvars)
## @knitr Add trailing stop loss response variables
#  Thu Jun 25 17:18:44 2020
# starting procedures ----
`%>%` <- magrittr::`%>%`
`!!!` <- rlang::`!!!`
`!!` <- rlang::`!!`
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
message(paste0(calling.script, ": ", lubridate::now(),"\nlocation: ",getwd()))
# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/dat.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
if (!exists(".add", mode = "logical")) .add <- F
TSLvars <- params$TSLvars
db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = .dbname)
.e <- new.env()
source("~/R/Quant/JobsScripts/QuantFunctions_TSL.R", local = .e)
# start the cluster
cl <- params$start_cluster(6, outfile = "~/R/Quant/out_log.log")
future::plan(future::cluster, workers = cl$cl)
  #.out <- furrr::future_imap(dat, ~cl$catch({
  purrr::iwalk(dat, ~{
    .d <- .x
    .sym <- attr(.d, "Sym")
    .runtime <- system.time({
    if (any(is.na(quantmod::HLC(.d)))) {
      .mice <- mice::mice(quantmod::OHLC(.d), m = 1, threshold = 1)
      .d[, names(quantmod::OHLC(.d))] <- mice::complete(.mice)
    }
      message(paste0(lubridate::now(), ": Begin ", .sym))
      .has_rvs <- any(purrr::imap_lgl(names(TSLvars), ~{any(grepl(.y, names(.d), fixed = TRUE))}))
      if (.has_rvs && !.add) {
        # Remove columns if .add = F
        .rv_nms <- c(paste0(names(TSLvars),"_rv"), paste0(names(TSLvars),"_ind"))
        .d  <- .d[, !names(.d) %in% .rv_nms]
      }
      .out <- furrr::future_imap_dfc(TSLvars, ~cl$catch({
      #.out <- purrr::imap_dfc(TSLvars, ~{
        message(paste0(lubridate::now(), ": ", .sym, " ", .y," begin"))
        #interpolate names
        tsl <- rlang::list2(!!.y := .x)
        .nms <- c(
          paste0(.y,"_rv"), # response variable name
          paste0(.y,"_ind") # index column name
        )
        .args <- list(v = .d[, stringr::str_which(names(.d),
                                                  stringr::regex("^date$|^time$|^open$|^high$|^low$|^close$", ignore_case = TRUE))],
                      tsl = tsl, tsl_amt = attr(TSLvars, "tsl_amt"), time_index = params$time_index)
        .rvs <- setNames(do.call(.e$TSL, .args), .nms)
        message(paste0(lubridate::now(), ": ", .sym, " ", .y," complete"))
        .rvs
      }
      )
      )
      # Add results to DB ----
      # Sat Jun 27 13:32:04 2020
      .tbl <- paste0(.sym,"_rv")
      message(paste0("Adding RVs to DB: ",.tbl))
      .ti <- params$time_index(.d)
      .tis <- rlang::sym(.ti)
      .out <- dplyr::bind_cols(.d[.ti],.out)
      if (RSQLite::dbExistsTable(db, .tbl)) {
        .in_p <- dplyr::mutate_at(RSQLite::dbReadTable(db, .tbl), dplyr::vars(!!.tis), lubridate::as_datetime, tz = "America/New_York")
        .to_append <- dplyr::anti_join(.out, .in_p, by = .ti)
        if (nrow(.to_append) > 0 && all(names(.out) %in% names(.in_p))) {
          .append = TRUE
          .overwrite = FALSE
          .in <- .to_append
        } else {
          .append = FALSE
          .overwrite = TRUE
          .in <- .out
        }
      } else {
        .append = FALSE
        .overwrite = TRUE
        .in <- .out
      }
      # Update rows
        RSQLite::dbWriteTable(db, .tbl, as.data.frame(.in), append = .append, overwrite = .overwrite, colClasses = purrr::map_chr(.in, class))
        message(paste0("RVs added to: ", basename(db@dbname)," ", .tbl))
  })
    message(paste0(.sym, " Elapsed: ",lubridate::as.duration(.runtime[3])))
  }
  #)
  )
  
  parallel::stopCluster(cl$cl)

# if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) save(dat,file = "dat.Rdata")

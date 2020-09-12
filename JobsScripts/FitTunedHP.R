`%>%` <- magrittr::`%>%`
`!!!` <- rlang::`!!!`
`!!` <- rlang::`!!`
`%||%` <- rlang::`%||%`
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
message(paste0(calling.script, ": ", lubridate::now(),"\nlocation: ",getwd()))
# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/data/input_FitTunedHP.Rdata")
}

.e <- new.env()
source("~/R/Quant/JobsScripts/QuantFunctions_optimReturn.R", local = .e)
source("~/R/Quant/JobsScripts/parameters.R")  
cl <- parallel::makeCluster(6)
future::plan(future::cluster, workers = cl)
preds <- furrr::future_imap(dat_oot, .options = furrr::future_options(packages = "rlang"), .f = ~{
  qf:iMessage(paste0(lubridate::now(),": Begin ",.y))
  tictoc::tic()
  .d <- .x
  .sym <- attr(.d$train, "Sym") %||% .y
  .btsl <- attr(.d$train, "tsl")
  frm <- formula(glue::glue("{.btsl$tsl}_rv ~ ."))
  .ti <- qf::time_int(.d$train)
  
  .tune <- attr(.d$train, "tune")
  .tune <- purrr::map(.tune, ~{
    .out <- dplyr::distinct_all(.x, .keep_all = TRUE)
    .out$mode <- "regression"
    .out
  })
  .tis <- rlang::sym(qf::time_index(.d$train))
  
  # models ----
  # Sat Aug 15 15:17:21 2020
  .mods <- purrr::imap(.tune, ~{
    .engine <- .y
    if (.y == "xgboost") {
      .fn <- parsnip::boost_tree
    } else if (.y == "earth") {
      .fn <- parsnip::mars
    }
    .wf <- workflows::workflow() %>% 
      workflows::add_recipe(.rec)
    # Try the top 3 best tuning parameters on the oot data
    .t <- .x
    .mods <- purrr::map(1:nrow(.t), ~{
      .dots <- as.list(.t[.x,])
      .wf <- .wf %>% 
        workflows::add_model(
          do.call(.fn, .dots) %>% 
            parsnip::set_engine(.engine)
        )
      .out <- parsnip::fit(.wf, data = .d$train)
    })
  })

  .d_p <- recipes::prep(.rec, data = .d$train)
  .d_b <- recipes::bake(.d_p, new_data = .d$test)
  # predictions & performance ----
  # Sat Aug 15 15:17:14 2020
  out <-  purrr::map_depth(.mods, 2, ~{
    .out <- list()
    .pred <- predict(.x, new_data = .d$test)
    .pred <- .pred[[1]] # Select the first col in the tibble (the predictions)
    .dat <- tibble::tibble(truth = .d_b[[frm[[2]]]], estimate = .pred)
    # rmse
    .out$rmse <- yardstick::rmse(.dat, truth, estimate) %>% 
      # Reverse scale to percentage
      dplyr::mutate(rmse_p = .d_p$steps[[1]]$means + (.estimate * .d_p$steps[[1]]$sds))
    # Confusion matrix of the number of times it accurately suggests a gain vs a loss
    .out$cm <- summary(yardstick::conf_mat(dplyr::mutate_all(.dat, ~factor(sign(.), levels = c(-1, 0 , 1))), truth, estimate))
    # transform predictions to percentages
    .pred <- .d_p$steps[[1]]$means + (.pred * .d_p$steps[[1]]$sds)
    .tsl <- attr(.d$train, "tsl")$tsl
    # Resplice predictions with NA from _ind such that predictions line up with sell dates
    .ind <- which(is.na(.d$test[[paste0(.tsl, "_ind")]]))
    if (any(.ind %in% 1:length(.pred))) {
      .pred <- R.utils::insert(.pred, which(.ind %in% 1:length(.pred)), values = 0, useNames = F)
    }
    .diff <- nrow(.d$test) - length(.pred)
    if (.diff > 0) {
      .pred <- c(.pred, rep(0, .diff))
    }
    
    .out$returns <- .e$optimReturn(.d$test,
                                   .p = attr(.d$train, "tsl")$p,
                                   tsl_name = attr(.d$train, "tsl")$tsl,
                                   time_index = qf::time_index,
                                   pred = .pred)
    .out$buy_hold <- (mean(unlist(quantmod::OHLC(.d$test) %>% .[nrow(.),])) - mean(unlist(quantmod::OHLC(.d$test)[1,]))) / mean(unlist(quantmod::OHLC(.d$test)[1,]))
    out <- list(
      model = .x,
      predictions = .pred,
      performance = .out
    )
  })
  saveRDS(out, file = glue::glue("~/R/Quant/MdlBkp/{.y}_MPP.rds"))
.t <- tictoc::toc()
qf::iMessage(lubridate::now(),": ",.y," Elasped ",lubridate::as.duration(round(.t[[2]] - .t[[1]],3)))
  out
})
parallel::stopCluster(cl)

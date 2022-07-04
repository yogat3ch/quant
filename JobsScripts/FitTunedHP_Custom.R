`%>%` <- magrittr::`%>%`
`!!!` <- rlang::`!!!`
`!!` <- rlang::`!!`
calling.script <- basename(purrr::keep(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script\\s\\=\\s\\')[A-Za-z0-9\\/\\:\\.\\_]+"),~!is.na(.x)))
message(paste0(calling.script, ": ", lubridate::now(),"\nlocation: ",getwd()))
# Load New Data
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  message("Sourced from Local Job, loading dependencies")
  load("~/R/Quant/data/input_train.Rdata")
}
source("~/R/Quant/JobsScripts/parameters.R")
purrr::map(dat_oot, ~{
  .d <-  .x
  .sym <- attr(.d$train, "Sym")
  .btsl <- attr(.d$train, "tsl")
  frm <- stats::formula(glue::glue("{.btsl$tsl}_rv ~ ."))
  .ti <- AlpacaforR::time_interval(.d$train)
  #.rec <- readRDS(fs::path("~/R/Quant/MdlBkp",.ti["t"],glue::glue("{.sym}_rec"), ext = "rds"))
  .tis <- rlang::sym(params$time_index(.d$train))
  # PreProcessing Data ----
  # Fri Jul 26 14:19:16 2019
  # Make a preprocessing recipe
  .rec <- recipes::recipe(frm, data = .d$train) %>% 
    recipes::step_normalize(recipes::all_outcomes(), skip = T) %>% 
    # 1. remove NA observations, rv, ind and time columns
    recipes::step_naomit(dplyr::ends_with("ind")) %>% 
    # 2. Remove tsl index and time
    recipes::step_rm(!!.tis, dplyr::ends_with("ind")) %>% 
    # 3. Remove zero variables
    recipes::step_zv(recipes::all_predictors()) %>% 
    # 4. Remove correlated
    recipes::step_corr(recipes::all_predictors()) %>% 
    # 5. Normalize
    recipes::step_normalize(recipes::all_predictors()) 
  .d_p <- recipes::prep(.rec, data = .d$train)
  .d_b <- recipes::bake(.d_p, new_data = .d$test)
  .tune <- attr(.d$train, "tune")
  .tune <- purrr::imap(.tune, ~{
    .x <- as.list(.x)
    if (.y == "xgboost") {
      .xgb_nm <- c(tree_depth = "max_depth",
                   trees = "nrounds",
                   learn_rate = "eta",
                   mtry = "colsample_bytree",
                   min_n = "min_child_weight",
                   loss_reduction = "gamma",
                   sample_size = "subsample",
                   stop_iter = "early_stop")
      .x <- stats::setNames(.x, .xgb_nm[names(.x)])
      .x$y <- .d_b[[frm[[2]]]]
      .x$x <- dplyr::select(.d_b, - !!frm[[2]])
    } else if (.y == "earth") {
      .mars_nm <- c(num_terms = "nprune",
                    prod_degree = "degree",
                    prune_method = "pmethod")
      .x <- stats::setNames(.x, .mars_nm[names(.x)])
    }
    .x
  })
  rlang::exec(parsnip::xgb_train, !!!.tune$xgboost)
  wf <- list(
  xgboost = workflows::workflow() %>% 
    workflows::add_recipe(.rec) %>% 
    workflows::add_model(
      
    ),
  mars = workflows::workflow() %>% 
    workflows::add_recipe(.rec) %>% 
    workflows::add_model(
      do.call(earth::earth, .tune$earth)
    )
  )
  #models 
  .mods <- purrr::map(wf, ~{
    .out <- parsnip::fit(.x, data = .d$train)
  })
  #predictions 
  
  .preds <- purrr::imap_dfc(.mods, ~{
    .pred <- stats::predict(.x, new_data = .d$test)
  # transform predictions to percentages
    stats::setNames(.d_p$steps[[1]]$means + (.pred * .d_p$steps[[1]]$sds), .y)
  })
  # rmse
  attr(.preds, "performance") <-  purrr::map(.preds, ~{
    .out <- list()
    .dat <- dplyr::tibble(truth = .d_b[[frm[[2]]]], estimate = .x)
    .out$rmse <- yardstick::rmse(.dat, truth, estimate)
    .out$cm <- summary(yardstick::conf_mat(dplyr::mutate_all(.dat, ~factor(sign(.), levels = c(-1, 0 , 1))), truth, estimate))
    .out
  })
  
})


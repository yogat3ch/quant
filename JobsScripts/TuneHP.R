options(error = qf::pb_error)
`%>%` <- magrittr::`%>%`
load(file = qf::wd("data/input_TuneHP.Rdata"))
source(qf::wd("JobsScripts/parameters.R"))
library(parsnip)


# Create Workflows
dat_wf <- purrr::map(dat, qf::mod_workflows)
.t_int <- AlpacaforR::time_interval(dat[[1]])
tictoc::tic("overall")
cl <- qf::start_cluster()
doParallel::registerDoParallel(cl)
#dat_wf <- furrr::future_map(dat, qf::mod_workflows)
.t <- qf::time_index(dat[[1]])
btsl <- purrr::map(dat, qf::get_btsl)
purrr::iwalk(dat_wf, ~{
  wflow_set <- .x
  .sym <- .y
  .tsl <- btsl[[.sym]]$tsl
  metric = "rmse"
  qf::iMessage(paste0(lubridate::now(), ": Begin ",.sym))
  tictoc::tic(.sym)
  # Rolling Windows ----
  # Sun Jul 12 16:27:30 2020
  .wind <- qf::time_windows(dat[[.sym]], params$wind$multipliers)
  i <- which.max(.wind[.wind < nrow(dat[[.sym]])])
  .d_roll <- rsample::rolling_origin(
    dat[[.sym]],
    initial = .wind[i],
    skip = .wind[i],
    cumulative = FALSE
  )
  
  # Retrieve engines ----
  # Thu May 06 10:35:53 2021
  .pkgs <- unique(purrr::map_chr(wflow_set$wflow_id, ~{
    workflowsets::pull_workflow(wflow_set, .x) %>%
      workflows::pull_workflow_spec() %>%
      `[[`("engine")
  }))
  
  # Train workflows ----
  # Thu May 06 10:13:42 2021
  .tg <- workflowsets::workflow_map(wflow_set, "tune_grid",
    resamples = .d_roll,
    grid = 12,
    control = tune::control_grid(
      verbose = TRUE,
      allow_par = TRUE,
      parallel_over = "resamples",
      pkgs = .pkgs
    ),
    metrics = yardstick::metric_set(yardstick::rmse)
  )
  if (save_intermediate) saveRDS(.tg, glue::glue("data/{.sym} - {paste0(.pkgs, collapse = '.')}.wflow_set.rds"))
  # Select the best workflow
  .best_wf <- workflowsets::rank_results(.tg, 
                                      rank_metric = metric, 
                                      select_best = TRUE)
  wf <- workflowsets::pull_workflow(wflow_set, .best_wf$wflow_id[1])
  .best_wf_res <- workflowsets::pull_workflow_set_result(.tg, .best_wf$wflow_id[1])
  .tunes <- tune::tunable(workflows::pull_workflow_spec(wf))
  # Using the best recipe get the top performing model hyperparameters from each resample
  .best_hp <- dplyr::bind_rows(purrr::map(.best_wf_res$.metrics, ~head(dplyr::arrange(.x, .estimate), 1))) %>% 
    dplyr::arrange(.estimate) %>% 
    dplyr::select(.tunes$name) %>% 
    head(5)
    
# .best_hp <- tibble::tibble(tree_depth = c(5,13), trees = c(111,1553), learn_rate = c(2.72e-10, 9.35e-06), mtry = c(36,163), min_n = c(16,28), loss_reduction = c(5.24e-05, 14.3), sample_size = c(.481, .727), stop_iter = c(3,15))
  # Tune a second time if not definitive ----
  # Thu May 06 09:52:44 2021 
  if (any(purrr::map_int(.best_hp, length) > 1)) {
    ts <- rsample::initial_time_split(dat[[.sym]])
    ts <- rsample::testing(ts)
    i <- which.max(.wind[.wind < nrow(ts)]) - 1
    .d_roll <- rsample::rolling_origin(
      ts,
      initial = .wind[i],
      skip = .wind[i],
      cumulative = TRUE
    )
    .tg <- tune::tune_bayes(
      wf,
      resamples = .d_roll,
      param_info = dials::parameters(qf::parameter_create(wf, .best_hp)),
      iter = 50,
      initial = .best_wf_res,
      control = tune::control_bayes(
        verbose = TRUE,
        parallel_over = "everything",
        pkgs = workflows::pull_workflow_spec(wf)$engine
      ),
      metrics = yardstick::metric_set(yardstick::rmse)
    )
    if (save_intermediate) saveRDS(.tg, glue::glue("data/{.sym} - {paste0(.pkgs, collapse = '.')}.tune_bayes.rds"))
    .best_hp <- tune::select_best(.tg, metric = metric) %>%
      dplyr::select(.tunes$name)
  } 
  .mdl_spec <- workflows::pull_workflow_spec(wf) %>% 
    update(parameters =  .best_hp)
  wf <- wf %>% 
    workflows::update_model(.mdl_spec)

  #qf::grab_env()

  #  Save the best hyperparameters ----
  # Thu May 06 09:52:14 2021
  .tn <- glue::glue("{workflows::pull_workflow_spec(wf)$engine}_tune")
  .best_hp <- dplyr::bind_cols(tibble::tibble(date = Sys.Date(), symbol = .sym, tsl = .tsl), .best_hp)
  DBI::dbWriteTable(db, .tn, .best_hp, append = ifelse(DBI::dbExistsTable(db, .tn), TRUE, FALSE))
  
  
  #  Save best model ----
  # Thu May 06 09:52:20 2021
  .dir <- "~/R/Quant/MdlBkp"
  .mdls <- list.files(.dir, recursive = TRUE)
  .fn <- glue::glue("{.sym}_{.best_wf$wflow_id}_{.tsl}.workflow")
  .p <- fs::path(.dir, .t_int$timeframe, .fn, ext = "rds")
  saveRDS(wf, .p)
  qf::iMessage(paste0("Saved ",.p))
  
  # Close timer and message
  .tt <- tictoc::toc(log = TRUE)
  qf::iMessage(glue::glue("{lubridate::now()} - {.sym} {.tsl} - Elapsed: {lubridate::as.duration(.tt$toc - .tt$tic)}"))
})

#  Complete overall and message ----
# Thu May 06 09:52:32 2021
.tt <- tictoc::toc(log = TRUE)
qf::iMessage(glue::glue("{lubridate::now()} - TuneHP Complete - Elapsed: {lubridate::as.duration(.tt$toc - .tt$tic)}"))
DBI::dbDisconnect(db)
doParallel::stopImplicitCluster()





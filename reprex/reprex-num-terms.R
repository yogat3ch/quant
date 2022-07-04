.dt <-
  data.frame(
    y = stats::rnorm(20),
    x = stats::rnorm(20),
    time = seq(
      from = lubridate::today() - 19,
      to = lubridate::today(),
      by = "day"
    )
  )



rec <- recipes::recipe(y ~ ., data = .dt)

.roll <- rsample::rolling_origin(
  .dt,
  initial = 5,
  skip = 5,
  cumulative = TRUE
)

par_mars = dials::parameters(
  list(
    num_terms = dials::num_terms(c(1,1)),
    prod_degree = dials::prod_degree(),
    prune_method = dials::prune_method(dials::values_prune_method[!dials::values_prune_method %in% c("backward", "cv", "exhaustive", "none")])
  )
)

mod_mars = parsnip::mars(
  mode = "regression",
  num_terms = tune::tune(),
  prod_degree = tune::tune(),
  prune_method = tune::tune()
) %>% 
  parsnip::set_engine("earth")
.tgr <- tune::tune_grid(
  mod_mars,
  # preprocessor = frm,
  preprocessor = rec,
  resamples = .roll,
  # iter = 2,
  # objective = "rmse",
  param_info = par_mars,
  grid = dials::grid_latin_hypercube(
    par_mars,
    size = 10
  ),
  control = tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    pkgs = "earth"
  )
)

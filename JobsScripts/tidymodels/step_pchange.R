step_qm_new <-
  function(terms, role, trained, dat, att, .f = .f, ex, params, skip, id) {
    recipes::step(
      subclass = "pchange",
      terms = terms,
      role = role,
      trained = trained,
      dat = dat,
      att = att,
      .f = .f,
      ex = ex,
      params = params,
      skip = skip,
      id = id
    )
  }

step_qm <- function(
  recipe, ...,
  role = NA,
  trained = FALSE,
  .f = NULL,
  ex = NULL,
  params = NULL,
  skip = FALSE,
  id = recipes::rand_id("qm")
) {
  
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos` function in `rlang`. `ellipse_check` captures the
  ##  values and also checks to make sure that they are not empty.  
  terms <- recipes::ellipse_check(...)
  if (is.null(.f) && is.null(ex)) rlang::abort("A function (`.f`) and parameters (`params`) or an expression (`ex`) is required.")
  recipes::add_step(
    recipe,
    step_qm_new(
      terms = terms,
      trained = trained,
      role = role,
      dat = dat,
      att = att,
      .f = .f,
      ex = ex,
      params = params,
      skip = skip,
      id = id
    )
  )
}


doQM <- function(x, ex = NULL, .f = NULL, params = NULL) {
  if (rlang::is_expression(ex)) {
    .out <- rlang::eval_tidy(ex, data = dat)
  } else {
    `!!` <- rlang::`!!`
    `!!!` <- rlang::`!!!`
    if (!inherits(params, "list")) params <- list()
    params$x1 <- x
    .out <- rlang::call2(!!.f, !!!params)
  }
  return(.out)
}

prep.step_pchange <- function(x, training, info = NULL) {
  .cnames <- recipes::terms_select(x$terms, info = info)
  # check that data is quantitative
  .dat <- training[.cnames]
  recipes::check_type(.dat, quant = TRUE)
  step_qm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ex = x$ex,
    att = x$att,
    dat = .dat,
    params = x$params,
    skip = x$skip,
    id = x$id
  )
}

bake.step_pchange <- function(object, new_data, ...) {
  new_data[paste0(names(object$dat), "_pc")] <- purrr::map(object$dat, pchange, params = object$params)
  tibble::as_tibble(new_data)
}

.prep <- recipes::recipe(open ~ ., data = dat$GOOG) %>%
  step_pchange(close) %>%
  recipes::prep(training = dat$GOOG)

.new <- recipes::bake(.prep, new_data = dat$GOOG)

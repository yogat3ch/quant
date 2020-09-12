`%>%` <- magrittr::`%>%`
load(file = "~/R/Quant/data/input_TuneHP.Rdata")
source("~/R/Quant/JobsScripts/parameters.R")
if (!HDA::go("modelSpecs")) modelSpecs <- as.list(rep(NA, length(dat))) %>% setNames(names(dat))
library(dials)
# mod.packages <- c("earth", "RSNNS", "xgboost", "plyr", "caret", "caretEnsemble")
# HDA::startPkgs(mod.packages)
.t <- qf::time_int(dat[[1]])
db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = fs::path("~/R/Quant/db/", .ti$t, ext = "db"))
tictoc::tic()
cl <- parallel:::makeCluster(round(future::availableCores()  * .75), outfile = "~/R/Quant/out_log.log")
future::plan(future::cluster, workers = cl)
doFuture::registerDoFuture()
purrr::iwalk(dat, ~{
  
  .d <- .x
  .sym <- .y
  .t <- qf::time_index(.d)
  .tsl <- attr(.d, "tsl")
  .b_tsl <- .tsl[1] # Get the symbol of the data and store it
  .p_tsl <- .tsl$p # get the percentage
  qf::iMessage(paste0(lubridate::now(), ": Begin ",.sym))
  if (zoo::is.zoo(.d)) .d <-  zoo::fortify.zoo(.d) # If still xts, convert to df
  .td <- qf::time_int(.d)
  #populate train_rvs from data
  train_rvs <- grep("\\_rv$", names(.d), value = TRUE)
  # Check to see if the training can proceed ----
  # Thu Jul 09 07:34:34 2020 
  if (length(train_rvs) < 1) {
    #If nothing to train clear the memory.
    qf::iMessage("No models to train.")
    gc()
    return(NULL)
  } else if (nrow(.d) < 100) {
    # If not enough data say so and return
    qf::iMessage(paste0(.sym,": Not Enough Data for Model"))
    return("Not Enough Data for Model")
  } 
  qf::iMessage(paste0(.sym," RVs: ", paste(train_rvs, collapse = ", ")))
  gc()
  #Initiate packages
# TODO Sort out what goes into qf::mod_workflows and what stays here.
  out <- purrr::walk(train_rvs, ~{
      .rv_ind <- c(rv = .x, ind = stringr::str_replace(.x, "\\_rv$","\\_ind"))
      metric <- switch(class(.d[[.rv_ind[1]]]),
             numeric = "RMSE",
             factor = "Accuracy")
      
      frm <- formula(glue::glue("{.x} ~ ."))
       
      # PreProcessing Data ----
      # Fri Jul 26 14:19:16 2019
      # Make a preprocessing recipe
      .rec <- recipes::recipe(frm, data = .d) %>% 
        recipes::step_normalize(recipes::all_outcomes(), skip = T) %>% 
        # 1. Remove time and rv_index
        recipes::step_rm(!!.ti, dplyr::ends_with("ind")) %>% 
        # 2. Remove zero variables
        recipes::step_zv(recipes::all_predictors()) %>% 
        # 3. Remove correlated
        recipes::step_corr(recipes::all_predictors()) %>% 
        # 4. Normalize
        recipes::step_normalize(recipes::all_predictors())     
      .prec <- recipes::prep(.rec, training = .d, retain = T) 
      gc()
      qf::iMessage(paste0(lubridate::now(), ": ", .sym," ", .rv_ind["rv"]))
      # Rolling Windows ----
      # Sun Jul 12 16:27:30 2020
      .wind <- qf::time_windows(.d, params$wind$multipliers)
      .d_roll <- rsample::rolling_origin(
        .d,
        initial = .wind[4],
        skip = .wind[2],
        cumulative = TRUE
      )
      qf::iMessage(glue::glue("{.sym}: Rolling Sample Created"))
      .rows <- .prec$term_info %>%
        dplyr::filter(role == "predictor") %>%
        {round(nrow(.) * .75)}

      .out <- purrr::map2(mods, mod_pars, ~{
        .e <- new.env()
        .e$params <- .y
        .e$mod <- .x
        .out <- purrr::map(1:3, ~{
          qf::iMessage(glue::glue("{.sym} {.e$mod$engine} Round {.x}"))
          tictoc::tic()
          .size <- round(100 / .x)
          .tg <- tune::tune_grid(
            .e$mod,
           # preprocessor = frm,
            preprocessor = .rec,
            resamples = .d_roll,
            # iter = 2,
            # objective = "rmse",
            param_info = .e$params,
            grid = dplyr::distinct(dials::grid_latin_hypercube(
              .e$params,
              size = .size
                )),
            control = tune::control_grid(
              verbose = TRUE,
              allow_par = TRUE,
              pkgs = .e$mod$engine
            )
          )
          .out <- tune::show_best(.tg, metric = "rmse")
          .e$params <<- .out %>% 
            dplyr::select(1:(which(names(.out) %in% ".metric") - 1)) %>% 
            purrr::imap(~{
              .fn <- get0(.y, as.environment(glue::glue("package:dials")))
              if (inherits(.x, c("integer", "numeric"))) {
                .l <- do.call(.fn, list(range = range(.x), trans = NULL))
              } else {
                .l <- do.call(.fn, list(values = unique(.x)))
              }
              dials::parameters(.l)
            }) %>%
            dplyr::bind_rows()
          tictoc::toc(log = T)
          .out
        })
        out <- dplyr::bind_rows(.out)
        .tn <- glue::glue("{.sym}_{stringr::str_remove(.rv_ind['rv'], '_rv$')}_{.e$mod$engine}.tune")
        out$time <- lubridate::now()
        .append <- ifelse(RSQLite::dbExistsTable(db, .tn), TRUE, FALSE)
        RSQLite::dbWriteTable(db, .tn, out, append = .append)
      })
      RSQLite::dbDisconnect(db)
      
        
      # tunelist <- sapply(methodList, simplify = F, USE.NAMES = T, tuneLength = 5, FUN = function(m, tuneLength = NULL, tuneGrids = NULL){
      #   if (is.numeric(tuneLength)){
      #     out <- caretEnsemble::caretModelSpec(method = m, tuneLength = tuneLength)
      #   } else {
      #     out <- caretEnsemble::caretModelSpec(method = m, tuneGrid = tuneGrids[[m]])
      #   }
      #   return(out)
      # })
      
      # train.time <- system.time({
      #   .iw <- length(.d_in[[.rv_ind["rv"]]]) %/% 10
      #   tr.index <- caret::createTimeSlices(.d_in[[.rv_ind["rv"]]],
      #                                       initialWindow = .iw,
      #                                       fixedWindow = FALSE,
      #                                       skip = round(length(.d_in[[.rv_ind["rv"]]]) %/% .iw * .25)
      #                                       )
      #   data.train <- caret::trainControl(method = "timeslice"
      #                                     , index = tr.index$train
      #                                     , indexOut = tr.index$test
      #                                     , search = "grid" 
      #                                     , returnData = F
      #                                     , allowParallel = T
      #                                     , savePredictions = 'final'
      #                                     , verboseIter = F
      #                                     )
      #   .w <- purrr::map_dbl(.d_in[[.rv_ind["rv"]]], ~{
      #     if (.x > .p_tsl) {
      #       .out <- 2
      #     } else if (.x > 0) {
      #       .out <- 1.5
      #     } else {
      #       .out <- 1
      #     }
      #     .out
      #   })
      #   
      #   caretlistmod <- caretEnsemble::caretList(form = frm,
      #                                            data = .d_in,
      #                                            trControl = data.train,
      #                                            methodList = params$methodList,
      #                                            metric = metric,
      #                                            weights = .w, 
      #                                            tuneLength = 6,
      #                                            continue_on_fail = FALSE)
      # })
      train.time <- lubridate::duration(round(sum(purrr::map_dbl(tictoc::tic.log(format = F), ~{
        .x[[2]] - .x[[1]] 
      })),2))
      qf::iMessage(glue::glue("{lubridate::now()}: {.sym} {.rv_ind['rv']} - Elapsed: {train.time}"))
      return(.out)
    })
  names(out) <- stringr::str_remove(train_rvs, "\\_rv")
  .dir <- "~/R/Quant/MdlBkp"
  if (!.replace) {
    .mdls <- list.files(.dir, recursive = TRUE)
    purrr::iwalk(out, ~{
      .fn <- glue::glue("{.sym}_{.y}")
      if (any(grepl(.fn, .mdls))) .fn <- paste0(.fn,"-",lubridate::today())
      .p <- fs::path(.dir, .td[["t"]], .fn, ext = "rds")
      saveRDS(.x, .p)
      qf::iMessage(paste0("Saved ",.p))
    })
  } else {
    # Replace all
    purrr::iwalk(out, ~{
      .fn <- glue::glue("{.sym}_{.y}")
      .p <- fs::path(.dir, .td[["t"]], .fn, ext = "rds")
      saveRDS(.x, .p)
      qf::iMessage(paste0("Saved ",.p))
    })
  }
    
})
total.time <- tictoc::toc()
parallel::stopCluster(cl)
# HDA::unloadPkgs(mod.packages)
.m <- paste0(lubridate::now(), ": Model Training Complete, Elapsed - ",lubridate::as.duration(total.time[[2]] - total.time[[1]]))
qf::iMessage(.m)
  
RPushbullet::pbPost("note", title = "Message from R", body = paste0(.m, " 1m til next model begins."))
Sys.sleep(60)
  
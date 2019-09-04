HDA::startPkgs(c("magrittr","dplyr", "caret"))
load(file = "~/R/Quant/dat_dt.Rdata")
if (!HDA::go("best_tsl")) {best_tsl <- NULL ; message("No best_tsl, setting as NULL")}
if (!HDA::go("train_rvs")) {train_rvs <- NULL ; message("No train_rvs, setting as NULL")}
#For Debugging
# tick <- "GOOGL"
# pmap.vars  <- list(tick = tick, .dat = dat[[tick]], .b_tsl = best_tsl[[tick]], .mS = modelSpecs[[tick]], train_rvs = train_rvs, nm = train_rvs[2])
# pmap.vars  %>%  list2env(envir = .GlobalEnv)
# pf <- new.env()
# pmap.vars %>% list2env(envir = pf)
mod.packages <- c("doParallel","iterators","parallel","foreach","earth", "RSNNS", "xgboost", "plyr") # [package dependencies]
HDA::startPkgs(mod.packages)
total.time <- system.time({
purrr::pwalk(list(.dat = dat, .b_tsl = best_tsl, .mS = modelSpecs), train_rvs = train_rvs, function(.dat, .b_tsl, .mS, train_rvs){
  tick <- attr(.dat, "Sym") %>% unlist # Get the symbol of the data and store it
  message(paste0("Training: ",tick," Now: ",lubridate::now()))
  if (zoo::is.zoo(.dat)) .dat <-  zoo::fortify.zoo(.dat) # If still xts, convert to df
  if (!is.null(.b_tsl)) train_rvs <- c(.b_tsl[["tsl_types"]][["tsl"]], train_rvs) %>% unique # If best_tsl is included then add that RV to the ones to be trained
  train_rvs <- paste0(train_rvs,"_rv")
    try({load(file = paste0("~/R/Quant/MdlBkp/",tick, "_cl.Rdata"))}) #Try to load previous models
  ob <- get0(paste0(tick, "_cl"), mode = "list") # If it loads, save it as ob
  # If .replace is TRUE Remove models from object
  if(!is.null(ob) & sum(train_rvs %in% names(ob)) > 0 & .replace) {
    ob <- ob[!names(ob) %in% train_rvs]
  } else if (!is.null(ob) & sum(train_rvs %in% names(ob)) > 0 & !.replace) {
    # If false remove already trained rvs from train_rvs
    train_rvs <- train_rvs[!names(train_rvs) %in% names(ob)]
    }
  
  message(paste0(tick," RVs: ", paste(train_rvs, collapse = ", ")))
  if (length(train_rvs) < 1) {
    message("No models to train.")
    #message(paste("Objects: ",paste(ls(), collapse = " ")))
    rm(list = c(paste0(tick,"_cl")))
    gc()
    return(NULL)
    } #If nothing to train just return the object and clear the memory.
  if (nrow(.dat) < 30) {
    message(paste0(tick,": Not Enough Data for Model"))
    return("Not Enough Data for Model")
    } # If not enough data say so and return
  rm(list = c(paste0(tick,"_cl")))
  #Initiate packages
  out <- purrr::map(train_rvs, .pf = sys.frame(sys.nframe()), function(nm, .pf){
      td_nm <- stringr::str_extract(names(.pf$.dat), "^time$|^date$") %>% purrr::discard(is.na)
      rvs <- stringr::str_extract_all(names(.pf$.dat), ".*rv$|.*ind$|.*type$") %>% purrr::compact() %>% unlist %>% .[{. != nm}]
      if (is.character(.pf$.dat[[nm]]) | is.factor(.pf$.dat[[nm]]) ) {metric <- "Accuracy"} else {metric <- "MAE"}
      
      frm <- formula(paste(paste0("`",nm,"`"), "~ ."))
      # ----------------------- Fri Jul 26 14:19:16 2019 ------------------------#
      # PreProcessing Data
      # 1. remove extraneous columns
      out <- .pf$.dat[ ,!names(.pf$.dat) %in% c(rvs,td_nm)]
      # 2. Make model frame
      out <- model.frame(frm, out)
      # 3. remove any columns that may be matrices
      out <- dplyr::mutate_if(out, .predicate = is.matrix, ~ as.vector(.))
      #4. Create dummyVars from factors
      out <- caret::dummyVars(frm, data = out) %>% predict(., newdata = out)
      # 5. remove highly correlated variables
      out.cor <- cor(out) %>% as.data.frame %>% tibble::rownames_to_column() %>% mutate_if(.predicate = is.numeric, ~ ifelse(is.na(.),0,.)) %>% tibble::column_to_rownames() %>% as.matrix
       out <- out[ , - caret::findCorrelation(out.cor, cutoff = .95)]
       rm(out.cor)
      #6.  Remove Variables with near Zero variance
      out <- out[ , - caret::nearZeroVar(out)]
      
      out <- cbind(.pf$.dat[, nm, drop = T], out)
      colnames(out)[1] <- nm
      # tunelist <- sapply(methodList, simplify = F, USE.NAMES = T, tuneLength = 5, FUN = function(m, tuneLength = NULL, tuneGrids = NULL){
      #   if (is.numeric(tuneLength)){
      #     out <- caretEnsemble::caretModelSpec(method = m, tuneLength = tuneLength)
      #   } else {
      #     out <- caretEnsemble::caretModelSpec(method = m, tuneGrid = tuneGrids[[m]])
      #   }
      #   return(out)
      # })
      gc()
      message(paste0(.pf$tick, ": ", nm, " Time:", lubridate::now()))
      cl <- parallel:::makePSOCKcluster(6, outfile = "~/R/Quant/dopar.log")
      doParallel::registerDoParallel(cl)
      train.time <- system.time({
        tr.index <- caret::createDataPartition(.pf$.dat[, nm, drop = T], times = 5, p = .75)
        data.train <- caret::trainControl(method = "repeatedcv",
                                          search = "grid", 
                                          returnData = F,
                                          repeats = 0,
                                          index = tr.index,
                                          allowParallel = T,
                                          savePredictions = 'none',
                                          verboseIter = F)
        caretlistmod <- caretEnsemble::caretList(form = frm, #enter model name here
                                                 data = out,
                                                 trControl = data.train,
                                                 metric = metric,
                                                 tuneList = .pf$.mS,
                                                 continue_on_fail = T)
      })
      parallel::stopCluster(cl)
      
      message(paste0(.pf$tick,": ", nm," / ", names(train.time)[3],": ",lubridate::as.duration(train.time[3])," / Time: ",lubridate::now()))
      return(caretlistmod)
    })
    
    names(out) <- train_rvs
    if (!is.null(ob)) {
      out <- append(ob, out)
      assign(paste0(tick, "_cl"), out)
    } else assign(paste0(tick,"_cl"), out)
    save(list = paste0(tick,"_cl"), file = paste0("~/R/Quant/MdlBkp/",tick,"_cl.Rdata"), compress = "xz")
    message(paste("Saved",tick, "Object"))
    message(paste0(tick, " cleanup:", paste(ls(all.names = T), collapse = ",")))
    if (!is.null(ob)) rm(list = c(paste0(tick,"_cl")))
    return(NULL)
})
})
HDA::unloadPkgs(mod.packages)
message(paste0("Model Training Complete, Elapsed: ",lubridate::as.duration(total.time[3]), " Now: ",lubridate::now()))
  
  
  
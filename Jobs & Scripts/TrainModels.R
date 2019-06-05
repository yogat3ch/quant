HDA::startPkgs(c("magrittr","caret","dplyr"))
load(file = "dat_dt.Rdata")
if (!exists("best_tsl", mode = "list")) best_tsl <- NULL
if (!exists("train_rvs", mode = "character")) train_rvs <- NULL
total.time <- system.time({
dat_dt <- lapply(dat, methodList = methodList, train_rvs = train_rvs, best_tsl = best_tsl, function(l, methodList, train_rvs, best_tsl){
  tick <- attr(l, "Sym") %>% unlist # Get the symbol of the data and store it
  if (zoo::is.zoo(l)) l <-  zoo::fortify.zoo(l) # If still xts, convert to df
  if (lubridate::is.Date(l$Index)) l$Dec_date <- l$Index %>% lubridate::decimal_date() %>% {. - as.numeric(stringr::str_extract(.,"\\d{4}"))} # Create a decimal date for prediction
  if (is.null(train_rvs) & is.null(best_tsl)) {train_rvs <- colnames(l)[grep("rv$", colnames(l))]} # If there are no specified RVs to train or a best TSL object, then train for all RVs
  if (!is.null(best_tsl)) train_rvs <- c(best_tsl[[tick]][["rowname"]], train_rvs) # If best_tsl is included then add that RV to the ones to be trained
  try({load(file = paste0("MdlBkp/",tick, "_cl.Rdata"))}) #Try to load previous models
  ob <- get0(paste0(tick, "_cl"), mode = "list") # If it loads, save it as ob
  #if (stringr::str_detect(train_rvs, "Gain_rv|Mix_rv|Period_rv") %>% any) train_rvs <- c(unique(l[[train_rvs[stringr::str_which(train_rvs, "Gain_rv|Mix_rv|Period_rv")] %>% stringr::str_replace("rv$","type")]]), train_rvs)
  if(!is.null(ob)) train_rvs <- train_rvs[!train_rvs %in% names(ob)] # Filter the rvs to train by what already exists in the object
  if (length(train_rvs) < 1) return(ob) #If nothing to train just return the objet
  if (nrow(l) < 200) {
    message(paste0(tick,": Not Enough Data for Model"))
    return("Not Enough Data for Model")
    } # If not enough data say so and return
  out <- purrr::map(train_rvs, l = l, tick = tick, function(nm, l, tick){
      iex <- colnames(l)[grep("Index|Time", colnames(l))]
      rvs <- colnames(l)[grep("rv$|ind$|type$", colnames(l))] %>% .[{. != nm}]
      if (is.character(l[[nm]]) | is.factor(l[[nm]]) ) {metric <- "Accuracy"} else {metric <- "RMSE"}
      frm <- formula(paste(paste0("`",nm,"`"), "~ ."))
      out <- model.frame(frm, l %>% select(- c(iex,rvs)))
      
      tunelist <- sapply(methodList, simplify = F, USE.NAMES = T, tuneLength = 5, FUN = function(m, tuneLength = NULL, tuneGrids = NULL){
        if (is.numeric(tuneLength)){
          out <- caretEnsemble::caretModelSpec(method = m, tuneLength = tuneLength)
        } else {
          out <- caretEnsemble::caretModelSpec(method = m, tuneGrid = tuneGrids[[m]])
        }
        return(out)
      })
      mod.packages <- c("caret","doParallel","iterators","parallel","foreach","caretEnsemble","nnet", "xgboost") # [package dependencies]
      HDA::startPkgs(mod.packages)
      message(paste0(tick, ": ", nm, " Time:", lubridate::now()))
      cl <- parallel::makeCluster(6)
      doParallel::registerDoParallel(cl)
      train.time <- system.time({
        data.train <- caret::trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 0,
                                          search = "grid", 
                                          returnData = F, 
                                          allowParallel = T,
                                          savePredictions = F,
                                          verboseIter = F)
        
        caretlistmod <- caretEnsemble::caretList(form = frm, #enter model name here
                                                 data = out,
                                                 trControl = data.train,
                                                 metric = metric,
                                                 methodList = methodList, 
                                                 tuneList = tunelist,
                                                 continue_on_fail = T)
      })
      parallel::stopCluster(cl)
      HDA::unloadPkgs(mod.packages)
      gc()
      message(paste0(tick,": ", nm," / ", names(train.time)[3],": ",lubridate::as.duration(train.time[3])," / Time: ",lubridate::now()))
      return(caretlistmod)
    })
    
    names(out) <- train_rvs
    if (!is.null(ob)) {
      out <- append(ob, out)
      assign(paste0(tick, "_cl"), out)
    } else assign(paste0(tick,"_cl"), out)
    save(list = paste0(tick,"_cl"), file = paste0("MdlBkp/",tick,"_cl.Rdata"), compress = "xz")
    message(paste("Saved",tick, "Object"))
    if (!is.null(ob)) rm(list = c(paste0(tick,"_cl")))
    return(out)
})
})

message(paste0("Model Training Complete, Elapsed: ",total.time[3], "Now: ",lubridate::now()))
  save(dat_dt, file = paste0("dat_dt",dat[[1]] %>% time %>% min,"_",dat[[1]] %>% time %>% max,".Rdata"), compress = "xz")
  
  
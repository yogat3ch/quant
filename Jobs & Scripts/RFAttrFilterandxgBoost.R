HDA::startPkgs(c("magrittr","caret"))
load(file = "Positions_ts2014-05-08_2019-05-07.Rdata")
Positions_dt <- lapply(Positions_ts, function(l){
  if(nrow(l) < 10){return(NULL)}
  out <- purrr::map(colnames(l)[grep("rv$", colnames(l))], dat = zoo::fortify.zoo(l), function(nm, dat){
    iex <- colnames(dat)[grep("high|low|volume|open|close|change|changePercent|chg|Index", colnames(dat))]
    nms <- colnames(dat)[grep("rv$", colnames(dat))] %>% .[{. != nm}]
    frm <- formula(paste(nm, "~ .", "-",paste0(nms, collapse = "-"),"-",paste0(iex, collapse = "-")))
    fitControl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 1, search = "grid")
    model <- caret::train(frm, data = dat, method = "xgbTree", trControl = fitControl)
    return(model)
  })
  return(out)
})
save(Positions_dt, file = paste0("Positions_dt",Positions_ts[[1]] %>% time %>% min,"_",Positions_ts[[1]] %>% time %>% max,".Rdata"))


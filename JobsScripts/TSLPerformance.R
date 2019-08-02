tsl_performance <- purrr::map_depth(Positions_ret, .depth = 3, .f = function(.x) {
  out <- c(Returns = attr(.x, "Returns"),Cum.Returns = attr(.x, "Cum.Returns"))
  return(out)
}) %>% purrr::map_depth(.depth = 2, .f = function(.x){
  d <- do.call("rbind",.x)
  ind <- which.max(d[,"Returns", drop = T])
  pct <- rownames(d)[ind]
  returns <- d[ind,"Returns", drop = T]
  if (stringr::str_detect(colnames(d),"Cum.Returns") %>% any){
  ind <- which.max(d[,"Cum.Returns", drop = T])
  pct <- rownames(d)[ind]
  cum.returns <- d[ind,"Cum.Returns", drop = T]} else cum.returns <- 0
  out <- c(pct = pct, Returns = returns, Cum.Returns = cum.returns)
}) %>% purrr::map(function(.x){
  d <- do.call("rbind",.x) 
  rd <- rownames(d) 
  d %<>% as.data.frame() 
  d %<>%  mutate_all(funs(as.numeric(as.character(.))))
  row.names(d) <- rd
  # arrange(d)
})

HDA::startPkgs(c("doParallel", "magrittr"))
load(file = "dat.Rdata")
cl <- makePSOCKcluster(6)
doParallel::registerDoParallel(cl, cores = 6)
dat <- foreach(l = dat, .inorder = F, .multicombine = T, .packages = c("magrittr", "dplyr"), .verbose = T) %dopar% {
  att <- attr(l, "Sym") %>% unlist
  if (xts::is.xts(l)){l %<>% fortify.zoo(l) %>% mutate_at(vars("Index"), funs(lubridate::ymd)) %>% tibbletime::as_tbl_time(index = "Index")}
  # Has to be tibbletime for apply to work since rollapply did not allow returning of DFs
  test <- apply(l[, c(1,stringr::str_which(names(l), "rv|ind$"))], 1, index_chr = tibbletime::get_index_char(l) ,function(r, index_chr){
    nms <- names(r)[stringr::str_which(names(r), "rv$")]
    Time <- r[[index_chr]]
    r <- r[-1]
    sp <- r %>% as.vector %>% split(f = factor(rep(1:2,length.out = length(r))))
    d <- do.call("cbind", sp)
    colnames(d) <- c("RV","Ind")
    rownames(d) <- nms
    d %<>% as.data.frame %>% tibble::rownames_to_column("Type") %>% mutate(Rank_RV = percent_rank(RV), Rank_Ind = 1 - percent_rank(Ind)) %>% mutate(Rank_Opt = Rank_RV * Rank_Ind)
    
    Gain <- d %>% arrange(desc(Rank_RV)) %>% .[1,] %>% select(-contains("Rank"))
    names(Gain) <- paste0("Gain",c("_type","_rv","_rv_ind"))
    Period <- d %>% arrange(desc(Rank_Ind)) %>% .[1,] %>% select(-contains("Rank"))
    names(Period) <- paste0("Period",c("_type","_rv","_rv_ind"))
    Mix <- d %>% arrange(desc(Rank_Opt)) %>% .[1,] %>% select(- contains("Rank"))
    names(Mix) <- paste0("Mix",c("_type","_rv","_rv_ind"))
    cbind.data.frame(Time = Time, Gain, Period, Mix)
  }) %>% do.call("rbind.data.frame", .)
  if (stringr::str_which(names(l), "^Gain|^Mix|^Period") %>% length > 0) l <- l[, - stringr::str_which(names(l), "type$")]
  if (tibble::is_tibble(l)) l <- l[,!names(l) %in% tibbletime::get_index_char(l)]
  out <- test %>% as.data.frame %>% mutate_at(vars("Time"), funs(lubridate::ymd(as.character(.)))) %>% cbind.data.frame(l) %>% tibbletime::as_tbl_time(index = Time)
  attr(out,"Sym") <- att
  out
}
names(dat) <- purrr::map(dat, function(l){
  attr(l,"Sym")
}) %>% unlist
dat <- purrr::map(dat, function(.x){
  att <- attr(.x, "Sym")
  filter_type <- function(Type,RV) {
    out <- apply(cbind(Type,RV), 1, function(r){
      if (r[[2]] > 0.03) out <- r[[1]] else out <- "None"
      return(out)
    })
    return(out)
  }
  .x <- .x %>% dplyr::mutate_if(.predicate = funs(is.factor), .funs = funs(as.numeric(as.character(.)))) %>% mutate_at(vars(Gain_type),funs(filter_type(Gain_type,Gain_rv))) %>% mutate_at(vars(Mix_type),funs(filter_type(Mix_type,Mix_rv))) %>% mutate_at(vars(Period_type),funs(filter_type(Period_type,Period_rv)))
  attr(.x, "Sym") <- att
  return(.x)
})
parallel::stopCluster(cl)
nms <- c("Gain_type", "Gain_rv", "Gain_rv_ind", "Period_type", "Period_rv", "Period_rv_ind", "Mix_type", "Mix_rv", "Mix_rv_ind")
purrr::map(dat, nms = nms, function(.x, nms){
  att <- attr(l, "Sym")
  num <- which(!c(nms) %in% names(.x))
  if(length(num) > 0) {message(paste0(att, ": NAME CHECK | Missing names are:", c(nms)[num]))}
})
save(dat,file = "dat.Rdata")

gpn <- function(Positions_v, params){
  .bgJob <- any(stringr::str_detect(deparse(sys.calls()), "sourceEnv"))
  message(paste0("Run get Positions_new as Background Task: ",.bgJob))
}
gpn()
purrr::map(1:10, function(.x){ 
  if (.x == 5) {
    e <<- new.env()
    e$v <<- .x
  }
  if (HDA::go("e", env = globalenv())) {
    message(paste0(ls(envir = e), collapse = ", "))
  }
})
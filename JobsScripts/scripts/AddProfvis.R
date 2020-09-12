##
.x <- "JobsScripts/AddIVstoNewData.R"
addPV <- function(.x) {
  lines <- readr::read_lines(.x)
  lines <- c(lines, "")
  .empty <- which(purrr::map_dbl(lines, nchar) == 0)
  .last <- tail(.empty, 1)
  names(.empty) <- 1:length(.empty)
  .al <- 0
  .s <- split(.empty,rep(1:round(length(.empty) / 2 + 1), each = 2, length.out = length(.empty)))
  purrr::iwalk(.s, ~{
    .l2 <- paste0("}) #pv")
    if (.x[1] >= .empty[[2]][1]) {  
      lines <<- R.utils::insert(lines, .x[1] + .al, .l2)
      .al <<- .al + 1
    }
    if (.x[1] == .last) return(NULL)
    .l1 <- paste0("profvis::profvis(prof_output = ",glue::glue("'profvis/pv{.x[1] + .al + 1}-{.x[2] + .al + 1}.txt'"), ", {")
    lines <<- R.utils::insert(lines, .x[1] + .al + 1, .l1)
    .al <<- .al + 1
    lines <<- R.utils::insert(lines, .x[2] + .al, .l2)
    .al <<- .al + 1
    if (.x[1] <= .last - 1) {
      .l1 <- paste0("profvis::profvis(prof_output = ",glue::glue("'profvis/pv{.x[2] + .al + 1}-{.s[[as.numeric(.y) + 1]][1] + .al + 1}.txt'"), ", {")
      lines <<- R.utils::insert(lines, .x[2] + .al + 1, .l1)
      .al <<- .al + 1
    }
  })
  return(lines)
}
.nf <- paste0("profvis/",stringr::str_remove(basename(.x), '\\.R'),"_profile.R")
readr::write_lines(lines, .nf)

remPV <- function(.x) {
  lines <- readr::read_lines(.x)
  return(lines[stringr::str_which(lines, "(?:^profvis)|(?:\\#pv$)", negate = TRUE)])
}
.lines <- remPV(.nf)
readr::write_lines(.lines, .x)




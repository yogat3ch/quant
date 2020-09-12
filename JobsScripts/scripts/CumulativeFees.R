.b <- 1e6
.p <- 0
sum(purrr::map2_dbl(rep(.005, 10), rep(.01, 10), .f = ~{
  .b <<- .b * .y + .b
  .b <<- .b - .b * .x
  .b * .x
  })) 
sum(purrr::map2_dbl(rep(.001, 10), rep(.01, 10), .f = ~{
  .b <<- .b * .y + .b
  .b <<- .b - .b * .x
  .b * .x
}))

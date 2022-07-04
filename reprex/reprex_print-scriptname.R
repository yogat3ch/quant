library(magrittr)
message(paste0("Begin Test_call.R sourced from",basename(stringr::str_extract(commandArgs(trailingOnly = FALSE), "(?<=script \\= \\')[A-Za-z0-9\\/\\_\\:\\.]+") %>% .[!is.na(.)]), " at ", lubridate::now()," From location: ",getwd()))
utils::capture.output(commandArgs(trailingOnly = FALSE), file = "test_call.log")
test <- NULL
source("Test.R", local = T)

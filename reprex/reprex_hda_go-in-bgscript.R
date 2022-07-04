# Test HDA::go running as job
debug <- T
bs <- data.frame(b = stats::rnorm(200))
v <- vector()
i <- 0
while (i <= nrow(bs)) {
  v[i] <- i
  if (i == nrow(bs)) {
    message("Breaking")
    .open <- T
    break
  }
  message(paste0("i: ", i))
  i <- i + 1
}
print(HDA::go(".open"))
if (!HDA::go(".open")) .open <- F
message(.open)

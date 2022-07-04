get_stream <- function(live = F){
  url <- get_url(live)
  url <- httr::parse_url(url)
  url$scheme <- "ws"
  url$path <- "/v1/stream"
  url <- httr::build_url(url)
  websocket::WebSocket$new(url, protocols = jsonlite::toJSON(list(action = "listen", data = list(streams = "trade_updates"))), headers = Sys.getenv(c("APCA-API-KEY-ID","APCA-API-SECRET-KEY"), names = T) %>% as.list, autoConnect = F, accessLogChannels = "all", errorLogChannels = "all")
  ws$onOpen(function(event) {
    cat("Connection opened\n")
  })
  ws$onMessage(function(event) {
    cat("message received: ", event$data, "\n")
  })
  ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
  })
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  ws$connect()
}
get_stream()


# API ---------------------------------------------------------------------
iex_user_agent <- function() {
  httr::user_agent("Rpackage")
}

iex_base <- function(version) {
  "https://api.iextrading.com/"
}

iex_endpoint <- function(version = "1.0",params = c("stock")) {
  httr::modify_url(iex_base(), path = paste(version,paste(params,collapse = "/"), sep = "/"))
}
iex_stock <- function(tSymbols, type = "quote", query_filter = NULL){
  req <- sapply(tSymbols, function(s){
    url  <- paste(iex_endpoint(), s, type,sep = "/")
  })
  req <- sapply(req,FUN=function(r){
    httr::modify_url(r,query = paste("filter=",paste(query_filter,collapse=","),sep=""))
  })
  return(req)
}

iex_stock_req <- function(tSymbols, type = "quote", query_filter = NULL){
  reqs <- iex_stock(tSymbols, type = type, query_filter = query_filter)
  resps <- sapply(reqs,FUN=function(req){
    resp <- httr::GET(req)
    if (httr::http_type(resp) != "application/json") {
      paste("API did not return JSON. Error:", httr::content(resp))
      parsed <- NA
    }
    parsed <- tryCatch({parsed <- jsonlite::fromJSON(httr::content(resp, "text"),simplifyVector = T) %>% unlist}, 
                       warning = function(w) {print(paste("The var:",req," generated the following warning: ",w,sep=""))},
                       error = function(e) {print(paste("The var:",req," generated the following error: ",e,sep=""))
                         out <- NA
                         return(out)},
                       finally = {})
        
    
    Sys.sleep(0.2)
    return(parsed)})
  if(type == "stats"|type == "quote"){
  stats_df <- do.call(rbind.data.frame, resps)
  names(stats_df) <- query_filter
  row.names(stats_df) <- tSymbols}else{
    stats_df <- resps
  }
  return(stats_df)
}
# ----------------------- Wed Aug 08 17:15:25 2018 ------------------------#
# Batch root function for creating the query for the batch_req (batch request function)

iex_batch <- function(tSymbols, range, filter = NULL) {
  q <- paste("symbols=",paste(tSymbols,collapse=","),paste("&types=chart",sep=""),paste("&range=",range,sep=""),sep="")
  if(!is.null(filter)){
    q <- paste(q,paste("&filter=",paste(filter,collapse=","),sep=""),sep = "")
  }
  
  (url <- httr::modify_url(iex_endpoint(params = c("stock","market","batch")), query = paste(q,sep="/")))
}
# ----------------------- Wed Aug 08 17:15:48 2018 ------------------------#
# Batch Request function for requesting timeseries data for a ticker symbol

iex_batch_req <- function(tSymbols, range, filter = NULL){
  resp <- httr::GET(iex_batch(tSymbols, range, filter = filter))
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),simplifyDataFrame = T,flatten = T)
  nms <- names(parsed)
  parsed %<>% purrr::flatten()
  names(parsed) <- nms
  parsed <- lapply(parsed,function(l){l$date %<>% lubridate::ymd()
  ts <- xts(l[,names(l)!= "date"],order.by = l[,names(l)== "date"])})
  return(parsed)
  on.exit(Sys.sleep(0.2))
}

iex_api <- function(path, query = NULL, version = "1.0") {
  # End points throttled: 5 requests per second. Enforce this
  on.exit(Sys.sleep(0.2))
  resp <- httr::GET(iex_endpoint(path, version), query = query, iex_user_agent())
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
    simplifyVector = FALSE)
  if (httr::http_error(resp)) {
    stop(sprintf("IEX API request failed [%s]\n%s",
      httr::status_code(resp), parsed$error), call. = FALSE)
  }
  structure(list(content = parsed, path = path, response = resp),
    class = 'iex_api')
}

#' @export
print.iex_api <- function(x, ...) {
  cat("<IEX ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

# TOPS --------------------------------------------------------------------

#' Top of book data (TOPS)
#'
#' Provides IEX's aggregated bid and offer position in near real time for
#' all securities on IEX's displayed limit order book.
#'
#' @param symbols a vector of tickers (case insensitive). Special characters
#'   will be escaped. A list of eligible symbols is
#'   [published daily](https://iextrading.com/trading/eligible-symbols/) by the
#'   IEX. When set to `NULL` (default) returns values for all symbols.
#' @param fields a vector of fields names to return (case sensitive). When
#'   set to `NULL` (default) returns values for all fields.
#' @param version the API version number (default: `"1.0"`) which is used to
#'   define the API URL.
#' @return an S3 object of class `iex_api` which has three accessible fields:
#'   `path`, `response` and `content` containing the API path, the unparsed API
#'   response and the parsed content from the API's response (the latter usually
#'   being a list). Note that this package causes R to pause 0.2 seconds after
#'   executing an API call to avoid the user being throttled by the API (which
#'   enforces a 5 request per second limit)
#' @examples
#' \dontrun{
#' tops(
#'   symbols = c("AAPL", "FB"),
#'   fields  = c("symbol", "bidSize", "bidPrice", "askSize", "askPrice")
#' )
#' }
#' @references [IEX API TOPS documentation](https://iextrading.com/developer/#tops-tops)
#' @export
tops <- function(symbols = NULL, fields = NULL, version = "1.0") {
  query <- list(
    symbols = paste0(symbols, collapse = ","),
    filter  = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("tops", query, version)
}

#' Last trade data
#'
#' Provides IEX near real time last sale price, size and time. Last is ideal for
#' developers that need a lightweight stock quote.
#'
#' @inherit tops return params references
#' @examples
#' \dontrun{
#' last(
#'   symbols = c("AAPL", "FB"),
#'   fields  = c("symbol", "price", "size")
#' )
#' }
#' @export
last <- function(symbols = NULL, fields = NULL, version = "1.0") {
  query <- list(
    symbols = paste0(symbols, collapse = ","),
    filter  = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("tops/last", query, version)
}


# Market ------------------------------------------------------------------

#' Market volume data
#'
#' Provides exchange trade volume data in near real time.
#'
#' @inherit tops params return
#' @examples
#' \dontrun{
#' market()
#' }
#' @references [IEX market API documentation](https://iextrading.com/developer/#market-market)
#' @export
market <- function(fields = NULL, version = "1.0") {
  query <- list(
    filter = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api("market", query, version)
}


# Stats -------------------------------------------------------------------

#' Trading stats
#'
#' A set of functions that return trading statistics.
#'
#' @inherit tops params return
#' @param date should be a string of the format `"YYYYMM"` or `"YYYYMMDD"` which
#'   is a valid option for `daily_stats()` or `NULL` (default) which returns the
#'   prior trading date's data for `daily_stats()`  and the prior month's
#'   trading data for `monthly_stats()`
#' @param last can be used in place of `date` to retrieve the last `n` number of
#'   trading days' data. If this is supplied, any value supplied to `date` is
#'   ignored.
#' @references [IEX stats API documentation](https://iextrading.com/developer/#stats)
#' @name stats
NULL

stats <- function(type, date = NULL, last = NULL, fields = NULL, version = "1.0") {
  path <- paste("stats", type, sep = "/")
  query <- list(
    date = date,
    last = last,
    filter = if (!is.null(fields)) paste0(fields, collapse = ",")
  )
  iex_api(path, query, version)
}

#' @rdname stats
#' @export
intraday_stats <- function(fields = NULL, version = "1.0") {
  stats("intraday", fields = fields, version = version)
}

#' @rdname stats
#' @export
recent_stats <- function(fields = NULL, version = "1.0") {
  stats("recent", fields = fields, version = version)
}

#' @rdname stats
#' @export
records_stats <- function(fields = NULL, version = "1.0") {
  stats("records", fields = fields, version = version)
}

#' @rdname stats
#' @export
monthly_stats <- function(date = NULL, fields = NULL, version = "1.0") {
  stats("historical", date = date, fields = fields, version = version)
}

#' @rdname stats
#' @export
daily_stats <- function(date = NULL, last = NULL, fields = NULL, version = "1.0") {
  if (!is.null(last)) date <- NULL
  stats("historical/daily", date, last, fields, version)
}
source(qf::wd("JobsScripts/parameters.R"))
addIVstoNewData <- function(dat) {
  qf::iMessage("Script: Add IVs to new data")
  .t <- qf::time_index(dat)
  wind <- qf::time_windows(dat, params$wind)
  .nms <- names(dat) %>% .[!. %in% .t]
  sma.wind <- c(wind, sma = 200)
  .sym <- attr(dat, "Sym")
  dat_final <- list()
  # Indicators  ----
  # Tue Sep 08 08:24:06 2020
  qf::iMessage(paste0(.sym, ": Moving Averages"))
  .sma <- qf::add_indicator(dat, sma.wind, TTR::SMA, bc = F, .na = "o")
  .ema <- qf::add_indicator(dat, sma.wind, TTR::EMA, bc = F, .na = "o", wilder = T)
  # Use only non-NA data
  dat_final$ma <- dplyr::bind_cols(utils::tail(dat, min(purrr::map_dbl(list(.ema,.sma), nrow))), .sma, .ema)
  qf::iMessage(paste0(.sym, ": Cross Averages"))
  dat_final$ma <- qf::cross_ma(dat_final$ma, sma.wind, params$IV$cross)
  
  qf::iMessage(paste0(.sym, ": Ultimate Oscillater"))
  .uo <- qf::add_indicator(dat, list(day.mp = wind[1] * c(1, 2, 4), week.mc = wind[2] * c(1, 2, 4)), TTR::ultimateOscillator, .na = "o") %>% {stats::setNames(., stringr::str_replace(names(.), "ultimateOscillator", "UO"))}
  dat_final$uo <- qf::uoc(.uo, wind = params$wind, .args = params$IV$uo) 
  
  # Now that all data that other indicators rely on is in the dataset, abbreviate it such that only the minimum number of calculations necessary to make a row for prediction will be made per window
  dat_short <- purrr::map(sma.wind, ~{
    if (.x <= 7) { 
      .m <- .x * 2 + 10
    } else {
      .m <- round(.x * 2.5)
    }
    utils::tail(dat_final$ma, .m)
  })
  
  qf::iMessage(paste0(.sym, ": ADX & ADXi"))
  dat_final$adx <- purrr::imap(dat_short[names(wind)], ~{
    adx <- qf::add_indicator(.x, wind[.y], TTR::ADX, bind_orig = TRUE, maType = TTR::SMA, .na = "o")
    adx <- qf::adxc(adx, wind[.y], .args = params$IV$adx, .na = "o")
    adx[!grepl("^di", names(adx), ignore.case = TRUE)]
  })
  
  qf::iMessage(paste0(.sym, ": WPR"))
  dat_final$wpr <- purrr::imap(dat_short[names(wind)], ~{
    wpr <- qf::add_indicator(.x, wind[.y], TTR::WPR, .na = "o")
  })
  
  qf::iMessage(paste0(.sym, ": RSI"))
  dat_final$rsi <- purrr::imap(dat_short[names(wind)], ~{
    rsi <- qf::add_indicator(.x, wind[.y], TTR::RSI, bind_orig = TRUE, .na = "o")
    rsi <- qf::rsic(rsi, sma.wind, .args = params$IV$rsi)
  })
  
  qf::iMessage(paste0(.sym,": ROC"))
  dat_final$roc <- purrr::imap(dat_short[names(wind)], ~{
    roc <- qf::add_indicator(.x, wind[.y], .f = TTR::ROC, .na = "o")
  })
  
  qf::iMessage(paste0(.sym,": SAR"))
  dat_final$sar <- purrr::imap(dat_short[names(wind)], ~{
    sar <- qf::add_indicator(.x, wind[.y], .f = TTR::SAR, accel = params$IV$sar$accel, bind_orig = TRUE, .na = "o")
    sar <- qf::sarc(sar, wind[.y], .args = params$IV$sar)
  })
  
  qf::iMessage(glue::glue("{.sym}: MACD"))
  dat_final$macd <- purrr::imap(dat_short[names(wind)], ~{
    .d <- .x
    
    macd <- purrr::imap(wind[.y], ~{
      .out <- as.data.frame(
        qf::add_indicator(
          .d,
          .x,
          TTR::MACD,
          bc = F,
          nFast = round(.x / 2, 0),
          nSlow = .x,
          nSig = round(.x / 2 * .75, 0),
          maType = TTR::EMA,
          .na = "o"
        )
      ) %>% {magrittr::set_colnames(., paste0(names(.),"_",.y))}
      .wi <- which(names(sma.wind) %in% .y) + 2
      .wi <- ifelse(.wi > length(sma.wind), length(sma.wind), .wi)
      .sma_nm <- grep(paste0("SMA_", names(sma.wind[.wi]),"$"), names(.d), ignore.case = TRUE, value = TRUE)
      .macd <- dplyr::bind_cols(.out, utils::tail(.d[,c(.sma_nm, qf::ohlcvt(.d)[4])], nrow(.out)))
      macd <- qf::macc(.macd, .x, .args = params$IV$macd)
      return(macd)
    })
  })
  
  qf::iMessage(paste0(.sym, ": roLM"))
  dat_final$rolm <- purrr::imap(dat_short[names(wind)], ~{
    .out <- qf::add_indicator(.x, .ns = wind[.y], .f = qf::roLM, x = .x[[.t]], .na = "o")
  })
  
  qf::iMessage("Add Date Based Variables")
  dat_final$timetk <- purrr::map(dat_short[1], qf::addTK)
  #TODO Why does uo return twice
  #TODO Remove the additional SMA columns from the cross_ma output
  dat_final %>% purrr::map(~{
    .x <- .x[!names(.x) %in% c(qf::ohlcvt(.x), "vw", "n")]
    if (inherits(.x, "list")) {
      .out <- dplyr::bind_cols(utils::tail(.x, 1))
    } else {
      .out <- utils::tail(.x, 1)
    }
    browser()
    .out
  })
  dplyr::bind_cols(dat_final)
  
}




# Load New Data if calling from a jobRunScript - allows for these files to be sourced by the automation and use the data from the automation calling environment
# if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
#   message("Loaded from BG Job, loading dependencies...")
#   load("~/R/Quant/data/input_AddIVstoNewData.Rdata")
#   # NOTE dat will be single DF here
# }
#library(qf)

# Add wind to this environment


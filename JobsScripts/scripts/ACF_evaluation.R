tidy_acf <- function(data, lags = 0:20) {
  value <- grep("rv$", names(data), value = T)
  value_expr <- rlang::sym(value)
  acf_values <- data %>%
    dplyr::pull(!!value) %>%
    stats::acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble::tibble(acf = acf_values) %>%
    tibble::rowid_to_column(var = "lag") %>%
    dplyr::mutate(lag = lag - 1) %>%
    dplyr::filter(lag %in% lags)
  
  return(ret)
}
purrr::imap(dat_full, ~{
  tidy_acf(.x, 0:700) %>% 
    ggplot2::ggplot(ggplot2::aes(lag, acf)) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "lightblue") +
    #geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
    ggplot2::labs(title = glue::glue("ACF: {.y}"))
  
})





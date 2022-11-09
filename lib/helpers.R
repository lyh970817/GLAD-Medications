id_select <- function(dat, ...) {
  dat %>%
    select(ID, ...)
}

na_convert <- function(data) {
  mutate_if(
    data, is.numeric,
    # -66.6 in age?
    ~ ifelse(. %in% c(-99, -88, -77, -888, -999, -777, -666, -66.6), NA, .)
  ) %>%
    mutate_if(
      ., is.factor,
      ~ fct_recode(.,
        NULL = "Prefer not to answer",
        NULL = "Seen but not answered"
      )
    )
}

na_row_remove <- function(data) {
  all_na_rows <- apply(
    select(data, -c(ID, sample, startDate, endDate)), 1,
    function(x) {
      all(is.na(x))
    }
  )
  data[!all_na_rows, ]
}

format_model <- function(model) {
  format_digit <- function(x) {
    # Two significant figures with `nsmall`
    format(round(x, 2), nsmall = 2)
  }

  combine_cicoef <- function(tab) {
    # Combine regression coefficient and confidence intervals

    tab["Coeff_CI"] <- paste0(
      format_digit(tab[["Coefficient"]]),
      "[",
      paste(
        format_digit(tab[["CI_low"]]),
        format_digit(tab[["CI_high"]]),
        sep = ", "
      ),
      "]"
    )

    tab
  }

  tab <- model %>%
    as_tibble() %>%
    filter(!grepl("Intercept", Parameter)) %>%
    # Intercept for clm
    filter(!grepl("\\d\\|\\d", Parameter)) %>%
    combine_cicoef() %>%
    mutate(Parameter = recode(Parameter, !!!labels))

  # For hurdel models with column `step`
  if ("step" %in% colnames(tab)) {
    step_filter <- function(tab, stp) {
      filter(tab, step == stp) %>%
        select(-step)
    }

    tab <- left_join(
      step_filter(tab, stp = 1),
      step_filter(tab, stp = 2),
      by = "Parameter",
      suffix = c("_logistic", "_gamma")
    )
  }

  attr(tab, "dep") <- attr(model, "dep")
  attr(tab, "n") <- attr(model, "n")

  tab
}

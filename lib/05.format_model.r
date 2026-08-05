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

  map_parameter_label <- function(parameter) {
    if (is.na(parameter)) {
      return(parameter)
    }

    exact <- unname(labels[parameter])
    if (!is.na(exact)) {
      return(exact)
    }

    normalized <- parameter
    normalized <- sub("Yes$", "", normalized)
    normalized <- sub("Female$", "", normalized)
    normalized <- sub("\\.(L|Q|C|S)$", "", normalized)
    normalized <- sub("\\^[0-9]+$", "", normalized)

    exact_norm <- unname(labels[normalized])
    if (!is.na(exact_norm)) {
      suffix <- if (startsWith(parameter, normalized)) {
        substring(parameter, nchar(normalized) + 1)
      } else {
        ""
      }

      if (suffix %in% c("", "Yes", "Female")) {
        return(exact_norm)
      }

      suffix_clean <- sub("^\\.", "", suffix)
      if (grepl("^\\^[0-9]+$", suffix)) {
        suffix_clean <- paste0("order ", sub("^\\^", "", suffix))
      }
      return(paste0(exact_norm, " (", suffix_clean, ")"))
    }

    lbl_names <- names(labels)
    hits <- lbl_names[startsWith(parameter, lbl_names)]
    if (length(hits) == 0) {
      hits <- lbl_names[startsWith(normalized, lbl_names)]
    }

    if (length(hits) > 0) {
      base_name <- hits[which.max(nchar(hits))]
      base_label <- unname(labels[base_name])

      suffix <- if (startsWith(parameter, base_name)) {
        substring(parameter, nchar(base_name) + 1)
      } else if (startsWith(normalized, base_name)) {
        substring(normalized, nchar(base_name) + 1)
      } else {
        ""
      }

      if (suffix %in% c("", "Yes", "Female")) {
        return(base_label)
      }

      suffix_clean <- sub("^\\.", "", suffix)
      if (grepl("^\\^[0-9]+$", suffix)) {
        suffix_clean <- paste0("order ", sub("^\\^", "", suffix))
      }
      return(paste0(base_label, " (", suffix_clean, ")"))
    }

    parameter
  }

  tab <- model %>%
    as_tibble() %>%
    filter(!grepl("Intercept", Parameter)) %>%
    # Intercept for clm
    filter(!grepl("\\d\\|\\d", Parameter)) %>%
    combine_cicoef() %>%
    mutate(Parameter = vapply(Parameter, map_parameter_label, character(1)))

  # For hurdle models with column `step`
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

  # Carry the fit's provenance through formatting: the outcome, the sample size
  # at both levels, the convergence warnings and the participant random-intercept
  # variance. Losing these here is what made a non-converged fit indistinguishable
  # from a good one downstream.
  for (a in c("dep", "n", "n_id", "fit_warnings", "converged", "re_var", "icc")) {
    attr(tab, a) <- attr(model, a)
  }

  tab
}

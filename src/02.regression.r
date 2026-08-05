require(ProjectTemplate)
require(tidyverse)
reload.project()

# Fitting the full model set takes >2h. reload.project() restores previously
# fitted objects from cache/, so reuse them unless FORCE_REFIT=TRUE. `code` is
# only evaluated when there is no cached object to return.
force_refit <- isTRUE(as.logical(Sys.getenv("FORCE_REFIT", "FALSE")))

fit_or_cached <- function(name, code) {
  if (!force_refit && exists(name, envir = globalenv())) {
    message("  Reusing cached model object: ", name)
    return(get(name, envir = globalenv()))
  }
  message("  Fitting: ", name)
  code
}

# Ensure labels cover both longitudinal GLMM and legacy non-longitudinal outputs
label_or_name <- function(vars) {
  out <- unname(labels[vars])
  out[is.na(out)] <- vars[is.na(out)]
  out
}

labels_extra <- c(
  mean_n_se = "Mean number of side effects",
  intolerance = "Treatment discontinuation",
  mean_eff = "Average effectiveness",
  first_imprv = "First improvement duration",
  time = "Total duration on antidepressants"
)
labels <- c(labels, labels_extra[setdiff(names(labels_extra), names(labels))])

# Match covariate terms in model output, including expanded factor coefficients
# (e.g., sex -> sexFemale)
is_covariate_parameter <- function(parameter, covs) {
  if (is.null(covs) || length(covs) == 0 || is.na(parameter)) {
    return(FALSE)
  }

  any(map_lgl(covs, function(cov) {
    identical(parameter, cov) || startsWith(parameter, cov)
  }))
}

drop_covariate_terms <- function(model_tbl, covs) {
  if (is.null(covs) || length(covs) == 0) {
    return(model_tbl)
  }

  cov_idx <- map_lgl(model_tbl$Parameter, is_covariate_parameter, covs = covs)
  model_tbl[!cov_idx, ]
}

# Wrapper to fit GLMM model for a specific dependent variable and list of independent variables
fit_model <-
  function(dep, indeps_list, covs = NULL,
           data) {
    map(indeps_list, function(indeps) {
      # Combine fixed effects and covariates
      all_fixed <- c(indeps, covs)

      # Remove duplicates if any
      all_fixed <- unique(all_fixed)

      # Fit GLMM
      m <- fit_glmm(dep, all_fixed, data)

      if (is.null(m)) return(NULL)

      # Filter out covariates from the result if requested (to show only main effects of interest)
      if (!is.null(covs)) {
        m <- drop_covariate_terms(m, covs)
      }

      m <- format_model(m)
      return(m)
    })
  }

fit_all <- function(deps, indeps_list, covs = NULL, data) {
  map(deps, function(dep) {
    fit_model(dep = dep, indeps_list = indeps_list, covs = covs, data = data)
  }) %>%
    setNames(label_or_name(deps))
}

bind_models <- function(models) {
  map(models, ~ list(bind_rows(.x)))
}

# Define dependencies
# ------------------------------------------------------------------------------
# We focus on longitudinal outcomes
glmm_deps <- c(
  "effectiveness",      # Ordinal
  "n_se",               # Count
  "remission",          # Binary
  "stopped_due_to_se"   # Binary
)

# Checked 2026-08-05: remission is binary. The raw item
# antidepressants_imprv.condition_period_time_experience only ever takes 0, 1
# and -777 (missing), and dat_long$remission has exactly two levels, both
# observed (0: 9812, 1: 7645). The factor(ordered = TRUE) applied in
# munge/01.dat_clean.r is therefore a no-op, and fit_glmm()'s binary branch
# routing this to binomial glmer is correct, not a fallback.

# Base covariates for adjustment (competing independents)
# Added cumulative_med_count as requested
compete_indeps <- c(
  "sex",
  "start_age",
  "n_relatives",
  "cumulative_med_count"
)

# Define Independent Variable Sets
# ------------------------------------------------------------------------------
# Same structure as before, but applied to longitudinal data

# Filter illnesses with enough cases (using dat_long, calculating unique IDs)
illnesses_case10 <- imap(dat_long[illnesses], function(x, name) {
    # Count unique IDs with the illness (assuming illness is static per ID)
    # x is the column in dat_long.
    # Since illness is static, we can just check if any value is 1 for an ID?
    # Actually, the illness columns are factors or numeric 0/1.
    # Let's assume numeric 0/1 or Factor 0/1.

    # Get unique values per ID (should be constant)
    # But easier: just table(x) / average_meds_per_person approximation
    # OR rigorous: distinct(dat_long, ID, .keep_all=T)

    unique_vals <- dat_long %>%
        select(ID, !!name) %>%
        distinct() %>%
        pull(!!name)

    if (table(unique_vals)[2] >= 10) { # Assuming 0/1 and we want count of 1s
        return(name)
    }
}) %>% unlist()

indeps_list_base <-
  c(
    list(
      c("sex"),
      c("start_age"),
      c("n_relatives"),
      c("cumulative_med_count"), # Added as single predictor
      c("bmi"),
      c("audit"),
      c("pack_year"),
      c(
        "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
        "In_paid_employment_or_self_employed_Full_or_part_time_student",
        "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
        "In_paid_employment_or_self_employed_Retired",
        "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
        "In_paid_employment_or_self_employed_Unemployed"
      ),
      c(
        "Not_in_relationship_In_relationship",
        "Not_in_relationship_Married"
      ),
      c(
        "eating_disorders_numeric",
        "mhd_addadhd_numeric",
        "obsessive_compulsive_disorders_numeric",
        "mhd_personality_disorder_numeric",
        "autism_spectrum_disorder_numeric",
        "comorbidity_total_count_numeric",
        "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
        "Depressive_and_anxiety_disorder_Only_depressive_disorder",
        "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
        "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
        "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder"
      ),
      c(
        "cidid_recurrence"
      )
      # "time" removed as it's likely collinear with cumulative count or age
    ),
    names(lab_grouped_illnesses)
    # illnesses_case10 # Optionally include individual illnesses
  )

# Cross-outcome predictors to include only for effectiveness/remission models
cross_outcome_indeps <- list(
  c("n_se"),
  c("se_rating"),
  c("stopped_due_to_se")
)

get_indeps_list_for_dep <- function(dep) {
  if (dep %in% c("effectiveness", "remission")) {
    return(c(indeps_list_base, cross_outcome_indeps))
  }
  indeps_list_base
}

get_adjusted_indeps_list_for_dep <- function(dep) {
  dep_indeps <- get_indeps_list_for_dep(dep)

  # In adjusted models, compete_indeps should be covariates only (not focal predictors)
  discard(dep_indeps, ~ length(.x) == 1 && .x %in% compete_indeps)
}

coerce_glmm_predictor <- function(x) {
  if (is.ordered(x)) {
    return(as.numeric(x))
  }

  if (is.factor(x) && nlevels(x) == 2) {
    return(as.numeric(x) - 1)
  }

  x
}

glmm_predictor_vars <- unique(unlist(c(
  indeps_list_base,
  cross_outcome_indeps,
  list(compete_indeps),
  list("phq9")
)))

dat_long_glmm <- dat_long %>%
  dplyr::mutate(across(any_of(glmm_predictor_vars), coerce_glmm_predictor))


# Run Models
# ------------------------------------------------------------------------------

# 1. Unadjusted (Single predictor set at a time)
glmm_models <- fit_or_cached("glmm_models", {
  map(glmm_deps, function(dep) {
    fit_model(
      dep = dep,
      indeps_list = get_indeps_list_for_dep(dep),
      data = dat_long_glmm
    )
  }) %>%
    setNames(label_or_name(glmm_deps))
})

# 2. Adjusted (Adjusting for compete_indeps + phq9)
# compete_indeps are covariates-only in adjusted models and removed from
# the focal predictor list by get_adjusted_indeps_list_for_dep().

fit_model_adjusted <-
  function(dep, indeps_list, covs = NULL,
           data) {
    map(indeps_list, function(indeps) {
      # Current main predictor

      # Actual covariates to add: covs excluding indeps
      real_covs <- setdiff(covs, indeps)

      all_fixed <- c(indeps, real_covs)
      all_fixed <- unique(all_fixed)

      m <- fit_glmm(dep, all_fixed, data)

      if (is.null(m)) return(NULL)

      # Remove adjustment covariates from output (including expanded factor terms)
      m <- drop_covariate_terms(m, real_covs)

      m <- format_model(m)
      return(m)
    })
  }

fit_all_adjusted <- function(deps, indeps_list, covs = NULL, data) {
  map(deps, function(dep) {
    fit_model_adjusted(dep = dep, indeps_list = indeps_list, covs = covs, data = data)
  }) %>%
    setNames(label_or_name(deps))
}

glmm_models_cov <- fit_or_cached("glmm_models_cov", {
  map(glmm_deps, function(dep) {
    fit_model_adjusted(
      dep = dep,
      indeps_list = get_adjusted_indeps_list_for_dep(dep),
      covs = c(compete_indeps, "phq9"),
      data = dat_long_glmm
    )
  }) %>%
    setNames(label_or_name(glmm_deps))
})

# Save Results
# ------------------------------------------------------------------------------
cache("glmm_models")
cache("glmm_models_cov")

write_xlsx_tab(bind_models(multi_adjust(glmm_models)),
  file = "./results/glmm_medications.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models))),
  file = "./results/glmm_medications_sig.xlsx", overwrite = TRUE
)

write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov)),
  file = "./results/glmm_medications_cov.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov))),
  file = "./results/glmm_medications_sig_cov.xlsx", overwrite = TRUE
)

# ------------------------------------------------------------------------------
# Additional non-longitudinal models (not suitable for GLMM random-effects setup)
# ------------------------------------------------------------------------------
fit_model_non_glmm <- function(dep, indeps_list, covs = NULL, data) {
  map(indeps_list, function(indeps) {
    fit <- if (is.numeric(data[[dep]])) fit_hurdle else fit_prop

    all_fixed <- c(indeps, covs)
    all_fixed <- unique(all_fixed)

    m <- fit(dep, all_fixed, data)

    if (!is.null(covs)) {
      m <- drop_covariate_terms(m, covs)
    }

    m <- format_model(m)
    return(m)
  })
}

fit_all_non_glmm <- function(deps, indeps_list, covs = NULL, data) {
  map(deps, function(dep) {
    fit_model_non_glmm(dep = dep, indeps_list = indeps_list, covs = covs, data = data)
  }) %>%
    setNames(label_or_name(deps))
}

nonlong_compete_indeps <- c(
  "sex",
  "avg_start_age",
  "n_relatives"
)

nonlong_indeps_base <- c(
  list(
    c("sex"),
    c("avg_start_age"),
    c("n_relatives"),
    c("bmi"),
    c("audit"),
    c("pack_year"),
    c(
      "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
      "In_paid_employment_or_self_employed_Full_or_part_time_student",
      "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
      "In_paid_employment_or_self_employed_Retired",
      "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
      "In_paid_employment_or_self_employed_Unemployed"
    ),
    c(
      "Not_in_relationship_In_relationship",
      "Not_in_relationship_Married"
    ),
    c(
      "eating_disorders_numeric",
      "mhd_addadhd_numeric",
      "obsessive_compulsive_disorders_numeric",
      "mhd_personality_disorder_numeric",
      "autism_spectrum_disorder_numeric",
      "comorbidity_total_count_numeric",
      "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
      "Depressive_and_anxiety_disorder_Only_depressive_disorder",
      "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
      "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
      "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder"
    ),
    c("cidid_recurrence"),
    c("time")
  ),
  names(lab_grouped_illnesses)
)

nonlong_sef_deps <- c("se_rating")
nonlong_eff_deps <- c("ben_rating", "n_best", "first_imprv")

nonlong_indeps_sef <- nonlong_indeps_base
nonlong_indeps_eff <- c(nonlong_indeps_base, list(c("mean_n_se", "se_rating", "intolerance")))

get_adjusted_nonlong_indeps <- function(indeps_list) {
  discard(indeps_list, ~ length(.x) == 1 && .x %in% nonlong_compete_indeps)
}

# Keep outcome scales; numeric-convert predictors as in legacy analysis
# (se_rating side-effect model)
dat_nonlong_sef <- dat %>%
  select(-ID) %>%
  dplyr::mutate(across(
    -all_of(nonlong_sef_deps),
    ~ if (is.factor(.x)) as.numeric(.x) - 1 else as.numeric(.x)
  ))

# (ben_rating / n_best / first_imprv effectiveness-related models)
dat_nonlong_eff <- dat %>%
  select(-ID) %>%
  dplyr::mutate(across(-all_of(nonlong_eff_deps), as.numeric))

sef_models <- fit_or_cached("sef_models", {
  fit_all_non_glmm(nonlong_sef_deps, nonlong_indeps_sef, data = dat_nonlong_sef)
})
eff_models <- fit_or_cached("eff_models", {
  fit_all_non_glmm(nonlong_eff_deps, nonlong_indeps_eff, data = dat_nonlong_eff)
})

sef_models_compete <- fit_or_cached("sef_models_compete", {
  fit_all_non_glmm(
    nonlong_sef_deps,
    get_adjusted_nonlong_indeps(nonlong_indeps_sef),
    covs = nonlong_compete_indeps,
    data = dat_nonlong_sef
  )
})

eff_models_compete <- fit_or_cached("eff_models_compete", {
  fit_all_non_glmm(
    nonlong_eff_deps,
    get_adjusted_nonlong_indeps(nonlong_indeps_eff),
    covs = nonlong_compete_indeps,
    data = dat_nonlong_eff
  )
})

cache("sef_models")
cache("sef_models_compete")
cache("eff_models")
cache("eff_models_compete")

write_xlsx_tab(bind_models(multi_adjust(c(sef_models, eff_models))),
  file = "./results/medications.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(c(sef_models, eff_models)))),
  file = "./results/medications_sig.xlsx", overwrite = TRUE
)

write_xlsx_tab(bind_models(multi_adjust(c(sef_models_compete, eff_models_compete))),
  file = "./results/medications_cov.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(c(sef_models_compete, eff_models_compete)))),
  file = "./results/medications_sig_cov.xlsx", overwrite = TRUE
)

# setwd(".")
require("ProjectTemplate")
require(ProjectTemplate)
require(tidyverse)
reload.project()

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
        m <- m %>%
          filter(!Parameter %in% covs)
      }

      m <- format_model(m)
      return(m)
    })
  }

fit_all <- function(deps, indeps_list, covs = NULL, data) {
  map(deps, function(dep) {
    fit_model(dep = dep, indeps_list = indeps_list, covs = covs, data = data)
  }) %>%
    setNames(labels[deps])
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
  "remission",          # Binary (Ordinal in code but 0/1/2 levels? Check)
  "stopped_due_to_se"   # Binary
)

# Check remission levels
# If remission has > 2 levels, it will be treated as ordinal.
# If it is 0/1, it is binary.
# In munge/01.dat_clean.r, we made it a factor. fit_glmm handles factors.

# Base covariates for adjustment (competing independents)
# Added cumulative_med_count as requested
compete_indeps <- c(
  "sex",
  "avg_start_age",
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

indeps_list <-
  c(
    list(
      c("sex"),
      c("avg_start_age"),
      c("n_relatives"),
      c("cumulative_med_count"), # Added as single predictor
      c("bmi"),
      c("audit"),
      c("pack_year"),
      c(
        "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
        "In_paid_employment_or_self_employed_Full_or_part_time_student",
        "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
        "In_paid_employment_or_self_employed_None_of_the_above",
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


# Run Models
# ------------------------------------------------------------------------------

# 1. Unadjusted (Single predictor set at a time)
glmm_models <- fit_all(glmm_deps, indeps_list, data = dat_long)

# 2. Adjusted (Adjusting for compete_indeps)
# Note: When the predictor IS in compete_indeps, we shouldn't add it again.
# The helper fit_model/fit_glmm handles duplicates in formula construction.
# However, if we are testing "sex", and we adjust for "sex", the coefficient is meaningless or NA.
# Typically "adjusted" means adjusting for the *other* core covariates.
# For simplicity, we pass compete_indeps as covs.
# We might want to remove the current predictor from covs if it's there.
# fit_glmm handles `unique(c(indeps, covs))`.
# If `indeps` is "sex" and `covs` includes "sex", it becomes just "sex".
# But then `fit_model` filters out `covs`.
# If "sex" is in `covs`, it gets filtered out from results!
# We need to handle this in `fit_model`.

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

      # We want to keep `indeps` in the output, but remove `real_covs`.
      # If `indeps` was part of `covs` (e.g. sex), it is the main predictor here, so keep it.

      m <- m %>%
        filter(!Parameter %in% real_covs)

      m <- format_model(m)
      return(m)
    })
  }

fit_all_adjusted <- function(deps, indeps_list, covs = NULL, data) {
  map(deps, function(dep) {
    fit_model_adjusted(dep = dep, indeps_list = indeps_list, covs = covs, data = data)
  }) %>%
    setNames(labels[deps])
}

glmm_models_cov <- fit_all_adjusted(glmm_deps, indeps_list, covs = c(compete_indeps, "phq9"), data = dat_long)

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

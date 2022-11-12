setwd("~/Disk/Projects/Research/GLAD/GLAD-Medications")
library(ProjectTemplate)
library(tidyverse)
load.project()

fit_model <-
  function(dep, indeps_list, covs = NULL,
           data) {
    map(indeps_list, function(indeps) {
      if (is.numeric(dat[[dep]])) {
        fit <- fit_hurdle
      } else {
        fit <- fit_prop
      }

      m <- fit(dep, c(indeps, covs), data)

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

compete_indeps <- c(
  "sex",
  "avg_start_age",
  "n_relatives"
)

sef_deps <- c("mean_n_se", "se_rating", "intolerance")
eff_deps <- c(
  "remission",
  "ben_rating",
  "n_best",
  "first_imprv",
  "mean_eff"
)

covs <- "phq9"
indeps_list <- list(
  compete_indeps,
  c(compete_indeps, "bmi"),
  c(compete_indeps, "audit", "pack_year"),
  c(
    compete_indeps,
    "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
    "In_paid_employment_or_self_employed_Full_or_part_time_student",
    "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
    "In_paid_employment_or_self_employed_None_of_the_above",
    "In_paid_employment_or_self_employed_Retired",
    "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
    "In_paid_employment_or_self_employed_Unemployed"
  ),
  c(
    compete_indeps,
    "Not_in_relationship_In_relationship",
    "Not_in_relationship_Married"
  ),
  c(
    compete_indeps,
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
    compete_indeps,
    "cidid_recurrence"
  ),
  c(
    compete_indeps,
    "time"
  ),
  c(
    compete_indeps,
    sef_deps
  )
)

dat_sef <- dat %>%
  select(-ID) %>%
  mutate_at(
    colnames(.)[!colnames(.) %in% sef_deps],
    as.numeric
  )
dat_eff <- dat %>%
  select(-ID) %>%
  mutate_at(
    colnames(.)[!colnames(.) %in% eff_deps],
    as.numeric
  )

# Associations with side effects
sef_models <- fit_all(sef_deps, indeps_list[1:8], data = dat_sef)
sef_covs_models <- fit_all(sef_deps, indeps_list[1:8],
  covs = covs, data = dat_sef
)
# Associations with effectiveness
eff_models <- fit_all(eff_deps, indeps_list, data = dat_eff)
eff_covs_models <- fit_all(eff_deps, indeps_list, covs = covs, data = dat_eff)

cache("sef_models")
cache("sef_covs_models")
cache("eff_models")
cache("eff_covs_models")

write_xlsx_tab(multi_adjust(c(sef_models, eff_models)),
  file = "./results/medications.xlsx", overwrite = TRUE
)
write_xlsx_tab(only_sig(multi_adjust(c(sef_models, eff_models))),
  file = "./results/medications_sig.xlsx", overwrite = TRUE
)

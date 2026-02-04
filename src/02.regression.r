# setwd(".")
require("ProjectTemplate")
require(ProjectTemplate)
require(tidyverse)
reload.project()

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

bind_models <- function(models) {
  map(models, ~ list(bind_rows(.x)))
}

illnesses_case10 <- imap(dat[illnesses], function(x, name) {
  # If the illness has at least 10 cases
  if (table(x)[2] >= 10) {
    return(name)
  }
}) %>% unlist()


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

map(dat[c(
  "score_cardiometabolic", "score_neurological", "score_autoimmune",
  "score_respiratory", "score_oncology", "score_musculoskeletal"
)], ~ sum(!is.na(.x)))


covs <- "phq9"
# indeps_list <- list(
#   compete_indeps,
#   c(compete_indeps, "bmi"),
#   c(compete_indeps, "audit", "pack_year"),
#   c(
#     compete_indeps,
#     "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
#     "In_paid_employment_or_self_employed_Full_or_part_time_student",
#     "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
#     "In_paid_employment_or_self_employed_None_of_the_above",
#     "In_paid_employment_or_self_employed_Retired",
#     "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
#     "In_paid_employment_or_self_employed_Unemployed"
#   ),
#   c(
#     compete_indeps,
#     "Not_in_relationship_In_relationship",
#     "Not_in_relationship_Married"
#   ),
#   c(
#     compete_indeps,
#     "eating_disorders_numeric",
#     "mhd_addadhd_numeric",
#     "obsessive_compulsive_disorders_numeric",
#     "mhd_personality_disorder_numeric",
#     "autism_spectrum_disorder_numeric",
#     "comorbidity_total_count_numeric",
#     "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
#     "Depressive_and_anxiety_disorder_Only_depressive_disorder",
#     "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
#     "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
#     "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder"
#   ),
#   c(
#     compete_indeps,
#     illnesses
#   ),
#   c(
#     compete_indeps,
#     "cidid_recurrence"
#   ),
#   c(
#     compete_indeps,
#     "time"
#   ),
#   c(
#     compete_indeps,
#     sef_deps
#   )
# )

indeps_sef_list <-
  c(
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
      ),
      c(
        "time"
      )
    ),
    names(lab_grouped_illnesses)
  )

lab_grouped_illnesses
indeps_eff_list <- c(indeps_sef_list, c(sef_deps))


dat_sef <- dat %>%
  select(-ID) %>%
  mutate_at(
    colnames(.)[!colnames(.) %in% sef_deps],
    ~ if (is.factor(.)) as.numeric(.) - 1 else as.numeric(.)
  )
dat_eff <- dat %>%
  select(-ID) %>%
  mutate_at(
    colnames(.)[!colnames(.) %in% eff_deps],
    as.numeric
  )

length(labels)
# Associations with side effects
sef_models <- fit_all(sef_deps, indeps_sef_list, data = dat_sef)
sef_models_compete <- fit_all(sef_deps, indeps_sef_list[-1:-3], cov = compete_indeps, data = dat_sef)

# Sensitivity
sef_covs_models <- fit_all(sef_deps, indeps_sef_list,
  covs = covs, data = dat_sef
)

sef_covs_models_compete <- fit_all(sef_deps, indeps_sef_list[-1:-3],
  covs = c(covs, compete_indeps), data = dat_sef
)

# Associations with effectiveness
eff_models <- fit_all(eff_deps, indeps_eff_list, data = dat_eff)
eff_models_compete <- fit_all(eff_deps, indeps_eff_list[-1:-3], cov = compete_indeps, data = dat_eff)

eff_covs_models <- fit_all(eff_deps, indeps_eff_list, covs = covs, data = dat_eff)
eff_covs_models_compete <- fit_all(eff_deps, indeps_eff_list[-1:-3], covs = c(covs, compete_indeps), data = dat_eff)

cache("compete_indeps")
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

# nrow(n_meds)

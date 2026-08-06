require(ProjectTemplate)
require(tidyverse)
require(rlist)
library(gtable)
library(grid)
reload.project()

# The legacy non-longitudinal models use their own names for the side-effect
# predictors; these labels are defined in src/02.regression.r and are not part
# of the cached `labels` object.
labels_extra <- c(
  mean_n_se = "Mean number of side effects",
  intolerance = "Treatment discontinuation",
  mean_eff = "Average effectiveness",
  first_imprv = "First improvement duration",
  time = "Total duration on antidepressants",
  # Medication-episode analogues of `avg_start_age` and `time`. Only the GLMM
  # has these; without them the GLMM grouped panels silently drop both, the
  # same way the legacy panels used to drop mean_n_se and intolerance.
  start_age = "Start Age",
  cumulative_med_count = "Cumulative Medication Count"
)
labels <- c(labels, labels_extra[setdiff(names(labels_extra), names(labels))])

# Bonferroni factor used by the result workbooks: the legacy workbooks correct
# across c(sef_models, eff_models) and the GLMM workbooks across glmm_models,
# which is four outcomes in both cases. Every plot below is corrected by the
# same factor so that figures and tables agree on what is significant.
N_DEPS_NONLONG <- length(sef_models) + length(eff_models)
N_DEPS_GLMM <- length(glmm_models)

# The monolithic all-predictors-in-one-figure plots were removed on 2026-08-05.
# They were 15 x 35-40 in, which is an aspect ratio of 2.3-2.7; anything above
# about 1.42 is capped by page height rather than column width when placed in
# the document, so they rendered at roughly 4 in wide with illegible text. The
# manuscript uses the thematic grouped panels below instead, and nothing else
# consumed the monolithic files. Removing them also drops the two slowest
# ggsave() calls in the script.

# Function to save grouped plots by thematic categories
predictor_groups_list <- list(
  "Demographics_Lifestyle" = c(
    "sex",
    # Legacy per-participant summaries and their GLMM medication-episode
    # analogues. Each pipeline contributes only the pair it actually fitted.
    "avg_start_age",
    "time",
    "start_age",
    "cumulative_med_count",
    "bmi",
    "audit",
    "pack_year",
    "Not_in_relationship_In_relationship",
    "Not_in_relationship_Married",
    "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
    "In_paid_employment_or_self_employed_Full_or_part_time_student",
    "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
    "In_paid_employment_or_self_employed_Retired",
    "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
    "In_paid_employment_or_self_employed_Unemployed"
  ),
  "Psychiatric_History" = c(
    "n_relatives",
    "eating_disorders_numeric",
    "mhd_addadhd_numeric",
    "obsessive_compulsive_disorders_numeric",
    "mhd_personality_disorder_numeric",
    "autism_spectrum_disorder_numeric",
    "comorbidity_total_count_numeric",
    "cidid_recurrence",
    "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
    "Depressive_and_anxiety_disorder_Only_depressive_disorder",
    "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
    "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
    "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder"
  ),
  "Somatic_Comorbidities" = c(
    "score_cardiometabolic",
    "score_neurological",
    "score_autoimmune",
    "score_respiratory",
    "score_oncology",
    "score_musculoskeletal"
  ),
  "Side_Effects" = c(
    # longitudinal GLMM names
    "n_se",
    "se_rating",
    "stopped_due_to_se",
    # legacy non-longitudinal equivalents
    "mean_n_se",
    "intolerance"
  )
)

save_grouped_plots <- function(models, prefix, n_deps = length(models)) {
  # Adjust models (p-values) first. `n_deps` must be the size of the full
  # outcome set the corresponding workbook corrects over, not the size of this
  # subset — see multi_adjust().
  #
  # bonferroni_ci() then widens the intervals to the same corrected level, so
  # that the colour (corrected significance) and the bar (the interval) agree.
  # Without it, one plotted point in ten was drawn grey with an interval that
  # visibly excluded 1.
  models_adj <- bonferroni_ci(multi_adjust(models, n_deps = n_deps), n_deps = n_deps)

  imap(predictor_groups_list, function(var_names, group_name) {
    # 1. Get target labels
    # We need to intersect with available labels to avoid errors if some are missing
    valid_vars <- intersect(var_names, names(labels))
    if (length(valid_vars) == 0) {
      return(NULL)
    }

    target_params <- labels[valid_vars]
    clean_targets <- str_remove(target_params, "v\\.s.*$")

    # 2. Filter models
    filtered_models <- map(models_adj, function(outcome_models) {
      map(outcome_models, function(df) {
        if (!is.data.frame(df)) {
          return(df)
        }
        df %>%
          mutate(clean_param = str_remove(Parameter, "v\\.s.*$")) %>%
          filter(clean_param %in% clean_targets) %>%
          select(-clean_param)
      })
    })

    # Check if we have any data left after filtering
    has_data <- any(map_lgl(filtered_models, function(outcome_models) {
      any(map_lgl(outcome_models, ~ is.data.frame(.x) && nrow(.x) > 0))
    }))

    if (!has_data) {
      # message(sprintf("No data for group %s in %s", group_name, prefix))
      return(NULL)
    }

    # 3. Generate plot
    p <- plot_models_publication(filtered_models)

    # 4. Calculate height
    # Extract data to count rows
    n_rows <- 0
    walk(filtered_models, function(outcome_models) {
      walk(outcome_models, function(m) {
        if (is.data.frame(m)) {
          if ("Coefficient_logistic" %in% names(m)) {
            n_rows <<- n_rows + nrow(m) * 2
          } else {
            n_rows <<- n_rows + nrow(m)
          }
        }
      })
    })

    # Adjusted height calculation for tighter plots
    # Base 2 inches + 0.25 inch per data point (was 0.4, 0.1 was too small)
    plot_height <- 2 + (n_rows * 0.25)

    # 5. Save
    fname <- sprintf("./graphs/%s_%s.png", prefix, group_name)
    tryCatch(
      {
        # Width reduced from 15 to 12
        ggsave(filename = fname, plot = p, height = plot_height, width = 12, limitsize = FALSE)
      },
      error = function(e) {
        warning(paste("Failed to save", fname, ":", e$message))
      }
    )
  })
}

# Generate grouped plots
save_grouped_plots(sef_models, "sef", n_deps = N_DEPS_NONLONG)
save_grouped_plots(eff_models, "eff", n_deps = N_DEPS_NONLONG)
save_grouped_plots(sef_models_compete, "sef_compete", n_deps = N_DEPS_NONLONG)
save_grouped_plots(eff_models_compete, "eff_compete", n_deps = N_DEPS_NONLONG)

# ==============================================================================
# GLMM PLOTS
# ==============================================================================

# GLMM Results are stored in:
# glmm_models (Unadjusted)
# glmm_models_cov (Adjusted)

# 1. Grouped Plots
# ------------------------------------------------------------------------------
# Split by outcome type for better visualization
# glmm_deps: "effectiveness", "n_se", "remission", "stopped_due_to_se"

# Define subsets of models based on outcome
# glmm_models is a list named by dependent variable
# We can subset the list.

glmm_eff_deps <- c("effectiveness", "remission")
glmm_sef_deps <- c("n_se", "stopped_due_to_se")

glmm_models_eff <- glmm_models[labels[glmm_eff_deps]]
glmm_models_sef <- glmm_models[labels[glmm_sef_deps]]

glmm_models_cov_eff <- glmm_models_cov[labels[glmm_eff_deps]]
glmm_models_cov_sef <- glmm_models_cov[labels[glmm_sef_deps]]

# Save grouped plots
save_grouped_plots(glmm_models_eff, "glmm_eff", n_deps = N_DEPS_GLMM)
save_grouped_plots(glmm_models_sef, "glmm_sef", n_deps = N_DEPS_GLMM)
save_grouped_plots(glmm_models_cov_eff, "glmm_eff_cov", n_deps = N_DEPS_GLMM)
save_grouped_plots(glmm_models_cov_sef, "glmm_sef_cov", n_deps = N_DEPS_GLMM)

# The combined per-outcome-group figures (glmm_eff.png, glmm_sef.png,
# glmm_eff_compete.png, glmm_sef_compete.png) were removed on 2026-08-05 for the
# same reason as the other monolithic plots: too tall to render legibly in the
# document, and superseded by the thematic panels above.

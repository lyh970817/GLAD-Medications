# setwd(".")
require(ProjectTemplate)
require(tidyverse)
require(rlist)
library(gtable)
library(grid)
# install.packages("ggplot2")
reload.project()

sef_models
plot_sef <- plot_models_publication(
  multi_adjust(sef_models)
)

plot_eff <- plot_models_publication(
  multi_adjust(eff_models)
)

cache("plot_sef")
cache("plot_eff")

ggsave(
  filename = "./graphs/sef.png", plot = plot_sef,
  height = 35, width = 15
)
ggsave(
  filename = "./graphs/eff.png", plot = plot_eff,
  height = 38, width = 15
)


# Plot sef_models_compete
plot_sef_compete <- plot_models_publication(
  multi_adjust(sef_models_compete)
)

# Save sef_compete plot
ggsave(
  filename = "./graphs/sef_compete_new.png",
  plot = plot_sef_compete,
  height = 35, width = 15
)

# Plot eff_models_compete
plot_eff_compete <- plot_models_publication(
  multi_adjust(eff_models_compete)
)

# Save eff_compete plot
ggsave(
  filename = "./graphs/eff_compete.png",
  plot = plot_eff_compete,
  height = 38, width = 15
)

# Function to save grouped plots by thematic categories
predictor_groups_list <- list(
  "Demographics_Lifestyle" = c(
    "sex",
    "avg_start_age",
    "bmi",
    "audit",
    "pack_year",
    "time",
    "Not_in_relationship_In_relationship",
    "Not_in_relationship_Married",
    "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
    "In_paid_employment_or_self_employed_Full_or_part_time_student",
    "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
    "In_paid_employment_or_self_employed_None_of_the_above",
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
    "mean_n_se",
    "se_rating",
    "intolerance"
  )
)

save_grouped_plots <- function(models, prefix) {
  # Adjust models (p-values) first
  models_adj <- multi_adjust(models)

  imap(predictor_groups_list, function(var_names, group_name) {
    # 1. Get target labels
    # We need to intersect with available labels to avoid errors if some are missing
    valid_vars <- intersect(var_names, names(labels))
    if (length(valid_vars) == 0) return(NULL)

    target_params <- labels[valid_vars]
    clean_targets <- str_remove(target_params, "v\\.s.*$")

    # 2. Filter models
    filtered_models <- map(models_adj, function(outcome_models) {
      map(outcome_models, function(df) {
        if (!is.data.frame(df)) return(df)
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
    tryCatch({
      # Width reduced from 15 to 12
      ggsave(filename = fname, plot = p, height = plot_height, width = 12, limitsize = FALSE)
    }, error = function(e) {
      warning(paste("Failed to save", fname, ":", e$message))
    })
  })
}

# Generate grouped plots
save_grouped_plots(sef_models, "sef")
save_grouped_plots(eff_models, "eff")
save_grouped_plots(sef_models_compete, "sef_compete")
save_grouped_plots(eff_models_compete, "eff_compete")

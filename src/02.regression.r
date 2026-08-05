require(ProjectTemplate)
require(tidyverse)
require(digest)
require(parallel)
reload.project()

# reload.project() restores every cached object, including the previous GLMM
# result tables and the cached plot objects. Those are surprisingly large -- the
# stored parameter tables carry `parameters` attributes that hold whole model
# environments, which is why cache/glmm_models_cov.RData is 99 MB -- and they are
# all about to be recomputed anyway. Drop them before fitting so the worker
# processes have room. This only clears the session; the cache files stay put.
rm(list = intersect(
  c("glmm_models", "glmm_models_cov", "glmm_models_cov_famhist",
    "cor_plot", "plot_eff", "plot_sef", "plot_list_eff", "plot_list_sef"),
  ls(globalenv())
), envir = globalenv())
invisible(gc())

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
#
# n_relatives (number of relatives with a psychiatric disorder) was dropped from
# the adjustment set on 2026-08-05 and is now reported only as a focal predictor,
# plus a sensitivity analysis. Three reasons:
#
#  1. It is missing for 40.8% of participants (10,489 of 25,453 medication
#     observations), and because the models are complete-case, including it cut
#     the adjusted samples to 56-58% of the observations. Without it they retain
#     95-96%.
#  2. It does nothing. In the adjusted Remission model it comes out at OR 1.005
#     per relative, p = 0.48. It correlates only r = 0.12 with phq9, r = 0.20
#     with comorbidity count and r = 0.05 with cumulative medication count.
#  3. For the psychiatric-diagnosis predictors it is over-adjustment rather than
#     adjustment: familial liability causes the exposure (having bipolar
#     disorder, ADHD, autism...), so conditioning on it partials out part of the
#     contrast the model is meant to estimate.
#
# The missingness is close to non-differential -- remission 0.432 vs 0.447,
# stopped-due-to-side-effects 0.352 vs 0.352, effectiveness 0.805 vs 0.817
# between observed and missing -- so the restricted subsample remains
# interpretable, which is what makes it usable as a sensitivity analysis
# (`glmm_models_cov_famhist`) rather than merely a caveat.
compete_indeps <- c(
  "sex",
  "start_age",
  "cumulative_med_count"
)

# Sensitivity adjustment set: the original one, adding family history back.
sens_compete_indeps <- c(compete_indeps, "n_relatives")

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
        "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
        "Depressive_and_anxiety_disorder_Only_depressive_disorder",
        "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
        "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
        "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder"
      ),
      # Split out of the diagnosis vector on 2026-08-05. Every other focal
      # predictor is fitted in its own model, so bundling the total comorbidity
      # count with the ten diagnosis dummies made it the only predictor competing
      # for variance with the others -- and it competes with exactly the
      # variables it is a summary of. Fitted alone it is comparable with the rest.
      c(
        "comorbidity_total_count_numeric"
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

get_adjusted_indeps_list_for_dep <- function(dep, competing = compete_indeps) {
  dep_indeps <- get_indeps_list_for_dep(dep)

  # In adjusted models, the competing set are covariates only (not focal
  # predictors). n_relatives is no longer in the main competing set, so it stays
  # a focal predictor here and is reported adjusted for the others.
  discard(dep_indeps, ~ length(.x) == 1 && .x %in% competing)
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
  list(sens_compete_indeps),
  list("phq9")
)))

dat_long_glmm <- dat_long %>%
  dplyr::mutate(across(any_of(glmm_predictor_vars), coerce_glmm_predictor))


# Run Models
# ------------------------------------------------------------------------------

# Caching one big object per model family meant that changing a single predictor
# set invalidated every fit in it, and a full refit is hours because the ordinal
# Effectiveness models are slow. Cache each model *set* instead, keyed by a hash
# of everything that determines its result: outcome, focal predictors,
# covariates, the data, and a recipe version bumped whenever fit_glmm() changes
# in a way that alters estimates. Changing one predictor set now refits one
# model, and re-running the script with nothing changed is free.
GLMM_RECIPE_VERSION <- "2026-08-05-scaled-bobyqa"

glmm_fit_dir <- file.path("cache", "glmm_fits")
dir.create(glmm_fit_dir, showWarnings = FALSE, recursive = TRUE)

set_cache_path <- function(dep, indeps, covs, data) {
  real_covs <- setdiff(covs, indeps)
  all_fixed <- unique(c(indeps, real_covs))

  key <- digest::digest(list(
    version = GLMM_RECIPE_VERSION,
    dep = dep,
    fixed = all_fixed,
    drop = real_covs,
    data = digest::digest(data[c("ID", dep, all_fixed)])
  ))
  file.path(glmm_fit_dir, paste0(key, ".rds"))
}

set_is_cached <- function(dep, indeps, covs, data) {
  !force_refit && file.exists(set_cache_path(dep, indeps, covs, data))
}

fit_set_cached <- function(dep, indeps, covs, data) {
  real_covs <- setdiff(covs, indeps)
  all_fixed <- unique(c(indeps, real_covs))
  path <- set_cache_path(dep, indeps, covs, data)

  if (!force_refit && file.exists(path)) {
    return(readRDS(path))
  }

  m <- fit_glmm(dep, all_fixed, data)
  if (is.null(m)) {
    saveRDS(NULL, path)
    return(NULL)
  }

  if (length(real_covs)) {
    # Remove adjustment covariates from output (including expanded factor terms)
    m <- drop_covariate_terms(m, real_covs)
  }

  m <- format_model(m)
  saveRDS(m, path)
  m
}

# The sets are independent, so fit them concurrently.
#
# PSOCK rather than fork. mclapply() forks the parent, and reload.project() has
# by then loaded every cached object -- several gigabytes, because the stored
# parameter tables carry `parameters` attributes that drag whole model
# environments with them. Forking that parent on a machine with a couple of
# gigabytes free left the children wedged with no CPU time at all. Fresh worker
# processes cost a little startup and load only what they are given.
glmm_workers <- as.integer(Sys.getenv("GLMM_WORKERS", "5"))

fit_family <- function(deps, indeps_list_for, covs, data) {
  jobs <- unlist(
    map(deps, function(dep) {
      map(indeps_list_for(dep), function(indeps) list(dep = dep, indeps = indeps))
    }),
    recursive = FALSE
  )

  todo <- sum(!vapply(jobs, function(j) set_is_cached(j$dep, j$indeps, covs, data), logical(1)))
  message("  ", length(jobs), " model sets, ", todo, " to fit, on ", glmm_workers, " workers")

  fits <- if (todo == 0) {
    # Everything is already on disk; no point paying for workers.
    map(jobs, function(job) fit_set_cached(job$dep, job$indeps, covs, data))
  } else {
    cl <- parallel::makePSOCKcluster(glmm_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      suppressMessages({
        library(dplyr); library(purrr); library(tibble); library(digest)
      })
      # Only the two helpers the fitting needs -- sourcing all of lib/ would
      # drag in the plotting stack for no reason.
      source("lib/02.fit_funs.r")
      source("lib/05.format_model.r")
      NULL
    })
    parallel::clusterExport(
      cl,
      c("fit_set_cached", "set_cache_path", "drop_covariate_terms",
        "is_covariate_parameter", "GLMM_RECIPE_VERSION", "glmm_fit_dir",
        "force_refit", "labels"),
      envir = globalenv()
    )

    # covs and data are passed as arguments rather than captured: a worker gets
    # its own global environment, so anything the closure picks up from the
    # master's globals is simply absent when it runs.
    #
    # Load balancing matters: the ordinal Effectiveness fits take minutes while
    # the binomial ones take seconds.
    parallel::parLapplyLB(cl, jobs, function(job, covs, data) {
      fit_set_cached(job$dep, job$indeps, covs, data)
    }, covs = covs, data = data)
  }

  # Regroup the flat job list back into one list of sets per outcome
  split(fits, factor(vapply(jobs, function(j) j$dep, character(1)), levels = deps)) %>%
    setNames(label_or_name(deps))
}

# 1. Unadjusted (single predictor set at a time)
glmm_models <- fit_family(
  glmm_deps, get_indeps_list_for_dep,
  covs = NULL, data = dat_long_glmm
)

# 2. Adjusted for sex, start age, cumulative medication count and phq9.
# The competing set are covariates only here, and are removed from the focal
# predictor list by get_adjusted_indeps_list_for_dep().
glmm_models_cov <- fit_family(
  glmm_deps, function(dep) get_adjusted_indeps_list_for_dep(dep, compete_indeps),
  covs = c(compete_indeps, "phq9"), data = dat_long_glmm
)

# 3. Sensitivity: the same adjusted models with family history added back. This
# is the adjustment set the analysis used until 2026-08-05; it costs 42-44% of
# the observations to complete-case exclusion, which is why it is no longer the
# main specification.
glmm_models_cov_famhist <- fit_family(
  glmm_deps, function(dep) get_adjusted_indeps_list_for_dep(dep, sens_compete_indeps),
  covs = c(sens_compete_indeps, "phq9"), data = dat_long_glmm
)

# Convergence report. fit_glmm() now records the fit warnings on each result, so
# a model that failed to converge cannot reach a table unnoticed the way the
# adjusted Remission models did.
report_convergence <- function(models, label) {
  bad <- list()
  for (dep in names(models)) {
    for (i in seq_along(models[[dep]])) {
      m <- models[[dep]][[i]]
      if (is.null(m) || isTRUE(attr(m, "converged"))) next
      bad[[length(bad) + 1]] <- paste0(
        label, " / ", dep, " / set ", i, " (", m$Parameter[1], "): ",
        paste(gsub("\n.*", "", attr(m, "fit_warnings")), collapse = "; ")
      )
    }
  }
  if (length(bad)) {
    message("!! ", length(bad), " model set(s) did not converge cleanly in ", label, ":")
    for (b in bad) message("   ", b)
  } else {
    message("   All model sets converged cleanly in ", label)
  }
  invisible(bad)
}

glmm_convergence <- c(
  report_convergence(glmm_models, "unadjusted"),
  report_convergence(glmm_models_cov, "adjusted"),
  report_convergence(glmm_models_cov_famhist, "sensitivity")
)

# Save Results
# ------------------------------------------------------------------------------
cache("glmm_models")
cache("glmm_models_cov")
cache("glmm_models_cov_famhist")

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

# Sensitivity analysis: adjusted models with family history added back
write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov_famhist)),
  file = "./results/glmm_medications_cov_famhist.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov_famhist))),
  file = "./results/glmm_medications_sig_cov_famhist.xlsx", overwrite = TRUE
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

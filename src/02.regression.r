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
  c("glmm_models", "glmm_models_cov", "glmm_models_cov_famhist", "glmm_models_cov_class",
    "glmm_models_cov_cohort", "glmm_models_cov_drug", "glmm_models_cov_strict", "cor_plot", "plot_eff", "plot_sef", "plot_list_eff", "plot_list_sef"),
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
  intolerance = "Treatment discontinuation (per 10% of antidepressants stopped)",
  first_imprv = "First improvement duration",
  time = "Mean duration per antidepressant (years)"
)
labels <- c(labels, labels_extra[setdiff(names(labels_extra), names(labels))])

# The within-person and between-person terms of the decomposed medication-level
# predictors (lib/09.within_between.r): "Start Age (within person)", ...
labels <- c(labels, within_between_labels(WITHIN_BETWEEN_VARS, labels))

# The drug-class dummies of the class sensitivity set (lib/10.drug_class.r):
# "Drug class: TCA vs SSRI", ...
labels <- c(labels, drug_class_labels())

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

# Remission is binary (ruling A4, defined in munge/01.dat_clean.r): 1 = the
# drug worked moderately or very well and no further symptoms followed, 0 =
# further symptoms or the drug did not work at all. It is a two-level ordered
# factor, which fit_glmm()'s binary branch routes to binomial glmer.

# Base covariates for adjustment (competing independents)
#
# Family history is not in the main adjustment set; it is reported as a focal
# predictor and added back in a sensitivity analysis. Since the move to the
# ilovedata extracts it is `fh_diagnosis_types`, the number of diagnosis types
# with an affected immediate relative (ruling 2026-11-06), replacing
# `n_relatives`, the sum of typed relative counts. The reasons below, and their
# figures, are those for `n_relatives` when it was dropped on 2026-08-05:
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
sens_compete_indeps <- c(compete_indeps, "fh_diagnosis_types")

# Survey-cohort sensitivity covariate (user ruling 2026-11-09: "Cohort can be
# included for sensitivity analyses"). `cohort` (Baseline / SA8) is not in the
# main adjustment sets; the cohort sets (glmm_models_cov_cohort,
# sef_models_cohort, eff_models_cohort) add it to the adjusted ones, and one
# extra set per outcome has it as the focal predictor (SA8 vs Baseline).
cohort_covariate <- "cohort"

# Define Independent Variable Sets
# ------------------------------------------------------------------------------
# Same structure as before, but applied to longitudinal data

# The individual illness ticks are no longer in the analysis frames; the six
# illness group counts below replace them (rulings `ill_*`).

indeps_list_base <-
  c(
    list(
      c("sex"),
      c("start_age"),
      c("fh_diagnosis_types"),
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
  # predictors). Family history is not in the main competing set, so it stays
  # a focal predictor here and is reported adjusted for the others.
  is_competing <- function(s) length(s) == 1 && s %in% competing
  # Exception (user ruling 2026-11-10): a competing covariate that varies
  # within participant (start age, cumulative medication count) also gets one
  # focal set of its own, which the caller decomposes into its within and
  # between terms and fit_family() fits adjusted for the other covariates but
  # not for its own whole version (covs_for_set()). These sets go at the end,
  # so every existing set keeps its position.
  own_wb <- function(s) is_competing(s) && s %in% WITHIN_BETWEEN_VARS
  c(discard(dep_indeps, is_competing), keep(dep_indeps, own_wb))
}

# coerce_glmm_predictor() is in lib/02.fit_funs.r, shared with
# src/06.clinical_quantities.r, which rebuilds this frame to refit three models.

glmm_predictor_vars <- unique(unlist(c(
  indeps_list_base,
  cross_outcome_indeps,
  list(compete_indeps),
  list(sens_compete_indeps),
  list("phq9"),
  # Two-level factor -> 0/1 (SA8 = 1), so its term is `cohort`, not `cohortSA8`.
  list(cohort_covariate)
)))

dat_long_glmm <- dat_long %>%
  dplyr::mutate(across(any_of(glmm_predictor_vars), coerce_glmm_predictor)) %>%
  # Hybrid within/between decomposition (user ruling 2026-11-09): adds
  # <X>_within and <X>_between for each medication-level predictor in
  # WITHIN_BETWEEN_VARS. The undecomposed columns stay, for the adjustment sets.
  add_within_between(WITHIN_BETWEEN_VARS)

# Each focal set that is a single medication-level predictor X (one that varies
# within participant: start age, cumulative medication count, number of side
# effects, stopped because of side effects) is fitted as the two-term set
# c(X_within, X_between) instead, in the same model, with the random intercept
# kept. This is applied after get_adjusted_indeps_list_for_dep() has removed the
# covariates from the focal list. Start age and the cumulative medication count
# are focal in every set: in the unadjusted set as usual, and in each adjusted
# set as one extra set each (user ruling 2026-11-10), adjusted for the other
# covariates; in every other adjusted set they are covariates, entered
# undecomposed.
hybrid_indeps_for <- function(indeps_list_for) {
  function(dep) decompose_indeps_list(indeps_list_for(dep), WITHIN_BETWEEN_VARS)
}


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
  # Each job carries its own covariates: `covs`, except that a decomposed
  # covariate's own focal set (start age or the cumulative medication count as
  # its within and between terms) drops that covariate's whole version
  # (covs_for_set()). Every other set gets `covs` unchanged, so its cache key,
  # and therefore its cached fit, is the same as before.
  jobs <- unlist(
    map(deps, function(dep) {
      map(indeps_list_for(dep), function(indeps) {
        list(dep = dep, indeps = indeps, covs = covs_for_set(covs, indeps, WITHIN_BETWEEN_VARS))
      })
    }),
    recursive = FALSE
  )

  todo <- sum(!vapply(jobs, function(j) set_is_cached(j$dep, j$indeps, j$covs, data), logical(1)))
  message("  ", length(jobs), " model sets, ", todo, " to fit, on ", glmm_workers, " workers")

  fits <- if (todo == 0) {
    # Everything is already on disk; no point paying for workers.
    map(jobs, function(job) fit_set_cached(job$dep, job$indeps, job$covs, data))
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

    # The covariates travel in each job and data is passed as an argument
    # rather than captured: a worker gets its own global environment, so
    # anything the closure picks up from the master's globals is simply absent
    # when it runs.
    #
    # Load balancing matters: the ordinal Effectiveness fits take minutes while
    # the binomial ones take seconds.
    parallel::parLapplyLB(cl, jobs, function(job, data) {
      fit_set_cached(job$dep, job$indeps, job$covs, data)
    }, data = data)
  }

  # Regroup the flat job list back into one list of sets per outcome
  split(fits, factor(vapply(jobs, function(j) j$dep, character(1)), levels = deps)) %>%
    setNames(label_or_name(deps))
}

# 1. Unadjusted (single predictor set at a time)
glmm_models <- fit_family(
  glmm_deps, hybrid_indeps_for(get_indeps_list_for_dep),
  covs = NULL, data = dat_long_glmm
)

# 2. Adjusted for sex, start age, cumulative medication count and phq9.
# The competing set are covariates only here, and are removed from the focal
# predictor list by get_adjusted_indeps_list_for_dep(), except that start age
# and the cumulative medication count each get one extra focal set as their
# within and between terms, adjusted for the other covariates (also in 3-5).
glmm_models_cov <- fit_family(
  glmm_deps, hybrid_indeps_for(function(dep) get_adjusted_indeps_list_for_dep(dep, compete_indeps)),
  covs = c(compete_indeps, "phq9"), data = dat_long_glmm
)

# 3. Sensitivity: the same adjusted models with family history added back. This
# is the adjustment set the analysis used until 2026-08-05; it costs 42-44% of
# the observations to complete-case exclusion, which is why it is no longer the
# main specification.
glmm_models_cov_famhist <- fit_family(
  glmm_deps, hybrid_indeps_for(function(dep) get_adjusted_indeps_list_for_dep(dep, sens_compete_indeps)),
  covs = c(sens_compete_indeps, "phq9"), data = dat_long_glmm
)

# 4. Sensitivity: drug class (user ruling 2026-11-09). The adjusted models of
# (2), fitted on the class-scoped frame -- the rows of the drugs the ruled
# mapping drops removed, and a participant left with none leaves -- with the
# class dummies (SSRI the reference) added to the covariates. The
# within/between means are recomputed over the class-scoped rows. One further
# set per outcome has the four class dummies as its focal set (adjusted for the
# same covariates), which is where the class contrasts are reported from; in
# every other set they are covariates and are not reported. Class is not in the
# main sets.
dat_long_class <- apply_class_scope(dat_long_glmm)
class_scope <- attr(dat_long_class, "class_scope")
dat_long_class <- add_within_between(dat_long_class, WITHIN_BETWEEN_VARS)
message(
  "  Class-scoped frame: ", class_scope$rows_after, " rows from ",
  class_scope$participants_after, " participants (", class_scope$rows_dropped,
  " rows of dropped drugs removed; ",
  class_scope$participants_before - class_scope$participants_after, " participants leave)"
)

glmm_models_cov_class <- fit_family(
  glmm_deps,
  function(dep) {
    c(
      decompose_indeps_list(get_adjusted_indeps_list_for_dep(dep, compete_indeps), WITHIN_BETWEEN_VARS),
      list(drug_class_dummy_columns())
    )
  },
  covs = c(compete_indeps, "phq9", drug_class_dummy_columns()), data = dat_long_class
)

# 5. Sensitivity: survey cohort (user ruling 2026-11-09). The adjusted models of
# (2) on the same full frame (no row scoping, so the within/between means are
# those of the main sets), with `cohort` (SA8 vs Baseline) added to the
# covariates. One further set per outcome has the cohort as its focal
# predictor, adjusted for the usual covariates; the cohort contrast is reported
# from it and is a covariate (not reported) everywhere else. The per-set cache
# means only these sets are fitted; the main sets are untouched.
glmm_models_cov_cohort <- fit_family(
  glmm_deps,
  function(dep) {
    c(
      decompose_indeps_list(get_adjusted_indeps_list_for_dep(dep, compete_indeps), WITHIN_BETWEEN_VARS),
      list(cohort_covariate)
    )
  },
  covs = c(compete_indeps, "phq9", cohort_covariate), data = dat_long_glmm
)

# 6. Sensitivity: individual antidepressant (user approval 2026-11-11). The
# within/between sets of the four medication-level predictors (number of side
# effects, stopped because of side effects, start age, cumulative medication
# count) for effectiveness and remission, adjusted exactly as in (2) --
# including covs_for_set() for start age and the cumulative count -- plus the
# individual drug as a factor (sertraline the reference; drugs with fewer than
# 50 rows pooled as "other", lib/12.drug_identity.r). On the full frame, with no
# row scoping, so the within/between terms are those of the main sets. Drug is
# a covariate only and is never reported. Reason: drugs with more side effects
# tend to be rated as working better, which confounds the within-person
# contrast of the side-effect measures.
dat_long_drug <- add_drug_identity(dat_long_glmm)
drug_identity_info <- attr(dat_long_drug, "drug_identity")
message(
  "  Drug-identity frame: ", nrow(dat_long_drug), " rows, ", length(drug_identity_info$levels) - 1,
  " drug contrasts vs ", drug_identity_info$reference, "; pooled as \"", DRUG_IDENTITY_OTHER, "\" (fewer than ",
  drug_identity_info$min_rows, " rows): ", drug_identity_pooled_text(drug_identity_info$pooled)
)

drug_deps <- c("effectiveness", "remission")
drug_focal_vars <- c("n_se", "stopped_due_to_se", "start_age", "cumulative_med_count")

glmm_models_cov_drug <- fit_family(
  drug_deps,
  function(dep) decompose_indeps_list(as.list(drug_focal_vars), WITHIN_BETWEEN_VARS),
  covs = c(compete_indeps, "phq9", DRUG_IDENTITY_COLUMN), data = dat_long_drug
)

# 7. Sensitivity: strict coding of blank side-effect checklists (ruling
# `a3_strict_sens`, 2026-11-11; supplementary only). Under the main rule (A3 as
# amended 2026-11-06, unchanged) a drug with Yes on the any-side-effects
# question whose checklist page was recorded with nothing ticked has 0 side
# effects and was not stopped because of them; under the strict rule such a row
# counts as 0 only when its "No side effects" box is ticked, otherwise both are
# missing (`n_se_strict`, `stopped_due_to_se_strict` from the munge;
# lib/13.strict_se.r). Same rows as the main frame; the within/between terms
# are recomputed from the strict values. The eight adjusted decomposed models
# the strict rule can move: effectiveness and remission on the number of side
# effects and on stopping because of side effects, and the number of side
# effects and stopping on the cumulative medication count and on start age, each
# adjusted exactly as in (2), including covs_for_set(). Bonferroni x4. The
# per-set cache (cache/glmm_fits/) is the reuse gate, as for every GLMM set;
# FORCE_REFIT=TRUE refits it.
dat_long_strict <- apply_strict_se(dat_long_glmm, WITHIN_BETWEEN_VARS)
strict_se_info <- attr(dat_long_strict, "strict_se")
message(
  "  Strict blank-checklist frame: ", strict_se_info$rows, " rows; ", strict_se_recoded_text(strict_se_info),
  " of ", strict_se_info$participants_recoded, " participants recoded to missing; ",
  strict_se_info$participants_lose_all_n_se, " participants left with no known number of side effects; ",
  "known n_se ", strict_se_info$n_se_known_main, " -> ", strict_se_info$n_se_known_strict,
  ", known stop status ", strict_se_info$stopped_known_main, " -> ", strict_se_info$stopped_known_strict
)

strict_focal_vars <- list(
  effectiveness = c("n_se", "stopped_due_to_se"),
  remission = c("n_se", "stopped_due_to_se"),
  n_se = c("cumulative_med_count", "start_age"),
  stopped_due_to_se = c("cumulative_med_count", "start_age")
)

glmm_models_cov_strict <- fit_family(
  names(strict_focal_vars),
  function(dep) decompose_indeps_list(as.list(strict_focal_vars[[dep]]), WITHIN_BETWEEN_VARS),
  covs = c(compete_indeps, "phq9"), data = dat_long_strict
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
  report_convergence(glmm_models_cov_famhist, "sensitivity"),
  report_convergence(glmm_models_cov_class, "drug-class sensitivity"),
  report_convergence(glmm_models_cov_cohort, "cohort sensitivity"),
  report_convergence(glmm_models_cov_drug, "drug-identity sensitivity"),
  report_convergence(glmm_models_cov_strict, "strict blank-checklist sensitivity")
)

# Save Results
# ------------------------------------------------------------------------------
cache("glmm_models")
cache("glmm_models_cov")
cache("glmm_models_cov_famhist")
cache("glmm_models_cov_class")
cache("glmm_models_cov_cohort")
cache("glmm_models_cov_drug")
cache("glmm_models_cov_strict")

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

# Sensitivity analysis: adjusted models on the class-scoped frame with drug class
write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov_class)),
  file = "./results/glmm_medications_cov_class.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov_class))),
  file = "./results/glmm_medications_sig_cov_class.xlsx", overwrite = TRUE
)

# Sensitivity analysis: adjusted models with survey cohort added
write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov_cohort)),
  file = "./results/glmm_medications_cov_cohort.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov_cohort))),
  file = "./results/glmm_medications_sig_cov_cohort.xlsx", overwrite = TRUE
)

# Sensitivity analysis: the within/between sets with the individual drug added.
# Two of the four longitudinal outcomes, so the Bonferroni factor is given
# explicitly: four, as for every other longitudinal workbook.
write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov_drug, n_deps = length(glmm_deps))),
  file = "./results/glmm_medications_cov_drug.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov_drug, n_deps = length(glmm_deps)))),
  file = "./results/glmm_medications_sig_cov_drug.xlsx", overwrite = TRUE
)

# Sensitivity analysis: strict coding of blank side-effect checklists. All four
# longitudinal outcomes, Bonferroni x4 (given explicitly, as for the drug set).
write_xlsx_tab(bind_models(multi_adjust(glmm_models_cov_strict, n_deps = length(glmm_deps))),
  file = "./results/glmm_medications_cov_strict.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(glmm_models_cov_strict, n_deps = length(glmm_deps)))),
  file = "./results/glmm_medications_sig_cov_strict.xlsx", overwrite = TRUE
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

    # A covariate that is also the focal set (the cohort set's cohort contrast)
    # is reported, as in fit_set_cached(); every other set has no overlap.
    real_covs <- setdiff(covs, indeps)
    if (length(real_covs)) {
      m <- drop_covariate_terms(m, real_covs)
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
  "fh_diagnosis_types"
)

nonlong_indeps_base <- c(
  list(
    c("sex"),
    c("avg_start_age"),
    c("fh_diagnosis_types"),
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
    # Split out of the diagnosis vector on 2026-11-07, as the GLMM block was on
    # 2026-08-05. Under ruling `comorb_groups` the count is exactly the sum of
    # the diagnosis indicators in the set above, so fitting it alongside them
    # made that design matrix rank deficient by one column. Fitted alone it is
    # comparable with the other single-predictor models.
    c("comorbidity_total_count_numeric"),
    c("cidid_recurrence"),
    c("time")
  ),
  names(lab_grouped_illnesses)
)

nonlong_sef_deps <- c("se_rating")
nonlong_eff_deps <- c("ben_rating", "n_best", "first_imprv")

nonlong_indeps_sef <- nonlong_indeps_base
# One model per side-effect measure, as in cross_outcome_indeps and the manuscript (user ruling 2026-11-08)
nonlong_indeps_eff <- c(
  nonlong_indeps_base,
  list(c("mean_n_se"), c("se_rating"), c("intolerance"))
)

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

# Sensitivity: survey cohort (user ruling 2026-11-09). The adjusted sets above
# with `cohort` (SA8 vs Baseline) added to the covariates, plus one set per
# outcome with the cohort as its focal predictor. Minutes to fit.
sef_models_cohort <- fit_or_cached("sef_models_cohort", {
  fit_all_non_glmm(
    nonlong_sef_deps,
    c(get_adjusted_nonlong_indeps(nonlong_indeps_sef), list(cohort_covariate)),
    covs = c(nonlong_compete_indeps, cohort_covariate),
    data = dat_nonlong_sef
  )
})

eff_models_cohort <- fit_or_cached("eff_models_cohort", {
  fit_all_non_glmm(
    nonlong_eff_deps,
    c(get_adjusted_nonlong_indeps(nonlong_indeps_eff), list(cohort_covariate)),
    covs = c(nonlong_compete_indeps, cohort_covariate),
    data = dat_nonlong_eff
  )
})

cache("sef_models")
cache("sef_models_compete")
cache("eff_models")
cache("eff_models_compete")
cache("sef_models_cohort")
cache("eff_models_cohort")

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

write_xlsx_tab(bind_models(multi_adjust(c(sef_models_cohort, eff_models_cohort))),
  file = "./results/medications_cov_cohort.xlsx", overwrite = TRUE
)
write_xlsx_tab(bind_models(only_sig(multi_adjust(c(sef_models_cohort, eff_models_cohort)))),
  file = "./results/medications_sig_cov_cohort.xlsx", overwrite = TRUE
)

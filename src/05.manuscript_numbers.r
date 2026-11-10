# Stand-alone numbers the manuscript text quotes.
#
# Sample sizes, the antidepressants per person, the outcome distributions, the
# remission discordance, family-history coverage and the side-effect count
# model's dispersion used to be computed ad hoc, so nothing recorded how any of
# them was derived and none of them followed a refit. This script computes every
# one of them from the analysis frames, the ilovedata extracts and, where they
# exist, the cached model objects. Nothing is typed in.
#
# Output: docs/manuscript-numbers-current.md, a key / value / definition table
# per section. Set MANUSCRIPT_OUT_DIR to write it somewhere other than docs/,
# e.g. for a dry run on development data that must not land in tracked files.
#
# Numbers that need a model object which is not in cache/ are reported as
# skipped, with the object they need.
#
# plyr, attached after dplyr, masks mutate(), summarise() and count(); keep them qualified
# (see "Namespace masking" in docs/analysis-pipeline.md).

require(ProjectTemplate)
require(tidyverse)
reload.project()

docs_dir <- Sys.getenv("MANUSCRIPT_OUT_DIR", "docs")
out_path <- file.path(docs_dir, "manuscript-numbers-current.md")
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

cohorts <- levels(droplevels(factor(dat$cohort)))
cohort_key <- function(x) tolower(gsub("[^A-Za-z0-9]+", "_", x))

numbers <- list()
section <- "Unsorted"
add <- function(key, value, definition) {
  numbers[[length(numbers) + 1]] <<- tibble(
    section = section, key = key, value = as.character(value), definition = definition
  )
}

# Adds one key per cohort and one overall, each from `f(rows)`, where `rows`
# selects that group's rows of `d`.
add_by_cohort <- function(key, d, f, definition) {
  groups <- c(
    setNames(lapply(cohorts, function(cc) which(d$cohort == cc)), cohort_key(cohorts)),
    overall = list(seq_len(nrow(d)))
  )
  for (g in names(groups)) {
    add(paste0(key, "_", g), f(groups[[g]]), definition)
  }
}

fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
pct_of <- function(k, n) fmt_n_pct(k, n)

skipped <- function(what) paste0("skipped: needs ", what)

# ------------------------------------------------------------------------------
section <- "Extract"
# ------------------------------------------------------------------------------
if (exists("ilovedata_manifest")) {
  mf <- setNames(ilovedata_manifest$value, ilovedata_manifest$key)
  for (k in intersect(c("snapshot", "git_commit", "exported_at", "n_person", "n_person_drug"), names(mf))) {
    add(paste0("extract_", k), mf[[k]], paste0("`", k, "` in data/ilovedata_manifest.rds (the extract these numbers come from)"))
  }
} else {
  add("extract_manifest", "missing", "data/ilovedata_manifest.rds is not loaded; the extract cannot be identified")
}

# ------------------------------------------------------------------------------
section <- "Who enters"
# ------------------------------------------------------------------------------
# The funnel from the extract to `dat`, recomputed from the extracts with the
# munge's own rules (munge/01.dat_clean.r, "WHO ENTERS").
if (exists("ilovedata_person") && exists("ilovedata_person_drug")) {
  person <- ilovedata_person %>% filter(cohort %in% cohorts)
  named_tick <- ilovedata_person_drug %>%
    group_by(person_id) %>%
    dplyr::summarise(any_named = any(is_tick(took)), .groups = "drop")
  other_cols <- intersect(c("prescription.none", "prescription.a_different_antidepressant.1"), names(person))
  flow <- person %>%
    left_join(named_tick, by = "person_id") %>%
    dplyr::mutate(
      any_named = coalesce(any_named, FALSE),
      any_other = if (length(other_cols)) rowSums(across(all_of(other_cols), is_tick)) > 0 else FALSE,
      med_survey = case_when(
        cohort == "Baseline" ~ in_glad_med,
        cohort == "SA8" ~ in_glad_sa8_signup,
        TRUE ~ NA
      ),
      in_dat = person_id %in% dat$ID
    )

  add_by_cohort("people_in_extract", flow, function(i) commafy(length(i)),
    "People in data/ilovedata_person.rds")
  add_by_cohort("medication_survey_respondents", flow, function(i) commafy(sum(flow$med_survey[i], na.rm = TRUE)),
    "Respondents to the survey carrying the medication items: `glad_med` for Baseline, `glad_sa8_signup` for SA8")
  add_by_cohort("answered_antidepressant_list", flow, function(i) commafy(sum(flow$any_named[i] | flow$any_other[i])),
    "People who ticked at least one option on the antidepressant list: a named drug, \"None\" (Baseline only) or \"a different antidepressant\"")
  add_by_cohort("named_antidepressant_takers", flow, function(i) commafy(sum(flow$any_named[i])),
    "People who ticked at least one of the 28 named antidepressants (the inclusion rule before the diagnosis filter)")
  dep_anx <- flow$mhd.depression_and_anxiety
  add_by_cohort("takers_diagnosis_unknown", flow,
    function(i) commafy(sum(flow$any_named[i] & !flow$in_dat[i] & is.na(dep_anx[i]))),
    "Named-antidepressant takers left out because whether they have a depressive or anxiety disorder is unknown")
  add_by_cohort("takers_neither_depression_nor_anxiety", flow,
    function(i) commafy(sum(flow$any_named[i] & !flow$in_dat[i] & !is.na(dep_anx[i]))),
    "Named-antidepressant takers left out because they report neither a depressive nor an anxiety disorder")
} else {
  add("who_enters", skipped("data/ilovedata_person.rds and data/ilovedata_person_drug.rds"),
    "The inclusion funnel is recomputed from the extracts")
}

# ------------------------------------------------------------------------------
section <- "Analysis frames"
# ------------------------------------------------------------------------------
add_by_cohort("participants_dat", dat, function(i) commafy(length(i)),
  "Participants in `dat` (per-participant frame)")
add_by_cohort("participants_dat_long", dat_long, function(i) commafy(n_distinct(dat_long$ID[i])),
  "Participants in `dat_long` (longitudinal frame); the munge requires these to be the people in `dat`")
add_by_cohort("medication_observations_dat_long", dat_long, function(i) commafy(length(i)),
  "Rows of `dat_long`: participant x named antidepressant taken")
add("cohort_overlap", commafy(length(intersect(dat$ID[dat$cohort == cohorts[1]], dat$ID[dat$cohort != cohorts[1]]))),
  "Participants in more than one cohort (the munge stops if this is not 0)")

# ------------------------------------------------------------------------------
section <- "Antidepressants per participant"
# ------------------------------------------------------------------------------
meds_per_person <- dat_long %>% dplyr::count(ID, cohort, name = "n_meds")
for (stat in c("mean", "sd", "median", "min", "max")) {
  f <- switch(stat,
    mean = function(x) fmt_num(mean(x)),
    sd = function(x) fmt_num(sd(x)),
    median = function(x) fmt_num(median(x), 1),
    min = function(x) commafy(min(x)),
    max = function(x) commafy(max(x))
  )
  add_by_cohort(paste0("antidepressants_per_participant_", stat), meds_per_person,
    function(i) f(meds_per_person$n_meds[i]),
    paste0(stat, " of the number of named antidepressants per participant (rows of `dat_long` per ID)"))
}
# The within-person terms of the hybrid longitudinal models (user ruling
# 2026-11-09, lib/09.within_between.r) are informed only by participants with
# two or more medications.
add_by_cohort("participants_two_or_more_medications", meds_per_person,
  function(i) pct_of(sum(meds_per_person$n_meds[i] >= 2), length(i)),
  "Participants in `dat_long` with two or more named antidepressants (of all participants in `dat_long`); only they inform the within-person terms of the longitudinal models")

# ------------------------------------------------------------------------------
section <- "Survey period"
# ------------------------------------------------------------------------------
date_cols <- if (exists("ilovedata_person")) {
  names(ilovedata_person)[vapply(ilovedata_person, function(x) inherits(x, c("Date", "POSIXt")), logical(1))]
} else {
  character()
}
if (length(date_cols)) {
  dated <- ilovedata_person %>% filter(person_id %in% dat$ID)
  for (dc in date_cols) {
    add_by_cohort(paste0(dc, "_min"), dated, function(i) format(min(as.Date(dated[[dc]][i]), na.rm = TRUE)),
      paste0("Earliest `", dc, "` among participants in `dat`"))
    add_by_cohort(paste0(dc, "_max"), dated, function(i) format(max(as.Date(dated[[dc]][i]), na.rm = TRUE)),
      paste0("Latest `", dc, "` among participants in `dat`"))
  }
} else {
  add("survey_period", "not available",
    paste(
      "The extract carries no response date. The ilovedata store keeps no response metadata: the live",
      "fetch allow-lists dictionary columns only, so Qualtrics' `startDate` (the variable the old frames",
      "used, as `startDate`) never reaches it (ilovedata issue 404). Exporting it per survey (`glad_med`,",
      "`glad_sa8_signup`) needs a pipeline change; this script then reports min and max per cohort from",
      "any Date column in data/ilovedata_person.rds"
    ))
}

# ------------------------------------------------------------------------------
section <- "Per-medication outcomes (dat_long)"
# ------------------------------------------------------------------------------
glmm_outcomes <- c(effectiveness = "Effectiveness", remission = "Remission",
  n_se = "Number of side effects", stopped_due_to_se = "Stopped because of side effects")
for (v in names(glmm_outcomes)) {
  known <- !is.na(dat_long[[v]])
  add_by_cohort(paste0(v, "_known_rows"), dat_long, function(i) pct_of(sum(known[i]), length(i)),
    paste0("Rows of `dat_long` with a known ", tolower(glmm_outcomes[[v]]), " (of all rows)"))
  add_by_cohort(paste0(v, "_known_participants"), dat_long, function(i) commafy(n_distinct(dat_long$ID[i][known[i]])),
    paste0("Participants with at least one row with a known ", tolower(glmm_outcomes[[v]])))
}

level_shares <- function(prefix, x, d, levels, labels, what, unit = "rows") {
  for (j in seq_along(levels)) {
    add_by_cohort(paste0(prefix, "_", labels[j]), d, function(i) {
      xi <- x[i]
      pct_of(sum(xi %in% levels[[j]]), sum(!is.na(xi)))
    }, paste0(what, ": ", names(labels)[j], " (of ", unit, " with a known value)"))
  }
}

level_shares("effectiveness", as.integer(as.character(dat_long$effectiveness)), dat_long,
  list(0, 1, 2), c("not at all" = "0", "moderately" = "1", "very well" = "2"), "Effectiveness")
add_by_cohort("remission_rows", dat_long, function(i) {
  x <- dat_long$remission[i]
  pct_of(sum(x == "1", na.rm = TRUE), sum(!is.na(x)))
}, "Rows with remission: worked moderately or very well and no further symptoms (of rows with known remission)")
add_by_cohort("stopped_due_to_se_rows", dat_long, function(i) {
  x <- dat_long$stopped_due_to_se[i]
  pct_of(sum(x == 1, na.rm = TRUE), sum(!is.na(x)))
}, "Rows stopped because of side effects (of rows with a known stop status)")

se_bins <- list(0, 1, 2, 3, 4, 5:9, 10:30)
names(se_bins) <- c("0", "1", "2", "3", "4", "5-9", "10 or more")
level_shares("n_se", dat_long$n_se, dat_long, se_bins,
  setNames(c("0", "1", "2", "3", "4", "5_9", "10plus"), names(se_bins)), "Number of side effects")
add_by_cohort("n_se_mean", dat_long, function(i) fmt_num(mean(dat_long$n_se[i], na.rm = TRUE)),
  "Mean number of side effects per medication (rows with a known count)")
add_by_cohort("n_se_max", dat_long, function(i) commafy(max(dat_long$n_se[i], na.rm = TRUE)),
  "Largest number of side effects ticked for one medication")

# Remission discordance (Methods, Remission): among participants with a known
# remission on two or more antidepressants, the share whose remission differs
# across them.
rem_person <- dat_long %>%
  filter(!is.na(remission)) %>%
  group_by(ID, cohort) %>%
  dplyr::summarise(k = n(), discordant = n_distinct(remission) > 1, .groups = "drop") %>%
  filter(k >= 2)
add_by_cohort("remission_discordant_participants", rem_person,
  function(i) pct_of(sum(rem_person$discordant[i]), length(i)),
  "Participants whose remission differs across their antidepressants (of those with known remission on two or more)")

# ------------------------------------------------------------------------------
section <- "Per-participant outcomes (dat)"
# ------------------------------------------------------------------------------
legacy_outcomes <- list(
  se_rating = list(label = "Side effect severity rating", levels = 0:5,
    names = c("0 (no side effects)", "1", "2", "3", "4", "5 (worst)")),
  ben_rating = list(label = "Benefit rating", levels = 1:5, names = c("1", "2", "3", "4", "5")),
  n_best = list(label = "Number of best aspects", levels = 0:8, names = as.character(0:8)),
  first_imprv = list(label = "First improvement duration", levels = 0:5,
    names = c("0 (no improvement)", "1 (under a month)", "2", "3", "4", "5 (more than 12 months)"))
)
for (v in names(legacy_outcomes)) {
  o <- legacy_outcomes[[v]]
  x <- as.integer(as.character(dat[[v]]))
  add_by_cohort(paste0(v, "_known"), dat, function(i) pct_of(sum(!is.na(x[i])), length(i)),
    paste0("Participants with a known ", tolower(o$label), " (of all in `dat`); the analysed n before predictors"))
  level_shares(v, x, dat, as.list(o$levels), setNames(as.character(o$levels), o$names), o$label,
    unit = "participants")
}
# Treatment discontinuation and mean duration are continuous predictors (user
# rulings 2026-11-09): `intolerance` is the proportion of the antidepressants
# with a known stop status that were stopped because of side effects x 10,
# `time` the mean duration per antidepressant in years.
add_by_cohort("intolerance_known", dat, function(i) pct_of(sum(!is.na(dat$intolerance[i])), length(i)),
  "Participants with a known treatment discontinuation proportion, i.e. at least one antidepressant with a known stop status (of all in `dat`)")
add_by_cohort("intolerance_mean_pct", dat, function(i) fmt_num(10 * mean(dat$intolerance[i], na.rm = TRUE), 1),
  "Mean percentage of a participant's antidepressants with a known stop status that were stopped because of side effects (of participants with a known value)")
add_by_cohort("intolerance_any", dat, function(i) {
  x <- dat$intolerance[i]
  pct_of(sum(x > 0, na.rm = TRUE), sum(!is.na(x)))
}, "Participants who stopped at least one antidepressant because of side effects (of participants with a known value)")
add_by_cohort("mean_duration_known", dat, function(i) pct_of(sum(!is.na(dat$time[i])), length(i)),
  "Participants with a known mean duration per antidepressant, i.e. a duration for at least one drug (of all in `dat`)")
add_by_cohort("mean_duration_median_years", dat, function(i) {
  q <- stats::quantile(dat$time[i], c(0.5, 0.25, 0.75), na.rm = TRUE)
  paste0(fmt_num(q[[1]], 2), " (IQR ", fmt_num(q[[2]], 2), "-", fmt_num(q[[3]], 2), ")")
}, "Median (IQR) of the per-participant mean duration per antidepressant in years: each drug's duration band at its midpoint (more than 4 years counted as 5), averaged over the drugs with a duration")
add_by_cohort("mean_n_se_mean", dat, function(i) fmt_num(mean(dat$mean_n_se[i], na.rm = TRUE)),
  "Mean of the per-participant mean number of side effects (over drugs with a known count)")
add_by_cohort("mean_n_se_median", dat, function(i) fmt_num(median(dat$mean_n_se[i], na.rm = TRUE)),
  "Median of the per-participant mean number of side effects")
add_by_cohort("mean_n_se_known", dat, function(i) pct_of(sum(!is.na(dat$mean_n_se[i])), length(i)),
  "Participants with at least one drug with a known side-effect count")

# Share answering No to "Did you experience side effects from any medication(s)
# that you have taken?", the gate before the checklist.
if (exists("ilovedata_person")) {
  gate <- ilovedata_person %>%
    transmute(ID = person_id, gate = answer_value(sideeffects.experience_side_effects_medication)) %>%
    inner_join(select(dat, ID, cohort), by = "ID")
  add_by_cohort("no_side_effects_from_any_medication", gate,
    function(i) pct_of(sum(gate$gate[i] == 0, na.rm = TRUE), sum(!is.na(gate$gate[i]))),
    "Participants answering No to having had side effects from any medication (of those who answered)")
}

# Coverage of family history (Sensitivity analyses): the share of the
# longitudinal rows, and of the participants, with a known value.
add_by_cohort("fh_diagnosis_types_known_rows", dat_long,
  function(i) pct_of(sum(!is.na(dat_long$fh_diagnosis_types[i])), length(i)),
  "Rows of `dat_long` with known family history (`fh_diagnosis_types`)")
add_by_cohort("fh_diagnosis_types_known_participants", dat,
  function(i) pct_of(sum(!is.na(dat$fh_diagnosis_types[i])), length(i)),
  "Participants in `dat` with known family history (`fh_diagnosis_types`)")

# ------------------------------------------------------------------------------
section <- "From the cached models"
# ------------------------------------------------------------------------------
set_attr <- function(sets, a) {
  vapply(sets, function(s) {
    v <- if (is.null(s)) NULL else attr(s, a)
    if (is.null(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
}
range_text <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  paste0(fmt_num(median(x), digits), " (", fmt_num(min(x), digits), " to ", fmt_num(max(x), digits), ")")
}

if (all(objects_present("glmm_models"))) {
  for (dep in names(glmm_models)) {
    k <- cohort_key(dep)
    add(paste0("glmm_max_n_", k), commafy(max(set_attr(glmm_models[[dep]], "n"), na.rm = TRUE)),
      paste0("Largest complete-case N (medication observations) over the unadjusted ", dep, " models"))
    add(paste0("glmm_max_n_id_", k), commafy(max(set_attr(glmm_models[[dep]], "n_id"), na.rm = TRUE)),
      paste0("Largest number of participants over the unadjusted ", dep, " models"))
    icc <- set_attr(glmm_models[[dep]], "icc")
    if (any(!is.na(icc))) {
      add(paste0("glmm_icc_", k), range_text(icc),
        paste0("Participant random-intercept ICC, median (min to max) over the unadjusted ", dep, " models (logit link only)"))
    }
  }
  add("glmm_max_n_id_any", commafy(max(unlist(map(glmm_models, ~ set_attr(.x, "n_id"))), na.rm = TRUE)),
    "Participants in the largest unadjusted longitudinal model, any outcome (\"had usable records\")")
} else {
  add("glmm_sample_sizes", skipped("`glmm_models` from src/02.regression.r"), "Longitudinal model N and ICC")
}

# Pearson dispersion of the number-of-side-effects Poisson models, recorded on
# each fit by fit_glmm() (attribute `dispersion`).
for (obj in c("glmm_models", "glmm_models_cov")) {
  kind <- if (obj == "glmm_models") "unadjusted" else "adjusted"
  key <- paste0("n_se_pearson_dispersion_", kind)
  if (!all(objects_present(obj))) {
    add(key, skipped(paste0("`", obj, "` from src/02.regression.r")),
      "Pearson dispersion of the Poisson number-of-side-effects models")
    next
  }
  sets <- get(obj)[["Number of Side Effects"]]
  d <- set_attr(sets, "dispersion")
  if (all(is.na(d))) {
    add(key, "skipped: the cached fits carry no dispersion",
      paste0("`", obj, "` was fitted before fit_glmm() recorded the Pearson dispersion; a refit records it"))
  } else {
    add(key, range_text(d),
      paste0("Pearson dispersion (sum of squared Pearson residuals / residual df), median (min to max) over the ",
        sum(!is.na(d)), " ", kind, " number-of-side-effects Poisson models (`", obj, "`)"))
    first <- which(!is.na(d))[1]
    add(paste0(key, "_first_set"), fmt_num(d[[first]]),
      paste0("Pearson dispersion of the first fitted ", kind, " number-of-side-effects model (",
        clean_label(sets[[first]]$Parameter[1]), ")"))
  }
}

# Adjusted within-person and between-person estimates of the two medication-level
# adjustment covariates (user ruling 2026-11-10): in `glmm_models_cov` each of
# start age and the cumulative medication count has one focal set of its own,
# its within and between terms, adjusted for the other covariates (sex, PHQ-9
# and the other of the two) but not for its own whole version.
wb_covariates <- c(start_age = "start age", cumulative_med_count = "cumulative medication count")
if (all(objects_present("glmm_models_cov"))) {
  adj <- multi_adjust(glmm_models_cov)
  for (v in names(wb_covariates)) {
    terms <- within_between_terms(v)
    term_labels <- within_between_labels(v, labels)
    others_text <- paste(c("sex", unname(wb_covariates[setdiff(names(wb_covariates), v)]), "PHQ-9"), collapse = ", ")
    for (dep in names(adj)) {
      st <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), adj[[dep]])
      if (!length(st)) {
        add(paste0("adjusted_", cohort_key(dep), "_", v), "not fitted",
          paste0("Adjusted within/between estimates of ", wb_covariates[[v]], " for ", dep))
        next
      }
      st <- st[[1]]
      effect <- if (dep == "Number of Side Effects") "RR" else "OR"
      conv <- attr(st, "converged")
      for (k in seq_along(terms)) {
        r <- st[st$Parameter == term_labels[[k]], ]
        ci <- r[[grep("^Coeff_CI", colnames(r), value = TRUE)[1]]]
        pv <- r[[grep("^p", colnames(r), value = TRUE)[1]]]
        add(paste0("adjusted_", cohort_key(dep), "_", terms[[k]]),
          paste0(ci, ", p = ", fmt_p(pv), if (!is.null(conv) && !isTRUE(conv)) " (convergence warning)" else ""),
          paste0(effect, " [95% CI], Bonferroni-corrected p (x", length(adj), "), for ", term_labels[[k]], " on ", dep,
            "; one model with both terms, adjusted for ", others_text, " (not for ", wb_covariates[[v]],
            " itself), N = ", commafy(attr(st, "n")[1]), " observations / ", commafy(attr(st, "n_id")), " participants"))
      }
    }
  }
} else {
  add("adjusted_within_between_covariates", skipped("`glmm_models_cov` from src/02.regression.r"),
    "Adjusted within/between estimates of start age and cumulative medication count")
}

# Sensitivity analysis: the adjusted models refitted with family history added
# to the covariates (glmm_models_cov_famhist) against the main adjusted models.
if (all(objects_present(c("glmm_models_cov", "glmm_models_cov_famhist")))) {
  flat <- function(models) {
    imap_dfr(multi_adjust(models), function(sets, dep) {
      map_dfr(sets, function(s) {
        if (is.null(s) || !nrow(s)) {
          return(NULL)
        }
        tibble(dep = dep, Parameter = s$Parameter, est = s$Coefficient, p = s$p, n = attr(s, "n")[1])
      })
    })
  }
  main <- flat(glmm_models_cov)
  sens <- flat(glmm_models_cov_famhist)
  both <- inner_join(main, sens, by = c("dep", "Parameter"), suffix = c("_main", "_sens")) %>%
    filter(Parameter != labels[["fh_diagnosis_types"]])
  add("sensitivity_coverage", fmt_num(100 * median(both$n_sens / both$n_main), 1),
    "Median over matched parameters of N(sensitivity model) / N(main adjusted model), in percent")
  add("sensitivity_median_abs_change_log_estimate", fmt_num(median(abs(log(both$est_sens) - log(both$est_main))), 3),
    "Median absolute change in the log odds (rate) ratio between the main adjusted and the family-history-adjusted models")
  changed <- (both$p_main <= 0.05) != (both$p_sens <= 0.05)
  add("sensitivity_significance_changes", paste0(sum(changed, na.rm = TRUE), " of ", sum(!is.na(changed))),
    "Matched associations whose Bonferroni-corrected significance (p <= 0.05) differs between the two adjustment sets")
} else {
  add("sensitivity_comparison",
    skipped("`glmm_models_cov` and `glmm_models_cov_famhist` from src/02.regression.r"),
    "Family-history sensitivity analysis: coverage, median change in log estimate, significance changes")
}

# Significance changes between the main adjusted set and a sensitivity set,
# counted on the unrounded Bonferroni-corrected p values carried by the cached
# models (the tables print p rounded to three decimals, so a count read off
# them can misplace a p near 0.05). Associations are matched by outcome and
# parameter label; a parameter estimated in only one of the two sets (the
# sensitivity set's own contrasts) is not matched. For a hurdle model the first
# (logistic) part is used, as in the tables.
flat_p <- function(models, n_deps = length(models)) {
  imap_dfr(multi_adjust(models, n_deps = n_deps), function(sets, dep) {
    map_dfr(sets, function(s) {
      if (is.null(s) || !nrow(s)) {
        return(NULL)
      }
      pc <- grep("^p", colnames(s), value = TRUE)[1]
      cc <- grep("^Coefficient", colnames(s), value = TRUE)[1]
      tibble(dep = dep, Parameter = s$Parameter, est = s[[cc]], p = s[[pc]], n = as.numeric(attr(s, "n")[1]))
    })
  })
}
add_significance_changes <- function(prefix, main, sens, what) {
  both <- inner_join(main, sens, by = c("dep", "Parameter"), suffix = c("_main", "_sens"), multiple = "first")
  sig_main <- both$p_main <= 0.05
  sig_sens <- both$p_sens <= 0.05
  lost <- which(sig_main & !sig_sens)
  gained <- which(!sig_main & sig_sens)
  name_of <- function(i) {
    if (!length(i)) {
      return("none")
    }
    paste0(
      both$dep[i], ": ", clean_label(both$Parameter[i]), " (p ", fmt_p(both$p_main[i]), " -> ", fmt_p(both$p_sens[i]), ")",
      collapse = "; "
    )
  }
  n_matched <- sum(!is.na(sig_main) & !is.na(sig_sens))
  add(paste0(prefix, "_significance_lost"), commafy(length(lost)),
    paste0("Matched associations significant (Bonferroni-corrected p <= 0.05, unrounded) in the main adjusted ",
      what, " models and not in the sensitivity set"))
  add(paste0(prefix, "_significance_gained"), commafy(length(gained)),
    paste0("Matched associations not significant in the main adjusted ", what,
      " models and significant in the sensitivity set (unrounded p)"))
  add(paste0(prefix, "_significance_changes"), paste0(commafy(length(lost) + length(gained)), " of ", commafy(n_matched)),
    paste0("Matched ", what, " associations whose significance status differs between the main adjusted and the ",
      "sensitivity set (lost + gained), of all matched associations; unrounded Bonferroni-corrected p"))
  add(paste0(prefix, "_significance_lost_associations"), name_of(lost),
    "Outcome: parameter (p main -> p sensitivity, Bonferroni-corrected) of the associations that lost significance")
  add(paste0(prefix, "_significance_gained_associations"), name_of(gained),
    "Outcome: parameter (p main -> p sensitivity, Bonferroni-corrected) of the associations that gained significance")
  add(paste0(prefix, "_median_abs_change_log_estimate"), fmt_num(median(abs(log(both$est_sens) - log(both$est_main)), na.rm = TRUE), 3),
    paste0("Median absolute change in the log odds (rate) ratio between the main adjusted and the sensitivity ",
      what, " models, over matched associations"))
}

# ------------------------------------------------------------------------------
section <- "Drug-class sensitivity analysis"
# ------------------------------------------------------------------------------
# The class-scoped frame (lib/10.drug_class.r) and the class contrasts of the
# drug-class sensitivity set `glmm_models_cov_class` (user ruling 2026-11-09).
class_scope <- attr(apply_class_scope(dat_long), "class_scope")
add("class_scope_rows", commafy(class_scope$rows_after),
  "Rows of the class-scoped `dat_long`: the drugs without a retained class removed")
add("class_scope_participants", commafy(class_scope$participants_after),
  "Participants in the class-scoped `dat_long`")
add("class_scope_rows_dropped", commafy(class_scope$rows_dropped),
  paste0("Rows removed by the class scope: ", paste0(names(class_scope$rows_dropped_by_drug), " ",
    commafy(as.integer(class_scope$rows_dropped_by_drug)), collapse = ", ")))
add("class_scope_participants_leaving", commafy(class_scope$participants_before - class_scope$participants_after),
  "Participants whose only medications are drugs without a retained class, so who leave the class-scoped frame")
for (k in names(class_scope$rows_by_class)) {
  add(paste0("class_scope_rows_", k), pct_of(as.integer(class_scope$rows_by_class[[k]]), class_scope$rows_after),
    paste0("Class-scoped rows of class ", DRUG_CLASS_LABELS[[k]], " (of all class-scoped rows)"))
}

if (all(objects_present("glmm_models_cov_class"))) {
  class_terms <- unname(drug_class_labels())
  class_adj <- multi_adjust(glmm_models_cov_class)
  for (dep in names(class_adj)) {
    s <- Filter(function(x) !is.null(x) && any(x$Parameter %in% class_terms), class_adj[[dep]])
    if (!length(s)) {
      add(paste0("class_effects_", cohort_key(dep)), "not fitted", paste0("Drug-class contrasts for ", dep))
      next
    }
    s <- s[[1]]
    effect <- if (dep == "Number of Side Effects") "RR" else "OR"
    for (term in class_terms) {
      r <- s[s$Parameter == term, ]
      contrast <- cohort_key(sub("^Drug class: ", "", term))
      add(paste0("class_", cohort_key(dep), "_", contrast),
        paste0(r$Coeff_CI, ", p = ", fmt_p(r$p), if (!isTRUE(attr(s, "converged"))) " (convergence warning)" else ""),
        paste0(effect, " [95% CI], Bonferroni-corrected p (x4), for ", term, " on ", dep,
          "; adjusted for sex, start age, cumulative medication count and PHQ-9, class-scoped frame, N = ",
          commafy(attr(s, "n")[1]), " observations / ", commafy(attr(s, "n_id")), " participants"))
    }
  }
  # Matched on the parameter labels; the class-scoped models have fewer rows, so
  # a change can reflect the scope as well as the class covariates.
  if (all(objects_present("glmm_models_cov"))) {
    add_significance_changes("class_sensitivity", flat_p(glmm_models_cov), flat_p(glmm_models_cov_class),
      "longitudinal")
  }
} else {
  add("class_effects", skipped("`glmm_models_cov_class` from src/02.regression.r"),
    "Drug-class contrasts (TCA, SNRI, NaSSA, SARI vs SSRI) for the four longitudinal outcomes")
}

# ------------------------------------------------------------------------------
section <- "Survey-cohort sensitivity analysis"
# ------------------------------------------------------------------------------
# The adjusted models with the survey cohort (SA8 vs Baseline) added to the
# covariates (user ruling 2026-11-09): `glmm_models_cov_cohort` on the full
# longitudinal frame, `sef_models_cohort` / `eff_models_cohort` per participant.
# The cohort contrast of each outcome comes from the set that has the cohort as
# its focal predictor.
cohort_term <- labels[["cohort"]]
add_cohort_contrasts <- function(models, n_deps, what, adjusted_for, unit) {
  adj <- multi_adjust(models, n_deps = n_deps)
  for (dep in names(adj)) {
    s <- Filter(function(x) !is.null(x) && any(x$Parameter == cohort_term), adj[[dep]])
    key <- paste0("cohort_", cohort_key(dep), "_sa8_vs_baseline")
    if (!length(s)) {
      add(key, "not fitted", paste0("Survey-cohort contrast (SA8 vs Baseline) for ", dep))
      next
    }
    s <- s[[1]]
    r <- s[s$Parameter == cohort_term, ]
    effect <- if (dep == "Number of Side Effects") "RR" else "OR"
    ci <- r[[grep("^Coeff_CI", colnames(r), value = TRUE)[1]]]
    p <- r[[grep("^p", colnames(r), value = TRUE)[1]]]
    conv <- attr(s, "converged")
    add(key,
      paste0(ci, ", p = ", fmt_p(p), if (!is.null(conv) && !isTRUE(conv)) " (convergence warning)" else ""),
      paste0(effect, " [95% CI], Bonferroni-corrected p (x", n_deps, "), for SA8 vs Baseline on ", dep, " (", what,
        "); adjusted for ", adjusted_for, ", N = ", commafy(attr(s, "n")[1]), " ", unit,
        if (!is.null(attr(s, "n_id"))) paste0(" / ", commafy(attr(s, "n_id")), " participants") else ""))
  }
}

if (all(objects_present(c("glmm_models_cov", "glmm_models_cov_cohort")))) {
  add_cohort_contrasts(glmm_models_cov_cohort, length(glmm_models_cov_cohort), "longitudinal",
    "sex, start age, cumulative medication count and PHQ-9", "observations")
  add_significance_changes("cohort_sensitivity_longitudinal", flat_p(glmm_models_cov), flat_p(glmm_models_cov_cohort),
    "longitudinal")
} else {
  add("cohort_effects_longitudinal", skipped("`glmm_models_cov` and `glmm_models_cov_cohort` from src/02.regression.r"),
    "Survey-cohort contrasts and significance changes for the four longitudinal outcomes")
}

legacy_cohort_needs <- c("sef_models", "eff_models", "sef_models_compete", "eff_models_compete",
  "sef_models_cohort", "eff_models_cohort")
if (all(objects_present(legacy_cohort_needs))) {
  n_legacy <- length(c(sef_models, eff_models))
  add_cohort_contrasts(c(sef_models_cohort, eff_models_cohort), n_legacy, "per-participant",
    "sex, average starting age and the number of psychiatric diagnosis types in the immediate family", "participants")
  add_significance_changes("cohort_sensitivity_per_participant",
    flat_p(c(sef_models_compete, eff_models_compete), n_deps = n_legacy),
    flat_p(c(sef_models_cohort, eff_models_cohort), n_deps = n_legacy),
    "per-participant")
} else {
  add("cohort_effects_per_participant",
    skipped(paste0("`", paste(legacy_cohort_needs[!objects_present(legacy_cohort_needs)], collapse = "`, `"),
      "` from src/02.regression.r")),
    "Survey-cohort contrasts and significance changes for the four per-participant outcomes")
}

# ------------------------------------------------------------------------------
# Write
# ------------------------------------------------------------------------------
numbers <- bind_rows(numbers)
md <- c(
  "# Manuscript numbers — current values",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/05.manuscript_numbers.r` from `dat`,"),
  "`dat_long`, the ilovedata extracts and, where present, the cached model objects.",
  "Every value is computed; none is typed in. Shares are of rows or participants",
  "with a known value unless the definition says otherwise. Keys ending in a cohort",
  paste0("name (", paste0("`", cohort_key(cohorts), "`", collapse = ", "), ") or `overall` give that group."),
  ""
)
for (s in unique(numbers$section)) {
  rows <- filter(numbers, section == s)
  md <- c(md, paste0("## ", s), "", md_table(c("Key", "Value", "Definition"), select(rows, key, value, definition)), "")
}
writeLines(md, out_path)
message("Wrote ", out_path, " (", nrow(numbers), " numbers)")

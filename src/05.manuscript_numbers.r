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

# Estimate keys from the cached models: "OR[CI], p = ..." (estimate [95% CI],
# Bonferroni-corrected p), with "(convergence warning)" appended for a fit that
# reported one. For a hurdle model the first (logistic) part is used, as in the
# tables. `adj` is a family already passed through multi_adjust().
est_text <- function(st, label) {
  r <- st[st$Parameter == label, ]
  ci <- r[[grep("^Coeff_CI", colnames(r), value = TRUE)[1]]][1]
  pv <- r[[grep("^p", colnames(r), value = TRUE)[1]]][1]
  conv <- attr(st, "converged")
  paste0(ci, ", p = ", fmt_p(pv), if (!is.null(conv) && !isTRUE(conv)) " (convergence warning)" else "")
}
effect_of <- function(dep) if (dep == "Number of Side Effects") "RR" else "OR"
n_text <- function(st) {
  paste0("N = ", commafy(attr(st, "n")[1]),
    if (!is.null(attr(st, "n_id"))) paste0(" observations / ", commafy(attr(st, "n_id")), " participants") else " participants")
}

# The decomposed medication-level predictors (lib/09.within_between.r) and the
# outcomes each is decomposed for: start age and the cumulative medication
# count on all four longitudinal outcomes, the two side-effect measures on
# effectiveness and remission only.
wb_vars_all <- c(n_se = "number of side effects", stopped_due_to_se = "stopped because of side effects",
  start_age = "start age", cumulative_med_count = "cumulative medication count")
wb_vars_for <- function(dep) {
  if (dep %in% c("Effectiveness", "Remission")) names(wb_vars_all) else c("start_age", "cumulative_med_count")
}
# "adjusted for sex, start age, ..., PHQ-9[, extra]", without the predictor's
# own whole version in its own set (covs_for_set()).
wb_adjusted_for <- function(v, extra = NULL) {
  own <- v %in% c("start_age", "cumulative_med_count")
  others <- c("sex", unname(wb_vars_all[setdiff(c("start_age", "cumulative_med_count"), v)]), "PHQ-9", extra)
  paste0("adjusted for ", paste(others, collapse = ", "),
    if (own) paste0(" (not for ", wb_vars_all[[v]], " itself)") else "")
}

# One key per within and between term, `<prefix>_<outcome>_<term>`, for every
# decomposed predictor `vars_for(outcome)` of the family `adj`. `model_text(v)`
# describes the model's adjustment.
add_wb_estimates <- function(prefix, adj, n_deps, model_text, vars_for = wb_vars_for, what = "") {
  for (dep in names(adj)) {
    for (v in vars_for(dep)) {
      term_labels <- within_between_labels(v, labels)
      terms <- within_between_terms(v)
      st <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), adj[[dep]])
      if (!length(st)) {
        add(paste0(prefix, "_", cohort_key(dep), "_", v), "not fitted",
          paste0(what, "within/between estimates of ", wb_vars_all[[v]], " for ", dep))
        next
      }
      st <- st[[1]]
      for (k in seq_along(terms)) {
        add(paste0(prefix, "_", cohort_key(dep), "_", terms[[k]]), est_text(st, term_labels[[k]]),
          paste0(effect_of(dep), " [95% CI], Bonferroni-corrected p (x", n_deps, "), for ", term_labels[[k]], " on ",
            dep, "; ", what, "one model with both terms, ", model_text(v), ", ", n_text(st)))
      }
    }
  }
}

# One key for one parameter `label` of outcome `dep` in the family `adj`, from
# the first set that estimates it.
add_param_estimate <- function(key, adj, dep, label, n_deps, model_text) {
  st <- Filter(function(x) !is.null(x) && label %in% x$Parameter, adj[[dep]])
  if (!length(st)) {
    add(key, "not fitted", paste0(clean_label(label), " on ", dep, "; ", model_text))
    return(invisible())
  }
  st <- st[[1]]
  add(key, est_text(st, label),
    paste0(effect_of(dep), " [95% CI], Bonferroni-corrected p (x", n_deps, "), for ", clean_label(label), " on ", dep,
      "; ", model_text, ", ", n_text(st)))
}

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
# munge's own rules (munge/01.dat_clean.r, "WHO ENTERS"). The ticks are read
# from the munge's `person_drug`, the extract after the glad_med nortriptyline
# recode (ruling `nort_collision`); a recoded tick was the first list's "a
# different antidepressant", so it counts as an answer to the list, not as a
# named drug.
if (exists("ilovedata_person") && exists("person_drug") && "took_recoded" %in% names(person_drug)) {
  person <- ilovedata_person %>% filter(cohort %in% cohorts)
  named_tick <- person_drug %>%
    group_by(person_id) %>%
    dplyr::summarise(any_named = any(is_tick(took)), any_recoded = any(!is.na(took_recoded)),
                     .groups = "drop")
  other_cols <- intersect(c("prescription.none", "prescription.a_different_antidepressant.1"), names(person))
  flow <- person %>%
    left_join(named_tick, by = "person_id") %>%
    dplyr::mutate(
      any_named = coalesce(any_named, FALSE),
      any_other = (if (length(other_cols)) rowSums(across(all_of(other_cols), is_tick)) > 0 else FALSE) |
        coalesce(any_recoded, FALSE),
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
  add_by_cohort("takers_excluded", flow,
    function(i) commafy(sum(flow$any_named[i] & !flow$in_dat[i])),
    paste0("Named-antidepressant takers left out by the diagnosis filter: `takers_neither_depression_nor_anxiety` ",
      "plus `takers_diagnosis_unknown`"))
} else {
  add("who_enters", skipped("data/ilovedata_person.rds and the munge's `person_drug` (with `took_recoded`)"),
    "The inclusion funnel is recomputed from the extracts")
}

# ------------------------------------------------------------------------------
section <- "Nortriptyline correction"
# ------------------------------------------------------------------------------
# The glad_med nortriptyline recode (ruling `nort_collision`, 2026-11-11): the
# munge (munge/01.dat_clean.r, "glad_med NORTRIPTYLINE COLLISION") leaves its
# counts in `nort_recode_summary`.
if (exists("nort_recode_summary")) {
  nrs <- nort_recode_summary
  add("nort_responses_total", commafy(nrs$ticks),
    "glad_med nortriptyline ticks in the extract, before the recode")
  add("nort_recoded_responses", commafy(nrs$recoded),
    paste0("glad_med nortriptyline ticks recoded to not ticked (the pre-edit \"a different antidepressant\"): ",
      "no stored value in the hidden list's nortriptyline choice (QID10_12), or a stored value in its \"None\" ",
      "choice (QID10_0)"))
  add("nort_kept_responses", commafy(nrs$kept),
    "glad_med nortriptyline ticks kept as genuine (QID10_12 stored, QID10_0 not)")
  add("nort_rule_history_disagreements", commafy(nrs$rule_history_disagreements),
    paste0("glad_med nortriptyline ticks on which the rule and the display of the prescription-history page ",
      "disagree: ", commafy(nrs$kept_no_history), " kept without a history page, ",
      commafy(nrs$recoded_with_history), " recoded with one"))
  add("nort_participants_no_named_drug", commafy(nrs$participants_no_named_drug),
    "People whose only named antidepressant was a recoded tick, so who have no named drug after the recode (any diagnosis)")
  add("nort_participants_leaving", commafy(nrs$participants_leaving_dat),
    paste0("Of these, the people with a known depressive or anxiety disorder, i.e. who would otherwise be in ",
      "`dat` and `dat_long` and leave the analytic sample through the recode"))
} else {
  add("nort_correction", skipped("`nort_recode_summary` from munge/01.dat_clean.r"),
    "Counts of the glad_med nortriptyline recode")
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
add_by_cohort("female_pct", dat, function(i) pct_of(sum(dat$sex[i] == "Female", na.rm = TRUE), sum(!is.na(dat$sex[i]))),
  "Female participants in `dat` (of participants with a known sex), as in Table 1")
add("cohort_overlap",commafy(length(intersect(dat$ID[dat$cohort == cohorts[1]], dat$ID[dat$cohort != cohorts[1]]))),
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
add_by_cohort("effectiveness_unknown_rows", dat_long,
  function(i) pct_of(sum(is.na(dat_long$effectiveness[i])), length(i)),
  paste0("Rows of `dat_long` with no known effectiveness (\"don't know\", unanswered or not shown; of all rows); ",
    "the Unknown row of Table 2"))

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

# The other longitudinal estimates the text quotes: the unadjusted
# within/between estimates of all four decomposed predictors, the main adjusted
# ones of the two side-effect measures (on effectiveness and remission; start
# age and the cumulative count are above), the severity rating (constant within
# person, one term) on effectiveness and remission, and being married on
# stopping because of side effects.
glmm_extra <- list(
  unadjusted = list(obj = "glmm_models", text = function(v) "no covariates (participant random intercept only)"),
  adjusted = list(obj = "glmm_models_cov", text = function(v) wb_adjusted_for(v))
)
married_label <- labels[["Not_in_relationship_Married"]]
for (kind in names(glmm_extra)) {
  g <- glmm_extra[[kind]]
  if (!all(objects_present(g$obj))) {
    add(paste0(kind, "_longitudinal_estimates"), skipped(paste0("`", g$obj, "` from src/02.regression.r")),
      paste0("The ", kind, " longitudinal within/between, severity-rating and married estimates"))
    next
  }
  models <- get(g$obj)
  n_g <- length(models)
  adj_g <- multi_adjust(models, n_deps = n_g)
  add_wb_estimates(kind, adj_g, n_g, g$text,
    vars_for = if (kind == "adjusted") function(dep) setdiff(wb_vars_for(dep), names(wb_covariates)) else wb_vars_for,
    what = if (kind == "adjusted") "main adjusted model, " else "unadjusted model, ")
  rest_text <- if (kind == "adjusted") wb_adjusted_for("se_rating") else g$text(NULL)
  for (dep in c("Effectiveness", "Remission")) {
    add_param_estimate(paste0(kind, "_", cohort_key(dep), "_se_rating"), adj_g, dep, labels[["se_rating"]], n_g,
      rest_text)
  }
  add_param_estimate(paste0(kind, "_stopped_due_to_side_effects_married"), adj_g, "Stopped due to Side Effects",
    married_label, n_g, rest_text)
}

# Per-participant estimates quoted in the text (Tables 3b, 4b, 4c and 4e): the
# mean duration per antidepressant on the four per-participant outcomes and the
# treatment discontinuation proportion on the three effectiveness outcomes,
# unadjusted (`sef_models`, `eff_models`) and adjusted (`*_compete`), both
# Bonferroni-corrected by the four per-participant outcomes.
legacy_needs <- c("sef_models", "eff_models", "sef_models_compete", "eff_models_compete")
if (all(objects_present(legacy_needs))) {
  n_legacy <- length(c(sef_models, eff_models))
  legacy_fams <- list(
    unadjusted = list(adj = multi_adjust(c(sef_models, eff_models), n_deps = n_legacy), text = "unadjusted"),
    adjusted = list(adj = multi_adjust(c(sef_models_compete, eff_models_compete), n_deps = n_legacy),
      text = "adjusted for sex, average starting age and the number of psychiatric diagnosis types in the immediate family")
  )
  # The cached `labels` lacks these two; the text is that of `labels_extra` in
  # src/02.regression.r, which is baked into the cached fits.
  legacy_preds <- c(mean_duration = "Mean duration per antidepressant (years)",
    proportion_stopped = "Treatment discontinuation (per 10% of antidepressants stopped)")
  for (kind in names(legacy_fams)) {
    f <- legacy_fams[[kind]]
    for (dep in names(f$adj)) {
      # The discontinuation proportion is a predictor of the effectiveness
      # outcomes only.
      preds <- if (dep %in% names(eff_models)) names(legacy_preds) else "mean_duration"
      for (p in preds) {
        add_param_estimate(paste0(kind, "_", cohort_key(dep), "_", p), f$adj, dep, legacy_preds[[p]], n_legacy,
          paste0("per-participant model, ", f$text))
      }
    }
  }
} else {
  add("per_participant_estimates",
    skipped(paste0("`", paste(legacy_needs[!objects_present(legacy_needs)], collapse = "`, `"), "` from src/02.regression.r")),
    "Per-participant estimates of the mean duration and the treatment discontinuation proportion")
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

# The family-history sensitivity set in the same form: the significance
# changes split into lost and gained (the total above is
# `sensitivity_significance_changes`; family history itself is not matched,
# as there) and its within/between estimates.
if (all(objects_present(c("glmm_models_cov", "glmm_models_cov_famhist")))) {
  n_fh <- length(glmm_models_cov)
  add_significance_changes("famhist", flat_p(glmm_models_cov, n_deps = n_fh),
    flat_p(glmm_models_cov_famhist, n_deps = n_fh) %>% filter(Parameter != labels[["fh_diagnosis_types"]]),
    "longitudinal")
  add_wb_estimates("famhist", multi_adjust(glmm_models_cov_famhist, n_deps = n_fh), n_fh,
    function(v) wb_adjusted_for(v, "the number of psychiatric diagnosis types in the immediate family"),
    what = "family-history sensitivity set, ")
} else {
  add("famhist_estimates", skipped("`glmm_models_cov` and `glmm_models_cov_famhist` from src/02.regression.r"),
    "Family-history sensitivity analysis: significance changes and within/between estimates")
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
  add_wb_estimates("class_adjusted", class_adj, length(class_adj),
    function(v) paste0(wb_adjusted_for(v, "drug class (TCA, SNRI, NaSSA, SARI vs SSRI)"), ", class-scoped frame"),
    what = "drug-class sensitivity set, ")
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
  # The cohort-adjusted estimates the survey-cohort paragraph quotes.
  n_co <- length(glmm_models_cov_cohort)
  cohort_adj <- multi_adjust(glmm_models_cov_cohort, n_deps = n_co)
  add_wb_estimates("cohort_adjusted", cohort_adj, n_co, function(v) wb_adjusted_for(v, "survey cohort"),
    what = "survey-cohort sensitivity set, ")
  for (dep in c("Effectiveness", "Remission")) {
    add_param_estimate(paste0("cohort_adjusted_", cohort_key(dep), "_se_rating"), cohort_adj, dep,
      labels[["se_rating"]], n_co, wb_adjusted_for("se_rating", "survey cohort"))
  }
  add_param_estimate("cohort_adjusted_remission_student", cohort_adj, "Remission",
    labels[["In_paid_employment_or_self_employed_Full_or_part_time_student"]], n_co,
    wb_adjusted_for("employment", "survey cohort"))
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
section <- "Drug-adjusted within/between sensitivity analysis"
# ------------------------------------------------------------------------------
# `glmm_models_cov_drug` (user approval 2026-11-11): the within/between sets of
# the four medication-level predictors for effectiveness and remission,
# adjusted as in `glmm_models_cov` plus the individual antidepressant
# (lib/12.drug_identity.r). Bonferroni factor: the four longitudinal outcomes,
# although the set has two.
drug_info <- attr(add_drug_identity(dat_long), "drug_identity")
add("drug_identity_reference", drug_info$reference, "Reference drug of the drug-identity covariate")
add("drug_identity_contrasts", commafy(length(drug_info$levels) - 1),
  "Drug-identity fixed effects (drugs other than the reference, plus the pooled category)")
add("drug_identity_pooled_drugs", drug_identity_pooled_text(drug_info$pooled),
  paste0("Drugs with fewer than ", drug_info$min_rows, " rows of `dat_long` (rows in brackets), pooled into one ",
    "category of the drug-identity covariate"))
add("drug_identity_pooled_rows", commafy(sum(drug_info$pooled)),
  paste0("Rows of `dat_long` in the pooled drug category (", length(drug_info$pooled), " drugs)"))

drug_wb_vars <- c(n_se = "number of side effects", stopped_due_to_se = "stopped because of side effects",
  start_age = "start age", cumulative_med_count = "cumulative medication count")
if (all(objects_present(c("glmm_models_cov", "glmm_models_cov_drug")))) {
  n_glmm <- length(glmm_models_cov)
  drug_adj <- multi_adjust(glmm_models_cov_drug, n_deps = n_glmm)
  main_adj <- multi_adjust(glmm_models_cov, n_deps = n_glmm)
  row_text <- function(st, label) {
    r <- st[st$Parameter == label, ]
    ci <- r[[grep("^Coeff_CI", colnames(r), value = TRUE)[1]]]
    pv <- r[[grep("^p", colnames(r), value = TRUE)[1]]]
    conv <- attr(st, "converged")
    paste0(ci, ", p = ", fmt_p(pv), if (!is.null(conv) && !isTRUE(conv)) " (convergence warning)" else "")
  }
  for (dep in names(drug_adj)) {
    for (v in names(drug_wb_vars)) {
      term_labels <- within_between_labels(v, labels)
      terms <- within_between_terms(v)
      st <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), drug_adj[[dep]])
      sm <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), main_adj[[dep]])
      if (!length(st)) {
        add(paste0("drug_", cohort_key(dep), "_", v), "not fitted",
          paste0("Drug-adjusted within/between estimates of ", drug_wb_vars[[v]], " for ", dep))
        next
      }
      st <- st[[1]]
      own <- v %in% c("start_age", "cumulative_med_count")
      others <- c("sex", unname(drug_wb_vars[setdiff(c("start_age", "cumulative_med_count"), v)]), "PHQ-9")
      for (k in seq_along(terms)) {
        main_txt <- if (length(sm)) row_text(sm[[1]], term_labels[[k]]) else "not fitted"
        add(paste0("drug_", cohort_key(dep), "_", terms[[k]]),
          row_text(st, term_labels[[k]]),
          paste0("OR [95% CI], Bonferroni-corrected p (x", n_glmm, "), for ", term_labels[[k]], " on ", dep,
            "; one model with both terms, adjusted for ", paste(others, collapse = ", "),
            if (own) paste0(" (not for ", drug_wb_vars[[v]], " itself)") else "",
            " and the individual antidepressant, N = ", commafy(attr(st, "n")[1]), " observations / ",
            commafy(attr(st, "n_id")), " participants. Main adjusted model (no drug): ", main_txt))
      }
    }
  }
  # The matched associations are the within/between terms of the drug set
  # (16: two outcomes x four predictors x two terms).
  add_significance_changes("drug_sensitivity", flat_p(glmm_models_cov), flat_p(glmm_models_cov_drug, n_deps = n_glmm),
    "longitudinal")

  # How much of the drug adjustment's shift in the within-person estimate of
  # the number of side effects on effectiveness the class adjustment already
  # produces, on the log odds ratio scale and from the unrounded estimates:
  # (log OR class - log OR main) / (log OR drug - log OR main).
  share_key <- "class_share_of_drug_shift_effectiveness_n_se_within"
  if (all(objects_present("glmm_models_cov_class"))) {
    wl <- within_between_labels("n_se", labels)[["n_se_within"]]
    coef_of <- function(models) {
      st <- Filter(function(x) !is.null(x) && wl %in% x$Parameter, models[["Effectiveness"]])
      if (!length(st)) NA_real_ else st[[1]]$Coefficient[st[[1]]$Parameter == wl][1]
    }
    lo <- log(c(main = coef_of(glmm_models_cov), class = coef_of(glmm_models_cov_class),
      drug = coef_of(glmm_models_cov_drug)))
    share <- 100 * (lo[["class"]] - lo[["main"]]) / (lo[["drug"]] - lo[["main"]])
    add(share_key, if (is.na(share)) "not fitted" else paste0(fmt_num(share, 1), "%"),
      paste0("Share of the shift in the log odds ratio of ", wl, " on effectiveness from the main adjusted to the ",
        "drug-adjusted model that the drug-class adjustment already produces: (log OR class - log OR main) / ",
        "(log OR drug - log OR main), unrounded estimates (OR main ", fmt_num(exp(lo[["main"]]), 3), ", class ",
        fmt_num(exp(lo[["class"]]), 3), ", drug ", fmt_num(exp(lo[["drug"]]), 3),
        "); the class model is fitted on the class-scoped frame"))
  } else {
    add(share_key, skipped("`glmm_models_cov_class` from src/02.regression.r"),
      "Share of the drug-adjusted shift in the within-person estimate already produced by class adjustment")
  }
} else {
  add("drug_within_between", skipped("`glmm_models_cov` and `glmm_models_cov_drug` from src/02.regression.r"),
    "Drug-adjusted within/between estimates and significance changes for effectiveness and remission")
}

# ------------------------------------------------------------------------------
section <- "Zero-coded blank-checklist sensitivity analysis"
# ------------------------------------------------------------------------------
# `glmm_models_cov_zero` (owner ruling 2026-11-12, which made the strict rule
# the main rule; this section held the strict-rule set, keys `strict_*`, until
# then): a Yes-gated checklist recorded with nothing ticked and no "No side
# effects" tick, missing under the main rule, is coded as 0 side effects and
# not stopped (lib/13.zero_se.r). Eight adjusted within/between models,
# Bonferroni x4.
zero_rec <- zero_se_recoded(dat_long)
add_by_cohort("zero_blank_checklist_rows", dat_long, function(i) pct_of(sum(zero_rec[i]), length(i)),
  paste0("Rows of `dat_long` with Yes on the any-side-effects question, the checklist displayed and recorded, ",
    "nothing ticked and no \"No side effects\" tick: missing under the main rule, 0 side effects and not ",
    "stopped under the zero rule (of all rows)"))
add_by_cohort("zero_blank_checklist_participants", dat_long,
  function(i) commafy(n_distinct(dat_long$ID[i][zero_rec[i]])),
  "Participants with at least one blank checklist that the main rule leaves missing and the zero rule codes as 0")
zero_info <- zero_se_info(dat_long)
add("zero_participants_no_known_n_se", commafy(zero_info$participants_no_known_n_se_main),
  "Participants with no known number of side effects under the main rule who have one under the zero rule")
add("zero_n_se_known_rows", paste0(commafy(zero_info$n_se_known_main), " -> ", commafy(zero_info$n_se_known_zero)),
  "Rows of `dat_long` with a known number of side effects, main rule -> zero rule")
add("zero_stopped_known_rows", paste0(commafy(zero_info$stopped_known_main), " -> ",
  commafy(zero_info$stopped_known_zero)),
  "Rows of `dat_long` with a known stop status (stopped because of side effects), main rule -> zero rule")

zero_wb_vars <- c(n_se = "number of side effects", stopped_due_to_se = "stopped because of side effects",
  start_age = "start age", cumulative_med_count = "cumulative medication count")
zero_focal <- list(
  Effectiveness = c("n_se", "stopped_due_to_se"),
  Remission = c("n_se", "stopped_due_to_se"),
  "Number of Side Effects" = c("cumulative_med_count", "start_age"),
  "Stopped due to Side Effects" = c("cumulative_med_count", "start_age")
)
if (all(objects_present(c("glmm_models_cov", "glmm_models_cov_zero")))) {
  n_glmm <- length(glmm_models_cov)
  zero_adj <- multi_adjust(glmm_models_cov_zero, n_deps = n_glmm)
  main_adj <- multi_adjust(glmm_models_cov, n_deps = n_glmm)
  row_text <- function(st, label) {
    r <- st[st$Parameter == label, ]
    ci <- r[[grep("^Coeff_CI", colnames(r), value = TRUE)[1]]]
    pv <- r[[grep("^p", colnames(r), value = TRUE)[1]]]
    conv <- attr(st, "converged")
    paste0(ci, ", p = ", fmt_p(pv), if (!is.null(conv) && !isTRUE(conv)) " (convergence warning)" else "")
  }
  for (dep in names(zero_adj)) {
    for (v in zero_focal[[dep]]) {
      term_labels <- within_between_labels(v, labels)
      terms <- within_between_terms(v)
      st <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), zero_adj[[dep]])
      sm <- Filter(function(x) !is.null(x) && all(term_labels %in% x$Parameter), main_adj[[dep]])
      if (!length(st)) {
        add(paste0("zero_", cohort_key(dep), "_", v), "not fitted",
          paste0("Zero-rule within/between estimates of ", zero_wb_vars[[v]], " for ", dep))
        next
      }
      st <- st[[1]]
      own <- v %in% c("start_age", "cumulative_med_count")
      others <- c("sex", unname(zero_wb_vars[setdiff(c("start_age", "cumulative_med_count"), v)]), "PHQ-9")
      for (k in seq_along(terms)) {
        main_txt <- if (length(sm)) {
          paste0(row_text(sm[[1]], term_labels[[k]]), ", N = ", commafy(attr(sm[[1]], "n")[1]), " / ",
            commafy(attr(sm[[1]], "n_id")))
        } else {
          "not fitted"
        }
        add(paste0("zero_", cohort_key(dep), "_", terms[[k]]),
          row_text(st, term_labels[[k]]),
          paste0(if (dep == "Number of Side Effects") "RR" else "OR",
            " [95% CI], Bonferroni-corrected p (x", n_glmm, "), for ", term_labels[[k]], " on ", dep,
            " with blank side-effect checklists coded as zero; one model with both terms, adjusted for ",
            paste(others, collapse = ", "),
            if (own) paste0(" (not for ", zero_wb_vars[[v]], " itself)") else "",
            ", N = ", commafy(attr(st, "n")[1]), " observations / ", commafy(attr(st, "n_id")),
            " participants. Main rule: ", main_txt))
      }
    }
  }
  # The matched associations are the within/between terms of the zero-rule set
  # (16: four outcomes x two predictors x two terms).
  add_significance_changes("zero_sensitivity", flat_p(glmm_models_cov),
    flat_p(glmm_models_cov_zero, n_deps = n_glmm), "longitudinal")
} else {
  add("zero_within_between", skipped("`glmm_models_cov` and `glmm_models_cov_zero` from src/02.regression.r"),
    "Zero-rule blank-checklist within/between estimates and significance changes")
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

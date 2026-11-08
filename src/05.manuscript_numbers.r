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
    names = c("0 (no improvement)", "1 (under a month)", "2", "3", "4", "5 (more than 12 months)")),
  intolerance = list(label = "Treatment discontinuation band", levels = 1:3, names = c("1 (lowest)", "2", "3 (highest)"))
)
for (v in names(legacy_outcomes)) {
  o <- legacy_outcomes[[v]]
  x <- as.integer(as.character(dat[[v]]))
  add_by_cohort(paste0(v, "_known"), dat, function(i) pct_of(sum(!is.na(x[i])), length(i)),
    paste0("Participants with a known ", tolower(o$label), " (of all in `dat`); the analysed n before predictors"))
  level_shares(v, x, dat, as.list(o$levels), setNames(as.character(o$levels), o$names), o$label,
    unit = "participants")
}
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

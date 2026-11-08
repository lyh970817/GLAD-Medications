# Export the GLAD-Medications analysis inputs from an ilovedata store.
#
# Run inside the ilovedata pipeline's Nix environment, never the ambient R
# (the pipeline's engine needs packages only that environment has):
#
#   nix develop <pipeline> -c Rscript --no-init-file extract/ilovedata_export.R \
#     --pipeline <pipeline> --store <pipeline>/store.duckdb --out data
#
# `--no-init-file` keeps this project's .Rprofile (renv) out of the pipeline's
# R session. Optional: `--decisions <dir>` (default: $ILOVEDATA_DECISIONS_ROOT,
# else <pipeline>/examples/decisions), `--chunk <n>`, the number of drugs
# pulled per get_data() call (default 7), and `--vars <n>`, the number of
# person-level variables per call (default 6). Lower either if a large store
# runs short of memory or of temporary disk space.
#
# The store is opened read-only and nothing is written outside `--out`. The
# script refuses to run with ILOVEDATA_LIVE_FETCH set: it reads a built store
# and never contacts Qualtrics.
#
# Output, three files in `--out`, which ProjectTemplate auto-loads from data/
# under the same names:
#
#   ilovedata_person.rds       one row per person: cohort, survey membership,
#                              every person-level variable the analysis uses,
#                              under its pipeline name, and one response date
#                              per survey (`response_date_<survey>`, Date)
#   ilovedata_person_drug.rds  one row per person x antidepressant for the 28
#                              antidepressants the analysis covers, with the
#                              per-drug items under short names, and
#                              `se_recorded`, the number of the drug's 30
#                              side-effect cells with a stored raw value
#   ilovedata_manifest.rds     provenance: snapshot, pipeline commit, store,
#                              decisions root, export time, the anchor
#                              resolved for each per-drug item and survey,
#                              and the Anchors each response date is read from
#
# Cohorts. Baseline people are those in the GLAD sign-up survey `glad`, the
# standalone medication survey `glad_med` or the Family History survey
# `glad_fh`, joined on `person_id`; SA8 people are those in
# `glad_sa8_signup`, which carries all three blocks itself. A person found in
# both cohorts is kept with cohort "Baseline+SA8" and values from the Baseline
# surveys; the munge refuses such rows rather than choose silently.
#
# Response statuses. Anchor (item) columns are pulled with `sentinel = TRUE`,
# so a cell's Response Status is carried in its value: an answered cell holds
# the answer, a never-shown cell is NA, and the other statuses are the legacy
# codes -777 (seen, not answered), -888 (don't know), -999 (prefer not to
# answer), -555 (not applicable) and -666 (implausible). Derived Variables
# carry no status; their NA is the pipeline's own "missing".
#
# Response dates. The store keeps no Qualtrics response metadata (RecordedDate,
# StartDate and EndDate are dropped at extraction; ilovedata issue 404). What
# it does keep are the surveys' own Embedded Data block timestamps,
# `<Block>_StartDate` / `<Block>_EndDate`, which the survey flow sets to the
# current date as a respondent enters and leaves each block, and which bind as
# ordinary Anchors. `response_date_<survey>` is the latest of these dates for
# the person in that survey: the last day the response recorded activity, the
# nearest stand-in for RecordedDate. It is NA where the person did not take the
# survey, and for every person of a survey whose flow sets no such fields or
# whose fields hold no values in the store.

`%||%` <- function(a, b) if (is.null(a)) b else a

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

# Arguments -------------------------------------------------------------------

parse_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    key <- sub("^--", "", args[[i]])
    if (!startsWith(args[[i]], "--") || i == length(args)) {
      stop("Arguments are --pipeline <dir> --store <file> --out <dir> [--decisions <dir>] [--chunk <n>] [--vars <n>]")
    }
    out[[key]] <- args[[i + 1]]
    i <- i + 2
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
for (req in c("pipeline", "store", "out")) {
  if (is.null(args[[req]])) stop("Missing --", req)
}
if (nzchar(Sys.getenv("ILOVEDATA_LIVE_FETCH"))) {
  stop("ILOVEDATA_LIVE_FETCH is set; this export reads a built store only. Unset it.")
}

pipeline <- normalizePath(args$pipeline, mustWork = TRUE)
store <- normalizePath(args$store, mustWork = TRUE)
decisions <- args$decisions %||% (if (nzchar(Sys.getenv("ILOVEDATA_DECISIONS_ROOT"))) {
  Sys.getenv("ILOVEDATA_DECISIONS_ROOT")
} else {
  file.path(pipeline, "examples", "decisions")
})
decisions <- normalizePath(decisions, mustWork = TRUE)
out_dir <- args$out
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- normalizePath(out_dir, mustWork = TRUE)

options(ilovedata.root = pipeline, ilovedata.decisions_root = decisions)
suppressPackageStartupMessages(pkgload::load_all(file.path(pipeline, "engine"), quiet = TRUE))

config <- pipeline_config()
config$store_path <- store
con <- db_connect(store, read_only = TRUE)
on.exit(db_disconnect(con), add = TRUE)
snapshot <- latest_snapshot_id(con)
message("Store ", store, ", snapshot ", snapshot, "; decisions ", decisions)

# What the analysis needs -----------------------------------------------------

BASELINE_SURVEYS <- c(signup = "glad", med = "glad_med", fh = "glad_fh")
SA8_SURVEY <- "glad_sa8_signup"
MED_SURVEYS <- c(BASELINE_SURVEYS[["med"]], SA8_SURVEY)

# The 28 named antidepressants of the current analysis. "A different
# antidepressant" stays excluded (ruling `drugs`).
DRUGS <- c(
  "citalopram", "sertraline", "fluoxetine", "paroxetine", "mirtazapine",
  "amitriptyline", "dosulepin", "duloxetine", "venlafaxine", "trazodone",
  "escitalopram", "nortriptyline", "clomipramine", "lofepramine",
  "imipramine", "trimipramine", "doxepin", "reboxetine", "fluvoxamine",
  "moclobemide", "phenelzine", "tranylcypromine", "mianserin", "dapoxetine",
  "tryptophan", "isocarboxazid", "hypericum_perforatum", "vortioxetine"
)

# The 30 side-effect checklist items, in the order of the current analysis.
SE_ITEMS <- c(
  "dry_mouth", "sweating", "nausea", "vomiting", "diarrhoea", "constipation",
  "headache", "dizziness", "memory_problems",
  "attentionconcentration_difficulties", "shaking", "muscle_pain",
  "sleepinessdrowsiness", "difficulty_getting_to_sleep", "increased_anxiety",
  "fast_heart_beat", "restlessnessagitation", "fatigue_or_weakness",
  "changes_in_appetite", "weight_gain", "weight_loss", "itching", "rash",
  "runny_nose", "reduced_sexual_desire_function",
  "menstrual_problemsirregularities", "blurred_vision", "suicidal_thoughts",
  "attempted_suicide", "other"
)

# Per-drug items: output column -> Anchor name templates, `{d}` = drug. The
# two medication pages (13 and 15 drugs) and the St John's wort row label give
# the same item different Anchor names; each (item, drug, survey) must resolve
# to exactly one Anchor, or the export stops.
hyp <- function(t) c(t, sub("\\{d\\}", "{d}_e.g._st_johns_wort", t))
DRUG_ITEMS <- c(
  list(
    took = "prescription.{d}",
    nse = hyp("sideeffects.no_side_effects.{d}"),
    stopped = c("sideeffects.{d}.5", "sideeffects.{d}.4"),
    eff = c("antidepressant_eff.{d}.6", "antidepressant_eff2.{d}.2"),
    start_age = c("antidepressant_why.started_taking.txt.{d}",
                  "antidepressant_why2.started_taking.txt.{d}"),
    length = c("antidepressant_why.long_takehave_taking.{d}",
               "antidepressant_why2.long_takehave_taking.{d}")
  ),
  setNames(lapply(SE_ITEMS, function(i) hyp(paste0("sideeffects.", i, ".{d}"))),
           paste0("se_", SE_ITEMS))
)
# "Other" on the second page's grid is named with a trailing ".1".
DRUG_ITEMS$se_other <- c(DRUG_ITEMS$se_other, "sideeffects.other.{d}.1")
# The improvement items are asked on two pages in SA8 and come back with a
# `context` column; they are pulled separately and kept per page.
IMPRV_ITEMS <- list(
  further = c("antidepressant_imprv.period_time_experience_condition.{d}",
              "antidepressant_imprv2.period_time_experience_condition.{d}"),
  duration = c("antidepressant_imprv.long_improvement_symptoms_experienced.{d}",
               "antidepressant_imprv2.long_improvement_symptoms_experienced.{d}")
)

# Person-level variables, by the surveys they are read from.
SIGNUP_VARS <- c(
  "dem.glad_signup_cohort",
  "dem.select_questionnaire_items_medical__2", # sex (Anchor, 0 Male / 1 Female)
  "demographics.bmi_signup",
  "employment.what_is_your_current_employment_status_glad_sa8",
  "dem.marital_status_grouped",
  "mhd.depression_and_anxiety",
  "mhd.bipolar_and_schizophrenia",
  "mhd.eating_disorders",
  "mhd.adhd_diagnosis",
  "mhd.obsessive_compulsive_disorders",
  "mhd.any_personality_disorder",
  "mhd.autism_diagnosis",
  "mhd.disorder_groups_count",
  "dem.disability", # Anchor, 0 No / 1 Yes
  "illness.cardiometabolic_count_glad_sa8",
  "illness.neurological_count_glad_sa8",
  "illness.autoimmune_count_glad_sa8",
  "illness.respiratory_atopic_count_glad_sa8",
  "illness.oncological_count_glad_sa8",
  "illness.musculoskeletal_count_glad_sa8",
  "audit.sum_score", # glad
  "alcohol.audit_sum_score_sa8", # glad_sa8_signup
  "cidid.number_of_episodes",
  "gad7.sum_score",
  "phq9.sum_score",
  "wsas.sum_score",
  "smk.pack_years_incl_never_smokers"
)
FH_VARS <- "fh.diagnosis_types_count_glad_sa8"
MED_VARS <- c(
  "med.sideeffect_severity",
  "med.benefit_rating",
  "sideeffects.experience_side_effects_medication", # the any-side-effects gate
  "prescription.none", # glad_med only; SA8 offers no "None"
  # "A different antidepressant" on the second drug list. Not one of the 28
  # analysed drugs, but a tick on it makes the person an antidepressant taker.
  # (glad_med's first-list option of the same name only points to this list.)
  "prescription.a_different_antidepressant.1",
  "prescription.answer_additional_questions_experiences",
  paste0("antidepressant_ben.", c(
    "relief_of_depressive_symptoms", "relief_of_other_key_symptoms",
    "reduction_in_suicidal_thinking_or_actions", "return_of_normal_emotions",
    "improved_relationships", "getting_back_to_normal_daily_activities",
    "restored_control_over_my_mood_and_actions", "other"
  ))
)

# Helpers ---------------------------------------------------------------------

bindings <- load_bindings(config$bindings_dir, con = con,
                          registry_path = config_registry_path(config, config$bindings_dir))
members <- unique(bindings[!is.na(bindings$anchor), c("anchor", "survey")])

# Resolve one item for one drug and one survey to the single Anchor that has a
# member in that survey. Unmerged copies carry a `__<n>` suffix, so that is
# allowed after any template.
resolve_anchor <- function(templates, item, drug, survey) {
  cands <- vapply(templates, function(t) gsub("{d}", drug, t, fixed = TRUE), "")
  pat <- paste0("^(", paste(gsub("([.])", "\\\\\\1", cands), collapse = "|"), ")(__[0-9]+)?$")
  hit <- unique(members$anchor[members$survey == survey & grepl(pat, members$anchor)])
  if (length(hit) != 1) {
    stop(sprintf("%s for %s in %s resolves to %d Anchors (%s); templates: %s",
                 item, drug, survey, length(hit),
                 paste(hit, collapse = ", "), paste(cands, collapse = ", ")))
  }
  hit
}

resolve_family <- function(items) {
  imap_dfr(items, function(templates, item) {
    expand_grid(drug = DRUGS, survey = MED_SURVEYS) %>%
      mutate(item = item,
             anchor = map2_chr(drug, survey, ~ resolve_anchor(templates, item, .x, .y)))
  })
}

pull <- function(vars, sentinel = TRUE) {
  t0 <- Sys.time()
  d <- get_data(variables = vars, con = con, config = config, sentinel = sentinel)
  message(sprintf("  pulled %d variables (%s ...) in %.0f s", length(vars), vars[[1]],
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (!"survey" %in% names(d)) {
    stop("get_data() returned no survey column for: ", paste(head(vars, 3), collapse = ", "))
  }
  d
}

# Long (person, survey, variable, value) for person-level variables, keeping
# only the surveys each variable is read from. Variables are pulled
# `--vars` at a time: one wide get_data() call over every sign-up variable
# spills gigabytes to DuckDB's temporary directory.
var_batch <- as.integer(args$vars %||% "6")
pull_person_vars <- function(vars, surveys) {
  groups <- split(vars, ceiling(seq_along(vars) / var_batch))
  map_dfr(groups, pull_person_group, surveys = surveys)
}

pull_person_group <- function(vars, surveys) {
  d <- filter(pull(vars), survey %in% surveys)
  # Another survey's Contexts can add a context axis to the pull; within these
  # surveys every person-level variable must have a single slice.
  if ("context" %in% names(d)) {
    ctx <- d %>%
      pivot_longer(-c(person_id, survey, context), names_to = "variable",
                   values_to = "value", values_transform = as.character) %>%
      filter(!is.na(value), !is.na(context))
    if (nrow(ctx)) {
      stop("Person-level variables with more than one Context in ",
           paste(surveys, collapse = "/"), ": ",
           paste(unique(ctx$variable), collapse = ", "))
    }
    d <- d %>% filter(is.na(context)) %>% select(-context)
  }
  d %>%
    mutate(across(all_of(intersect(vars, names(d))), as.character)) %>%
    pivot_longer(-c(person_id, survey), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))
}

# People and cohorts ----------------------------------------------------------

who <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT survey_id, person_id FROM response_statuses
   WHERE snapshot_id = %d AND survey_id IN (%s)",
  as.integer(snapshot),
  paste0("'", c(BASELINE_SURVEYS, SA8_SURVEY), "'", collapse = ", ")
))

person <- who %>%
  mutate(flag = TRUE, survey_id = paste0("in_", survey_id)) %>%
  pivot_wider(names_from = survey_id, values_from = flag, values_fill = FALSE)
for (s in paste0("in_", c(BASELINE_SURVEYS, SA8_SURVEY))) {
  if (!s %in% names(person)) person[[s]] <- FALSE
}
person <- person %>%
  mutate(
    in_baseline = in_glad | in_glad_med | in_glad_fh,
    cohort = case_when(
      in_baseline & in_glad_sa8_signup ~ "Baseline+SA8",
      in_baseline ~ "Baseline",
      TRUE ~ "SA8"
    )
  ) %>%
  select(person_id, cohort, in_glad, in_glad_med, in_glad_fh, in_glad_sa8_signup) %>%
  arrange(person_id)

# Person-level variables ------------------------------------------------------

# Values from a person's own cohort's survey; for a Baseline+SA8 person the
# Baseline survey is used (the munge refuses those rows anyway).
pick_source <- function(long, baseline_survey) {
  long %>%
    left_join(person[c("person_id", "cohort")], by = "person_id") %>%
    filter(
      (cohort %in% c("Baseline", "Baseline+SA8") & survey == baseline_survey) |
        (cohort == "SA8" & survey == SA8_SURVEY)
    ) %>%
    select(person_id, variable, value)
}

person_long <- bind_rows(
  pick_source(pull_person_vars(SIGNUP_VARS, c(BASELINE_SURVEYS[["signup"]], SA8_SURVEY)),
              BASELINE_SURVEYS[["signup"]]),
  pick_source(pull_person_vars(FH_VARS, c(BASELINE_SURVEYS[["fh"]], SA8_SURVEY)),
              BASELINE_SURVEYS[["fh"]]),
  pick_source(pull_person_vars(MED_VARS, MED_SURVEYS), BASELINE_SURVEYS[["med"]])
)
dup <- person_long %>% count(person_id, variable) %>% filter(n > 1)
if (nrow(dup)) stop(nrow(dup), " person x variable cells have more than one value")

person_vars <- person_long %>%
  pivot_wider(names_from = variable, values_from = value)
for (v in setdiff(c(SIGNUP_VARS, FH_VARS, MED_VARS), names(person_vars))) {
  # A variable absent from every slice (e.g. Held) is recorded, not invented.
  warning("Variable returned no values for these cohorts: ", v)
  person_vars[[v]] <- NA_character_
}
person_vars <- person_vars %>%
  mutate(across(-c(person_id, dem.glad_signup_cohort, dem.marital_status_grouped,
                   starts_with("mhd.") & !matches("disorder_groups_count")),
                ~ suppressWarnings(as.numeric(.x))))

person <- person %>%
  left_join(person_vars, by = "person_id") %>%
  relocate(all_of(c(SIGNUP_VARS, FH_VARS, MED_VARS)), .after = in_glad_sa8_signup)

# The pipeline's own cohort label must agree with survey membership.
bad_cohort <- person %>%
  filter(!is.na(dem.glad_signup_cohort), cohort %in% c("Baseline", "SA8"),
         dem.glad_signup_cohort != cohort)
if (nrow(bad_cohort)) stop(nrow(bad_cohort), " people whose dem.glad_signup_cohort contradicts their surveys")

# Response dates --------------------------------------------------------------

DATE_SURVEYS <- c(BASELINE_SURVEYS, sa8 = SA8_SURVEY)
DATE_COLUMN_PATTERN <- "_(Start|End)Date$"

date_anchors <- anchor_resolutions_as_of(con, snapshot) %>%
  filter(survey_id %in% DATE_SURVEYS, grepl(DATE_COLUMN_PATTERN, response_column_id)) %>%
  distinct(anchor, survey = survey_id, response_column_id)

date_long <- if (nrow(date_anchors)) {
  date_groups <- split(unique(date_anchors$anchor),
                       ceiling(seq_along(unique(date_anchors$anchor)) / 25))
  map_dfr(date_groups, function(vars) {
    d <- filter(pull(vars, sentinel = FALSE), survey %in% DATE_SURVEYS)
    if ("context" %in% names(d)) {
      if (any(!is.na(d$context))) stop("Response-date Anchors with a Context axis")
      d <- select(d, -context)
    }
    d %>%
      mutate(across(-c(person_id, survey), as.character)) %>%
      pivot_longer(-c(person_id, survey), names_to = "anchor", values_to = "value") %>%
      # An Anchor bound in several surveys counts only in the ones it resolves in.
      semi_join(date_anchors, by = c("survey", "anchor")) %>%
      filter(!is.na(value), nzchar(value))
  })
} else {
  tibble(person_id = character(), survey = character(), anchor = character(),
         value = character())
}

# Qualtrics writes these as Y-M-D, unpadded ("2024-9-9"); a time, if present,
# is ignored. Anything else stops the export rather than be dropped silently.
date_ok <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}( .*)?$", date_long$value)
if (!all(date_ok)) {
  stop(sum(!date_ok), " response-date cells are not Y-M-D dates, e.g. in ",
       paste(head(unique(date_long$anchor[!date_ok]), 3), collapse = ", "))
}
date_long$date <- as.Date(sub(" .*$", "", date_long$value), format = "%Y-%m-%d")
if (anyNA(date_long$date)) stop(sum(is.na(date_long$date)), " response-date cells do not parse")

response_dates <- date_long %>%
  group_by(person_id, survey) %>%
  summarise(date = max(date), .groups = "drop") %>%
  mutate(survey = paste0("response_date_", survey)) %>%
  pivot_wider(names_from = survey, values_from = date)
for (s in DATE_SURVEYS) {
  col <- paste0("response_date_", s)
  if (!col %in% names(response_dates)) {
    warning("No response dates in the store for ", s,
            if (!s %in% date_anchors$survey) " (no <Block>_Start/EndDate fields)" else
              " (its <Block>_Start/EndDate fields hold no values)")
    response_dates[[col]] <- as.Date(rep(NA_character_, nrow(response_dates)))
  }
}
response_date_cols <- paste0("response_date_", DATE_SURVEYS)

person <- person %>%
  left_join(select(response_dates, person_id, all_of(response_date_cols)), by = "person_id")
# A date only for a survey the person is in.
for (s in DATE_SURVEYS) {
  stray <- !is.na(person[[paste0("response_date_", s)]]) & !person[[paste0("in_", s)]]
  if (any(stray)) stop(sum(stray), " people with a ", s, " response date but no ", s, " response")
}

# Per-drug items --------------------------------------------------------------

drug_map <- resolve_family(DRUG_ITEMS)
imprv_map <- resolve_family(IMPRV_ITEMS)

# Drugs are pulled a few at a time: each get_data() call has a fixed cost, and
# one call for all ~1,000 per-drug Anchors would hold every cell in memory at
# once on a full-cohort store. `--chunk` sets the number of drugs per call.
chunk <- as.integer(args$chunk %||% "7")
drug_chunks <- split(DRUGS, ceiling(seq_along(DRUGS) / chunk))

per_drug <- map_dfr(drug_chunks, function(drs) {
  message("  per-drug items: ", paste(drs, collapse = ", "))
  dr <- paste(drs, collapse = ", ")
  m <- filter(drug_map, drug %in% drs)
  d <- pull(unique(m$anchor)) %>%
    filter(survey %in% MED_SURVEYS)
  if ("context" %in% names(d)) stop("Unexpected context axis in per-drug pull for ", dr)
  long <- d %>%
    mutate(across(-c(person_id, survey), as.character)) %>%
    pivot_longer(-c(person_id, survey), names_to = "anchor", values_to = "value") %>%
    inner_join(select(m, survey, anchor, item, drug), by = c("survey", "anchor"),
               relationship = "many-to-many")
  if (anyDuplicated(long[c("person_id", "survey", "drug", "item")])) {
    stop("Two Anchors map to one item for ", dr)
  }

  mi <- filter(imprv_map, drug %in% drs)
  di <- pull(unique(mi$anchor)) %>%
    filter(survey %in% MED_SURVEYS)
  if (!"context" %in% names(di)) di$context <- NA_character_
  imp <- di %>%
    mutate(across(-c(person_id, survey, context), as.character)) %>%
    pivot_longer(-c(person_id, survey, context), names_to = "anchor", values_to = "value") %>%
    inner_join(select(mi, survey, anchor, item, drug), by = c("survey", "anchor"),
               relationship = "many-to-many") %>%
    # SA8 asks the improvement items on page "a" or page "b" of each drug's
    # block (Contexts antidepressant_imprv1a/1b, 2a/2b); glad_med has one page. Keep the page so the munge can apply the
    # "whichever was answered" ruling. Rows for the other half of SA8's list
    # (page-1 drugs under the page-2 Contexts and vice versa) carry nothing.
    mutate(page = case_when(
      is.na(context) ~ "a",
      grepl("a$", context) ~ "a",
      grepl("b$", context) ~ "b",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(value) | is.na(context))
  if (any(is.na(imp$page))) stop("Unrecognised improvement Context for ", dr, ": ",
                                  paste(unique(imp$context[is.na(imp$page)]), collapse = ", "))
  imp <- imp %>%
    mutate(item = paste0(item, "_", page)) %>%
    select(person_id, survey, drug, item, value)
  if (anyDuplicated(imp[c("person_id", "survey", "drug", "item")])) {
    stop("Two improvement values for one page for ", dr)
  }

  bind_rows(select(long, person_id, survey, drug, item, value), imp)
})

# Checklist evidence ----------------------------------------------------------

# The sentinel coding cannot tell a side-effect checklist that was displayed and
# submitted with nothing ticked from one that was never reached: Qualtrics
# writes -77 into a shown-but-blank cell and 0 into an unticked one, while a
# page that was never displayed or never recorded (dropout before it) leaves
# the cells empty, and the engine labels both -77 and empty
# `seen_not_answered` (-777). The raw values keep the difference, so count, per
# person x drug, the 30 checklist cells that hold any stored value (1, 0 or
# -77). This reads `responses` with the interval filter of the engine's
# canonical reader `responses_as_of()`, restricted to the checklist columns.
se_cols <- drug_map %>%
  filter(item %in% paste0("se_", SE_ITEMS)) %>%
  inner_join(
    anchor_resolutions_as_of(con, snapshot) %>%
      select(anchor, survey = survey_id, response_column_id),
    by = c("anchor", "survey")
  )
if (nrow(se_cols) != nrow(filter(drug_map, item %in% paste0("se_", SE_ITEMS))) ||
    anyDuplicated(se_cols[c("survey", "response_column_id")])) {
  stop("Side-effect checklist Anchors do not resolve to one column each at snapshot ", snapshot)
}
duckdb::duckdb_register(con, "glad_medications_se_cols",
                        distinct(se_cols, survey_id = survey, response_column_id))
se_raw <- DBI::dbGetQuery(con, sprintf(
  "SELECT r.survey_id AS survey, r.person_id, r.response_column_id
     FROM responses r
     JOIN glad_medications_se_cols c
       ON c.survey_id = r.survey_id AND c.response_column_id = r.response_column_id
    WHERE r.first_seen_snapshot <= %1$d AND r.last_seen_snapshot >= %1$d
      AND r.value IS NOT NULL AND r.value <> ''",
  as.integer(snapshot)
))
duckdb::duckdb_unregister(con, "glad_medications_se_cols")
se_recorded <- se_raw %>%
  inner_join(select(se_cols, survey, response_column_id, drug),
             by = c("survey", "response_column_id")) %>%
  count(person_id, survey, drug, name = "se_recorded")

person_drug <- per_drug %>%
  pivot_wider(names_from = item, values_from = value) %>%
  left_join(person[c("person_id", "cohort")], by = "person_id") %>%
  # A person's medication answers come from their own cohort's survey.
  filter((cohort %in% c("Baseline", "Baseline+SA8") & survey == BASELINE_SURVEYS[["med"]]) |
           (cohort == "SA8" & survey == SA8_SURVEY)) %>%
  select(-cohort)
for (col in c(names(DRUG_ITEMS), paste0(rep(names(IMPRV_ITEMS), each = 2), c("_a", "_b")))) {
  if (!col %in% names(person_drug)) person_drug[[col]] <- NA_real_
}
person_drug <- person_drug %>%
  mutate(across(-c(person_id, survey, drug), ~ suppressWarnings(as.numeric(.x)))) %>%
  select(person_id, survey, drug, took, all_of(paste0("se_", SE_ITEMS)), nse, stopped, eff,
         further_a, further_b, duration_a, duration_b, start_age, length) %>%
  # Keep every row with anything stored: the drug was ticked, or some per-drug
  # cell holds a value or a non-answer code.
  filter(if_any(-c(person_id, survey, drug), ~ !is.na(.x))) %>%
  left_join(se_recorded, by = c("person_id", "survey", "drug")) %>%
  mutate(se_recorded = coalesce(se_recorded, 0L)) %>%
  relocate(se_recorded, .before = nse) %>%
  arrange(person_id, match(drug, DRUGS))

# Write -----------------------------------------------------------------------

manifest <- tibble(
  key = c("snapshot", "git_commit", "store", "decisions_root", "exported_at",
          "n_person", "n_person_drug", "response_date_columns", "response_date_source",
          paste0("n_", response_date_cols)),
  value = c(as.character(snapshot), as.character(current_git_commit(pipeline)), store,
            decisions, format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
            nrow(person), nrow(person_drug), paste(response_date_cols, collapse = ", "),
            "latest <Block>_StartDate/_EndDate Embedded Data date per person and survey",
            vapply(response_date_cols, function(v) as.character(sum(!is.na(person[[v]]))), ""))
)
attr(manifest, "anchor_map") <- bind_rows(drug_map, imprv_map)
attr(manifest, "response_date_anchors") <- date_anchors

saveRDS(person, file.path(out_dir, "ilovedata_person.rds"))
saveRDS(person_drug, file.path(out_dir, "ilovedata_person_drug.rds"))
saveRDS(manifest, file.path(out_dir, "ilovedata_manifest.rds"))

message("Wrote ", out_dir, ": ", nrow(person), " people (",
        paste(names(table(person$cohort)), table(person$cohort), collapse = ", "),
        "), ", nrow(person_drug), " person x drug rows")

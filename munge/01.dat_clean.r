# Builds the two analysis frames from the ilovedata extracts.
#
#   dat_long  one row per participant x antidepressant taken (GLMM outcomes)
#   dat       one row per participant who took at least one of the 28 named
#             antidepressants, i.e. the people in dat_long (legacy
#             per-participant outcomes)
#
# Inputs, written to data/ by extract/ilovedata_export.R and auto-loaded by
# ProjectTemplate: `ilovedata_person` (one row per person) and
# `ilovedata_person_drug` (one row per person x antidepressant). Item columns
# carry their Response Status as legacy codes; see lib/01.munge_helpers.r.
#
# Two cohorts go through one path: Baseline (GLAD sign-up `glad`, the
# standalone medication survey `glad_med` and the Family History survey
# `glad_fh`, joined on person_id) and SA8 (`glad_sa8_signup`). Definitions
# follow the rulings in docs/sa8-migration-plan.md, sections C and D; each is
# cited where it is applied. Survey cohort is kept as `cohort` for descriptive
# tables but is not a model term (ruling `wave` = c).

if (!exists("ilovedata_person") || !exists("ilovedata_person_drug")) {
  stop(
    "data/ilovedata_person.rds and data/ilovedata_person_drug.rds are missing. ",
    "Run extract/ilovedata_export.R (see docs/analysis-pipeline.md)."
  )
}

person <- ilovedata_person
person_drug <- ilovedata_person_drug

if (!"se_recorded" %in% names(person_drug)) {
  stop("data/ilovedata_person_drug.rds predates the `se_recorded` column; re-run extract/ilovedata_export.R.")
}
if (!all(c("hidden_list_recorded", "history_recorded") %in% names(person_drug)) ||
    !"hidden_list_none_recorded" %in% names(person)) {
  stop("data/ilovedata_person*.rds predate the display-evidence columns (`hidden_list_recorded`, ",
       "`history_recorded`, `hidden_list_none_recorded`); re-run extract/ilovedata_export.R.")
}

# People who answered both sign-ups (cohort "Baseline+SA8"): Baseline wins
# (ruling 2026-11-08). The export already applies it: such a person carries
# their Baseline answers, and their medication rows come only from glad_med.
# The rule was made for the overlap people seen so far, none of whom took a
# named antidepressant; if one ever does, they would enter dat_long, so stop
# and have the rule revisited. "Took a named antidepressant" is the condition
# WHO ENTERS uses below (`is_tick(took)` in the person-drug frame).
overlap_ids <- person$person_id[person$cohort == "Baseline+SA8"]
if (length(overlap_ids) > 0) {
  overlap_takers <- person_drug %>%
    filter(person_id %in% overlap_ids, is_tick(took)) %>%
    distinct(person_id) %>%
    nrow()
  if (overlap_takers > 0) {
    stop(
      overlap_takers, " of the ", length(overlap_ids), " people who answered ",
      "both the Baseline and the SA8 sign-ups took a named antidepressant and ",
      "would enter the analysis; the Baseline-wins rule (2026-11-08) was made ",
      "for overlap people with no named drug, so it needs revisiting."
    )
  }
  message(
    length(overlap_ids), " people answered both sign-ups; Baseline answers ",
    "kept (rule 2026-11-08); none took a named antidepressant"
  )
}

# The 28 named antidepressants, in the order the extract writes them. "A
# different antidepressant" is not one of them (ruling `drugs`).
antidepressants <- unique(person_drug$drug)

# ==============================================================================
# glad_med NORTRIPTYLINE COLLISION (ruling `nort_collision`, 2026-11-11)
# ==============================================================================
# Until a survey edit in early March 2020, glad_med exported the first drug
# list's "A different antidepressant" into the nortriptyline column QID5_11
# (`prescription.nortriptyline`); QID5_13, the column that holds it later, is
# empty before then. So most early nortriptyline ticks are "a different
# antidepressant" ticks. The export records two raw cells of glad_med's hidden
# question QID10, which carries the first list's ticks forward (see "Display
# evidence" in extract/ilovedata_export.R): whether the nortriptyline choice
# QID10_12 holds a stored value (`hidden_list_recorded`), and whether the "None"
# choice QID10_0 does (`hidden_list_none_recorded`), which happens when the
# question fell back to showing every choice.
#
# Rule: a glad_med nortriptyline tick is genuine if and only if QID10_12 holds a
# stored value and QID10_0 does not. Any other tick is recoded to not ticked
# (took = 0, an answered 0: not missing and not "not shown") and flagged in
# `took_recoded`. Only glad_med nortriptyline is touched; SA8 asks the list in
# its own columns and is unaffected. A recoded tick is no longer "taken", so its
# history and side-effect cells create no row in either frame, and a person
# whose only named drug it was has no named drug and leaves through the
# existing `no_listed` rule (WHO ENTERS below).
#
# The checks below stop the munge if the evidence no longer looks the way it
# did when the rule was made. The early responses are fixed, so the recoded
# count and the rule's disagreements with the history page are pinned; later
# glad_med responses can only add genuine ticks.
NORT_COLLISION <- list(
  survey = "glad_med",
  drug = "nortriptyline",
  flag = "pre-edit QID5_11 collision",
  min_ticks = 973L,       # glad_med nortriptyline ticks in the 2026-11-11 export
  recoded = 738L,         # ticks recoded by the rule
  disagree = 2L,          # ticks on which the rule and the history page disagree
  min_hidden_share = 0.98 # other first-list drugs: share of ticks with a QID10 entry
)
local({
  nc <- NORT_COLLISION
  none <- person %>% select(person_id, hidden_list_none_recorded)
  pd <- person_drug %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    left_join(none, by = "person_id")
  target <- pd$survey == nc$survey & pd$drug == nc$drug & is_tick(pd$took)
  if (anyNA(pd$hidden_list_recorded[target]) || anyNA(pd$hidden_list_none_recorded[target])) {
    stop("glad_med nortriptyline ticks without hidden-list evidence; re-run the export.")
  }
  genuine <- pd$hidden_list_recorded == 1 & pd$hidden_list_none_recorded == 0
  recode <- target & !genuine
  history_shown <- pd$history_recorded > 0

  # The rule against the prescription-history page, which is displayed only for
  # a drug carried forward by the hidden list.
  n_ticks <- sum(target)
  n_recoded <- sum(recode)
  n_kept <- n_ticks - n_recoded
  kept_no_history <- sum(target & genuine & !history_shown)
  recoded_with_history <- sum(recode & history_shown)
  message(sprintf(paste0(
    "nort_collision: %d glad_med nortriptyline ticks; %d kept (QID10_12 stored, QID10_0 not), ",
    "%d recoded to 0. Rule vs history page: %d kept without a history page, ",
    "%d recoded with one."), n_ticks, n_kept, n_recoded, kept_no_history, recoded_with_history))
  if (n_ticks < nc$min_ticks) {
    stop("nort_collision: ", n_ticks, " glad_med nortriptyline ticks, expected at least ", nc$min_ticks)
  }
  if (n_recoded != nc$recoded) {
    stop("nort_collision: the rule recodes ", n_recoded, " ticks, expected ", nc$recoded,
         "; the early glad_med responses or their evidence have changed, so re-examine the rule.")
  }
  if (kept_no_history + recoded_with_history != nc$disagree) {
    stop("nort_collision: the rule and the history page disagree on ",
         kept_no_history + recoded_with_history, " ticks, expected ", nc$disagree)
  }

  # Every other first-list drug: nearly every tick has its hidden-list entry, so
  # the evidence works where nothing collided.
  other <- pd %>%
    filter(survey == nc$survey, drug != nc$drug, !is.na(hidden_list_recorded), is_tick(took)) %>%
    group_by(drug) %>%
    dplyr::summarise(ticks = n(), share = mean(hidden_list_recorded == 1), .groups = "drop")
  if (nrow(other) < 12 || any(other$share < nc$min_hidden_share)) {
    stop("nort_collision: hidden-list coverage of the other first-list drugs is below ",
         nc$min_hidden_share, " (lowest ", round(min(other$share), 4), ", ",
         other$drug[which.min(other$share)], ") or drugs are missing (", nrow(other), " of 12).")
  }
  message(sprintf("nort_collision: other first-list glad_med drugs, %d ticks, %.1f%%-%.1f%% with a hidden-list entry.",
                  sum(other$ticks), 100 * min(other$share), 100 * max(other$share)))

  stopifnot(nrow(pd) == nrow(person_drug), identical(pd$row, seq_len(nrow(person_drug))))
  new_took <- ifelse(recode, 0, pd$took)
  changed <- xor(is.na(pd$took), is.na(new_took)) |
    (!is.na(pd$took) & !is.na(new_took) & pd$took != new_took)
  if (!identical(which(changed), which(recode)) ||
      any(pd$survey[changed] != nc$survey | pd$drug[changed] != nc$drug)) {
    stop("nort_collision: the recode touched rows other than glad_med nortriptyline ticks")
  }

  out <- person_drug
  out$took <- new_took
  out$took_recoded <- ifelse(recode, nc$flag, NA_character_)

  # Prevalence after the recode: glad_med nortriptyline among glad_med people
  # with a named drug, against the same in SA8, whose list did not collide.
  prev <- function(d, s) {
    t <- d %>% filter(survey == s, is_tick(took))
    c(n = sum(t$drug == nc$drug), takers = length(unique(t$person_id)))
  }
  g <- prev(out, nc$survey); s8 <- prev(out, "glad_sa8_signup")
  g_pct <- 100 * g[["n"]] / g[["takers"]]; s8_pct <- 100 * s8[["n"]] / s8[["takers"]]
  message(sprintf("nort_collision: nortriptyline prevalence among takers, glad_med %.2f%% (%d/%d) vs SA8 %.2f%% (%d/%d).",
                  g_pct, g[["n"]], g[["takers"]], s8_pct, s8[["n"]], s8[["takers"]]))
  if (abs(g_pct - s8_pct) > 1) {
    stop("nort_collision: corrected glad_med nortriptyline prevalence (", round(g_pct, 2),
         "%) is more than 1 point from SA8's (", round(s8_pct, 2), "%)")
  }

  # People whose only named drug was a recoded tick.
  before <- person_drug %>% filter(is_tick(took)) %>% distinct(person_id)
  after <- out %>% filter(is_tick(took)) %>% distinct(person_id)
  message("nort_collision: ", nrow(before) - nrow(after),
          " people had no named drug but the recoded tick and leave both frames (`no_listed`).")

  person_drug <<- out
})

# ==============================================================================
# WHO ENTERS
# ==============================================================================
# A participant enters when they ticked at least one of the 28 named
# antidepressants on either list (ruling 2026-11-07). People who ticked "None",
# and people whose only tick is the second list's "a different antidepressant
# that isn't listed above", have no named drug and are in neither frame. The
# ticks are those left after the nortriptyline recode above, so a person whose
# only "nortriptyline" was glad_med's pre-edit "a different antidepressant" is
# in neither frame either.
# Membership is decided by the ticks alone, not by the any-side-effects
# question or the rating items: in SA8 the pipeline cannot yet mark people who
# were never routed into the medication section as `not_shown`, so "seen, not
# answered" on those items is not evidence that a person was asked them.
#
# dat_long has one row per taken named drug; dat has one row per participant
# with at least one, so the two frames hold exactly the same people and
# n_meds >= 1 in dat.
taken <- person_drug %>%
  filter(is_tick(took)) %>%
  select(-took)

takers <- unique(taken$person_id)

# ==============================================================================
# PER-MEDICATION OUTCOMES
# ==============================================================================
se_items <- setdiff(grep("^se_", names(person_drug), value = TRUE), "se_recorded")
stopifnot(length(se_items) == 30)

gate <- person %>%
  transmute(
    person_id,
    # "Did you experience side effects from any medication(s) that you have
    # taken?" 0 No / 1 Yes.
    gate = answer_value(sideeffects.experience_side_effects_medication)
  )

med_long <- taken %>%
  left_join(gate, by = "person_id") %>%
  dplyr::mutate(
    # Number of side effects (ruling A2; A3 as amended 2026-11-06, matching
    # GLAD-Medications-SEM). Count the ticked boxes. With none ticked:
    #   - No to having had side effects from any medication: 0 (the checklist
    #     is not shown after No, so it is blank there).
    #   - Yes, and the drug's checklist page was displayed and recorded: 0,
    #     whether or not its "No side effects" box is ticked. Displayed and
    #     recorded means the drug's checklist cells hold stored raw values
    #     (`se_recorded` > 0: Qualtrics' 0 for an unticked box, or its -77 for
    #     shown-but-blank). The sentinel codes cannot show this, since the
    #     engine codes -77 and an empty cell alike as -777.
    #   - Otherwise (the drug's cells are empty: page never reached or not
    #     recorded), missing.
    # The "No side effects" tick does not enter the count. A strict coding of
    # the blank checklists is a supplementary sensitivity analysis only
    # (`n_se_strict` below).
    n_se_ticked = rowSums(across(all_of(se_items), is_tick)),
    n_se = case_when(
      n_se_ticked > 0 ~ n_se_ticked,
      !is.na(gate) & gate == 0 ~ 0,
      !is.na(gate) & gate == 1 & se_recorded > 0 ~ 0,
      TRUE ~ NA_real_
    ),
    # Stopped because of side effects, 0 No / 1 Yes (ruling 2026-11-07). The
    # item is shown for a drug only when a side effect was ticked for it
    # (ruling stop_display), so it follows n_se:
    #   - n_se is 0, for whatever reason (gate No; the "No side effects" box
    #     ticked; a recorded checklist with nothing ticked): 0. A drug with no
    #     side effects was not stopped because of them. This does not look at
    #     the stop item's own status.
    #   - n_se is missing: missing.
    #   - n_se is above 0: the answer where the item was answered; missing when
    #     it was seen and not answered.
    stopped_due_to_se = case_when(
      is.na(n_se) ~ NA_real_,
      n_se == 0 ~ 0,
      answered(stopped) ~ stopped,
      TRUE ~ NA_real_
    ),
    # Strict blank-checklist sensitivity coding (ruling `a3_strict_sens`,
    # 2026-11-11; supplementary only, the rule above stays primary as amended
    # by the owner on 2026-11-06). A row with Yes on the any-side-effects
    # question whose checklist page was displayed and recorded
    # (`se_recorded` > 0) but has nothing ticked and no "No side effects" tick
    # is not confirmed as 0 side effects: under the strict rule its number of
    # side effects is missing, and so is its stop status (which follows n_se).
    # Every other row keeps its main value. Used only by the strict
    # sensitivity set (lib/13.strict_se.r, `glmm_models_cov_strict`).
    se_blank_unconfirmed = !is.na(gate) & gate == 1 & se_recorded > 0 & n_se_ticked == 0 & !is_tick(nse),
    n_se_strict = ifelse(se_blank_unconfirmed, NA_real_, n_se),
    stopped_due_to_se_strict = ifelse(is.na(n_se_strict), NA_real_, stopped_due_to_se),
    # How well the drug worked: 0 not at all, 1 moderately, 2 very well.
    # "Don't know" is missing.
    effectiveness = answer_value(eff),
    # SA8 asks the improvement questions on one of two pages per drug; take
    # whichever was answered (ruling `imprv`). glad_med has one page ("a").
    # Two different answers would need a rule; there are none on record, and
    # such a row is left missing.
    further_symptoms = case_when(
      answered(further_a) & answered(further_b) & further_a != further_b ~ NA_real_,
      answered(further_a) ~ further_a,
      answered(further_b) ~ further_b,
      TRUE ~ NA_real_
    ),
    improvement_duration = case_when(
      answered(duration_a) & answered(duration_b) & duration_a != duration_b ~ NA_real_,
      answered(duration_a) ~ duration_a,
      answered(duration_b) ~ duration_b,
      TRUE ~ NA_real_
    ),
    # Remission (ruling A4, option b): 1 when the drug worked moderately or
    # very well and there were no further symptoms afterwards; 0 when there
    # were further symptoms, or when the drug did not work at all (the
    # follow-up is never shown then); missing when effectiveness is missing or
    # "don't know", or when a responder left the further-symptoms item blank.
    remission = case_when(
      !is.na(further_symptoms) & further_symptoms == 1 ~ 0,
      !is.na(effectiveness) & effectiveness == 0 ~ 0,
      !is.na(effectiveness) & effectiveness %in% c(1, 2) &
        !is.na(further_symptoms) & further_symptoms == 0 ~ 1,
      TRUE ~ NA_real_
    ),
    start_age_raw = answer_value(start_age),
    length_taken = answer_value(length)
  ) %>%
  select(
    person_id, medication = drug, gate, n_se, stopped_due_to_se, n_se_strict, stopped_due_to_se_strict,
    effectiveness, further_symptoms, improvement_duration, remission, start_age_raw, length_taken
  )

# The strict coding differs from the main one only on the unconfirmed blank
# checklists, all of which are 0 under the main rule.
local({
  d <- !is.na(med_long$n_se) & is.na(med_long$n_se_strict)
  stopifnot(
    all(med_long$n_se[d] == 0), all(med_long$stopped_due_to_se[d] == 0),
    identical(is.na(med_long$n_se) | d, is.na(med_long$n_se_strict)),
    all(med_long$n_se_strict == med_long$n_se, na.rm = TRUE),
    all(med_long$stopped_due_to_se_strict == med_long$stopped_due_to_se, na.rm = TRUE),
    identical(is.na(med_long$stopped_due_to_se_strict), is.na(med_long$stopped_due_to_se) | d)
  )
})

# Start age & cumulative medication count. Start age orders a participant's
# medications. Ages outside 5-100 are invalid; a taken medication without a
# valid start age is placed at the participant's last valid age.
start_age_long <- med_long %>%
  dplyr::mutate(
    start_age_valid = ifelse(start_age_raw < 5 | start_age_raw > 100, NA, start_age_raw)
  ) %>%
  group_by(person_id) %>%
  dplyr::arrange(start_age_valid, medication, .by_group = TRUE) %>%
  dplyr::mutate(
    n_missing_start_age = sum(is.na(start_age_valid)),
    n_valid_start_age = sum(!is.na(start_age_valid)),
    last_start_age = ifelse(n_valid_start_age > 0, max(start_age_valid, na.rm = TRUE), NA_real_),
    base_cumulative_med_count = ifelse(!is.na(start_age_valid), cumsum(!is.na(start_age_valid)), NA_integer_),
    final_cumulative_med_count = ifelse(n_valid_start_age > 0,
      n_valid_start_age + n_missing_start_age,
      n_missing_start_age
    ),
    cumulative_med_count = case_when(
      !is.na(start_age_valid) & base_cumulative_med_count == n_valid_start_age ~ final_cumulative_med_count,
      !is.na(start_age_valid) ~ base_cumulative_med_count,
      TRUE ~ final_cumulative_med_count
    ),
    start_age = ifelse(is.na(start_age_valid) & n_valid_start_age > 0, last_start_age, start_age_valid)
  ) %>%
  ungroup() %>%
  select(person_id, medication, start_age_valid, start_age, cumulative_med_count)

med_long <- med_long %>%
  left_join(start_age_long, by = c("person_id", "medication"))

# ==============================================================================
# PER-PARTICIPANT VARIABLES
# ==============================================================================
# Duration taken, by band code -> months at the band midpoint: 1 = 1-3 months
# (2), 2 = 4-6 months (5), 3 = 7-12 months (9.5), 4 = 1-2 years (18),
# 5 = 2-3 years (30), 6 = 3-4 years (42), 7 = more than 4 years (60). The top
# band is open; its value assumes 5 years (user ruling 2026-11-09). Non-answers
# (-777, -555) are already NA in length_taken.
duration_band_months <- c(2, 5, 9.5, 18, 30, 42, 60)
stopifnot(all(is.na(med_long$length_taken) | med_long$length_taken %in% 1:7))

# Medication summaries over a participant's taken drugs.
med_person <- tibble(person_id = takers) %>%
  left_join(
    med_long %>%
      group_by(person_id) %>%
      dplyr::summarise(
        n_meds = n(),
        # Mean number of side effects over the drugs whose count is known; a
        # missing count is left out of the mean (ruling A2).
        mean_n_se = if (any(!is.na(n_se))) mean(n_se, na.rm = TRUE) else NA_real_,
        # Proportion of the participant's antidepressants stopped because of
        # side effects, among the drugs whose stop status is known (answered,
        # or 0 because n_se is 0) (user ruling 2026-11-09, afternoon): the
        # numerator is the drugs stopped because of side effects, the
        # denominator the drugs with a known status, so a drug whose status is
        # unknown is in neither. It used to count every taken drug in the
        # denominator. Missing when no drug has a known status.
        intolerance = if (any(!is.na(stopped_due_to_se))) {
          sum(stopped_due_to_se == 1, na.rm = TRUE) / sum(!is.na(stopped_due_to_se))
        } else {
          NA_real_
        },
        # Mean duration per antidepressant, in years (user ruling 2026-11-09,
        # afternoon): each drug's duration code is converted to months at its
        # band midpoint and averaged over the drugs where one was given, then
        # divided by 12; missing when none was. It used to be the sum over
        # those drugs (total duration). See duration_band_months above for the
        # bands.
        time = if (any(!is.na(length_taken))) {
          mean(duration_band_months[length_taken], na.rm = TRUE) / 12
        } else {
          NA_real_
        },
        # Average age at starting an antidepressant, in decades, over valid ages.
        avg_start_age = if (any(!is.na(start_age_valid))) {
          mean(start_age_valid, na.rm = TRUE) / 10
        } else {
          NA_real_
        },
        # Duration of improvement on the first medication (by valid start age,
        # then list order) for which one was reported: 0 none, 1 under a month
        # ... 5 more than 12 months. "Not sure" is missing.
        first_imprv = {
          o <- order(start_age_valid, match(medication, antidepressants), na.last = TRUE)
          v <- improvement_duration[o]
          v[!is.na(v)][1]
        },
        .groups = "drop"
      ),
    by = "person_id"
  )

stopifnot(all(med_person$n_meds >= 1))

# Benefit questions, answered once for all antidepressants.
best_items <- grep("^antidepressant_ben\\.", names(person), value = TRUE)
ben_person <- person %>%
  filter(person_id %in% takers) %>%
  dplyr::mutate(
    n_best_ticked = rowSums(across(all_of(best_items), is_tick)),
    any_best_answered = rowSums(across(all_of(best_items), answered)) > 0,
    # Number of best aspects ticked. With none ticked it is 0 when the
    # participant answered the benefit questions (rated the benefit or
    # answered a box), and missing when they answered none of them.
    n_best = case_when(
      n_best_ticked > 0 ~ n_best_ticked,
      any_best_answered | !is.na(med.benefit_rating) ~ 0,
      TRUE ~ NA_real_
    ),
    # Side-effect severity (ruling A1): 0 = No to the any-side-effects
    # question, 1-5 = the star rating reversed so that 5 is the worst.
    # Benefit rating (ruling A5): 1-5 stars; a raw 0 is missing. Both are
    # derived by the pipeline.
    se_rating = med.sideeffect_severity,
    ben_rating = med.benefit_rating
  ) %>%
  select(person_id, se_rating, ben_rating, n_best)

# Covariates (rulings in section D; every one comes from the pipeline).
employment_levels <- c(
  "1" = "In_paid_employment_or_self_employed",
  "2" = "Retired",
  "3" = "Looking_after_home_and_or_family",
  "4" = "Unable_to_work_because_of_sickness_or_disability",
  "5" = "Unemployed",
  "6" = "Doing_unpaid_or_voluntary_work",
  "7" = "Full_or_part_time_student"
)

covariates <- person %>%
  transmute(
    person_id,
    cohort = factor(cohort, levels = c("Baseline", "SA8")),
    # Sex: one variable from the sign-up survey in both cohorts (ruling `sex`).
    sex = factor(
      answer_value(dem.select_questionnaire_items_medical__2),
      levels = c(0, 1), labels = c("Male", "Female")
    ),
    bmi = demographics.bmi_signup,
    # Employment: seven shared categories (ruling `employment`); "None of the
    # above" and "Other" are missing. Treatment-coded against paid employment.
    employment = unname(employment_levels[as.character(
      employment.what_is_your_current_employment_status_glad_sa8
    )]),
    # Marital status: three groups (ruling `marital`), coded against "Not in a
    # relationship".
    marital = dem.marital_status_grouped,
    Not_in_relationship_In_relationship = ifelse(is.na(marital), NA_integer_,
      as.integer(marital == "In a relationship")),
    Not_in_relationship_Married = ifelse(is.na(marital), NA_integer_,
      as.integer(marital == "Married")),
    # Psychiatric diagnoses (rulings `adhd_asd`, `pd`, `panic`, `ed_group`,
    # `ed_screen`, `comorb_groups`, `comorb_blank`). The eating-disorder group
    # and the comorbidity count are taken as the pipeline delivers them; no
    # value is filled here.
    eating_disorders_numeric = label_to_binary(mhd.eating_disorders,
      "Eating disorder", "No eating disorder"),
    mhd_addadhd_numeric = label_to_binary(mhd.adhd_diagnosis, "ADHD", "No ADHD"),
    obsessive_compulsive_disorders_numeric = label_to_binary(
      mhd.obsessive_compulsive_disorders,
      "Obsessive compulsive disorder", "No obsessive compulsive disorder"
    ),
    mhd_personality_disorder_numeric = label_to_binary(mhd.any_personality_disorder,
      "Personality disorder", "No personality disorder"),
    autism_spectrum_disorder_numeric = label_to_binary(mhd.autism_diagnosis,
      "Autism spectrum disorder", "No autism spectrum disorder"),
    comorbidity_total_count_numeric = mhd.disorder_groups_count,
    depression_and_anxiety = mhd.depression_and_anxiety,
    Depressive_and_anxiety_disorder_Only_anxiety_disorder =
      level_dummy(depression_and_anxiety, "Only anxiety disorder"),
    Depressive_and_anxiety_disorder_Only_depressive_disorder =
      level_dummy(depression_and_anxiety, "Only depressive disorder"),
    No_psychotic_or_bipolar_disorder_Only_bipolar_disorder =
      level_dummy(mhd.bipolar_and_schizophrenia, "Only bipolar disorder"),
    No_psychotic_or_bipolar_disorder_Only_psychotic_disorder =
      level_dummy(mhd.bipolar_and_schizophrenia, "Only psychotic disorder"),
    No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder =
      level_dummy(mhd.bipolar_and_schizophrenia, "Psychotic and bipolar disorder"),
    # Disability: the shared disability question (ruling `disability`).
    disable_bin = answer_value(dem.disability),
    # Physical illness groups: counts of conditions both surveys ask, missing
    # when a page was not answered (rulings `ill_*`).
    score_cardiometabolic = illness.cardiometabolic_count_glad_sa8,
    score_neurological = illness.neurological_count_glad_sa8,
    score_autoimmune = illness.autoimmune_count_glad_sa8,
    score_respiratory = illness.respiratory_atopic_count_glad_sa8,
    score_oncology = illness.oncological_count_glad_sa8,
    score_musculoskeletal = illness.musculoskeletal_count_glad_sa8,
    # AUDIT: glad's score for Baseline; SA8 scored on all ten items (ruling
    # `audit`). How SA8 never-drinkers are scored is the pipeline's to decide;
    # whatever it delivers is used as is.
    audit = case_when(
      cohort == "Baseline" ~ audit.sum_score,
      cohort == "SA8" ~ alcohol.audit_sum_score_sa8
    ),
    # Number of depressive episodes (ruling `episodes`).
    cidid_recurrence = cidid.number_of_episodes,
    gad7 = gad7.sum_score,
    phq9 = phq9.sum_score,
    wsas = wsas.sum_score,
    # Family history: the number of diagnosis types with at least one affected
    # immediate relative (rulings `fh_types`, `fh_ed`, `fh_other`). Replaces the
    # old sum of typed relative counts, `n_relatives`. Baseline people without
    # the Family History survey stay in, with it missing (`missing_surveys`).
    fh_diagnosis_types = fh.diagnosis_types_count_glad_sa8,
    # Pack-years, never-smokers and vapers 0 (rulings `pack_*`).
    pack_year = smk.pack_years_incl_never_smokers
  )

for (lev in employment_levels[-1]) {
  covariates[[paste0("In_paid_employment_or_self_employed_", lev)]] <-
    level_dummy(covariates$employment, lev)
}

covariates <- covariates %>%
  select(-employment, -marital) %>%
  relocate(starts_with("In_paid_employment_or_self_employed_"), .after = sex)

# The analysis is of people with a depressive or anxiety disorder: drop those
# with neither, and those for whom it is unknown.
in_scope <- covariates %>%
  filter(
    !is.na(depression_and_anxiety),
    depression_and_anxiety != "No depressive or anxiety disorder"
  ) %>%
  select(-depression_and_anxiety)

# ==============================================================================
# FRAMES
# ==============================================================================
static_dat <- in_scope %>%
  inner_join(ben_person, by = "person_id") %>%
  left_join(select(med_person, person_id, avg_start_age), by = "person_id") %>%
  dplyr::mutate(
    se_rating = factor(se_rating, levels = 0:5, ordered = TRUE),
    ben_rating = factor(ben_rating, levels = 1:5, ordered = TRUE),
    n_best = factor(n_best, levels = 0:8, ordered = TRUE)
  )

dat_long <- med_long %>%
  select(
    person_id, medication, start_age, cumulative_med_count,
    effectiveness, n_se, remission, stopped_due_to_se,
    n_se_strict, stopped_due_to_se_strict
  ) %>%
  inner_join(static_dat, by = "person_id") %>%
  dplyr::mutate(
    effectiveness = factor(effectiveness, levels = 0:2, ordered = TRUE),
    # Binary; kept as a two-level ordered factor as before, which fit_glmm()
    # routes to binomial glmer.
    remission = factor(remission, levels = 0:1, ordered = TRUE)
  ) %>%
  dplyr::rename(ID = person_id) %>%
  relocate(ID, cohort)

cache("dat_long")

# Per-participant analysis frame for the legacy outcomes (side-effect
# severity, benefit rating, number of best aspects, first improvement
# duration), which are single responses per participant.
dat <- static_dat %>%
  left_join(select(med_person, -avg_start_age), by = "person_id") %>%
  dplyr::mutate(
    # `intolerance` enters the models as the raw proportion of the
    # participant's antidepressants with a known stop status that were stopped
    # because of side effects, scaled per 10 percentage points (0-10), so its
    # odds ratio is per extra 10% of antidepressants stopped (user rulings
    # 2026-11-09; it used to be cut into three equal-width bands, and its
    # denominator used to be every taken drug).
    intolerance = intolerance * 10,
    first_imprv = factor(first_imprv, levels = 0:5, ordered = TRUE)
  ) %>%
  dplyr::rename(ID = person_id) %>%
  relocate(ID, cohort, n_meds)

# Both frames hold exactly the participants with at least one named
# antidepressant.
stopifnot(setequal(dat$ID, unique(dat_long$ID)))

cache("dat")

# Labels
# ------------------------------------------------------------------------------
lab_grouped_illnesses <- c(
  score_cardiometabolic = "Cardiometabolic & endocrine Disorders",
  score_neurological    = "Neurological conditions",
  score_autoimmune      = "Autoimmune & inflammatory Diseases",
  score_respiratory     = "Respiratory & atopic conditions",
  score_oncology        = "Oncological disorders",
  score_musculoskeletal = "Musculoskeletal & pain disorders"
)

cache("lab_grouped_illnesses")

labels <- c(
  medication = "Medication Name",
  start_age = "Start Age",
  cumulative_med_count = "Cumulative Medication Count",
  effectiveness = "Effectiveness",
  n_se = "Number of Side Effects",
  remission = "Remission",
  stopped_due_to_se = "Stopped due to Side Effects",
  cohort = "Survey cohort (SA8 vs. Baseline)",
  sex = "Sex (female)",
  In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work =
    "Doing unpaid or voluntary work v.s. In paid employment or self-employed",
  In_paid_employment_or_self_employed_Full_or_part_time_student =
    "Full or part-time student v.s In paid employment or self-employed",
  In_paid_employment_or_self_employed_Looking_after_home_and_or_family =
    "Looking after home and/or family v.s In paid employment or self-employed",
  In_paid_employment_or_self_employed_Retired = "Retired v.s In paid employment",
  In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability =
    "Unable to work because of sickness or disability v.s In paid employment or self-employed",
  In_paid_employment_or_self_employed_Unemployed =
    "Unemployed v.s. In paid employment or self-employed",
  bmi = "BMI (kg/m^2)",
  Not_in_relationship_In_relationship = "In relationship v.s Not in relationship",
  Not_in_relationship_Married = "Married v.s Not in relationship",
  eating_disorders_numeric = "Eating disorders",
  mhd_addadhd_numeric = "ADHD",
  obsessive_compulsive_disorders_numeric = "Obsessive compulsive disorders",
  mhd_personality_disorder_numeric = "Personality disorders",
  autism_spectrum_disorder_numeric = "Autism spectrum disorders",
  comorbidity_total_count_numeric = "Number of comorbidities",
  Depressive_and_anxiety_disorder_Only_anxiety_disorder =
    "Only anxiety disorder v.s Depressive and anxiety disorder",
  Depressive_and_anxiety_disorder_Only_depressive_disorder =
    "Only depressive disorder v.s Depressive and anxiety disorder",
  No_psychotic_or_bipolar_disorder_Only_bipolar_disorder =
    "Only bipolar disorder v.s No psychotic or bipolar disorder",
  No_psychotic_or_bipolar_disorder_Only_psychotic_disorder =
    "Only psychotic disorder v.s No psychotic or bipolar disorder",
  No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder =
    "Psychotic and bipolar disorder v.s No psychotic or bipolar disorder",
  disable_bin = "Disability or illness",
  lab_grouped_illnesses,
  audit = "Alcohol use disorder",
  cidid_recurrence = "Number of depressive disorder episodes",
  gad7 = "Current anxiety",
  phq9 = "Current depression",
  wsas = "Work and social impairment",
  fh_diagnosis_types = "Number of psychiatric diagnosis types in immediate family",
  pack_year = "Pack years of cigarettes smoked",
  ben_rating = "Benefit rating",
  n_best = "Number of best aspects",
  avg_start_age = "Average starting age/10",
  se_rating = "Side effect severity rating"
)

cache("labels")

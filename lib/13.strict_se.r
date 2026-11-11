# Strict blank-checklist coding for the strict sensitivity set
# (`glmm_models_cov_strict` in src/02.regression.r; ruling `a3_strict_sens`,
# 2026-11-11, docs/sa8-migration-plan.md).
#
# Main rule (ruling A3 as amended by the owner on 2026-11-06, unchanged and
# primary): with Yes on the any-side-effects question, a drug whose checklist
# page was displayed and recorded (`se_recorded` > 0) with nothing ticked has
# 0 side effects, whether or not its "No side effects" box is ticked; its stop
# status is then 0 too.
#
# Strict rule (supplementary): such a row counts as 0 only when the "No side
# effects" box is ticked. A blank checklist with no "No side effects" tick
# (the ad hoc audit's categories "c mixed" and "c77 stored -77",
# logs/adhoc-se-zero-2026-11-11/) is not confirmed: its number of side effects
# and its stop status are missing. The munge carries both codings
# (`n_se_strict`, `stopped_due_to_se_strict` in `dat_long`).
#
# apply_strict_se() puts the strict values in the `n_se` and
# `stopped_due_to_se` columns, so the strict frame is fitted with exactly the
# same formulas (and labels) as the main sets, and recomputes the within/between
# terms of every decomposed predictor from the strict values: a participant's
# mean of the number of side effects is taken over the drugs with a known
# strict count. The rows are the same as in the main frame (no row is dropped;
# the models' complete cases shrink).
STRICT_SE_COLUMNS <- c(n_se = "n_se_strict", stopped_due_to_se = "stopped_due_to_se_strict")

# Rows whose number of side effects is known under the main rule and missing
# under the strict rule (the unconfirmed blank checklists).
strict_se_recoded <- function(data) {
  missing_cols <- setdiff(STRICT_SE_COLUMNS, names(data))
  if (length(missing_cols)) {
    stop(
      "The frame lacks ", paste(missing_cols, collapse = ", "),
      "; re-run munge/01.dat_clean.r (load.project()).", call. = FALSE
    )
  }
  !is.na(data$n_se) & is.na(data$n_se_strict)
}

# The strict frame: the strict values in `n_se` / `stopped_due_to_se`, the
# `_strict` columns removed, the within/between terms recomputed. The recode
# travels as attribute `strict_se`: rows recoded (overall and by `cohort` where
# present), participants left with no known number of side effects, and the
# known-value counts before and after.
apply_strict_se <- function(data, vars = WITHIN_BETWEEN_VARS, id = "ID") {
  recoded <- strict_se_recoded(data)
  if (any(data$n_se[recoded] != 0) || any(data$stopped_due_to_se[recoded] %in% 1)) {
    stop("A row recoded by the strict rule is not 0 under the main rule.", call. = FALSE)
  }
  known_main <- tapply(!is.na(data$n_se), data[[id]], any)
  known_strict <- tapply(!is.na(data$n_se_strict), data[[id]], any)
  info <- list(
    rows = nrow(data),
    rows_recoded = sum(recoded),
    rows_recoded_by_cohort = if ("cohort" %in% names(data)) table(as.character(data$cohort[recoded])) else NULL,
    participants_recoded = length(unique(data[[id]][recoded])),
    participants_lose_all_n_se = sum(known_main & !known_strict),
    n_se_known_main = sum(!is.na(data$n_se)),
    n_se_known_strict = sum(!is.na(data$n_se_strict)),
    stopped_known_main = sum(!is.na(data$stopped_due_to_se)),
    stopped_known_strict = sum(!is.na(data$stopped_due_to_se_strict))
  )
  for (v in names(STRICT_SE_COLUMNS)) {
    data[[v]] <- data[[STRICT_SE_COLUMNS[[v]]]]
  }
  data <- data[setdiff(names(data), STRICT_SE_COLUMNS)]
  data <- add_within_between(data, vars, id = id)
  attr(data, "strict_se") <- info
  data
}

# "3,299 rows (Baseline 2,454, SA8 845)": the recoded rows, for logs and the
# markdown index. `by_cohort = FALSE` gives "3,299 rows" alone, for table
# footnotes, which do not split the sample by survey cohort (ruling
# 2026-11-11; only the survey-cohort tables S5a-S5e name the cohorts).
strict_se_recoded_text <- function(info, by_cohort = TRUE) {
  by <- if (by_cohort) info$rows_recoded_by_cohort else NULL
  paste0(
    formatC(info$rows_recoded, format = "d", big.mark = ","), " rows",
    if (length(by)) {
      paste0(" (", paste0(names(by), " ", formatC(as.integer(by), format = "d", big.mark = ","), collapse = ", "), ")")
    } else {
      ""
    }
  )
}

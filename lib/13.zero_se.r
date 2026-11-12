# Zero-rule coding of blank side-effect checklists, for the zero-rule
# sensitivity set (`glmm_models_cov_zero` in src/02.regression.r, Supplementary
# Table S6).
#
# Main rule (ruling A3 as ruled by the owner on 2026-11-12, which made the
# strict blank-checklist rule the main rule): with Yes on the any-side-effects
# question, a drug whose checklist page was displayed and recorded
# (`se_recorded` > 0) with nothing ticked counts as 0 side effects only when
# its "No side effects" box is ticked. Without that tick (the 2026-11-11
# audit's categories "c mixed" and "c77 stored -77",
# logs/adhoc-se-zero-2026-11-11/) its number of side effects and its stop
# status are missing.
#
# Zero rule (supplementary; the main rule from the 2026-11-06 amendment of A3
# until 2026-11-12): such a row is 0 side effects and not stopped because of
# side effects, with or without the "No side effects" tick. The munge carries
# both codings (`n_se_zero`, `stopped_due_to_se_zero` in `dat_long`, beside the
# main `n_se`, `stopped_due_to_se`) and checks that they differ on those rows
# only.
#
# apply_zero_se() puts the zero-rule values in the `n_se` and
# `stopped_due_to_se` columns, drops the `_zero` columns and recomputes the
# within/between terms of every decomposed predictor, so the zero-rule frame is
# fitted with exactly the same formulas (and labels) as the main sets and a
# participant's mean number of side effects is taken over the drugs with a
# known zero-rule count. The rows are the same as in the main frame.
#
# Cache reuse: the model frame of a zero-rule set (ID, outcome and fixed
# columns) is then the model frame the same set had in `glmm_models_cov` under
# the previous main rule (code up to a3e15bd): same column names, same values.
# So set_cache_path() in src/02.regression.r gives it the same key and the fits
# made then are reused. That is why the frame carries no extra attribute (the
# counts are zero_se_info()'s) and why the column names must not change.
ZERO_SE_COLUMNS <- c(n_se = "n_se_zero", stopped_due_to_se = "stopped_due_to_se_zero")

check_zero_se_columns <- function(data) {
  missing_cols <- setdiff(ZERO_SE_COLUMNS, names(data))
  if (length(missing_cols)) {
    stop(
      "The frame lacks ", paste(missing_cols, collapse = ", "),
      "; re-run munge/01.dat_clean.r (load.project()).", call. = FALSE
    )
  }
  invisible(TRUE)
}

# Rows whose number of side effects is missing under the main rule and known
# (0) under the zero rule: the unconfirmed blank checklists.
zero_se_recoded <- function(data) {
  check_zero_se_columns(data)
  is.na(data$n_se) & !is.na(data$n_se_zero)
}

# The recode counts: rows recoded (overall and by `cohort` where present),
# their participants, participants with no known number of side effects under
# the main rule who have one under the zero rule, and the known-value counts
# under both rules.
zero_se_info <- function(data, id = "ID") {
  recoded <- zero_se_recoded(data)
  known_main <- tapply(!is.na(data$n_se), data[[id]], any)
  known_zero <- tapply(!is.na(data$n_se_zero), data[[id]], any)
  list(
    rows = nrow(data),
    rows_recoded = sum(recoded),
    rows_recoded_by_cohort = if ("cohort" %in% names(data)) table(as.character(data$cohort[recoded])) else NULL,
    participants_recoded = length(unique(data[[id]][recoded])),
    participants_no_known_n_se_main = sum(known_zero & !known_main),
    n_se_known_main = sum(!is.na(data$n_se)),
    n_se_known_zero = sum(!is.na(data$n_se_zero)),
    stopped_known_main = sum(!is.na(data$stopped_due_to_se)),
    stopped_known_zero = sum(!is.na(data$stopped_due_to_se_zero))
  )
}

# The zero-rule frame: the zero-rule values in `n_se` / `stopped_due_to_se`,
# the `_zero` columns removed, the within/between terms recomputed.
apply_zero_se <- function(data, vars = WITHIN_BETWEEN_VARS, id = "ID") {
  recoded <- zero_se_recoded(data)
  if (any(data$n_se_zero[recoded] != 0) || any(data$stopped_due_to_se_zero[recoded] %in% 1)) {
    stop("A row recoded by the zero rule is not 0 under it.", call. = FALSE)
  }
  same_n <- identical(data$n_se[!recoded], data$n_se_zero[!recoded])
  same_s <- identical(is.na(data$stopped_due_to_se) & !recoded, is.na(data$stopped_due_to_se_zero) & !recoded) &&
    isTRUE(all(data$stopped_due_to_se == data$stopped_due_to_se_zero, na.rm = TRUE))
  if (!same_n || !same_s) {
    stop("The zero rule and the main rule differ outside the blank checklists.", call. = FALSE)
  }
  for (v in names(ZERO_SE_COLUMNS)) {
    data[[v]] <- data[[ZERO_SE_COLUMNS[[v]]]]
  }
  data <- data[setdiff(names(data), ZERO_SE_COLUMNS)]
  add_within_between(data, vars, id = id)
}

# "3,273 rows (Baseline 2,428, SA8 845)": the recoded rows, for logs and the
# markdown index. `by_cohort = FALSE` gives "3,273 rows" alone, for table
# footnotes, which do not split the sample by survey cohort (ruling
# 2026-11-11; only the reference-only survey-cohort tables name the cohorts).
zero_se_recoded_text <- function(info, by_cohort = TRUE) {
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

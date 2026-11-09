# Hybrid within/between decomposition for the longitudinal GLMMs (user ruling
# 2026-11-09, afternoon).
#
# A medication-level predictor X of a medication-level outcome mixes two
# contrasts: between participants (people whose X is higher on average) and
# within a participant (the same person's medications with a higher X than
# their own average). The random-intercept GLMM with X entered once returns a
# weighted blend of the two. The hybrid model enters both in the same model:
#
#   X_between  the participant's mean of X over their medications with a
#              known X (in the frame the model is fitted on)
#   X_within   X minus that mean
#
# with the participant random intercept kept. X_within is informed only by
# participants with two or more medications with a known X; for everyone else
# it is 0. The mean is taken over every row of the frame with a known X, not
# only the complete-case rows of a particular model, so X_within need not sum
# to exactly 0 within a person in a model's own sample.
#
# Which predictors: those that vary within participant in `dat_long`, checked
# empirically on the full-cohort frame (2026-11-09): start age, cumulative
# medication count, number of side effects and stopped because of side
# effects. Every other focal predictor (and `se_rating`) is constant within a
# participant. The decomposition replaces a focal predictor set only; a
# within-person covariate in an adjustment set (start age and the cumulative
# medication count in the adjusted models) is entered as it is.
WITHIN_BETWEEN_VARS <- c("start_age", "cumulative_med_count", "n_se", "stopped_due_to_se")

# The two term names, within first, for each of `vars`.
within_between_terms <- function(vars) {
  as.vector(rbind(paste0(vars, "_within"), paste0(vars, "_between")))
}

# Adds `<v>_between` and `<v>_within` for each of `vars`, computed per `id`.
# Re-running it on a subset (for example the class-scoped frame) recomputes
# the means over that subset's rows.
add_within_between <- function(data, vars = WITHIN_BETWEEN_VARS, id = "ID") {
  for (v in vars) {
    x <- as.numeric(data[[v]])
    m <- stats::ave(x, data[[id]], FUN = function(z) {
      if (all(is.na(z))) rep(NA_real_, length(z)) else rep(mean(z, na.rm = TRUE), length(z))
    })
    data[[paste0(v, "_between")]] <- m
    data[[paste0(v, "_within")]] <- x - m
  }
  data
}

# Replaces every one-variable predictor set c(X), X in `vars`, by the two-term
# set c(X_within, X_between). Run it after any filtering that matches sets by
# variable name (get_adjusted_indeps_list_for_dep()).
decompose_indeps_list <- function(indeps_list, vars = WITHIN_BETWEEN_VARS) {
  lapply(indeps_list, function(s) {
    if (length(s) == 1 && s %in% vars) within_between_terms(s) else s
  })
}

# "<label> (within person)" and "<label> (between persons)" for each of
# `vars`, from the label of the undecomposed variable.
within_between_labels <- function(vars = WITHIN_BETWEEN_VARS, labels) {
  base <- unname(labels[vars])
  base[is.na(base)] <- vars[is.na(base)]
  stats::setNames(
    as.vector(rbind(paste0(base, " (within person)"), paste0(base, " (between persons)"))),
    within_between_terms(vars)
  )
}

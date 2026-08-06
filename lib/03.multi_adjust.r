# Widen Wald confidence intervals to the Bonferroni-corrected level.
#
# The panels colour each estimate by its Bonferroni-corrected p value, but used
# to draw the *uncorrected* 95% interval alongside it. The two disagree for 55 of
# the 534 plotted points (10.3%): an estimate is drawn grey while its interval
# visibly excludes 1 -- ADHD on side effect severity rating, at 1.33 [1.01, 1.77]
# with a corrected p of 0.177, is the clearest case. Nothing was drawn orange
# with an interval covering 1, so the colour was never wrong; the interval simply
# answered a different question from the colour.
#
# Correcting the interval too makes the two agree by construction, because the
# Bonferroni-corrected Wald test and the correspondingly widened Wald interval
# are the same test: p * n < 0.05 exactly when the interval at the
# 1 - 0.05/n level excludes 1.
#
# parameters() returns intervals that are symmetric on the log scale (checked
# across every cached model: maximum asymmetry 1e-15), so the standard error is
# recovered exactly rather than approximated.
bonferroni_ci <- function(models, n_deps = length(models)) {
  z_raw <- stats::qnorm(0.975)
  z_adj <- stats::qnorm(1 - 0.025 / n_deps)

  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      if (is.null(model) || nrow(model) == 0) {
        return(model)
      }
      # "" for the ordinary models, the two suffixes for the hurdle models.
      for (sfx in c("", "_logistic", "_gamma")) {
        cc <- paste0("Coefficient", sfx)
        lc <- paste0("CI_low", sfx)
        hc <- paste0("CI_high", sfx)
        if (!all(c(cc, lc, hc) %in% colnames(model))) next
        se <- (log(model[[hc]]) - log(model[[lc]])) / (2 * z_raw)
        model[[lc]] <- exp(log(model[[cc]]) - z_adj * se)
        model[[hc]] <- exp(log(model[[cc]]) + z_adj * se)
      }
      model
    })
  })
}

multi_adjust <- function(models, n_deps = length(models)) {
  # Adjust by the number of dependent variables.
  #
  # `n_deps` defaults to the number of outcomes in `models`, which is correct
  # when the whole outcome set is passed at once (as the result workbooks do).
  # Pass it explicitly when plotting a *subset* of an outcome set, so that the
  # figures apply the same Bonferroni factor as the tables they accompany —
  # otherwise a subset is under-corrected and an estimate can be drawn as
  # significant in a panel while being absent from the corresponding table.
  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      if (is.null(model) || nrow(model) == 0) return(model)
      p_cols <- grep("^p", colnames(model))
      model[p_cols] <- model[p_cols] * n_deps
      model
    })
  })
}

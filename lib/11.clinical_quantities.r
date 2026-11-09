# Helpers for src/06.clinical_quantities.r: clinically interpretable quantities
# (a cross-table with cluster-bootstrap proportions, model-based predicted
# probabilities) from the analysis frames and the cached model objects, without
# refitting.

# ------------------------------------------------------------------------------
# Recovering a fitted model from a cached result table
# ------------------------------------------------------------------------------
# fit_glmm() keeps no fitted model on its result, but for the lme4 fits one
# survives by accident: `parameters()` stores the model call, and lme4 puts
# the evaluated formula into it, whose environment is the frame of fit_glmm()
# itself. That frame holds `m` (the glmerMod), `data` (the complete-case rows,
# unscaled), `fit_data` (the rows the model saw, continuous predictors centred
# and scaled) and `scales`. This is why the cached glmer sets weigh ~18 MB each.
# The clmm call names `fm` as a bare symbol, so no environment is kept: the
# ordinal (effectiveness) fits keep only the exponentiated focal estimates and
# their CIs, with no thresholds, covariate coefficients or covariance, and
# nothing model-based can be predicted from them without a refit.
#
# Returns that frame, or NULL when the set carries none.
cached_fit_frame <- function(set) {
  if (is.null(set)) {
    return(NULL)
  }
  mc <- attr(set, "model_call")
  if (!is.call(mc)) {
    return(NULL)
  }
  f <- as.list(mc)$formula
  if (!inherits(f, "formula")) {
    return(NULL)
  }
  en <- environment(f)
  if (is.null(en) || !all(c("m", "data", "scales") %in% ls(en))) {
    return(NULL)
  }
  if (!inherits(en$m, "merMod")) {
    return(NULL)
  }
  en
}

# The set of `sets` whose formula's first focal terms are `terms`, matched on
# the `model_formula` attribute rather than on list position.
find_set_by_terms <- function(sets, terms) {
  hit <- Filter(function(s) {
    if (is.null(s)) {
      return(FALSE)
    }
    f <- attr(s, "model_formula")
    if (is.null(f)) {
      return(FALSE)
    }
    rhs <- trimws(strsplit(sub("^[^~]*~", "", f), "+", fixed = TRUE)[[1]])
    identical(rhs[seq_along(terms)], terms)
  }, sets)
  if (length(hit)) hit[[1]] else NULL
}

# ------------------------------------------------------------------------------
# Proportions with a participant cluster bootstrap
# ------------------------------------------------------------------------------
# Proportion sum(num) / sum(den) over medication rows, with a percentile CI
# from resampling participants (with replacement), so that the dependence
# between one person's medications is respected. `num` and `den` are logical or
# 0/1 per row; `id` is the participant.
cluster_boot_prop <- function(num, den, id, B = 2000, level = 0.95) {
  agg <- tibble(id = id, num = as.numeric(num), den = as.numeric(den)) %>%
    group_by(id) %>%
    dplyr::summarise(num = sum(num), den = sum(den), .groups = "drop") %>%
    filter(den > 0)
  n <- nrow(agg)
  est <- sum(agg$num) / sum(agg$den)
  boot <- vapply(seq_len(B), function(b) {
    i <- sample.int(n, n, replace = TRUE)
    sum(agg$num[i]) / sum(agg$den[i])
  }, numeric(1))
  a <- (1 - level) / 2
  list(
    k = sum(agg$num), n = sum(agg$den), n_id = n, est = est,
    lo = unname(stats::quantile(boot, a)), hi = unname(stats::quantile(boot, 1 - a)), B = B
  )
}

# ------------------------------------------------------------------------------
# Predicted probabilities from a recovered binomial glmer
# ------------------------------------------------------------------------------
# A predictor value on the model's own (centred and scaled) footing:
# fit_glmm() centres and scales every continuous predictor in its complete-case
# `data` and leaves binary ones alone (scale 1, not centred).
to_fit_scale <- function(value, var, frame) {
  s <- frame$scales[[var]]
  if (s == 1) value else (value - mean(frame$data[[var]])) / s
}

# `n` draws of the fixed effects from N(beta_hat, V), by the Cholesky factor.
draw_fixef <- function(beta, V, n) {
  z <- matrix(stats::rnorm(n * length(beta)), n)
  sweep(z %*% chol(V), 2, beta, "+")
}

# Average predicted probability of the outcome's second level over the model's
# own rows, with the participant random intercept set to 0, under each of
# `scenarios`: a named list of functions(data) returning a named list of
# predictor values (unscaled; a vector of one value per row, or one value).
# The estimate uses the fitted fixed effects; the 95% CI is the percentile
# interval over `draws` (a matrix of fixed-effect draws, one per row), the
# parametric bootstrap. Returns per-scenario estimates and the draws of each,
# so that contrasts between scenarios get their own interval.
predict_scenarios <- function(frame, scenarios, draws) {
  m <- frame$m
  X0 <- lme4::getME(m, "X")
  beta <- lme4::fixef(m)
  stopifnot(identical(colnames(X0), names(beta)), nrow(X0) == nrow(frame$data))
  lapply(scenarios, function(sc) {
    X <- X0
    vals <- sc(frame$data)
    for (v in names(vals)) {
      if (!v %in% colnames(X)) stop("Scenario term not in the model: ", v)
      X[, v] <- to_fit_scale(vals[[v]], v, frame)
    }
    est <- mean(stats::plogis(drop(X %*% beta)))
    boot <- colMeans(stats::plogis(X %*% t(draws)))
    list(est = est, boot = boot)
  })
}

ci_text <- function(est, boot, scale = 100, digits = 1) {
  q <- stats::quantile(boot, c(0.025, 0.975))
  f <- function(x) formatC(scale * x, format = "f", digits = digits)
  paste0(f(est), " [", f(q[[1]]), ", ", f(q[[2]]), "]")
}

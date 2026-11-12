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
# Refitting the three adjusted ordinal effectiveness models (authorised
# 2026-11-09 for exactly these three)
# ------------------------------------------------------------------------------
# The cached clmm results cannot be predicted from (see cached_fit_frame()).
# src/06.clinical_quantities.r refits the adjusted effectiveness models with
# the stopped, number-of-side-effects (within/between) and severity sets, with
# the formula taken verbatim from the cached set's `model_formula` attribute and
# the data built as src/02.regression.r builds it (coerce_glmm_predictor(), then
# add_within_between(), then fit_glmm()'s na.omit() over ID, outcome and
# predictors; clmm predictors are not scaled). Full fits are kept in
# cache/clinical_fits/<key>.rds, keyed by formula + data, and reused.
CLINICAL_FIT_DIR <- file.path("cache", "clinical_fits")

# The complete-case rows and formula for one model, as fit_glmm() builds them.
clmm_job <- function(model_formula, data_long) {
  dep <- trimws(sub("~.*$", "", model_formula))
  indeps <- trimws(strsplit(sub("^[^~]*~", "", model_formula), "+", fixed = TRUE)[[1]])
  d <- na.omit(data_long[c("ID", dep, indeps)])
  fm <- paste(dep, "~", paste(indeps, collapse = "+"), "+ (1 | ID)")
  key <- digest::digest(list(recipe = "clmm-logit-unscaled", formula = fm, data = digest::digest(d)))
  list(dep = dep, indeps = indeps, formula = fm, data = d,
    path = file.path(CLINICAL_FIT_DIR, paste0(key, ".rds")))
}

# Fits each job whose file is missing, `workers` at a time on fresh PSOCK
# workers (see fit_family() in src/02.regression.r for why not fork), and
# returns the saved objects: list(m, data, formula, seconds, fitted_at).
fit_clinical_clmms <- function(jobs, workers = 3) {
  dir.create(CLINICAL_FIT_DIR, showWarnings = FALSE, recursive = TRUE)
  todo <- Filter(function(j) !file.exists(j$path), jobs)
  message("  ", length(jobs), " effectiveness models, ", length(todo), " to fit, on ",
    min(workers, max(length(todo), 1)), " workers")
  if (length(todo)) {
    cl <- parallel::makePSOCKcluster(min(workers, length(todo)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    started <- Sys.time()
    done <- parallel::parLapplyLB(cl, todo, function(job) {
      suppressMessages(library(ordinal))
      fm <- stats::as.formula(job$formula)
      # The formula environment would otherwise drag this frame (and the data)
      # into the saved object.
      environment(fm) <- baseenv()
      t0 <- Sys.time()
      m <- ordinal::clmm(fm, data = job$data, link = "logit")
      secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      saveRDS(list(m = m, data = job$data, formula = job$formula, seconds = secs, fitted_at = Sys.time()), job$path)
      c(path = job$path, seconds = secs)
    })
    for (d in done) message("  fitted ", basename(d[["path"]]), " in ", round(as.numeric(d[["seconds"]]) / 60, 1), " min")
    message("  all fits done in ", round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), " min wall time")
  }
  lapply(jobs, function(j) readRDS(j$path))
}

# Exponentiated focal estimates with Wald 95% CIs from a clmm fit, as
# parameters() reports them, for comparison with the cached table.
clmm_focal_or <- function(m, terms) {
  b <- m$beta[terms]
  se <- sqrt(diag(stats::vcov(m))[terms])
  z <- stats::qnorm(0.975)
  tibble(term = terms, Coefficient = exp(b), CI_low = exp(b - z * se), CI_high = exp(b + z * se))
}

# ------------------------------------------------------------------------------
# Predicted probabilities, random intercept at 0, averaged over the model's rows
# ------------------------------------------------------------------------------
# A predictor value on the model's own footing: fit_glmm() centres and scales
# every continuous predictor of an lme4 fit in its complete-case `data` and
# leaves binary ones (scale 1) alone; clmm fits are unscaled (scales all 1).
to_fit_scale <- function(value, var, frame) {
  s <- frame$scales[[var]]
  if (s == 1) value else (value - mean(frame$data[[var]])) / s
}

# `n` draws from N(mu, V), by the Cholesky factor.
draw_mvn <- function(mu, V, n) {
  z <- matrix(stats::rnorm(n * length(mu)), n)
  sweep(z %*% chol(V), 2, mu, "+")
}

# What prediction needs from a fitted model: the point estimate `theta` and
# covariance `V` of the fixed parameters, the design matrix of the model's own
# rows, and `probs(X, theta_rows)`, which returns one row per parameter vector
# (row of `theta_rows`) and one column per outcome category: the probability
# averaged over the rows of X with the random intercept at 0.
#   glmer (binomial): P(second level) = plogis(X beta), X with intercept.
#   clmm (logit): P(Y <= j) = plogis(alpha_j - X beta), X without intercept.
model_predictor <- function(frame, categories) {
  m <- frame$m
  if (inherits(m, "glmerMod")) {
    stopifnot(stats::family(m)$family == "binomial")
    theta <- lme4::fixef(m)
    X0 <- lme4::getME(m, "X")
    stopifnot(identical(colnames(X0), names(theta)))
    probs <- function(X, th) matrix(colMeans(stats::plogis(X %*% t(th))), ncol = 1, dimnames = list(NULL, categories))
    V <- as.matrix(stats::vcov(m))
  } else if (inherits(m, "clmm")) {
    theta <- c(m$alpha, m$beta)
    k <- length(m$alpha)
    X0 <- as.matrix(frame$data[names(m$beta)])
    V <- as.matrix(stats::vcov(m))[names(theta), names(theta)]
    probs <- clmm_probs(k, categories)
  } else {
    stop("No predictor for a ", class(m)[1], " fit")
  }
  stopifnot(nrow(X0) == nrow(frame$data))
  list(theta = theta, V = V, X0 = X0, probs = probs)
}

# P(category), averaged over the rows of X with the random intercept at 0, for
# each parameter vector (row of `th`: k thresholds, then the coefficients):
# P(Y <= j) = plogis(alpha_j - X beta).
clmm_probs <- function(k, categories) {
  function(X, th) {
    eta <- X %*% t(th[, -seq_len(k), drop = FALSE])
    cum <- vapply(seq_len(k), function(j) colMeans(stats::plogis(sweep(-eta, 2, th[, j], "+"))), numeric(nrow(th)))
    cum <- matrix(cum, nrow(th))
    out <- cbind(cum, 1) - cbind(0, cum)
    colnames(out) <- categories
    out
  }
}

# The same predictor from a cached ordinal result of src/02.regression.r, for
# fits made since 2026-11-11, which carry their thresholds, coefficients and
# covariance (attributes `clmm_alpha`, `clmm_beta`, `clmm_vcov`, set by
# fit_glmm()), so nothing needs refitting. `data` is the model's complete-case
# rows (clmm_job()); a factor covariate (the drug identity) becomes its
# indicator columns by the same treatment coding clmm used. Returns
# list(frame, pred), `frame` as predict_scenarios() expects it (clmm
# predictors are unscaled), or NULL when the set carries no thresholds.
cached_clmm_predictor <- function(set, data, indeps, categories) {
  alpha <- attr(set, "clmm_alpha")
  beta <- attr(set, "clmm_beta")
  V <- attr(set, "clmm_vcov")
  if (is.null(alpha) || is.null(beta) || is.null(V)) {
    return(NULL)
  }
  mm <- stats::model.matrix(stats::reformulate(indeps), data)
  if (!all(names(beta) %in% colnames(mm))) {
    stop("Design matrix lacks coefficient(s): ", paste(setdiff(names(beta), colnames(mm)), collapse = ", "))
  }
  theta <- c(alpha, beta)
  list(
    frame = list(data = data, scales = stats::setNames(rep(1, length(indeps)), indeps)),
    pred = list(theta = theta, V = V[names(theta), names(theta)], X0 = mm[, names(beta), drop = FALSE],
      probs = clmm_probs(length(alpha), categories))
  )
}

# For each of `scenarios` (functions(data) returning a named list of unscaled
# predictor values, one per row or one overall), the point estimate (a named
# vector over categories) and the parametric bootstrap draws (a draws x
# categories matrix) of the averaged predicted probabilities. `draws` is a
# matrix of parameter draws (one per row) from draw_mvn().
predict_scenarios <- function(frame, pred, scenarios, draws) {
  lapply(scenarios, function(sc) {
    X <- pred$X0
    vals <- sc(frame$data)
    for (v in names(vals)) {
      if (!v %in% colnames(X)) stop("Scenario term not in the model: ", v)
      X[, v] <- to_fit_scale(vals[[v]], v, frame)
    }
    list(est = pred$probs(X, matrix(pred$theta, 1))[1, ], boot = pred$probs(X, draws))
  })
}

ci_text <- function(est, boot, scale = 100, digits = 1) {
  q <- stats::quantile(boot, c(0.025, 0.975))
  f <- function(x) formatC(scale * x, format = "f", digits = digits)
  paste0(f(est), " [", f(q[[1]]), ", ", f(q[[2]]), "]")
}

# ------------------------------------------------------------------------------
# Table text (Tables 2 and 5)
# ------------------------------------------------------------------------------
# Kept here rather than in src/06 so that the captions, row labels and
# footnotes can be rebuilt and checked without fitting anything. Main-text
# tables make no distinction between the survey cohorts (ruling 2026-11-11).
#
# Main-text numbers of the tables src/06 writes or cites (renumbered
# 2026-11-11: the cross-table 4 -> 2, effectiveness tables 3a-3e -> 4a-4e).
CLINICAL_TABLE_NUMBERS <- c(
  cross = "2", predicted = "5", effectiveness = "4a", remission = "4d"
)
CLINICAL_TABLE_TITLES <- c(
  cross = "Reported effectiveness by stopping because of side effects, per medication",
  predicted = "Predicted probabilities of reported effectiveness and remission by side-effect measures"
)
clinical_table_heading <- function(which) {
  paste0("#### Table ", CLINICAL_TABLE_NUMBERS[[which]], ": ", CLINICAL_TABLE_TITLES[[which]])
}

# Table 2 footnote. `effective_stopped` and `not_effective_not_stopped` are the
# overall cells of the two derived proportions ("k / n: est% [lo, hi]").
cross_table_footnote <- function(effective_stopped, not_effective_not_stopped, n_boot) {
  paste0(
    "Medications (rows of the longitudinal frame: participant x named antidepressant taken). Columns: stopped ",
    "because of side effects. Cells: n (row %); the Total row gives the column distribution. Unknown: ",
    "effectiveness \"don't know\", not answered or not shown; stop status unknown because the number of ",
    "side effects is unknown (including a side-effect checklist displayed but left with nothing ticked and ",
    "without the \"No side effects\" box ticked) or the stop item was seen and not answered (a medication ",
    "with no side effects counts as not stopped). Of medications reported as working moderately or very well and with ",
    "a known stop status, stopped because of side effects: ", effective_stopped,
    ". Of medications reported as not working at all and with a known stop status, not stopped: ",
    not_effective_not_stopped, ". 95% CI by a participant cluster bootstrap (",
    formatC(n_boot, format = "d", big.mark = ","), " resamples of participants, percentile interval), which ",
    "respects the clustering of medications within participants."
  )
}

# Table 5 italic heading row of each outcome block.
pp_outcome_headings <- function() {
  c(
    Effectiveness = paste0(
      "Effectiveness: probability of each reported effectiveness category (Table ",
      CLINICAL_TABLE_NUMBERS[["effectiveness"]], ")"
    ),
    Remission = paste0(
      "Remission: probability of remission on the medication (Table ", CLINICAL_TABLE_NUMBERS[["remission"]], ")"
    )
  )
}

# Table 5 footnote.
pp_footnote <- function(any_unadjusted, n_boot) {
  te <- CLINICAL_TABLE_NUMBERS[["effectiveness"]]
  tr <- CLINICAL_TABLE_NUMBERS[["remission"]]
  paste0(
    "Predicted probability of each outcome category for a medication, from the ",
    if (any_unadjusted) "longitudinal models (adjusted where marked, otherwise unadjusted)" else
      "adjusted longitudinal models (sex, starting age, cumulative medication count and PHQ-9)",
    ", each side-effect measure in its own model, with the participant random intercept: ordinal logistic for ",
    "effectiveness, logistic for remission. ",
    "Predictions are averaged over the observed covariate values of the model's own medication observations ",
    "with the random intercept set to 0, i.e. for a typical participant (conditional, not marginal over ",
    "the random effect). Stopped vs not stopped: the same medication with the participant's own mean ",
    "(between-person term) kept at its observed value, i.e. the within-person contrast. Mean number of side ",
    "effects: the participant's mean at its 25th and 75th percentile over the participants in the model, ",
    "the within-person term at 0. Severity rating (0-5): 25th and 75th percentile over participants. ",
    "95% CI: parametric bootstrap of the fixed effects (", formatC(n_boot, format = "d", big.mark = ","),
    " multivariate normal draws from their estimated covariance, including the effectiveness thresholds; ",
    "percentile interval); differences are from the same draws. OR: the adjusted estimate of Table ", te,
    " (effectiveness, per step towards better effectiveness) or Table ", tr, " (remission). The remission ",
    "models are the cached fits; the three effectiveness models were refitted with the same formula and ",
    "complete-case rows, reproducing Table ", te, "'s estimates."
  )
}

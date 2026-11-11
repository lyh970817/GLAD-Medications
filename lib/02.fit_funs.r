require(ordinal)
require(parameters)
require(lme4)
require(Matrix)

select_model_fields <- function(model, stp = NULL) {
  m_ptrs <- parameters(model) %>%
    as_tibble() %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
    select(Parameter, Coefficient, CI_low, CI_high, p)

  if (!is.null(stp)) {
    # Specify which part of the model
    mutate(m_ptrs, step = stp)
  } else {
    m_ptrs
  }
}

# lme4's default optimiser (nloptwrap) terminates at a badly scaled point when
# the predictors span very different ranges -- 0/1 dummies alongside, say,
# n_relatives (0-51) or phq9 (0-27). lme4 then builds the fixed-effect
# variance-covariance matrix from a Hessian whose eigenvalues are of order 1e7,
# and hands every coefficient the same, absurdly small standard error. That is
# what produced the zero-width confidence intervals with p < 0.001 in the
# adjusted Remission models (e.g. "Only psychotic disorder" at 1.47[1.47, 1.47]).
# lme4 does warn about it, but the warnings were previously discarded.
#
# The remedy is the one lme4's own warning suggests: centre and scale the
# continuous predictors, and use bobyqa. Coefficients are converted back to the
# original units afterwards by `rescale_parameters()`, so reported odds ratios
# stay per unit rather than per standard deviation.
glmm_control <- function() {
  lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
}

# Standard deviations used to scale each continuous predictor. A predictor with
# two or fewer distinct values (dummies, binary factors) is left alone.
predictor_scales <- function(data, indeps) {
  vapply(indeps, function(v) {
    x <- data[[v]]
    if (!is.numeric(x) || length(unique(x)) <= 2) {
      return(1)
    }
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) 1 else s
  }, numeric(1))
}

scale_predictors <- function(data, scales) {
  for (v in names(scales)[scales != 1]) {
    data[[v]] <- (data[[v]] - mean(data[[v]])) / scales[[v]]
  }
  data
}

# `select_model_fields()` has already exponentiated, so undoing a scaling by s
# means exp(beta / s) = exp(beta)^(1/s). Wald intervals are symmetric on the log
# scale, so the same power applies to both bounds. p values are invariant.
rescale_parameters <- function(tab, scales) {
  scaled <- names(scales)[scales != 1]
  idx <- match(tab$Parameter, scaled)
  hit <- !is.na(idx)
  if (any(hit)) {
    p <- 1 / scales[scaled][idx[hit]]
    for (col in c("Coefficient", "CI_low", "CI_high")) {
      tab[[col]][hit] <- tab[[col]][hit]^p
    }
  }
  tab
}

# Participant random-intercept variance and the intraclass correlation implied
# by it. For a logit link the level-1 variance is pi^2/3; for a log link there is
# no closed form, so only the variance is reported.
random_intercept_summary <- function(m) {
  v <- tryCatch({
    if (inherits(m, "merMod")) {
      as.numeric(lme4::VarCorr(m)[["ID"]][1, 1])
    } else if (inherits(m, "clmm")) {
      as.numeric(m$ST[[1]])^2
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  logit <- inherits(m, "clmm") ||
    (inherits(m, "glmerMod") && stats::family(m)$link == "logit")

  list(var = v, icc = if (isTRUE(logit)) v / (v + pi^2 / 3) else NA_real_)
}

# Pearson dispersion of a count fit: the sum of squared Pearson residuals over
# the residual degrees of freedom. Near 1 the Poisson variance assumption holds;
# well above 1 the data are overdispersed and the Poisson standard errors are too
# small. The fitted model is not kept, so this is the only point at which it can
# be computed. The Limitations section quotes it for the number of side effects.
pearson_dispersion <- function(m) {
  tryCatch(
    sum(stats::residuals(m, type = "pearson")^2) / stats::df.residual(m),
    error = function(e) NA_real_
  )
}

# How src/02.regression.r turns a predictor column of `dat_long` into the
# numeric column the GLMMs are fitted on: an ordered factor becomes its level
# index (so `se_rating`, levels 0-5, is coded 1-6), a two-level factor 0/1.
# Shared with src/06.clinical_quantities.r, which rebuilds the same frame.
coerce_glmm_predictor <- function(x) {
  if (is.ordered(x)) {
    return(as.numeric(x))
  }

  if (is.factor(x) && nlevels(x) == 2) {
    return(as.numeric(x) - 1)
  }

  x
}

# The covariance of a clmm fit, or NULL with the reason. vcov.clmm() inverts the
# numerical Hessian by a Cholesky factorisation and stops when it is not
# positive definite; on the corrected data (2026-11-11) one ordinal fit did, and
# the bare as.matrix(vcov(m)) call took its whole model family down with it.
clmm_vcov <- function(m) {
  tryCatch(
    list(V = as.matrix(stats::vcov(m)), error = NULL),
    error = function(e) list(V = NULL, error = gsub("\\s+", " ", conditionMessage(e)))
  )
}

# Refits tried, in this order, only when a clmm's Hessian is not positive
# definite; a fit whose Hessian is fine never reaches them, so every cached
# clean fit is unaffected. A remedy is accepted only if its Hessian is positive
# definite and its log-likelihood is not worse than the original fit's (to
# 1e-6). Both are deterministic. With a single random intercept clmm takes its
# ssr path, where ucminf is supported (tau on the log-SD scale, central
# difference gradient); the second keeps nlminb but lets it run longer to a
# tighter tolerance, with a tighter inner (random-mode) Newton tolerance.
CLMM_HESSIAN_REMEDIES <- list(
  ucminf = list(method = "ucminf", maxIter = 200, gradTol = 1e-6),
  nlminb_tight = list(method = "nlminb", maxIter = 200, gradTol = 1e-6,
    eval.max = 5000, iter.max = 2500, rel.tol = 1e-12)
)

# A set that produced no estimates because fitting (or extracting from the fit)
# raised an error: a zero-row table, which bind_models(), multi_adjust(),
# only_sig() and the manuscript tables already treat as a failed set, marked
# not converged so report_convergence() lists it with the error message. Never
# written to the per-set cache, so a rerun tries the fit again.
failed_fit <- function(dep, indeps, message) {
  ms <- tibble::tibble(
    Parameter = character(), Coefficient = numeric(), CI_low = numeric(),
    CI_high = numeric(), p = numeric(), Coeff_CI = character()
  )
  attr(ms, "dep") <- dep
  attr(ms, "formula") <- paste(dep, "~", paste(indeps, collapse = "+"), "+ (1 | ID)")
  attr(ms, "fit_warnings") <- paste("ERROR:", message)
  attr(ms, "converged") <- FALSE
  attr(ms, "fit_failed") <- TRUE
  ms
}

fit_glmm <- function(dep, indeps, data, family = NULL) {
  # Clean data
  # Ensure ID is included in selection for the random effect
  data <- na.omit(data[c("ID", dep, indeps)])

  if (nrow(data) == 0) {
      warning(paste("Data empty for", dep, "with indeps:", paste(indeps, collapse=", ")))
      return(NULL)
  }

  # Construct formula
  fm_str <- paste(dep, "~", paste(indeps, collapse = "+"), "+ (1 | ID)")
  fm <- as.formula(fm_str)

  # Detect model type
  is_ordered <- is.ordered(data[[dep]])
  is_factor <- is.factor(data[[dep]])
  n_lev <- if (is_factor) nlevels(data[[dep]]) else 0

  engine <- if (is_factor && n_lev == 2) {
    # Binary (whether ordered or not)
    "binomial"
  } else if (is_ordered) {
    # Ordinal mixed model; clmm requires an ordered factor
    "clmm"
  } else if (!is.null(family)) {
    "family"
  } else {
    # Count data (integers >= 0)
    is_count <- is.numeric(data[[dep]]) && all(data[[dep]] %% 1 == 0, na.rm = TRUE) && min(data[[dep]], na.rm = TRUE) >= 0

    if (is_count) {
      if (all(data[[dep]] %in% c(0, 1, NA))) "binomial" else "poisson"
    } else {
      "gaussian"
    }
  }

  # Only the lme4 fits are rescaled. clmm uses its own optimiser and showed no
  # convergence problems on these data, so the ordinal models are left as they
  # were rather than perturbed for the sake of uniformity.
  scales <- if (engine == "clmm") {
    setNames(rep(1, length(indeps)), indeps)
  } else {
    predictor_scales(data, indeps)
  }
  fit_data <- scale_predictors(data, scales)

  # Warnings used to be discarded, which is how a non-converged fit reached a
  # results table unnoticed. Collect them and hand them back on the result.
  # `clmm_control` is NULL for the ordinary fit (clmm's defaults, exactly as
  # before) and one of CLMM_HESSIAN_REMEDIES for a remedy refit.
  run_fit <- function(clmm_control = NULL) {
    w <- character()
    m <- tryCatch(
      withCallingHandlers(
        switch(engine,
          binomial = glmer(fm, data = fit_data, family = binomial, control = glmm_control()),
          poisson  = glmer(fm, data = fit_data, family = poisson, control = glmm_control()),
          family   = glmer(fm, data = fit_data, family = family, control = glmm_control()),
          clmm     = if (is.null(clmm_control)) {
            clmm(fm, data = fit_data, link = "logit")
          } else {
            clmm(fm, data = fit_data, link = "logit",
              control = do.call(ordinal::clmm.control, clmm_control))
          },
          gaussian = lmer(fm, data = fit_data)
        ),
        warning = function(cond) {
          w <<- c(w, conditionMessage(cond))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        w <<- c(w, paste("ERROR:", conditionMessage(e)))
        NULL
      }
    )
    list(m = m, warns = w)
  }

  fit <- run_fit()
  m <- fit$m
  warns <- fit$warns

  if (is.null(m)) {
    # A failed fit used to come back NULL, which the per-set cache stored and
    # the convergence report skipped, so it vanished silently. It now comes
    # back as a failed set carrying its error (not cached; see failed_fit()).
    msg <- tail(warns, 1)
    warning(paste("Model failed for", dep, ":", msg))
    return(failed_fit(dep, indeps, sub("^ERROR: ", "", msg)))
  }

  # An ordinal fit whose Hessian is not positive definite has no covariance, so
  # no standard errors, intervals or p values. Try the remedies in turn and keep
  # the first whose Hessian is positive definite and whose log-likelihood is not
  # worse. If none works the fit is kept (its point estimates are still the
  # optimiser's) but flagged: not converged, intervals and p values NA, no
  # covariance stored.
  clmm_v <- NULL
  remedy <- NULL
  if (engine == "clmm") {
    clmm_v <- clmm_vcov(m)
    if (is.null(clmm_v$V)) {
      ll0 <- as.numeric(stats::logLik(m))
      tried <- character()
      for (rname in names(CLMM_HESSIAN_REMEDIES)) {
        refit <- run_fit(CLMM_HESSIAN_REMEDIES[[rname]])
        if (is.null(refit$m)) {
          tried <- c(tried, paste0(rname, ": ", tail(refit$warns, 1)))
          next
        }
        v2 <- clmm_vcov(refit$m)
        ll2 <- as.numeric(stats::logLik(refit$m))
        if (!is.null(v2$V) && is.finite(ll2) && ll2 >= ll0 - 1e-6) {
          remedy <- paste0(
            "clmm Hessian not positive definite with the default optimiser (logLik ",
            format(ll0, digits = 10), "); refitted with ", rname,
            ": Hessian positive definite, logLik ", format(ll2, digits = 10)
          )
          m <- refit$m
          warns <- refit$warns
          clmm_v <- v2
          break
        }
        tried <- c(tried, paste0(
          rname, ": ",
          if (is.null(v2$V)) "Hessian still not positive definite" else "Hessian positive definite",
          ", logLik ", format(ll2, digits = 10)
        ))
      }
      if (is.null(clmm_v$V)) {
        remedy <- paste0(
          "no remedy accepted (default logLik ", format(ll0, digits = 10), "; ",
          paste(tried, collapse = "; "), ")"
        )
        warns <- c(warns, paste0(
          "clmm Hessian is not positive definite (", clmm_v$error, "); standard errors, ",
          "intervals and p values set to NA; ", remedy
        ))
      }
    }
  }

  # Extract parameters, then undo the scaling so the estimates are per unit of
  # the original predictor rather than per standard deviation. When an ordinal
  # fit has no covariance parameters() only warns, unseen on a worker; make the
  # missing intervals and p values explicit instead.
  ms <- rescale_parameters(select_model_fields(m), scales)
  if (engine == "clmm" && is.null(clmm_v$V)) {
    ms$CI_low <- NA_real_
    ms$CI_high <- NA_real_
    ms$p <- NA_real_
  }
  re <- random_intercept_summary(m)

  # Store dependent variable, and the formula, so a flagged set can be named
  attr(ms, "dep") <- dep
  attr(ms, "formula") <- fm_str
  # Store sample size, at both levels
  attr(ms, "n") <- nrow(data)
  attr(ms, "n_id") <- length(unique(data$ID))
  # Convergence provenance, so a bad fit can never again reach a table silently.
  # A clmm Hessian that is not positive definite counts as a convergence problem.
  attr(ms, "fit_warnings") <- warns
  attr(ms, "converged") <-
    !any(grepl("converge|unidentifiable|degenerate|singular|positive definite|ERROR", warns, ignore.case = TRUE))
  # The remedy refit (if any) behind this fit, or why none was accepted; NULL
  # for an ordinary fit.
  attr(ms, "fit_remedy") <- remedy
  # Participant clustering, reported in Methods
  attr(ms, "re_var") <- re$var
  attr(ms, "icc") <- re$icc
  # Overdispersion check for the Poisson fits; NA for the other families.
  attr(ms, "dispersion") <- if (engine == "poisson") pearson_dispersion(m) else NA_real_
  # An ordinal fit keeps nothing else that can be predicted from (the clmm call
  # carries no environment; see lib/11.clinical_quantities.r), so keep its
  # thresholds, all fixed coefficients (covariates included) and their
  # covariance. Small, and it changes no estimate. Fits cached before
  # 2026-11-11 do not carry them. A fit with no covariance (Hessian not
  # positive definite, no remedy accepted) keeps none of the three, so
  # cached_clmm_predictor() treats it as a set that carries no thresholds.
  if (engine == "clmm" && !is.null(clmm_v$V)) {
    attr(ms, "clmm_alpha") <- m$alpha
    attr(ms, "clmm_beta") <- m$beta
    th <- c(names(m$alpha), names(m$beta))
    attr(ms, "clmm_vcov") <- clmm_v$V[th, th]
  }

  return(ms)
}

fit_hurdle <- function(dep, indeps, data) {
  data <- na.omit(data[c(dep, indeps)])

  # Create variable for logistic regression
  lg <- paste(dep, "lg", sep = "_")
  data[lg] <- ifelse(data[[dep]] > 0, 1, 0)
  # Create formula for logistic regression
  fm_zero <- as.formula(paste(lg, "~", paste(indeps, collapse = "+")))

  # Keep only observations larger than 0 for gamma glm
  data_gamma <- data[data[[dep]] > 0, ]
  # Create formula for gamma glm
  fm <- as.formula(paste(dep, "~", paste(indeps, collapse = "+")))

  # Fit logistic and gamma glm
  m0 <- glm(fm_zero, data = data, family = binomial(link = logit))
  m <- glm(fm, data = data_gamma, Gamma(link = "log"))

  ms <- select_model_fields(m0, stp = 1) %>%
    bind_rows(select_model_fields(m, stp = 2))

  # Store dependent variable
  attr(ms, "dep") <- dep
  # Store sample size
  attr(ms, "n") <- c(nrow(data), nrow(data_gamma))

  return(ms)
}

fit_prop <- function(dep, indeps, data) {
  data <- na.omit(data[c(dep, indeps)])

  fm <- paste(dep, "~", paste(indeps, collapse = "+"))
  m <- clm(as.formula(fm), data = data, link = "logit")

  m <- select_model_fields(m)

  # Store dependent variable
  attr(m, "dep") <- dep
  # Store sample size
  attr(m, "n") <- nrow(data)

  m
}

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
  warns <- character()
  m <- tryCatch(
    withCallingHandlers(
      switch(engine,
        binomial = glmer(fm, data = fit_data, family = binomial, control = glmm_control()),
        poisson  = glmer(fm, data = fit_data, family = poisson, control = glmm_control()),
        family   = glmer(fm, data = fit_data, family = family, control = glmm_control()),
        clmm     = clmm(fm, data = fit_data, link = "logit"),
        gaussian = lmer(fm, data = fit_data)
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      warns <<- c(warns, paste("ERROR:", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(m)) {
    warning(paste("Model failed for", dep, ":", tail(warns, 1)))
    return(NULL)
  }

  # Extract parameters, then undo the scaling so the estimates are per unit of
  # the original predictor rather than per standard deviation.
  ms <- rescale_parameters(select_model_fields(m), scales)
  re <- random_intercept_summary(m)

  # Store dependent variable
  attr(ms, "dep") <- dep
  # Store sample size, at both levels
  attr(ms, "n") <- nrow(data)
  attr(ms, "n_id") <- length(unique(data$ID))
  # Convergence provenance, so a bad fit can never again reach a table silently
  attr(ms, "fit_warnings") <- warns
  attr(ms, "converged") <-
    !any(grepl("converge|unidentifiable|degenerate|singular|ERROR", warns, ignore.case = TRUE))
  # Participant clustering, reported in Methods
  attr(ms, "re_var") <- re$var
  attr(ms, "icc") <- re$icc

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

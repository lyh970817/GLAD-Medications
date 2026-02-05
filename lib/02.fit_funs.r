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

  m <- NULL

  tryCatch({
      if (is_factor && n_lev == 2) {
         # Binary (whether ordered or not)
         m <- glmer(fm, data = data, family = binomial)
      } else if (is_ordered) {
        # Ordinal Mixed Model
        # clmm requires ordered factor
        m <- clmm(fm, data = data, link = "logit")
      } else if (!is.null(family)) {
        # GLMM with specified family
        m <- glmer(fm, data = data, family = family)
      } else if (is_factor && nlevels(data[[dep]]) == 2) {
         # Binary factor
         m <- glmer(fm, data = data, family = binomial)
      } else {
         # Check for count data (integers >= 0)
         is_count <- is.numeric(data[[dep]]) && all(data[[dep]] %% 1 == 0, na.rm = TRUE) && min(data[[dep]], na.rm = TRUE) >= 0

         if (is_count) {
             # Check if binary numeric (0/1)
             if (all(data[[dep]] %in% c(0, 1, NA))) {
                 m <- glmer(fm, data = data, family = binomial)
             } else {
                 # Default to Poisson
                 m <- glmer(fm, data = data, family = poisson)
             }
         } else {
             # Gaussian
             m <- lmer(fm, data = data)
         }
      }
  }, error = function(e) {
      warning(paste("Model failed for", dep, ":", e$message))
      return(NULL)
  })

  if (is.null(m)) return(NULL)

  # Extract parameters
  ms <- select_model_fields(m)

  # Store dependent variable
  attr(ms, "dep") <- dep
  # Store sample size
  attr(ms, "n") <- nrow(data)

  return(ms)
}

fit_hurdle <- function(dep, indeps, data) {
  # indeps <- indeps[indeps %in% colnames(dat)]
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

  # Check residuals
  # print(termplot(m0, partial.resid = T, dataa = data))
  # print(termplot(m, partial.resid = T, dataa = data_gamma))

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

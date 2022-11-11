require(ordinal)
require(parameters)

select_model_fields <- function(model, stp = NULL) {
  m <- parameters(model) %>%
    as_tibble() %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
    select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))

  if (!is.null(stp)) {
    # Specify which part of the model
    mutate(m, step = stp)
  } else {
    m
  }
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
  n <- c(nrow(data), nrow(data_gamma))
  attr(ms, "n") <- n

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
  n <- nrow(dat)
  attr(m, "n") <- n
  m
}

require(ordinal)
require(parameters)

format_digit <- function(x) {
  format(round(x, 2), nsmall = 2)
}

as_numeric <- function(f) {
  if (inherits(f, "factor")) {
    as.numeric(levels(f)[f])
  } else {
    f
  }
}

fit_hurdle <- function(dat, dep, indeps) {
  indeps <- indeps[indeps %in% colnames(dat)]

  dat <- na.omit(dat[c(dep, indeps)])

  # Create variable for logistic regression
  lg <- paste(dep, "l", sep = "_")
  dat[lg] <- ifelse(dat[[dep]] > 0, 1, 0)


  fm_zero <- as.formula(paste(lg, "~", paste(indeps, collapse = "+")))
  fm <- as.formula(paste(dep, "~", paste(indeps, collapse = "+")))

  # Keep observations larger than 0 for gamma glm
  dat_gamma <- dat[dat[[dep]] > 0, ]

  n <- c(nrow(dat), nrow(dat_gamma))

  m0 <- glm(fm_zero, data = dat, family = binomial(link = logit))
  m <- glm(fm, data = dat_gamma, Gamma(link = "log"))

  # Check residuals
  # print(termplot(m0, partial.resid = T, data = dat))
  # print(termplot(m, partial.resid = T, data = dat_gamma))

  ms <- imap(list(m0, m), function(mod, i) {
    parameters(mod) %>%
      as_tibble() %>%
      mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
      select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p")) %>%
      mutate(Parameter = paste(i, Parameter, sep = "."))
  }) %>%
    bind_rows()

  attr(ms, "dep") <- dep
  attr(ms, "n") <- n

  return(ms)
}

fit_prop <- function(dat, dep, indeps) {
  indeps <- indeps[indeps %in% colnames(dat)]

  dat <- na.omit(dat[c(dep, indeps)])
  n <- nrow(dat)

  fm <- paste(dep, "~", paste(indeps, collapse = "+"))
  mod <- clm(as.formula(fm), data = dat, link = "logit")

  ms <- parameters(mod) %>%
    as_tibble() %>%
    mutate_at(vars(Coefficient, CI_low, CI_high), exp) %>%
    select(c("Parameter", "Coefficient", "CI_low", "CI_high", "p"))

  attr(ms, "dep") <- dep
  attr(ms, "n") <- n
  return(ms)
}

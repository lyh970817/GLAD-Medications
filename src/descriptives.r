setwd("~/Disk/Projects/Research/GLAD/GLAD_Medications_New")
table(employment_glad_clean$dem.what_is_your_current_employment_status)

library(ProjectTemplate)
library(tidyverse)
library(ggcorrplot)
library(polycor)
reload.project()

# Missing stjohn's wort from n_meds
# Education years over 50?
map(dat_uncut, summary)

cor_mat <- hetcor(
  as.data.frame(
    select(
      dat,
      -c(ID, depression_and_anxiety, bipolar_and_schizophrenia, phq9_bin)
    )
  ),
  use = "pairwise.complete.obs", std.err = F
)$correlations

cor_labels <- discard(labels, names(labels) %in% c(
  "ID",
  "depression_and_anxiety",
  "bipolar_and_schizophrenia",
  "phq9_bin"
))

dimnames(cor_mat) <- list(cor_labels, cor_labels)
cache("cor_mat")

cor_plot <- ggcorrplot(cor_mat,
  type = "lower",
  lab = TRUE,
  title = "Correlations Between All Variables in the Dataset"
) +
  labs(subtitle = "Pearson product-moment between numeric variables, polyserial between numeric and ordinal variables, and polychoric between ordinal variables")
cache("cor_plot")

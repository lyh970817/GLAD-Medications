setwd("~/Disk/Projects/Research/GLAD/GLAD-Medications")

library(ProjectTemplate)
library(tidyverse)
library(ggcorrplot)
library(polycor)
load.project()

cor_mat <- hetcor(
  as.data.frame(
    select(dat, -ID)
  ),
  use = "pairwise.complete.obs", std.err = F
)$correlations

cor_labels <- discard(labels, names(labels) %in% c(
  "ID"
))

dimnames(cor_mat) <- list(cor_labels, cor_labels)
cache("cor_mat")
dimnames(cor_mat)[[1]]

cor_plot <- ggcorrplot(cor_mat,
  type = "lower",
  lab = TRUE,
  title =
    "Correlations Between Ordinal and Continuous Variables in the Dataset",
  lab_size = 3
) +
  labs(
    subtitle =
      "Pearson product-moment between numeric variables, polyserial between numeric and ordinal variables, and polychoric between ordinal variables"
  )

ggsave(
  filename = "cor_plot.pdf",
  path = "./graphs",
  plot = cor_plot,
  width = 20,
  height = 20,
  units = "in"
)

cache("cor_plot")

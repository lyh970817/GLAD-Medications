setwd("~/Disk/Projects/Research/GLAD/GLAD-Medications")
require(ProjectTemplate)
require(tidyverse)
require(rlist)
load.project()

tm_sef <- forest_theme(
  base_size = 10,
  refline_lty = "dashed",
  ci_pch = c(15, 18, 18, 18),
  ci_col = c("#377eb8", "#377eb8", "#762a83", "#34eb7a"),
  footnote_col = "blue",
  legend_name = "Outcome",
  legend_position = "right",
  legend_value = paste(
    c(names(sef_models)[1], names(sef_models)),
    c("(Logistic)", "(Gamma)", "", "")
  ),
  vertline_lty = c("dashed", "dotted"),
  vertline_col = c("#d6604d", "#bababa"),
  ref_line = 1,
)

tm_eff <- forest_theme(
  base_size = 10,
  refline_lty = "dashed",
  ci_pch = c(18, 18, 18, 18, 18),
  ci_col = c("#377eb8", "#762a83", "#eb5334", "#34eb71", "#eb346b"),
  footnote_col = "blue",
  legend_name = "Outcome",
  legend_position = "right",
  legend_value = names(eff_models),
  vertline_lty = c("dashed", "dotted"),
  vertline_col = c("#d6604d", "#bababa"),
  ref_line = 1,
)

plot_sef <- plot_models(only_sig(sef_models, discard_row = FALSE),
  theme = tm_sef
)
plot_eff <- plot_models(only_sig(eff_models, discard_row = FALSE),
  theme = tm_eff
)

cache("plot_sef")
cache("plot_eff")

ggsave(
  filename = "./graphs/sef.png", plot = plot_sef,
  height = 15, width = 10
)
ggsave(
  filename = "./graphs/eff.png", plot = plot_eff,
  height = 20, width = 10
)

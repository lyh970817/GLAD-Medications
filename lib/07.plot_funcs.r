require(forestploter)
require(gridExtra)
plot_hurdle <- function(model_plot) {
  tm <- forest_theme(
    base_size = 10,
    refline_lty = "dashed",
    ci_pch = c(15, 18),
    ci_col = c("#377eb8", "#4daf4a"),
    footnote_col = "blue",
    legend_name = "Model",
    legend_position = "bottom",
    legend_value = c("Logistic", "Gamma"),
    vertline_lty = c("dashed", "dotted"),
    vertline_col = c("#d6604d", "#bababa"),
    ref_line = 1,
  )

  xlim <- c(
    min(c(model_plot$CI_low_logistic, model_plot$CI_low_gamma), na.rm = T),
    max(c(model_plot$CI_high_logistic, model_plot$CI_high_gamma), na.rm = T)
  ) +
    c(-0.2, 0.2)

  forest(model_plot[c("Parameter", " ")],
    est = list(
      model_plot$Coefficient_logistic,
      model_plot$Coefficient_gamma
    ),
    lower = list(
      model_plot$CI_low_logistic,
      model_plot$CI_low_gamma
    ),
    upper = list(
      model_plot$CI_high_logistic,
      model_plot$CI_high_gamma
    ),
    sizes = 0.8,
    xlim = xlim,
    title = labels[attr(model_plot, "dep")],
    ci_column = 2,
    ref_line = 1,
    theme = tm
  )
}

plot_prop <- function(model_plot) {
  tm <- forest_theme(
    base_size = 10,
    refline_lty = "dashed",
    ci_pch = 15,
    ci_col = "#377eb8",
    footnote_col = "blue",
    vertline_lty = "dashed",
    vertline_col = "#d6604d",
  )

  xlim <- c(
    min(model_plot$CI_low, na.rm = T),
    max(model_plot$CI_high, na.rm = T)
  ) +
    c(-0.2, 0.2)

  forest(model_plot[c("Parameter", " ")],
    est = model_plot$Coefficient,
    lower = model_plot$CI_low,
    upper = model_plot$CI_high,
    sizes = 0.8,
    xlim = xlim,
    title = labels[attr(model_plot, "dep")],
    ci_column = 2,
    ref_line = 1,
    theme = tm
  )
}

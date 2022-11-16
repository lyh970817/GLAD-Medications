require(forestploter)
require(gridExtra)
plot_models <- function(models, theme) {
  models_plot <- map(models, function(models_indeps) {
    model_indeps_plot <- map(
      models_indeps[-1], function(model) {
        # sample_size <- attr(model, "n")

        model_plot <- model %>%
          filter(!Parameter %in% labels[compete_indeps]) %>%
          # mutate(n = paste(sample_size, collapse = ", ")) %>%
          mutate(Parameter = str_remove(Parameter, "v\\.s.*$"))

        model_plot
      }
    ) %>%
      bind_rows(models_indeps[[1]], .)


    model_indeps_plot
  })


  get_plot_value_list <- function(models_plot, which) {
    map(models_plot, function(model_plot) {
      select(model_plot, matches(paste0("^", which),
        ignore.case = FALSE
      )) %>%
        as.list()
    }) %>%
      list.flatten()
  }


  coefficients_list <- get_plot_value_list(models_plot, which = "Coefficient")
  p_list <- get_plot_value_list(models_plot, which = "p")
  ci_high_list <- get_plot_value_list(models_plot, which = "CI_high")
  ci_low_list <- get_plot_value_list(models_plot, which = "CI_low")

  xlim <- c(
    min(unlist(ci_low_list), na.rm = T),
    max(unlist(ci_high_list), na.rm = T)
  ) +
    c(-0.2, 0.2)

  forest(tibble(
    Parameter = models_plot[[1]]$Parameter,
    ` ` = paste(rep(" ", 40), collapse = " ")
  ),
  est = coefficients_list,
  lower = ci_low_list,
  upper = ci_high_list,
  sizes = 0.8,
  xlim = xlim,
  ci_column = 2,
  ref_line = 1,
  theme = theme
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

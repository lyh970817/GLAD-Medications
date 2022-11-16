setwd("~/Disk/Projects/Research/GLAD/GLAD-Medications")
require(ProjectTemplate)
require(tidyverse)
load.project()

plot_models <- function(models, theme, file) {
  models_plot <- only_sig(models_plot, discard_row = FALSE)
  models_plot <- map(models, function(models_indeps) {
    model_indeps_plot <- map(
      models_indeps[-1], function(model) {
        sample_size <- attr(model, "n")
        model_plot <- model %>%
          filter(!Parameter %in% labels[compete_indeps]) %>%
          # mutate(n = paste(sample_size, collapse = ", ")) %>%
          mutate(Parameter = str_remove(Parameter, "v\\.s.*$"))
        model_plot
      }
    ) %>%
      bind_rows(models_indeps[[1]], .)

    model_indeps_plot$` ` <- paste(rep(" ", 40), collapse = " ")

    model_indeps_plot
  })

  plot_list <- map(models_plot, function(model_indeps_plot) {
    if ("p" %in% colnames(model_indeps_plot)) {
      plot_prop(model_indeps_plot)
    } else {
      plot_hurdle(model_indeps_plot)
    }
  })

  return(plot_list)
}

plot_list_sef <- plot_models(sef_models)
plot_list_eff <- plot_models(eff_models)

cache("plot_list_sef")
cache("plot_list_eff")

ggsave(
  filename = "./graphs/sef.pdf", plot = marrangeGrob(plot_list_sef,
    nrow = 1, ncol = length(plot_list_sef), top = NULL,
    bottom = "Only significant results are displayed"
  ),
  width = 6.45 * length(plot_list_sef), height = 10.5, units = "in"
)

ggsave(
  filename = "./graphs/eff.pdf", plot = marrangeGrob(plot_list_eff,
    nrow = 1, ncol = length(plot_list_eff), top = NULL,
    bottom = "Only significant results are displayed"
  ),
  width = 6.45 * length(plot_list_eff), height = 10.5, units = "in"
)

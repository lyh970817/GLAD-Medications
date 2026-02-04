plot_models_publication <- function(models) {
  # --- 1. DATA EXTRACTION & TIDYING ---
  extract_model_data <- function(df, outcome_label) {
    if (!is.data.frame(df)) {
      return(NULL)
    }

    df <- df %>% mutate(Parameter = str_remove(Parameter, "v\\.s.*$"))

    if ("Coefficient_logistic" %in% names(df)) {
      part1 <- df %>%
        select(Parameter,
          Estimate = Coefficient_logistic,
          CI_low = CI_low_logistic, CI_high = CI_high_logistic, p = p_logistic
        ) %>%
        mutate(Outcome = paste(outcome_label, "(Logistic)"))

      part2 <- df %>%
        select(Parameter,
          Estimate = Coefficient_gamma,
          CI_low = CI_low_gamma, CI_high = CI_high_gamma, p = p_gamma
        ) %>%
        mutate(Outcome = paste(outcome_label, "(Gamma)"))

      bind_rows(part1, part2)
    } else {
      df %>%
        select(Parameter,
          Estimate = Coefficient,
          CI_low = CI_low, CI_high = CI_high, p = p
        ) %>%
        mutate(Outcome = outcome_label)
    }
  }

  plot_data <- imap_dfr(models, function(sub_models, outcome_name) {
    if (is.data.frame(sub_models)) sub_models <- list(sub_models)
    map_dfr(sub_models, function(item) {
      extract_model_data(item, outcome_name)
    })
  })

  # --- 2. PREPARATION ---
  plot_data <- plot_data %>%
    mutate(
      Significance = ifelse(p < 0.05, "Significant", "Insignificant"),
      Outcome = as.factor(Outcome),
      Parameter = factor(Parameter, levels = rev(unique(Parameter)))
    )

  dodge_width <- 0.7

  # --- 3. AESTHETICS ---
  color_palette <- c("Significant" = "#E69F00", "Insignificant" = "#7F7F7F")
  shape_palette <- c(16, 15, 17, 18, 4, 8)

  # --- 4. PLOTTING ---
  ggplot(plot_data, aes(x = Estimate, y = Parameter, group = Outcome)) +
    geom_vline(xintercept = 1, linetype = "solid", color = "black", linewidth = 0.4) +

    # FIXED: Use geom_errorbar instead of geom_errorbarh
    # Note: 'width' here controls the height of the caps on the Y-axis
    geom_errorbar(
      aes(xmin = CI_low, xmax = CI_high, color = Significance),
      width = 0.2,
      linewidth = 0.6,
      position = position_dodge(width = dodge_width)
    ) +
    geom_point(
      aes(shape = Outcome, color = Significance),
      size = 3,
      stroke = 0.8,
      position = position_dodge(width = dodge_width)
    ) +
    scale_color_manual(values = color_palette, guide = "none") +
    scale_shape_manual(values = shape_palette, name = NULL) +
    guides(shape = guide_legend(nrow = 2)) +

    # Use coord_cartesian to zoom without deleting data
    coord_cartesian(xlim = c(NA, 2), clip = "off") +
    labs(x = "Estimate (95% CI)", y = NULL) +
    theme_minimal(base_size = 16, base_family = "sans") +
    theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.text.y = element_text(color = "black", face = "bold", margin = margin(r = 10)),
      axis.text.x = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linetype = "dashed"),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
      legend.position = "bottom",
      legend.justification = "center",
      legend.margin = margin(t = 10),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    )
}

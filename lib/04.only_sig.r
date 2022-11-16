only_sig <- function(models, discard_row = TRUE) {
  # Keep only the significant variables
  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      p_cols <- grep("^p", colnames(model))

      if (discard_row) {
        sig_rows <- apply(model[p_cols], 1, function(pvals) {
          any(pvals <= 0.05)
        }) %>%
          which()

        model <- model[sig_rows, ]
      } else {
        ci_cols <- grep("^CI", colnames(model))
        coef_cols <- grep("^Coefficient", colnames(model))

        insig_lgl <- model[p_cols] > 0.05
        model[p_cols][insig_lgl] <- NA
        model[ci_cols][rep_cols(insig_lgl, n = 2)] <- NA
        model[coef_cols][insig_lgl] <- NA
      }

      model
    })
  })
}

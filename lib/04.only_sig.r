only_sig <- function(models) {
  # Keep only the significant variables
  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      p_cols <- grep("^p", colnames(model))
      sig_rows <- apply(model[p_cols], 1, function(pvals) {
        any(pvals <= 0.05)
      }) %>%
        which()

      model[sig_rows, ]
    })
  })
}

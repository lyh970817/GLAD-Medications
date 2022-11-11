multi_adjust <- function(models) {
  # Adjust by the number of dependent variables
  n_deps <- length(models)
  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      p_cols <- grep("^p", colnames(model))
      model[p_cols] <- model[p_cols] * n_deps
      model
    })
  })
}

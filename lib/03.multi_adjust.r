multi_adjust <- function(models, n_deps = length(models)) {
  # Adjust by the number of dependent variables.
  #
  # `n_deps` defaults to the number of outcomes in `models`, which is correct
  # when the whole outcome set is passed at once (as the result workbooks do).
  # Pass it explicitly when plotting a *subset* of an outcome set, so that the
  # figures apply the same Bonferroni factor as the tables they accompany —
  # otherwise a subset is under-corrected and an estimate can be drawn as
  # significant in a panel while being absent from the corresponding table.
  map(models, function(model_indeps) {
    map(model_indeps, function(model) {
      if (is.null(model) || nrow(model) == 0) return(model)
      p_cols <- grep("^p", colnames(model))
      model[p_cols] <- model[p_cols] * n_deps
      model
    })
  })
}

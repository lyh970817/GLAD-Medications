rep_cols <- function(x, n) {
  # Repeat each column of a matrix x for n times
  apply(x, 1, function(r) {
    rep(r, each = n)
  }) %>%
    t()
}

# Reading the ilovedata extracts.
#
# extract/ilovedata_export.R pulls Anchor (item) columns with the pipeline's
# `sentinel = TRUE`, so each cell's Response Status travels in its value:
#
#   answered            the answer itself
#   not_shown           NA
#   seen_not_answered   -777
#   dont_know           -888
#   prefer_not_answer   -999
#   not_applicable      -555
#   implausible         -666
#
# A cell counts as an answer only when its status is `answered`. Derived
# Variables carry no status: the pipeline has already applied that gate, and
# their NA is simply "missing".
SENTINEL_CODES <- c(-777, -888, -999, -555, -666)

# TRUE where the cell holds a real answer.
answered <- function(x) {
  !is.na(x) & !(x %in% SENTINEL_CODES)
}

# The answer where there is one, NA for every kind of non-answer.
answer_value <- function(x) {
  ifelse(answered(x), x, NA)
}

# TRUE where a checkbox was ticked.
is_tick <- function(x) {
  answered(x) & x == 1
}

# Pipeline category label -> 0/1, NA for anything else (missing).
label_to_binary <- function(x, yes, no) {
  dplyr::case_when(x == yes ~ 1, x == no ~ 0, TRUE ~ NA_real_)
}

# 0/1 indicator of one level of a categorical variable, as a No/Yes factor,
# NA where the variable is missing. Used for treatment-coded dummies whose
# reference level is fixed by name rather than by frequency.
level_dummy <- function(x, level) {
  factor(ifelse(is.na(x), NA, ifelse(x == level, "Yes", "No")), levels = c("No", "Yes"))
}

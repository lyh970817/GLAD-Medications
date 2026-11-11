# Individual-antidepressant identity for the drug-adjusted sensitivity set
# (`glmm_models_cov_drug` in src/02.regression.r; user approval 2026-11-11).
#
# Why: the within-person association of the number of side effects with
# effectiveness is confounded by the drug. Drugs with more side effects also
# tend to be rated as working better, so a person's medication with more side
# effects than their own average is, on average, also a drug that tends to work
# better, which masks the within-person association. Adjusting for the
# individual drug removes that between-drug contrast (an ad hoc check of
# 2026-11-10: the adjusted within-person odds ratio moved from 0.99 [0.98,
# 1.00] to 0.96 [0.95, 0.97], equal to the between-person one).
#
# Coding: one factor column, `drug_identity`, with the reference drug
# (sertraline, the most common) as its first level, so the model has one fixed
# effect per other drug. A drug with fewer than DRUG_IDENTITY_POOL_MIN rows in
# the frame the models are fitted on (the full `dat_long`) is pooled into the
# level "other". The pooled drugs are derived from the data, not listed here.
# Drug identity is an adjustment covariate only; its contrasts are never
# reported.
DRUG_IDENTITY_REFERENCE <- "sertraline"
DRUG_IDENTITY_POOL_MIN <- 50
DRUG_IDENTITY_OTHER <- "other"
DRUG_IDENTITY_COLUMN <- "drug_identity"

# Rows per drug that fall under the pooling rule, largest first: a named
# integer vector (drug -> rows).
drug_identity_pooled <- function(drugs, min_rows = DRUG_IDENTITY_POOL_MIN) {
  tab <- table(as.character(drugs))
  rare <- tab[tab < min_rows]
  rare <- rare[order(-as.integer(rare), names(rare))]
  stats::setNames(as.integer(rare), names(rare))
}

# Adds the factor column `col` (reference first, then the other retained drugs
# alphabetically, then "other" if any drug is pooled). The pooling travels as
# attribute `drug_identity`: reference, threshold, pooled drugs with their rows,
# and the rows per level.
add_drug_identity <- function(data, drug_col = "medication", ref = DRUG_IDENTITY_REFERENCE,
                              min_rows = DRUG_IDENTITY_POOL_MIN, col = DRUG_IDENTITY_COLUMN) {
  drugs <- as.character(data[[drug_col]])
  if (anyNA(drugs)) {
    stop("Missing medication name(s) in `", drug_col, "`; drug identity cannot be coded.", call. = FALSE)
  }
  pooled <- drug_identity_pooled(drugs, min_rows)
  if (!ref %in% drugs) {
    stop("Reference drug `", ref, "` is not in the frame.", call. = FALSE)
  }
  if (ref %in% names(pooled)) {
    stop("Reference drug `", ref, "` has fewer than ", min_rows, " rows and would be pooled.", call. = FALSE)
  }
  if (DRUG_IDENTITY_OTHER %in% drugs) {
    stop("A medication is literally named `", DRUG_IDENTITY_OTHER, "`; pick another pooled-level name.", call. = FALSE)
  }
  coded <- ifelse(drugs %in% names(pooled), DRUG_IDENTITY_OTHER, drugs)
  lv <- c(ref, setdiff(sort(unique(coded)), c(ref, DRUG_IDENTITY_OTHER)), if (length(pooled)) DRUG_IDENTITY_OTHER)
  data[[col]] <- factor(coded, levels = lv)
  attr(data, "drug_identity") <- list(
    reference = ref,
    min_rows = min_rows,
    pooled = pooled,
    levels = lv,
    rows_by_level = table(data[[col]])
  )
  data
}

# "doxepin (46), phenelzine (46), ...": the pooled drugs with their rows.
drug_identity_pooled_text <- function(pooled) {
  if (!length(pooled)) {
    return("none")
  }
  paste0(names(pooled), " (", formatC(as.integer(pooled), format = "d", big.mark = ","), ")", collapse = ", ")
}

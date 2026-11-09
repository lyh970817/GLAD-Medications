# Antidepressant drug -> class mapping for the drug-class sensitivity set
# (`glmm_models_cov_class` in src/02.regression.r).
#
# Provenance: copied from GLAD-SEM, R/drug_class.R (commit 30daa69, "drug_class:
# encode the ruled drug->class mapping and the class scope"), the GLAD-SEM
# ruling of 2026-08-30. That mapping was fixed before any class-model fit and is
# reproduced verbatim, with one addition: `hypericum_perforatum` (St John's
# wort), which is in this project's 28 named antidepressants (387 medication
# rows on the full cohort) but not on the GLAD-SEM drug menu, is DROPPED (user
# ruling 2026-11-09).
#
# Retained classes, dummy-coded with SSRI the reference:
#
#   ssri   citalopram, escitalopram, fluoxetine, fluvoxamine, paroxetine,
#          sertraline
#   tca    amitriptyline, clomipramine, dosulepin, doxepin, imipramine,
#          lofepramine, nortriptyline, trimipramine
#   snri   duloxetine, venlafaxine
#   nassa  mirtazapine, mianserin
#   sari   trazodone
#
# Dropped from the class-scoped frame (classes too thin to estimate, or no
# shared conventional class): the MAOIs phenelzine, tranylcypromine,
# isocarboxazid and moclobemide; vortioxetine, reboxetine, tryptophan,
# dapoxetine; and hypericum_perforatum. A participant whose only medications
# are dropped leaves the frame.
#
# The map is total over the 28 names in `dat_long$medication` and strict
# beyond them: an unknown name is an error, so a change to the drug menu stops
# the run instead of silently misclassifying a drug.
DRUG_CLASS_MAP <- c(
  # ssri
  citalopram = "ssri", escitalopram = "ssri", fluoxetine = "ssri",
  fluvoxamine = "ssri", paroxetine = "ssri", sertraline = "ssri",
  # tca
  amitriptyline = "tca", clomipramine = "tca", dosulepin = "tca",
  doxepin = "tca", imipramine = "tca", lofepramine = "tca",
  nortriptyline = "tca", trimipramine = "tca",
  # snri
  duloxetine = "snri", venlafaxine = "snri",
  # nassa
  mirtazapine = "nassa", mianserin = "nassa",
  # sari
  trazodone = "sari",
  # dropped: the MAOIs, then the four singletons (GLAD-SEM ruling 2026-08-30)
  phenelzine = "dropped", tranylcypromine = "dropped",
  isocarboxazid = "dropped", moclobemide = "dropped",
  vortioxetine = "dropped", reboxetine = "dropped",
  tryptophan = "dropped", dapoxetine = "dropped",
  # dropped: not on the GLAD-SEM menu (user ruling 2026-11-09)
  hypericum_perforatum = "dropped"
)

# Retained classes, reference first: the order is the dummy coding.
DRUG_CLASSES <- c("ssri", "tca", "snri", "nassa", "sari")

DRUG_CLASS_LABELS <- c(ssri = "SSRI", tca = "TCA", snri = "SNRI", nassa = "NaSSA", sari = "SARI")

# Class of each drug name, or "dropped"; an error for a name not in the map.
drug_class <- function(drugs) {
  drugs <- as.character(drugs)
  unknown <- sort(unique(drugs[!drugs %in% names(DRUG_CLASS_MAP)]))
  if (length(unknown)) {
    stop(
      "Medication name(s) not in the ruled drug -> class mapping (lib/10.drug_class.r): ",
      paste(unknown, collapse = ", "),
      ". The drug menu has changed and the mapping needs a new ruling, not a guess.",
      call. = FALSE
    )
  }
  unname(DRUG_CLASS_MAP[drugs])
}

# The 0/1 dummy columns, one per non-reference class.
drug_class_dummy_columns <- function() {
  paste0("drug_class_", DRUG_CLASSES[-1])
}

# Labels for the dummy terms: "Drug class: TCA vs SSRI", ...
drug_class_labels <- function() {
  stats::setNames(
    paste0("Drug class: ", DRUG_CLASS_LABELS[DRUG_CLASSES[-1]], " vs ", DRUG_CLASS_LABELS[[DRUG_CLASSES[1]]]),
    drug_class_dummy_columns()
  )
}

# The class-scoped longitudinal frame: rows of dropped drugs removed (a
# participant left with no rows leaves), and the class dummies added. The
# exclusion counts travel as attribute `class_scope`.
apply_class_scope <- function(data, drug_col = "medication", id = "ID") {
  cls <- drug_class(data[[drug_col]])
  drop <- cls == "dropped"
  out <- data[!drop, , drop = FALSE]
  cls_kept <- cls[!drop]
  for (k in DRUG_CLASSES[-1]) {
    out[[paste0("drug_class_", k)]] <- as.numeric(cls_kept == k)
  }
  attr(out, "class_scope") <- list(
    rows_before = nrow(data),
    participants_before = length(unique(data[[id]])),
    rows_dropped = sum(drop),
    rows_dropped_by_drug = table(data[[drug_col]][drop]),
    rows_after = nrow(out),
    participants_after = length(unique(out[[id]])),
    rows_by_class = table(factor(cls_kept, levels = DRUG_CLASSES))
  )
  out
}

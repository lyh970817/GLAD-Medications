# Tables for the manuscript.
#
# The eight Results tables were previously assembled by hand from the workbooks,
# which is why nothing recorded how the bracket convention or the N footnotes
# were derived. This script regenerates all of them from the cached model
# objects, so the tables in the document can be reproduced and re-checked. It
# also builds the sample description (Table 1, from `dat`) and the
# supplementary tables S2a-S3e (every estimate behind each Results table).
#
# Numbering: the manuscript renumbered its tables on 2026-11-09 (sample table
# 0 -> 1, side-effect tables 1a-1c -> 2a-2c, effectiveness tables 2a-2e ->
# 3a-3e, supplementary S1a-S2e -> S2a-S3e). Only the displayed captions
# follow it: `label` in TABLES is the manuscript number, `id` the file name
# and the key the footnote rules use, so table_0.html, table_1a.html,
# supp_1a.html and so on keep their names.
#
# Convention for the Results tables, matching the existing manuscript:
#   - one row per parameter significant after Bonferroni correction in the
#     unadjusted model, the adjusted model, or both
#   - adjusted estimates in parentheses after the unadjusted ones
#   - a row significant only after adjustment appears in parentheses alone
#
# Output: markdown to docs/manuscript-tables-current.md, and one ready-to-paste
# HTML file per table in docs/manuscript-tables/ (table_0, table_1a-2e,
# supp_1a-2e, the drug-class tables supp_class_1a, 1c, 2a, 2d, and the
# survey-cohort tables supp_cohort_1a, 1c, 2a, 2d, legacy; file names predate
# the renumbering, see above). Set
# MANUSCRIPT_OUT_DIR to write both somewhere other than docs/,
# e.g. for a dry run on development data that must not land in tracked files.
#
# A table whose model objects are not in cache/ (src/02.regression.r has not
# been run on these data) is skipped with a message naming what it needs, and
# the markdown says so. Its old HTML file, if any, is left as it was.

require(ProjectTemplate)
require(tidyverse)
reload.project()

docs_dir <- Sys.getenv("MANUSCRIPT_OUT_DIR", "docs")
out_dir <- file.path(docs_dir, "manuscript-tables")
md_path <- file.path(docs_dir, "manuscript-tables-current.md")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Model objects each table family needs, all written by src/02.regression.r.
family_needs <- list(
  glmm = c("glmm_models", "glmm_models_cov"),
  legacy = c("sef_models", "eff_models", "sef_models_compete", "eff_models_compete"),
  class = c("glmm_models_cov", "glmm_models_cov_class"),
  cohort = c("glmm_models_cov", "glmm_models_cov_cohort"),
  cohort_legacy = c("sef_models", "eff_models", "sef_models_compete", "eff_models_compete",
    "sef_models_cohort", "eff_models_cohort")
)
family_missing <- map(family_needs, ~ .x[!objects_present(.x)])
for (f in names(family_missing)) {
  if (length(family_missing[[f]])) {
    message(
      "!! ", f, " tables skipped: need ", paste(family_missing[[f]], collapse = ", "),
      " in cache/ (run src/02.regression.r on these data)"
    )
  }
}
skip_note <- function(family) {
  paste0(
    "_Not generated: needs `", paste(family_missing[[family]], collapse = "`, `"),
    "` from `src/02.regression.r`, which are not in `cache/`._"
  )
}

# Both workbook families correct by the number of outcomes they contain, which
# is four in each case. Passing the whole family keeps the tables and the
# figures on the same Bonferroni factor.
families <- list()
if (!length(family_missing$glmm)) {
  families$glmm <- list(
    unadj = multi_adjust(glmm_models),
    adj = multi_adjust(glmm_models_cov)
  )
}
if (!length(family_missing$legacy)) {
  legacy <- c(sef_models, eff_models)
  families$legacy <- list(
    unadj = multi_adjust(legacy),
    adj = multi_adjust(c(sef_models_compete, eff_models_compete), n_deps = length(legacy))
  )
}

# Flatten one outcome's list of predictor sets into a single table, keeping the
# sample sizes, which differ between sets because the models are complete-case.
flatten_outcome <- function(family, outcome) {
  sets <- family[[outcome]]
  sets <- Filter(function(s) !is.null(s) && nrow(s) > 0, sets)
  if (!length(sets)) {
    return(NULL)
  }

  tab <- bind_rows(sets)
  attr(tab, "n_range") <- range(vapply(sets, function(s) attr(s, "n")[1], numeric(1)))
  attr(tab, "n_id_range") <- {
    ids <- vapply(sets, function(s) {
      v <- attr(s, "n_id")
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
    if (all(is.na(ids))) NULL else range(ids, na.rm = TRUE)
  }
  attr(tab, "icc") <- {
    v <- vapply(sets, function(s) {
      x <- attr(s, "icc")
      if (is.null(x)) NA_real_ else as.numeric(x)
    }, numeric(1))
    if (all(is.na(v))) NULL else v
  }
  tab
}

# The hurdle models carry two estimate columns (logistic and gamma). The
# manuscript reports the ordinal/logistic estimate, which is the first one.
estimate_col <- function(tab) {
  cols <- grep("^Coeff_CI", colnames(tab), value = TRUE)
  cols[1]
}
p_col <- function(tab) {
  cols <- grep("^p", colnames(tab), value = TRUE)
  cols[1]
}

build_table <- function(unadj_family, adj_family, outcome) {
  u <- flatten_outcome(unadj_family, outcome)
  a <- flatten_outcome(adj_family, outcome)
  if (is.null(u)) {
    return(NULL)
  }

  uu <- tibble(
    Parameter = u$Parameter,
    est_u = u[[estimate_col(u)]],
    p_u = u[[p_col(u)]]
  )
  aa <- if (is.null(a)) {
    tibble(Parameter = character(), est_a = character(), p_a = numeric())
  } else {
    tibble(
      Parameter = a$Parameter,
      est_a = a[[estimate_col(a)]],
      p_a = a[[p_col(a)]]
    )
  }

  tab <- full_join(uu, aa, by = "Parameter") %>%
    filter((!is.na(p_u) & p_u <= 0.05) | (!is.na(p_a) & p_a <= 0.05))

  # An outcome with nothing significant is a result, not an error: report it as
  # a missing table rather than failing partway through the run.
  if (nrow(tab) == 0) {
    return(NULL)
  }

  # A row appears when *either* model is significant, and then both estimates are
  # shown. The previous convention printed the adjusted estimate only when it was
  # itself significant, which made a blank bracket ambiguous: the reader could not
  # tell a variable that was never estimated (because it is one of the adjustment
  # covariates) from one that was estimated and attenuated to the null. The second
  # case is a finding in its own right -- "Married vs. Not in relationship" goes
  # from 0.88 [0.82, 0.94] to 0.94 [0.88, 1.01] for the number of side effects,
  # and reverses direction for stopping -- and it used to be invisible.
  #
  # A bracket is therefore now absent only when the adjusted model genuinely has
  # no estimate, i.e. the variable is an adjustment covariate.
  tab <- tab %>%
    mutate(
      show_u = !is.na(est_u),
      show_a = !is.na(est_a),
      estimate = case_when(
        show_u & show_a ~ paste0(est_u, " (", est_a, ")"),
        show_u ~ est_u,
        TRUE ~ paste0("(", est_a, ")")
      ),
      pval = case_when(
        show_u & show_a ~ paste0(fmt_p(p_u), " (", fmt_p(p_a), ")"),
        show_u ~ fmt_p(p_u),
        TRUE ~ paste0("(", fmt_p(p_a), ")")
      ),
      Parameter = clean_label(Parameter)
    ) %>%
    select(Parameter, estimate, pval)

  attr(tab, "n_range") <- attr(u, "n_range")
  attr(tab, "n_id_range") <- attr(u, "n_id_range")
  attr(tab, "icc") <- attr(u, "icc")
  tab
}

n_footnote <- function(tab, level) {
  nr <- attr(tab, "n_range")
  ids <- attr(tab, "n_id_range")

  if (level == "observations") {
    base <- paste0(
      "N = ", commafy(nr[2]), " medication observations",
      if (!is.null(ids)) paste0(" from ", commafy(ids[2]), " participants") else ""
    )
    if (nr[1] != nr[2]) {
      base <- paste0(
        base, ". Models vary between ", commafy(nr[1]), " and ", commafy(nr[2]),
        " observations because each is fitted on complete cases"
      )
    }
    paste0(base, ".")
  } else {
    base <- paste0("N = ", commafy(nr[2]), " participants")
    if (nr[1] != nr[2]) {
      base <- paste0(
        base, ". Models vary between ", commafy(nr[1]), " and ", commafy(nr[2]),
        " participants because each is fitted on complete cases"
      )
    }
    paste0(base, ".")
  }
}

TABLES <- list(
  list(id = "1a", label = "2a", outcome = "Number of Side Effects", family = "glmm", effect = "RR",
       title = "Variables significantly associated with the number of side effects"),
  list(id = "1b", label = "2b", outcome = "Side effect severity rating", family = "legacy", effect = "OR",
       title = "Variables significantly associated with side effect severity rating"),
  list(id = "1c", label = "2c", outcome = "Stopped due to Side Effects", family = "glmm", effect = "OR",
       title = "Variables significantly associated with stopping a medication because of side effects"),
  list(id = "2a", label = "3a", outcome = "Effectiveness", family = "glmm", effect = "OR",
       title = "Variables significantly associated with effectiveness"),
  list(id = "2b", label = "3b", outcome = "Benefit rating", family = "legacy", effect = "OR",
       title = "Variables significantly associated with benefit rating"),
  list(id = "2c", label = "3c", outcome = "Number of best aspects", family = "legacy", effect = "OR",
       title = "Variables significantly associated with the number of best aspects"),
  # "Occurrence of remission" was participant-level wording for what is in fact a
  # per-medication outcome: remission is recorded for each antidepressant, and
  # participants who took more than one often differ across them (the share is
  # computed by src/05.manuscript_numbers.r).
  list(id = "2d", label = "3d", outcome = "Remission", family = "glmm", effect = "OR",
       title = "Variables significantly associated with remission on a medication"),
  list(id = "2e", label = "3e", outcome = "First improvement duration", family = "legacy", effect = "OR",
       title = "Variables significantly associated with first improvement duration")
)

# Medication-level predictors in the longitudinal models are split into a
# within-person and a between-persons term (user ruling 2026-11-09; see
# lib/09.within_between.r). Both rows carry the predictor's label with
# "(within person)" / "(between persons)" appended.
WITHIN_BETWEEN_NOTE <- paste0(
  "Within person / between persons: a predictor that varies between a participant's medications ",
  "is split into the medication's deviation from the participant's own mean (within person) and ",
  "that mean (between persons), fitted together in one model with the participant random intercept; ",
  "the within-person term is informed only by participants with two or more medications. ",
  "Start age and cumulative medication count are adjustment covariates in the adjusted models and ",
  "enter them undecomposed."
)
is_within_between_row <- function(parameter) {
  grepl("\\((within person|between persons)\\)$", parameter)
}

ADJUSTMENT_SETS <- c(
  glmm = "sex, starting age, cumulative medication count and PHQ-9",
  legacy = "sex, average starting age and the number of psychiatric diagnosis types in the immediate family"
)

md <- c(
  "# Manuscript tables — current numbers",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/04.manuscript_tables.r`. Table 1"),
  "describes `dat`; the Results tables and the supplementary tables come from the",
  "cached model objects, under the **combination** decision: longitudinal (GLMM)",
  "estimates for the four outcomes the GLMM covers, per-participant (legacy)",
  "estimates for the four it does not.",
  "",
  "In the Results tables, adjusted estimates appear in parentheses after the",
  "unadjusted ones whenever the adjusted model has one. Rows shown only in",
  "parentheses are significant after adjustment but not before. The longitudinal",
  paste0("models are adjusted for ", ADJUSTMENT_SETS[["glmm"]], "; the"),
  paste0("per-participant models for ", ADJUSTMENT_SETS[["legacy"]], "."),
  ""
)

# ------------------------------------------------------------------------------
# Table 1 (file table_0.html): sample description by survey cohort
# ------------------------------------------------------------------------------
# Counts are n (% of those with a known value); continuous variables are mean
# (SD). The Missing column is the number (and share) of all participants in
# `dat` without a value.

employment_labels <- c(
  In_paid_employment_or_self_employed = "In paid employment or self-employed",
  Retired = "Retired",
  Looking_after_home_and_or_family = "Looking after home and/or family",
  Unable_to_work_because_of_sickness_or_disability = "Unable to work because of sickness or disability",
  Unemployed = "Unemployed",
  Doing_unpaid_or_voluntary_work = "Doing unpaid or voluntary work",
  Full_or_part_time_student = "Full or part-time student"
)

# The frames carry employment and marital status as treatment-coded dummies;
# rebuild the categories from them. All dummies "No" is the reference level.
employment_from_dummies <- function(d) {
  dummies <- paste0("In_paid_employment_or_self_employed_", names(employment_labels)[-1])
  m <- as.data.frame(lapply(d[dummies], function(x) as.character(x) == "Yes"))
  known <- rowSums(!is.na(m)) == length(dummies)
  hit <- apply(m, 1, function(r) {
    w <- which(r)
    if (length(w)) w[1] else 0L
  })
  out <- ifelse(hit == 0, employment_labels[[1]], unname(employment_labels[-1])[pmax(hit, 1)])
  out[!known] <- NA
  factor(out, levels = unname(employment_labels))
}

marital_from_dummies <- function(d) {
  rel <- d$Not_in_relationship_In_relationship
  mar <- d$Not_in_relationship_Married
  out <- case_when(
    is.na(rel) | is.na(mar) ~ NA_character_,
    mar == 1 ~ "Married",
    rel == 1 ~ "In a relationship",
    TRUE ~ "Not in a relationship"
  )
  factor(out, levels = c("Not in a relationship", "In a relationship", "Married"))
}

describe_sample <- function(d) {
  cohorts <- levels(droplevels(factor(d$cohort)))
  groups <- c(split(seq_len(nrow(d)), factor(d$cohort, levels = cohorts)), Overall = list(seq_len(nrow(d))))
  n_all <- nrow(d)
  missing_cell <- function(x) fmt_n_pct(sum(is.na(x)), n_all)

  continuous <- function(label, x, digits = 1) {
    tibble(
      label = label, italic = FALSE,
      !!!setNames(lapply(groups, function(i) fmt_mean_sd(x[i], digits)), names(groups)),
      Missing = missing_cell(x)
    )
  }
  categorical <- function(label, x) {
    x <- droplevels(factor(x, levels = levels(factor(x))))
    head <- tibble(
      label = label, italic = TRUE,
      !!!setNames(as.list(rep("", length(groups))), names(groups)),
      Missing = missing_cell(x)
    )
    lev <- map_dfr(levels(x), function(l) {
      tibble(
        label = paste0(" ", l), italic = FALSE,
        !!!setNames(lapply(groups, function(i) {
          xi <- x[i]
          fmt_n_pct(sum(xi == l, na.rm = TRUE), sum(!is.na(xi)))
        }), names(groups)),
        Missing = ""
      )
    })
    bind_rows(head, lev)
  }

  rows <- list(
    tibble(
      label = "Participants, n", italic = FALSE,
      !!!setNames(lapply(groups, function(i) commafy(length(i))), names(groups)),
      Missing = ""
    )
  )
  # Age is not in the ilovedata extract yet (the pipeline's `dem.selfreport_age`
  # is not exported); the row appears once the munge carries an `age` column.
  if ("age" %in% names(d)) rows <- c(rows, list(continuous("Age, years", d$age)))
  rows <- c(rows, list(
    categorical("Sex", d$sex),
    categorical("Employment status", employment_from_dummies(d)),
    categorical("Relationship status", marital_from_dummies(d)),
    continuous("PHQ-9 (current depression)", d$phq9),
    continuous("GAD-7 (current anxiety)", d$gad7),
    continuous("AUDIT", d$audit),
    continuous("BMI, kg/m²", d$bmi),
    continuous("Number of comorbidities", d$comorbidity_total_count_numeric),
    continuous("Number of psychiatric diagnosis types in immediate family", d$fh_diagnosis_types),
    continuous("Number of antidepressants taken", d$n_meds)
  ))
  tab <- bind_rows(rows)
  attr(tab, "group_n") <- vapply(groups, length, integer(1))
  tab
}

sample_tab <- describe_sample(dat)
group_n <- attr(sample_tab, "group_n")
sample_header <- c(
  "Characteristic",
  paste0(names(group_n), " (n = ", commafy(group_n), ")"),
  "Missing, n (%)"
)
sample_body <- sample_tab %>% select(-label, -italic) %>% {
  bind_cols(tibble(Characteristic = sample_tab$label), .)
}
sample_foot <- paste0(
  "Participants in the per-participant frame: everyone with a depressive or anxiety ",
  "disorder who ticked at least one of the 28 named antidepressants. Baseline: GLAD sign-up ",
  "with the standalone medication questionnaire; SA8: the later sign-up survey, which carries ",
  "the medication items itself. n (%) of those with ",
  "a known value, or mean (SD). Missing: participants without a value, of all ",
  commafy(nrow(dat)), ". ",
  if ("age" %in% names(dat)) "Education is" else "Age and education are",
  " not in the analysis extract."
)

writeLines(
  html_table(sample_header, sample_body, sample_foot, italic_rows = sample_tab$italic),
  file.path(out_dir, "table_0.html")
)
md <- c(
  md, "",
  "#### Table 1: Sample characteristics by survey cohort",
  sample_foot,
  md_table(sample_header, sample_body)
)
message("  wrote table 1 (table_0.html, ", nrow(sample_body), " rows)")

# ------------------------------------------------------------------------------
# Tables 2a-3e (files table_1a-2e): significant associations
# ------------------------------------------------------------------------------
for (spec in TABLES) {
  fam <- families[[spec$family]]
  if (is.null(fam)) {
    md <- c(md, "", paste0("#### Table ", spec$label, ": ", spec$title), skip_note(spec$family))
    next
  }

  tab <- build_table(fam$unadj, fam$adj, spec$outcome)

  if (is.null(tab)) {
    message("!! no table for ", spec$label, " (", spec$outcome, ")")
    next
  }

  level <- if (spec$family == "glmm") "observations" else "participants"
  foot <- n_footnote(tab, level)

  icc <- attr(tab, "icc")
  if (!is.null(icc) && any(!is.na(icc))) {
    foot <- paste0(
      foot, " Participant random-intercept ICC ",
      formatC(median(icc, na.rm = TRUE), format = "f", digits = 2), "."
    )
  }
  if (spec$family == "glmm" && any(is_within_between_row(tab$Parameter))) {
    foot <- paste0(foot, " ", WITHIN_BETWEEN_NOTE)
  }

  header <- c("Parameter", paste0(spec$effect, " [95% CI] (adjusted)"), "p (adjusted)")
  body <- select(tab, Parameter, estimate, pval)

  md <- c(
    md, "",
    paste0("#### Table ", spec$label, ": ", spec$title),
    foot,
    md_table(header, body)
  )

  writeLines(html_table(header, body, foot), file.path(out_dir, paste0("table_", spec$id, ".html")))
  message("  wrote table ", spec$label, " (table_", spec$id, ".html, ", nrow(tab), " rows)")
}

# ------------------------------------------------------------------------------
# Supplementary Tables S2a-S3e (files supp_1a-2e): every estimate behind each Results table
# ------------------------------------------------------------------------------
# One supplementary table per Results table, with the same outcome and model
# family: every parameter of every predictor set, significant or not, with the
# unadjusted and the adjusted estimate side by side, their Bonferroni-corrected
# p values and the complete-case N of each model. A parameter with no adjusted
# estimate is one of the adjustment covariates.

# One row per parameter, carrying its own set's N, participant count and
# convergence flag. Sets that could not be fitted (NULL) are counted.
flatten_sets <- function(family, outcome) {
  sets <- family[[outcome]]
  rows <- map_dfr(seq_along(sets), function(i) {
    s <- sets[[i]]
    if (is.null(s) || nrow(s) == 0) {
      return(NULL)
    }
    n_id <- attr(s, "n_id")
    conv <- attr(s, "converged")
    tibble(
      Parameter = s$Parameter,
      est = s[[estimate_col(s)]],
      p = s[[p_col(s)]],
      n = as.numeric(attr(s, "n")[1]),
      n_id = if (is.null(n_id)) NA_real_ else as.numeric(n_id),
      converged = if (is.null(conv)) NA else isTRUE(conv)
    )
  })
  attr(rows, "n_failed") <- sum(vapply(sets, function(s) is.null(s) || nrow(s) == 0, logical(1)))
  rows
}

fmt_model_n <- function(n, n_id) {
  ifelse(is.na(n), "",
    ifelse(is.na(n_id), commafy(n), paste0(commafy(n), " / ", commafy(n_id)))
  )
}

build_supp_table <- function(unadj_family, adj_family, outcome) {
  u <- flatten_sets(unadj_family, outcome)
  a <- flatten_sets(adj_family, outcome)
  if (!nrow(u)) {
    return(NULL)
  }
  dup <- c(u$Parameter[duplicated(u$Parameter)], a$Parameter[duplicated(a$Parameter)])
  if (length(dup)) {
    warning("Parameter labels repeated within ", outcome, ": ", paste(unique(dup), collapse = "; "))
  }

  mark <- function(est, converged) ifelse(!is.na(converged) & !converged, paste0(est, "†"), est)
  tab <- full_join(u, a, by = "Parameter", suffix = c("_u", "_a"), multiple = "first") %>%
    transmute(
      Parameter = clean_label(Parameter),
      est_u = ifelse(is.na(est_u), "–", mark(est_u, converged_u)),
      p_u = coalesce(fmt_p(p_u), ""),
      n_u = fmt_model_n(n_u, n_id_u),
      est_a = ifelse(is.na(est_a), "–", mark(est_a, converged_a)),
      p_a = coalesce(fmt_p(p_a), ""),
      n_a = fmt_model_n(n_a, n_id_a),
      any_unconverged = (!is.na(converged_u) & !converged_u) | (!is.na(converged_a) & !converged_a)
    )
  attr(tab, "n_failed") <- c(unadjusted = attr(u, "n_failed"), adjusted = attr(a, "n_failed"))
  tab
}

supp_footnote <- function(tab, spec) {
  glmm <- spec$family == "glmm"
  failed <- attr(tab, "n_failed")
  paste0(
    "Every parameter of every predictor set, unadjusted and adjusted for ",
    ADJUSTMENT_SETS[[spec$family]], ". ",
    if (glmm) {
      "Longitudinal mixed models with a participant random intercept. "
    } else {
      "Per-participant ordinal models. "
    },
    "Each predictor set is a separate model: one variable, the levels of employment or of ",
    "relationship status, or the psychiatric diagnosis indicators together",
    ". ",
    # The side-effect predictors enter only the effectiveness outcomes. Both the
    # legacy effectiveness models and the GLMM fit each of the three measures
    # (mean number, severity, discontinuation) on its own, one model per measure.
    if (!glmm && spec$id %in% c("2b", "2c", "2e")) {
      "The mean number of side effects, the side-effect severity rating and treatment discontinuation were each fitted separately (one model per measure). "
    } else if (glmm && spec$id %in% c("2a", "2d")) {
      "The number of side effects, the severity rating and stopping because of side effects are each a separate model. "
    } else {
      ""
    },
    spec$effect, " with nominal 95% CI; p values are Bonferroni-corrected for the four outcomes of the ",
    if (glmm) "longitudinal" else "per-participant", " analysis. ",
    "N is the complete-case sample of each model",
    if (glmm) " (medication observations / participants)" else " (participants)", ". ",
    "–: adjustment covariate, so no adjusted estimate of its own.",
    if (glmm && any(is_within_between_row(tab$Parameter))) paste0(" ", WITHIN_BETWEEN_NOTE) else "",
    if (any(tab$any_unconverged)) " †: the model reported a convergence warning." else "",
    if (sum(failed)) {
      paste0(
        " Not shown: ", failed[["unadjusted"]], " unadjusted and ", failed[["adjusted"]],
        " adjusted predictor set(s) that could not be fitted."
      )
    } else {
      ""
    }
  )
}

supp_index <- character()
for (spec in TABLES) {
  sid <- paste0("S", spec$label)
  title <- sub("^Variables significantly associated with", "All modelled associations with", spec$title)
  fam <- families[[spec$family]]
  if (is.null(fam)) {
    supp_index <- c(supp_index, "", paste0("#### Supplementary Table ", sid, ": ", title), skip_note(spec$family))
    next
  }

  tab <- build_supp_table(fam$unadj, fam$adj, spec$outcome)
  if (is.null(tab)) {
    message("!! no supplementary table for ", sid, " (", spec$outcome, "): no fitted predictor set")
    next
  }

  foot <- supp_footnote(tab, spec)
  est <- paste0(spec$effect, " [95% CI]")
  header <- c("Parameter", paste("Unadjusted", est), "p", "N", paste("Adjusted", est), "p", "N")
  body <- select(tab, Parameter, est_u, p_u, n_u, est_a, p_a, n_a)

  file <- paste0("supp_", spec$id, ".html")
  writeLines(html_table(header, body, foot), file.path(out_dir, file))
  supp_index <- c(
    supp_index, "",
    paste0("#### Supplementary Table ", sid, ": ", title),
    paste0("`manuscript-tables/", file, "`, ", nrow(body), " rows. ", foot),
    md_table(header, body)
  )
  message("  wrote supplementary table ", sid, " (", nrow(body), " rows)")
}

md <- c(md, "", "## Supplementary tables", supp_index)

# ------------------------------------------------------------------------------
# Supplementary Tables S4a-S4d (files supp_class_1a, 1c, 2a, 2d): drug class
# ------------------------------------------------------------------------------
# Sensitivity analysis (user ruling 2026-11-09): the adjusted longitudinal
# models refitted on the class-scoped frame with the drug-class dummies (SSRI
# the reference) among the covariates (`glmm_models_cov_class`), beside the
# main adjusted estimates. One table per longitudinal outcome, with the four
# class contrasts as rows; they come from the set that has the class dummies
# as its focal predictors. The S4 numbering is provisional (the manuscript has
# no drug-class table yet).
class_scope <- attr(apply_class_scope(dat_long), "class_scope")
class_scope_note <- with(class_scope, paste0(
  "Class-scoped frame: ", commafy(rows_after), " medication observations from ",
  commafy(participants_after), " participants, after removing the ", commafy(rows_dropped),
  " observations of the drugs without a retained class (",
  paste0(names(rows_dropped_by_drug), " ", commafy(as.integer(rows_dropped_by_drug)), collapse = ", "),
  "); ", commafy(participants_before - participants_after),
  " participants with only those drugs leave (full frame: ", commafy(rows_before),
  " observations, ", commafy(participants_before), " participants)."
))

class_supp_footnote <- function(tab, spec, n_max) {
  failed <- attr(tab, "n_failed")
  paste0(
    "Sensitivity analysis by drug class. Left: the main adjusted longitudinal models (adjusted for ",
    ADJUSTMENT_SETS[["glmm"]], ", full frame). Right: the same models on the class-scoped frame with drug ",
    "class (TCA, SNRI, NaSSA, SARI vs SSRI) added to the covariates; the drug-class rows are from one model ",
    "per outcome with the four class contrasts and the same covariates. ", class_scope_note, " ",
    "Largest class-scoped model: N = ", commafy(n_max[1]), " observations from ", commafy(n_max[2]),
    " participants. ",
    spec$effect, " with nominal 95% CI; p values are Bonferroni-corrected for the four longitudinal outcomes. ",
    "N is the complete-case sample of each model (medication observations / participants). ",
    "–: not estimated in that model.",
    if (any(is_within_between_row(tab$Parameter))) paste0(" ", WITHIN_BETWEEN_NOTE) else "",
    if (any(tab$any_unconverged)) " †: the model reported a convergence warning." else "",
    if (sum(failed)) {
      paste0(
        " Not shown: ", failed[["unadjusted"]], " main and ", failed[["adjusted"]],
        " class-adjusted predictor set(s) that could not be fitted."
      )
    } else {
      ""
    }
  )
}

class_index <- character()
if (length(family_missing$class)) {
  class_index <- c(class_index, "", skip_note("class"))
} else {
  class_main <- multi_adjust(glmm_models_cov)
  class_fam <- multi_adjust(glmm_models_cov_class)
  glmm_specs <- Filter(function(sp) sp$family == "glmm", TABLES)
  for (j in seq_along(glmm_specs)) {
    spec <- glmm_specs[[j]]
    sid <- paste0("S4", letters[j])
    title <- sub("^Variables significantly associated with", "All adjusted associations with", spec$title)
    title <- paste0(title, ", with and without adjustment for drug class")
    tab <- build_supp_table(class_main, class_fam, spec$outcome)
    if (is.null(tab)) {
      message("!! no drug-class table for ", sid, " (", spec$outcome, ")")
      next
    }
    sets <- class_fam[[spec$outcome]]
    n_max <- c(
      max(vapply(sets, function(x) if (is.null(x)) NA_real_ else as.numeric(attr(x, "n")[1]), numeric(1)), na.rm = TRUE),
      max(vapply(sets, function(x) if (is.null(x)) NA_real_ else as.numeric(attr(x, "n_id")), numeric(1)), na.rm = TRUE)
    )
    foot <- class_supp_footnote(tab, spec, n_max)
    est <- paste0(spec$effect, " [95% CI]")
    header <- c("Parameter", paste("Adjusted", est), "p", "N", paste("Adjusted + drug class", est), "p", "N")
    body <- select(tab, Parameter, est_u, p_u, n_u, est_a, p_a, n_a)
    file <- paste0("supp_class_", spec$id, ".html")
    writeLines(html_table(header, body, foot), file.path(out_dir, file))
    class_index <- c(
      class_index, "",
      paste0("#### Supplementary Table ", sid, ": ", title),
      paste0("`manuscript-tables/", file, "`, ", nrow(body), " rows. ", foot),
      md_table(header, body)
    )
    message("  wrote drug-class table ", sid, " (", nrow(body), " rows)")
  }
}

md <- c(
  md, "", "## Sensitivity analysis: drug class",
  "",
  paste0(
    "`glmm_models_cov_class`: the adjusted longitudinal models refitted on the class-scoped frame ",
    "with drug class (SSRI reference) added to the covariates; workbook ",
    "`results/glmm_medications_cov_class.xlsx`. Supplementary Tables S4a-S4d (numbering provisional)."
  ),
  class_index
)

# ------------------------------------------------------------------------------
# Supplementary Tables S5a-S5e (files supp_cohort_1a, 1c, 2a, 2d, legacy): survey cohort
# ------------------------------------------------------------------------------
# Sensitivity analysis (user ruling 2026-11-09, "Cohort can be included for
# sensitivity analyses"): the adjusted models with the survey cohort (SA8 vs
# Baseline) added to the covariates, beside the main adjusted estimates. S5a-S5d
# are the four longitudinal outcomes (`glmm_models_cov_cohort`, full frame, the
# same rows as the main adjusted models), S5e the four per-participant outcomes
# (`sef_models_cohort`, `eff_models_cohort`) in one table. The cohort contrast
# row comes from the set per outcome that has the cohort as its focal
# predictor. The S5 numbering is provisional.
cohort_counts <- function(d, rows = TRUE) {
  tab <- if (rows) table(d$cohort) else table(d$cohort[!duplicated(d$ID)])
  paste0(names(tab), " ", commafy(as.integer(tab)), collapse = ", ")
}
cohort_long_note <- paste0(
  "Survey cohort: Baseline (GLAD sign-up with the standalone medication questionnaire) or SA8 (the later ",
  "sign-up survey); medication observations ", cohort_counts(dat_long), "; participants ",
  cohort_counts(dat_long, rows = FALSE), "."
)
cohort_person_note <- paste0(
  "Survey cohort: Baseline (GLAD sign-up with the standalone medication questionnaire) or SA8 (the later ",
  "sign-up survey); participants ", cohort_counts(dat, rows = FALSE), "."
)

cohort_supp_footnote <- function(tab, spec) {
  failed <- attr(tab, "n_failed")
  paste0(
    "Sensitivity analysis by survey cohort. Left: the main adjusted longitudinal models (adjusted for ",
    ADJUSTMENT_SETS[["glmm"]], "). Right: the same models, on the same frame, with survey cohort (SA8 vs ",
    "Baseline) added to the covariates; the survey-cohort row is from one model per outcome with the cohort ",
    "as its focal predictor and the same covariates. ", cohort_long_note, " ",
    spec$effect, " with nominal 95% CI; p values are Bonferroni-corrected for the four longitudinal outcomes. ",
    "N is the complete-case sample of each model (medication observations / participants). ",
    "–: not estimated in that model.",
    if (any(is_within_between_row(tab$Parameter))) paste0(" ", WITHIN_BETWEEN_NOTE) else "",
    if (any(tab$any_unconverged)) " †: the model reported a convergence warning." else "",
    if (sum(failed)) {
      paste0(
        " Not shown: ", failed[["unadjusted"]], " main and ", failed[["adjusted"]],
        " cohort-adjusted predictor set(s) that could not be fitted."
      )
    } else {
      ""
    }
  )
}

cohort_index <- character()
if (length(family_missing$cohort)) {
  cohort_index <- c(cohort_index, "", skip_note("cohort"))
} else {
  cohort_main <- multi_adjust(glmm_models_cov)
  cohort_fam <- multi_adjust(glmm_models_cov_cohort)
  glmm_specs <- Filter(function(sp) sp$family == "glmm", TABLES)
  for (j in seq_along(glmm_specs)) {
    spec <- glmm_specs[[j]]
    sid <- paste0("S5", letters[j])
    title <- sub("^Variables significantly associated with", "All adjusted associations with", spec$title)
    title <- paste0(title, ", with and without adjustment for survey cohort")
    tab <- build_supp_table(cohort_main, cohort_fam, spec$outcome)
    if (is.null(tab)) {
      message("!! no survey-cohort table for ", sid, " (", spec$outcome, ")")
      next
    }
    foot <- cohort_supp_footnote(tab, spec)
    est <- paste0(spec$effect, " [95% CI]")
    header <- c("Parameter", paste("Adjusted", est), "p", "N", paste("Adjusted + survey cohort", est), "p", "N")
    body <- select(tab, Parameter, est_u, p_u, n_u, est_a, p_a, n_a)
    file <- paste0("supp_cohort_", spec$id, ".html")
    writeLines(html_table(header, body, foot), file.path(out_dir, file))
    cohort_index <- c(
      cohort_index, "",
      paste0("#### Supplementary Table ", sid, ": ", title),
      paste0("`manuscript-tables/", file, "`, ", nrow(body), " rows. ", foot),
      md_table(header, body)
    )
    message("  wrote survey-cohort table ", sid, " (", nrow(body), " rows)")
  }
}

# S5e: the four per-participant outcomes in one table, an italic heading row
# per outcome. The Bonferroni factor is the four per-participant outcomes, as
# in S2b, S3b, S3c and S3e.
if (length(family_missing$cohort_legacy)) {
  cohort_index <- c(cohort_index, "", skip_note("cohort_legacy"))
} else {
  n_legacy <- length(c(sef_models, eff_models))
  legacy_main <- multi_adjust(c(sef_models_compete, eff_models_compete), n_deps = n_legacy)
  legacy_cohort <- multi_adjust(c(sef_models_cohort, eff_models_cohort), n_deps = n_legacy)
  legacy_specs <- Filter(function(sp) sp$family == "legacy", TABLES)
  blocks <- map(legacy_specs, function(spec) {
    tab <- build_supp_table(legacy_main, legacy_cohort, spec$outcome)
    if (is.null(tab)) {
      message("!! no survey-cohort rows for S5e (", spec$outcome, ")")
      return(NULL)
    }
    heading <- tibble(
      Parameter = paste0(spec$outcome, " (", spec$effect, ")"),
      est_u = "", p_u = "", n_u = "", est_a = "", p_a = "", n_a = "", any_unconverged = FALSE
    )
    out <- bind_rows(heading, select(tab, Parameter, est_u, p_u, n_u, est_a, p_a, n_a, any_unconverged))
    attr(out, "n_failed") <- attr(tab, "n_failed")
    attr(out, "italic") <- c(TRUE, rep(FALSE, nrow(tab)))
    out
  })
  blocks <- Filter(Negate(is.null), blocks)
  if (length(blocks)) {
    tab <- bind_rows(blocks)
    italic <- unlist(map(blocks, ~ attr(.x, "italic")))
    failed <- Reduce(`+`, map(blocks, ~ attr(.x, "n_failed")))
    sid <- "S5e"
    title <- paste0(
      "All adjusted associations with the per-participant outcomes (side effect severity rating, benefit ",
      "rating, number of best aspects, first improvement duration), with and without adjustment for survey cohort"
    )
    foot <- paste0(
      "Sensitivity analysis by survey cohort. Left: the main adjusted per-participant ordinal models (adjusted for ",
      ADJUSTMENT_SETS[["legacy"]], "). Right: the same models with survey cohort (SA8 vs Baseline) added to the ",
      "covariates; the survey-cohort row of each outcome is from one model with the cohort as its focal predictor ",
      "and the same covariates. ", cohort_person_note, " ",
      "OR with nominal 95% CI; p values are Bonferroni-corrected for the four per-participant outcomes. ",
      "The mean number of side effects, the side-effect severity rating and treatment discontinuation were each ",
      "fitted separately (one model per measure) for the three effectiveness outcomes. ",
      "N is the complete-case sample of each model (participants). –: not estimated in that model.",
      if (any(tab$any_unconverged)) " †: the model reported a convergence warning." else "",
      if (sum(failed)) {
        paste0(
          " Not shown: ", failed[["unadjusted"]], " main and ", failed[["adjusted"]],
          " cohort-adjusted predictor set(s) that could not be fitted."
        )
      } else {
        ""
      }
    )
    header <- c("Parameter", "Adjusted OR [95% CI]", "p", "N", "Adjusted + survey cohort OR [95% CI]", "p", "N")
    body <- select(tab, Parameter, est_u, p_u, n_u, est_a, p_a, n_a)
    file <- "supp_cohort_legacy.html"
    writeLines(html_table(header, body, foot, italic_rows = italic), file.path(out_dir, file))
    cohort_index <- c(
      cohort_index, "",
      paste0("#### Supplementary Table ", sid, ": ", title),
      paste0("`manuscript-tables/", file, "`, ", nrow(body), " rows. ", foot),
      md_table(header, body)
    )
    message("  wrote survey-cohort table ", sid, " (", nrow(body), " rows)")
  }
}

md <- c(
  md, "", "## Sensitivity analysis: survey cohort",
  "",
  paste0(
    "`glmm_models_cov_cohort`, `sef_models_cohort`, `eff_models_cohort`: the adjusted longitudinal and ",
    "per-participant models with survey cohort (SA8 vs Baseline) added to the covariates; workbooks ",
    "`results/glmm_medications_cov_cohort.xlsx` and `results/medications_cov_cohort.xlsx`. Supplementary ",
    "Tables S5a-S5e (numbering provisional)."
  ),
  cohort_index
)

writeLines(md, md_path)
message("Wrote ", md_path, " and the HTML tables in ", out_dir)

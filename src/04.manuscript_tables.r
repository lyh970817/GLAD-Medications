# Tables for the manuscript.
#
# The eight Results tables were previously assembled by hand from the workbooks,
# which is why nothing recorded how the bracket convention or the N footnotes
# were derived. This script regenerates all of them from the cached model
# objects, so the tables in the document can be reproduced and re-checked. It
# also builds the sample description (Table 0, from `dat`) and the
# supplementary tables S1a-S2e (every estimate behind each Results table).
#
# Convention for the Results tables, matching the existing manuscript:
#   - one row per parameter significant after Bonferroni correction in the
#     unadjusted model, the adjusted model, or both
#   - adjusted estimates in parentheses after the unadjusted ones
#   - a row significant only after adjustment appears in parentheses alone
#
# Output: markdown to docs/manuscript-tables-current.md, and one ready-to-paste
# HTML file per table in docs/manuscript-tables/ (table_0, table_1a-2e,
# supp_1a-2e). Set MANUSCRIPT_OUT_DIR to write both somewhere other than docs/,
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
  legacy = c("sef_models", "eff_models", "sef_models_compete", "eff_models_compete")
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
  list(id = "1a", outcome = "Number of Side Effects", family = "glmm", effect = "RR",
       title = "Variables significantly associated with the number of side effects"),
  list(id = "1b", outcome = "Side effect severity rating", family = "legacy", effect = "OR",
       title = "Variables significantly associated with side effect severity rating"),
  list(id = "1c", outcome = "Stopped due to Side Effects", family = "glmm", effect = "OR",
       title = "Variables significantly associated with stopping a medication because of side effects"),
  list(id = "2a", outcome = "Effectiveness", family = "glmm", effect = "OR",
       title = "Variables significantly associated with effectiveness"),
  list(id = "2b", outcome = "Benefit rating", family = "legacy", effect = "OR",
       title = "Variables significantly associated with benefit rating"),
  list(id = "2c", outcome = "Number of best aspects", family = "legacy", effect = "OR",
       title = "Variables significantly associated with the number of best aspects"),
  # "Occurrence of remission" was participant-level wording for what is in fact a
  # per-medication outcome: remission is recorded for each antidepressant, and
  # participants who took more than one often differ across them (the share is
  # computed by src/05.manuscript_numbers.r).
  list(id = "2d", outcome = "Remission", family = "glmm", effect = "OR",
       title = "Variables significantly associated with remission on a medication"),
  list(id = "2e", outcome = "First improvement duration", family = "legacy", effect = "OR",
       title = "Variables significantly associated with first improvement duration")
)

ADJUSTMENT_SETS <- c(
  glmm = "sex, starting age, cumulative medication count and PHQ-9",
  legacy = "sex, average starting age and the number of psychiatric diagnosis types in the immediate family"
)

md <- c(
  "# Manuscript tables — current numbers",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/04.manuscript_tables.r`. Table 0"),
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
# Table 0: sample description by survey cohort
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
  "#### Table 0: Sample characteristics by survey cohort",
  sample_foot,
  md_table(sample_header, sample_body)
)
message("  wrote table 0 (", nrow(sample_body), " rows)")

# ------------------------------------------------------------------------------
# Tables 1a-2e: significant associations
# ------------------------------------------------------------------------------
for (spec in TABLES) {
  fam <- families[[spec$family]]
  if (is.null(fam)) {
    md <- c(md, "", paste0("#### Table ", spec$id, ": ", spec$title), skip_note(spec$family))
    next
  }

  tab <- build_table(fam$unadj, fam$adj, spec$outcome)

  if (is.null(tab)) {
    message("!! no table for ", spec$id, " (", spec$outcome, ")")
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

  header <- c("Parameter", paste0(spec$effect, " [95% CI] (adjusted)"), "p (adjusted)")
  body <- select(tab, Parameter, estimate, pval)

  md <- c(
    md, "",
    paste0("#### Table ", spec$id, ": ", spec$title),
    foot,
    md_table(header, body)
  )

  writeLines(html_table(header, body, foot), file.path(out_dir, paste0("table_", spec$id, ".html")))
  message("  wrote table ", spec$id, " (", nrow(tab), " rows)")
}

# ------------------------------------------------------------------------------
# Supplementary Tables S1a-S2e: every estimate behind each Results table
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
  sid <- paste0("S", spec$id)
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

writeLines(md, md_path)
message("Wrote ", md_path, " and the HTML tables in ", out_dir)

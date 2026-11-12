# Clinically interpretable quantities for the manuscript, from the analysis
# frames and the cached model objects. The only models fitted are the three
# adjusted ordinal effectiveness models of Table 5 (authorised 2026-11-09),
# because the cached ordinal results cannot be predicted from; they are kept
# in cache/clinical_fits/ and reused, so only the first run fits them (20-50
# minutes each, three at a time; CLINICAL_WORKERS sets the number of workers).
#
# Numbering (2026-11-11): the cross-table is Table 2 (was 4), the cached
# effectiveness and remission tables it checks against are Tables 4a and 4d
# (were 3a and 3d); Table 5 kept its number. The numbers and titles live in
# CLINICAL_TABLE_NUMBERS / CLINICAL_TABLE_TITLES (lib/11.clinical_quantities.r),
# with the table footnotes, so they can be checked without fitting.
#
# A. Table 2: reported effectiveness by stopping because of side effects, per
#    medication, from `dat_long`, with unknown values kept as their own row
#    and column; and two derived proportions with participant cluster-bootstrap
#    95% CIs. The table and its footnote are for the sample overall (no survey
#    cohort in main-text tables, ruling 2026-11-11); the by-cohort counts and
#    proportions are still computed, checked and written as number keys.
# B. Table 5: predicted probabilities of each outcome
#    category by the side-effect measures, from the adjusted longitudinal
#    models: for remission the cached fits in `glmm_models_cov` (else the
#    unadjusted `glmm_models`), for effectiveness the three refits. The
#    predictions average over the model's own rows with the participant random
#    intercept set to 0 (conditional on a typical participant, not marginal over
#    the random effect); 95% CIs are a parametric bootstrap of the fixed effects
#    (1,000 multivariate normal draws from their covariance). See
#    lib/11.clinical_quantities.r for why the effectiveness models had to be
#    refitted: the cached ordinal results keep no thresholds, covariate
#    coefficients or covariance.
#
# Output (MANUSCRIPT_OUT_DIR, default docs/):
#   manuscript-tables/table_cross_effectiveness_stopping.html  (Table 2)
#   manuscript-tables/table_predicted_probabilities.html       (Table 5)
#   manuscript-tables-current.md: both tables, in a block between the
#     "BEGIN/END src/06.clinical_quantities.r" markers that is replaced on every
#     run. src/04.manuscript_tables.r rewrites that file in full, so run this
#     script after it.
#   manuscript-numbers-clinical.md: the key numbers, as key / value /
#     definition, in a sidecar beside src/05's manuscript-numbers-current.md
#     (which src/05 rewrites in full and which only reads cached tables).
#
# C. Number keys only (no table): the predicted change in P(effectiveness =
#    very well) and P(not at all) for one side effect above the person's own
#    mean, from the adjusted number-of-side-effects model without and with the
#    individual antidepressant (`glmm_models_cov_drug`); the drug-adjusted
#    model is predicted from src/02's cached fit, which keeps its thresholds
#    and covariance, so it is not refitted.
#
# Checks (the run stops if one fails): the cross-table margins equal the rows
# of `dat_long` by cohort and the medication counts in
# manuscript-numbers-current.md; every predicted contrast has the sign of its
# odds ratio in the cached adjusted table (Tables 4a and 4d); before that, the
# refitted effectiveness models must reproduce Table 4a's N, odds ratios and
# CIs (to two decimals).
#
# plyr, attached after dplyr, masks mutate(), summarise() and count(); keep them
# qualified (see "Namespace masking" in docs/analysis-pipeline.md).

require(ProjectTemplate)
require(tidyverse)
reload.project()

# Only the main adjusted and unadjusted sets are used; the sensitivity sets are
# large and not needed here.
rm(list = intersect(
  c("glmm_models_cov_famhist", "glmm_models_cov_class", "glmm_models_cov_cohort", "glmm_models_cov_zero",
    "glmm_models_cov_strict"),
  ls(globalenv())
), envir = globalenv())
invisible(gc())

docs_dir <- Sys.getenv("MANUSCRIPT_OUT_DIR", "docs")
out_dir <- file.path(docs_dir, "manuscript-tables")
md_path <- file.path(docs_dir, "manuscript-tables-current.md")
numbers_path <- file.path(docs_dir, "manuscript-numbers-clinical.md")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(20261109)
N_CLUSTER_BOOT <- 2000
N_PARAM_BOOT <- 1000

cohorts <- levels(droplevels(factor(dat_long$cohort)))
cohort_key <- function(x) tolower(gsub("[^A-Za-z0-9]+", "_", x))
fmt_pct <- function(x, digits = 1) formatC(100 * x, format = "f", digits = digits)

# Owner ruling 2026-11-12: by-cohort values are reference only (not in the write-up).
REF_SECTION <- "Reference only: survey cohort (not in the manuscript)"

numbers <- list()
add <- function(section, key, value, definition) {
  numbers[[length(numbers) + 1]] <<- tibble(
    section = section, key = key, value = as.character(value), definition = definition
  )
}

# ==============================================================================
# A. Effectiveness by stopping because of side effects (Table 2)
# ==============================================================================
CROSS_SECTION <- paste0("Effectiveness by stopping because of side effects (Table ",
  CLINICAL_TABLE_NUMBERS[["cross"]], ")")
EFF_LEVELS <- c("0" = "Not at all", "1" = "Moderately", "2" = "Very well")
STOP_LEVELS <- c("0" = "Not stopped", "1" = "Stopped")
UNKNOWN <- "Unknown"

cross <- dat_long %>%
  transmute(
    ID, cohort,
    eff = factor(coalesce(unname(EFF_LEVELS[as.character(effectiveness)]), UNKNOWN),
      levels = c(EFF_LEVELS, UNKNOWN)),
    stop = factor(coalesce(unname(STOP_LEVELS[as.character(stopped_due_to_se)]), UNKNOWN),
      levels = c(STOP_LEVELS, UNKNOWN))
  )

groups <- c(list(Overall = seq_len(nrow(cross))), split(seq_len(nrow(cross)), factor(cross$cohort, levels = cohorts)))

# One block per group: a row per effectiveness category and a Total row, cells
# n (row %), the Total column n.
cross_block <- function(i) {
  tab <- table(cross$eff[i], cross$stop[i])
  rows <- rbind(tab, Total = colSums(tab))
  tot <- rowSums(rows)
  body <- tibble(Effectiveness = rownames(rows))
  for (j in colnames(rows)) body[[j]] <- fmt_n_pct(rows[, j], tot)
  body$Total <- commafy(tot)
  attr(body, "counts") <- rows
  body
}
cross_blocks <- map(groups, cross_block)

# Check: the margins are the rows of dat_long, by cohort, and the medication
# counts src/05.manuscript_numbers.r reports.
numbers_current <- file.path(docs_dir, "manuscript-numbers-current.md")
reported <- if (file.exists(numbers_current)) {
  l <- readLines(numbers_current)
  l <- grep("^\\| medication_observations_dat_long_", l, value = TRUE)
  setNames(
    as.integer(gsub(",", "", trimws(vapply(strsplit(l, "|", fixed = TRUE), `[`, "", 3)))),
    sub("^medication_observations_dat_long_", "", trimws(vapply(strsplit(l, "|", fixed = TRUE), `[`, "", 2)))
  )
} else {
  integer()
}
for (g in names(cross_blocks)) {
  cnt <- attr(cross_blocks[[g]], "counts")
  margin <- sum(cnt["Total", ])
  stopifnot(
    "row margins do not add up" = sum(cnt[rownames(cnt) != "Total", ]) == margin,
    "cross-table total differs from dat_long" = margin == length(groups[[g]])
  )
  k <- cohort_key(g)
  if (k %in% names(reported)) {
    if (margin != reported[[k]]) {
      stop("Cross-table total for ", g, " (", margin, ") differs from medication_observations_dat_long_", k,
        " in ", numbers_current, " (", reported[[k]], ")")
    }
    message("  check: ", g, " margin ", commafy(margin), " = medication_observations_dat_long_", k)
  } else {
    warning("No medication_observations_dat_long_", k, " in ", numbers_current, "; margin checked against dat_long only")
  }
}

# Derived proportions, over medications with a known effectiveness and a known
# stop status: (1) of those that worked moderately or very well, the share
# stopped because of side effects; (2) of those that did not work at all, the
# share not stopped. 95% CI: percentile, from resampling participants.
derived_specs <- list(
  effective_stopped = list(
    den = function(d) d$eff %in% EFF_LEVELS[c("1", "2")] & d$stop %in% STOP_LEVELS,
    num = function(d) d$eff %in% EFF_LEVELS[c("1", "2")] & d$stop == STOP_LEVELS[["1"]],
    text = "Of medications reported as working moderately or very well, stopped because of side effects"
  ),
  not_effective_not_stopped = list(
    den = function(d) d$eff == EFF_LEVELS[["0"]] & d$stop %in% STOP_LEVELS,
    num = function(d) d$eff == EFF_LEVELS[["0"]] & d$stop == STOP_LEVELS[["0"]],
    text = "Of medications reported as not working at all, not stopped because of side effects"
  )
)
derived <- map_dfr(names(derived_specs), function(nm) {
  sp <- derived_specs[[nm]]
  map_dfr(names(groups), function(g) {
    d <- cross[groups[[g]], ]
    r <- cluster_boot_prop(sp$num(d), sp$den(d), d$ID, B = N_CLUSTER_BOOT)
    tibble(quantity = nm, text = sp$text, group = g, k = r$k, n = r$n, n_id = r$n_id,
      est = r$est, lo = r$lo, hi = r$hi)
  })
}) %>%
  dplyr::mutate(cell = paste0(
    commafy(k), " / ", commafy(n), ": ", fmt_pct(est), "% [", fmt_pct(lo), ", ", fmt_pct(hi), "]"
  ))

for (r in seq_len(nrow(derived))) {
  x <- derived[r, ]
  add(if (x$group == "Overall") CROSS_SECTION else REF_SECTION,
    paste0(x$quantity, "_", cohort_key(x$group)),
    paste0(fmt_pct(x$est), "% [", fmt_pct(x$lo), ", ", fmt_pct(x$hi), "] (", commafy(x$k), " of ", commafy(x$n), ")"),
    paste0(x$text, ", ", x$group, "; of medications with a known stop status, from ", commafy(x$n_id),
      " participants; 95% CI: participant cluster bootstrap (", commafy(N_CLUSTER_BOOT), " resamples, percentile)"))
}
for (g in names(cross_blocks)) {
  cnt <- attr(cross_blocks[[g]], "counts")
  for (e in rownames(cnt)) for (s in colnames(cnt)) {
    add(if (g == "Overall") CROSS_SECTION else REF_SECTION,
      paste0("cross_", cohort_key(e), "_", cohort_key(s), "_", cohort_key(g)),
      fmt_n_pct(cnt[e, s], sum(cnt[e, ])),
      paste0("Medications (rows of `dat_long`), ", g, ": effectiveness ", e, ", ", s,
        " (row %, of all medications with effectiveness ", e, ")"))
  }
}

# The table shows the sample overall only: no group heading rows, no cohort
# blocks.
cross_header <- c("Effectiveness", unname(STOP_LEVELS), UNKNOWN, "Total")
cross_body <- cross_blocks[["Overall"]]
attr(cross_body, "counts") <- NULL
cross_italic <- rep(FALSE, nrow(cross_body))
derived_overall <- function(q) filter(derived, quantity == q, group == "Overall")$cell
cross_foot <- cross_table_footnote(
  derived_overall("effective_stopped"), derived_overall("not_effective_not_stopped"), N_CLUSTER_BOOT
)
writeLines(
  html_table(cross_header, cross_body, cross_foot, italic_rows = cross_italic),
  file.path(out_dir, "table_cross_effectiveness_stopping.html")
)
message("  wrote Table ", CLINICAL_TABLE_NUMBERS[["cross"]], " (table_cross_effectiveness_stopping.html, ", nrow(cross_body), " rows)")

# ==============================================================================
# B. Predicted probabilities (Table 5)
# ==============================================================================
# The three side-effect predictor sets of the effectiveness and remission
# models, by the terms their formulas start with (lib/09.within_between.r).
SE_SETS <- list(
  stopped = within_between_terms("stopped_due_to_se"),
  n_se = within_between_terms("n_se"),
  se_rating = "se_rating"
)
pp_outcomes <- c(Effectiveness = "effectiveness", Remission = "remission")
CATEGORIES <- list(Effectiveness = unname(EFF_LEVELS), Remission = "Remission")
TABLE_REF <- c(Effectiveness = CLINICAL_TABLE_NUMBERS[["effectiveness"]], Remission = CLINICAL_TABLE_NUMBERS[["remission"]])

FOCAL_TERM <- c(stopped = "stopped_due_to_se_within", n_se = "n_se_between", se_rating = "se_rating")
SET_LABELS <- c(
  stopped = "Stopped because of side effects (within person)",
  n_se = "Mean number of side effects (between persons; within-person term at 0)",
  se_rating = "Side-effect severity rating (person level)"
)
# The labels the cached tables carry for the focal terms, as src/02.regression.r
# builds them ("... (within person)", "... (between persons)").
term_labels <- c(labels, within_between_labels(WITHIN_BETWEEN_VARS, labels))

if (!all(objects_present("glmm_models_cov"))) {
  stop("Table 5 needs `glmm_models_cov` from src/02.regression.r in cache/, fitted on the current `dat_long`")
}

# ------------------------------------------------------------------------------
# Remission: the fitted glmer models recovered from the cached tables
# ------------------------------------------------------------------------------
# The adjusted fit where it can be recovered, else the unadjusted one.
locate_frame <- function(outcome, terms) {
  for (obj in c("glmm_models_cov", "glmm_models")) {
    if (!objects_present(obj)) next
    s <- find_set_by_terms(get(obj, envir = globalenv())[[outcome]], terms)
    fr <- cached_fit_frame(s)
    if (!is.null(fr)) {
      return(list(frame = fr, set = s, source = obj))
    }
  }
  NULL
}

# ------------------------------------------------------------------------------
# Effectiveness: the three adjusted ordinal models, refitted (authorised
# 2026-11-09 for exactly these three) or reused from cache/clinical_fits/
# ------------------------------------------------------------------------------
# The formula is the cached set's own `model_formula`; the frame is built as in
# src/02.regression.r (coerce_glmm_predictor() on the predictors, then the
# within/between terms over all rows), and clmm_job() takes fit_glmm()'s
# complete cases. The complete-case N and participants must equal the cached
# set's before anything is fitted, and the refitted odds ratios and CIs must
# equal Table 4a's to two decimals before anything is predicted.
eff_sets <- map(SE_SETS, ~ find_set_by_terms(glmm_models_cov[["Effectiveness"]], .x))
if (any(map_lgl(eff_sets, is.null))) {
  stop("Adjusted effectiveness set(s) not in glmm_models_cov: ", paste(names(eff_sets)[map_lgl(eff_sets, is.null)], collapse = ", "))
}
formula_vars <- unique(unlist(map(eff_sets, function(st) {
  trimws(strsplit(sub("^[^~]*~", "", attr(st, "model_formula")), "+", fixed = TRUE)[[1]])
})))
coerce_vars <- union(intersect(formula_vars, names(dat_long)), WITHIN_BETWEEN_VARS)
dat_long_glmm <- dat_long %>%
  dplyr::mutate(across(all_of(coerce_vars), coerce_glmm_predictor)) %>%
  add_within_between(WITHIN_BETWEEN_VARS)

eff_jobs <- map(eff_sets, ~ clmm_job(attr(.x, "model_formula"), dat_long_glmm))
for (k in names(eff_jobs)) {
  j <- eff_jobs[[k]]
  st <- eff_sets[[k]]
  if (nrow(j$data) != attr(st, "n")[1] || n_distinct(j$data$ID) != attr(st, "n_id")) {
    stop("Rebuilt effectiveness frame for ", k, " has ", nrow(j$data), " rows / ", n_distinct(j$data$ID),
      " participants; the cached model has ", attr(st, "n")[1], " / ", attr(st, "n_id"))
  }
  message("  effectiveness / ", k, ": ", j$formula, " on ", commafy(nrow(j$data)), " rows (= cached N)")
}
eff_fits <- fit_clinical_clmms(eff_jobs, workers = as.integer(Sys.getenv("CLINICAL_WORKERS", "3")))
names(eff_fits) <- names(eff_jobs)

eff_frames <- list()
for (k in names(eff_fits)) {
  f <- eff_fits[[k]]
  st <- eff_sets[[k]]
  mine <- clmm_focal_or(f$m, SE_SETS[[k]])
  cached <- st[match(unname(term_labels[SE_SETS[[k]]]), st$Parameter), c("Coefficient", "CI_low", "CI_high")]
  same <- function(x, y) round(x, 2) == round(y, 2) | abs(x - y) < 1e-4
  ok <- all(same(mine$Coefficient, cached$Coefficient), same(mine$CI_low, cached$CI_low), same(mine$CI_high, cached$CI_high))
  if (!isTRUE(ok)) {
    stop("Refitted effectiveness / ", k, " does not reproduce Table ", TABLE_REF[["Effectiveness"]], ":\n",
      paste(capture.output(print(bind_cols(mine, rename_with(cached, ~ paste0("cached_", .x))))), collapse = "\n"))
  }
  message("  check: effectiveness / ", k, " refit reproduces Table ", TABLE_REF[["Effectiveness"]], " (",
    paste0(mine$term, " ", formatC(mine$Coefficient, format = "f", digits = 2), " [",
      formatC(mine$CI_low, format = "f", digits = 2), ", ", formatC(mine$CI_high, format = "f", digits = 2), "]",
      collapse = "; "), "); fitted in ", round(f$seconds / 60, 1), " min")
  eff_frames[[k]] <- list(
    frame = list(m = f$m, data = f$data, scales = setNames(rep(1, length(eff_jobs[[k]]$indeps)), eff_jobs[[k]]$indeps)),
    set = st, source = "glmm_models_cov", seconds = f$seconds, file = basename(eff_jobs[[k]]$path)
  )
  add("Refitted effectiveness models", paste0("clinical_fit_minutes_", k), formatC(f$seconds / 60, format = "f", digits = 1),
    paste0("Minutes to fit the adjusted ordinal effectiveness model `", f$formula, "` (", commafy(nrow(f$data)),
      " observations), cache/clinical_fits/", basename(eff_jobs[[k]]$path), "; fitted ", format(f$fitted_at, "%Y-%m-%d %H:%M")))
}

# ------------------------------------------------------------------------------
# Scenarios, as functions of the model's complete-case data
# ------------------------------------------------------------------------------
participant_quantiles <- function(d, var, type = 7) {
  x <- d %>% distinct(ID, .data[[var]]) %>% pull(var)
  stats::quantile(x, c(0.25, 0.75), type = type, names = FALSE)
}
scenarios_for <- function(set_name, d) {
  switch(set_name,
    # (i) the same medication not stopped vs stopped, for the same person: the
    # participant's mean (between term) at its observed value, the within term
    # at 0 - mean or 1 - mean.
    stopped = list(
      values = c("Not stopped", "Stopped"),
      keys = c("not_stopped", "stopped"),
      fns = list(
        function(d) list(stopped_due_to_se_within = 0 - d$stopped_due_to_se_between),
        function(d) list(stopped_due_to_se_within = 1 - d$stopped_due_to_se_between)
      )
    ),
    # (ii) the participant's mean number of side effects at the 25th and 75th
    # percentile over participants, the within term at 0.
    n_se = {
      q <- participant_quantiles(d, "n_se_between")
      list(
        values = paste0(c("25th", "75th"), " percentile (", formatC(q, format = "f", digits = 2), ")"),
        keys = c("n_se_between_p25", "n_se_between_p75"),
        fns = lapply(q, function(v) function(d) list(n_se_within = 0, n_se_between = v)),
        q = q
      )
    },
    # (iii) the person-level severity rating at its 25th and 75th percentile
    # over participants (an observed rating, type 1). The models code it 1-6
    # (as.numeric of the ordered factor 0-5), so 1 is subtracted for display.
    se_rating = {
      q <- participant_quantiles(d, "se_rating", type = 1)
      list(
        values = paste0(c("25th", "75th"), " percentile (rating ", q - 1, ")"),
        keys = c("se_rating_p25", "se_rating_p75"),
        fns = lapply(q, function(v) function(d) list(se_rating = v)),
        q = q - 1
      )
    }
  )
}

# ------------------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------------------
pp_rows <- list()
for (outcome in names(pp_outcomes)) {
  for (set_name in names(SE_SETS)) {
    loc <- if (outcome == "Effectiveness") eff_frames[[set_name]] else locate_frame(outcome, SE_SETS[[set_name]])
    if (is.null(loc)) stop(outcome, " / ", set_name, ": no fitted model available")
    fr <- loc$frame
    cats <- CATEGORIES[[outcome]]
    pred <- model_predictor(fr, cats)
    sc <- scenarios_for(set_name, fr$data)
    draws <- draw_mvn(pred$theta, pred$V, N_PARAM_BOOT)
    pr <- predict_scenarios(fr, pred, sc$fns, draws)

    # Check: the contrast follows the sign of the focal odds ratio in the
    # cached table (Table 4a / 4d, adjusted). For the ordinal outcome a
    # positive log odds ratio moves probability to the higher categories, so the
    # top category moves with it and the bottom one against it.
    beta <- pred$theta[[FOCAL_TERM[[set_name]]]]
    or_row <- loc$set[loc$set$Parameter == unname(term_labels[FOCAL_TERM[[set_name]]]), ]
    if (nrow(or_row) != 1) stop("No row ", term_labels[FOCAL_TERM[[set_name]]], " in the cached ", outcome, " set")
    d_est <- pr[[2]]$est - pr[[1]]$est
    ok <- sign(log(or_row$Coefficient)) == sign(beta) && sign(d_est[[length(cats)]]) == sign(beta) &&
      (length(cats) == 1 || sign(d_est[[1]]) == -sign(beta))
    if (!ok) {
      stop("Predicted contrast for ", outcome, " / ", set_name, " (", paste(signif(d_est, 3), collapse = ", "),
        ") does not follow the sign of its odds ratio (", or_row$Coeff_CI, ")")
    }
    message("  check: ", outcome, " / ", set_name, " contrast ", paste0(cats, " ", fmt_pct(d_est), " pp", collapse = ", "),
      "; OR ", or_row$Coeff_CI, " (Table ", TABLE_REF[[outcome]], "; same direction)")

    n_txt <- paste0(commafy(attr(loc$set, "n")[1]), " / ", commafy(attr(loc$set, "n_id")))
    n_long <- paste0(commafy(attr(loc$set, "n")[1]), " observations from ", commafy(attr(loc$set, "n_id")), " participants")
    for (j in seq_along(pr)) {
      for (cc in seq_along(cats)) {
        pp_rows[[length(pp_rows) + 1]] <- tibble(
          outcome = outcome, set = set_name, value = sc$values[j], key = sc$keys[j], j = j,
          category = cats[cc], cat_key = if (length(cats) > 1) paste0("_", cohort_key(cats[cc])) else "",
          prob = ci_text(pr[[j]]$est[[cc]], pr[[j]]$boot[, cc]),
          diff = if (j == 1) "Reference" else ci_text(pr[[j]]$est[[cc]] - pr[[1]]$est[[cc]], pr[[j]]$boot[, cc] - pr[[1]]$boot[, cc]),
          or = or_row$Coeff_CI, n = n_txt, n_long = n_long, source = loc$source,
          q = if (!is.null(sc$q)) sc$q[j] else NA_real_
        )
      }
    }
    rm(draws, pr)
    invisible(gc())
  }
}
pp <- bind_rows(pp_rows)

for (r in seq_len(nrow(pp))) {
  x <- pp[r, ]
  what <- if (x$outcome == "Effectiveness") paste0("effectiveness \"", tolower(x$category), "\"") else "remission"
  add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$key, x$cat_key),
    sub("^(\\S+) ", "\\1% ", x$prob),
    paste0("Predicted probability (%, 95% CI) of ", what, " for a medication, ",
      SET_LABELS[[x$set]], ": ", x$value, "; ", if (x$source == "glmm_models_cov") "adjusted" else "unadjusted",
      " model (", x$n_long, "), averaged over the model's rows with the random intercept at 0; ",
      "parametric bootstrap CI (", commafy(N_PARAM_BOOT), " draws)"))
  if (x$diff != "Reference") {
    add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$set, "_difference", x$cat_key),
      sub("^(\\S+) ", "\\1 pp ", x$diff),
      paste0("Difference in predicted probability of ", what, ", ", SET_LABELS[[x$set]],
        ", second minus first scenario; 95% CI from the same parametric bootstrap draws"))
  }
  if (!is.na(x$q) && x$category == CATEGORIES[[x$outcome]][1]) {
    add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$key, "_value"),
      formatC(x$q, format = "f", digits = 2),
      paste0("Value of the ", SET_LABELS[[x$set]], " at that percentile over the participants in the ", tolower(x$outcome), " model"))
  }
}

pp_header <- c("Side-effect measure", "Value", "Outcome category", "Predicted probability, % [95% CI]",
  "Difference, percentage points [95% CI]", "Adjusted OR [95% CI]", "N (observations / participants)")
OUTCOME_HEADINGS <- pp_outcome_headings()
pp_body <- list()
pp_italic <- logical()
for (outcome in unique(pp$outcome)) {
  pp_body[[length(pp_body) + 1]] <- tibble(a = OUTCOME_HEADINGS[[outcome]], b = "", c = "", d = "", e = "", f = "", g = "")
  pp_italic <- c(pp_italic, TRUE)
  for (s in unique(pp$set[pp$outcome == outcome])) {
    x <- filter(pp, outcome == !!outcome, set == s)
    first_of_value <- !duplicated(x$j)
    pp_body[[length(pp_body) + 1]] <- tibble(
      a = c(SET_LABELS[[s]], rep("", nrow(x) - 1)), b = ifelse(first_of_value, x$value, ""), c = x$category,
      d = x$prob, e = x$diff,
      f = c(x$or[1], rep("", nrow(x) - 1)), g = c(x$n[1], rep("", nrow(x) - 1))
    )
    pp_italic <- c(pp_italic, rep(FALSE, nrow(x)))
  }
}
pp_body <- bind_rows(pp_body)
names(pp_body) <- pp_header

any_unadjusted <- any(pp$source != "glmm_models_cov")
pp_foot <- pp_footnote(any_unadjusted, N_PARAM_BOOT)
writeLines(html_table(pp_header, pp_body, pp_foot, italic_rows = pp_italic),
  file.path(out_dir, "table_predicted_probabilities.html"))
message("  wrote Table ", CLINICAL_TABLE_NUMBERS[["predicted"]], " (table_predicted_probabilities.html, ", nrow(pp_body), " rows)")

# ==============================================================================
# C. One side effect above the person's own mean (number keys only, not Table 5)
# ==============================================================================
# From the adjusted effectiveness ~ number of side effects (within/between)
# model, without and with adjustment for the individual antidepressant
# (`glmm_models_cov_drug`, lib/12.drug_identity.r; user approval 2026-11-11):
# the predicted change in P(effectiveness = very well) and P(not at all) when
# the within-person term goes from 0 to +1 (one side effect more than the
# person's own mean), the between-person term and the covariates at their
# observed values, random intercept 0, averaged over the model's rows; CIs from
# the same kind of parametric bootstrap as Table 5. The main model is the refit
# above. The drug-adjusted one is not refitted: src/02 fitted it after
# fit_glmm() began keeping an ordinal fit's thresholds, coefficients and
# covariance on the result, so it is predicted from the cached set, on its
# complete-case rows rebuilt here (N checked against the cached set). Its own
# seed, set after Table 5, so Table 5's draws are unchanged.
set.seed(20261111)
WITHIN_SECTION <- "One side effect above the person's mean (number of side effects, within person)"
WITHIN_CATS <- c(very_well = "Very well", not_at_all = "Not at all")
eff_cats <- CATEGORIES[["Effectiveness"]]
within_models <- list(adjusted = list(
  frame = eff_frames$n_se$frame, pred = model_predictor(eff_frames$n_se$frame, eff_cats), set = eff_sets$n_se,
  what = "adjusted for sex, start age, cumulative medication count and PHQ-9"
))

if (all(objects_present("glmm_models_cov_drug"))) {
  st_drug <- find_set_by_terms(glmm_models_cov_drug[["Effectiveness"]], SE_SETS$n_se)
  if (is.null(st_drug)) stop("No number-of-side-effects set in glmm_models_cov_drug$Effectiveness")
  drug_job <- clmm_job(attr(st_drug, "model_formula"), add_drug_identity(dat_long_glmm))
  if (nrow(drug_job$data) != attr(st_drug, "n")[1] || n_distinct(drug_job$data$ID) != attr(st_drug, "n_id")) {
    stop("Rebuilt drug-adjusted effectiveness frame has ", nrow(drug_job$data), " rows / ", n_distinct(drug_job$data$ID),
      " participants; the cached model has ", attr(st_drug, "n")[1], " / ", attr(st_drug, "n_id"))
  }
  cp <- cached_clmm_predictor(st_drug, drug_job$data, drug_job$indeps, eff_cats)
  if (is.null(cp)) {
    stop("The cached drug-adjusted effectiveness / n_se set carries no thresholds or covariance; refit it with src/02")
  }
  # The stored coefficients are the ones the cached table reports.
  or_cached <- st_drug$Coefficient[match(unname(term_labels[SE_SETS$n_se]), st_drug$Parameter)]
  if (!isTRUE(all.equal(unname(exp(cp$pred$theta[SE_SETS$n_se])), or_cached, tolerance = 1e-8))) {
    stop("Stored clmm coefficients of the drug-adjusted set do not match its reported odds ratios")
  }
  message("  effectiveness / n_se, drug-adjusted: ", drug_job$formula, " on ", commafy(nrow(drug_job$data)),
    " rows (= cached N), predicted from the cached fit (", length(cp$pred$theta), " parameters)")
  within_models$drug_adjusted <- list(
    frame = cp$frame, pred = cp$pred, set = st_drug,
    what = "adjusted for sex, start age, cumulative medication count, PHQ-9 and the individual antidepressant"
  )
} else {
  message("!! drug-adjusted within-person contrast skipped: needs glmm_models_cov_drug in cache/")
  add(WITHIN_SECTION, "pp_within_n_se_plus1_drug_adjusted", "skipped: needs `glmm_models_cov_drug` from src/02.regression.r",
    "Predicted change for one side effect above the person's mean, drug-adjusted model")
}

for (mod in names(within_models)) {
  wm <- within_models[[mod]]
  pred <- wm$pred
  fns <- list(function(d) list(n_se_within = 0), function(d) list(n_se_within = 1))
  draws <- draw_mvn(pred$theta, pred$V, N_PARAM_BOOT)
  pr <- predict_scenarios(wm$frame, pred, fns, draws)
  beta <- pred$theta[["n_se_within"]]
  d_est <- pr[[2]]$est - pr[[1]]$est
  if (!(sign(d_est[["Very well"]]) == sign(beta) && sign(d_est[["Not at all"]]) == -sign(beta))) {
    stop("Within-person +1 contrast (", mod, ") does not follow the sign of its coefficient")
  }
  n_long <- paste0(commafy(attr(wm$set, "n")[1]), " observations from ", commafy(attr(wm$set, "n_id")), " participants")
  message("  ", mod, ": one side effect above the person's mean: ",
    paste0(eff_cats, " ", formatC(100 * d_est, format = "f", digits = 2), " pp", collapse = ", "))
  for (ck in names(WITHIN_CATS)) {
    cc <- WITHIN_CATS[[ck]]
    key <- paste0("pp_within_n_se_plus1_", mod, "_", ck)
    add(WITHIN_SECTION, paste0(key, "_at_0"), sub("^(\\S+) ", "\\1% ", ci_text(pr[[1]]$est[[cc]], pr[[1]]$boot[, cc])),
      paste0("Predicted probability (%, 95% CI) of effectiveness \"", tolower(cc), "\", number of side effects at the ",
        "person's own mean (within-person term 0), ", wm$what, " (", n_long, "); between-person term and covariates ",
        "observed, random intercept 0, averaged over the model's rows; parametric bootstrap CI (",
        commafy(N_PARAM_BOOT), " draws)"))
    add(WITHIN_SECTION, paste0(key, "_at_plus1"), sub("^(\\S+) ", "\\1% ", ci_text(pr[[2]]$est[[cc]], pr[[2]]$boot[, cc])),
      paste0("As `", key, "_at_0`, with one side effect more than the person's own mean (within-person term +1)"))
    add(WITHIN_SECTION, paste0(key, "_difference"),
      sub("^(\\S+) ", "\\1 pp ", ci_text(pr[[2]]$est[[cc]] - pr[[1]]$est[[cc]], pr[[2]]$boot[, cc] - pr[[1]]$boot[, cc], digits = 2)),
      paste0("Change in the predicted probability of effectiveness \"", tolower(cc), "\" (percentage points, 95% CI) for ",
        "one side effect above the person's own mean (within-person term +1 vs 0), ", wm$what,
        "; same draws as `", key, "_at_0`"))
  }
  rm(draws, pr)
  invisible(gc())
}

# ==============================================================================
# Markdown: the block in manuscript-tables-current.md, and the numbers sidecar
# ==============================================================================
BEGIN <- "<!-- BEGIN src/06.clinical_quantities.r -->"
END <- "<!-- END src/06.clinical_quantities.r -->"
block <- c(
  BEGIN,
  "",
  "## Clinical quantities",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/06.clinical_quantities.r` from `dat_long`, the cached"),
  "remission models and three refitted adjusted effectiveness models (`cache/clinical_fits/`).",
  "Table numbers as renumbered on 2026-11-11 (cross-table Table 2, predicted probabilities Table 5).",
  "",
  clinical_table_heading("cross"),
  paste0("`manuscript-tables/table_cross_effectiveness_stopping.html`. ", cross_foot),
  md_table(cross_header, cross_body),
  "",
  clinical_table_heading("predicted"),
  paste0("`manuscript-tables/table_predicted_probabilities.html`. ", pp_foot),
  md_table(pp_header, pp_body),
  "",
  END
)
md <- if (file.exists(md_path)) readLines(md_path) else character()
b <- match(BEGIN, md)
e <- match(END, md)
md <- if (!is.na(b) && !is.na(e) && e > b) {
  c(md[seq_len(b - 1)], block, md[-seq_len(e)])
} else {
  c(md, "", block)
}
writeLines(md, md_path)
message("Wrote the clinical-quantities block in ", md_path)

numbers <- bind_rows(numbers)
nmd <- c(
  "# Manuscript numbers — clinical quantities",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/06.clinical_quantities.r` from `dat_long` and the"),
  "longitudinal models (remission: cached fits; effectiveness: three refits in `cache/clinical_fits/`).",
  "A sidecar to `manuscript-numbers-current.md`",
  "(`src/05.manuscript_numbers.r`); keys ending in `overall` give the whole sample, keys ending in a cohort",
  "name are in the last section, reference only (not in the manuscript).",
  ""
)
secs <- unique(numbers$section)
secs <- c(setdiff(secs, REF_SECTION), intersect(secs, REF_SECTION))
for (s in secs) {
  rows <- filter(numbers, section == s)
  nmd <- c(nmd, paste0("## ", s), "", md_table(c("Key", "Value", "Definition"), select(rows, key, value, definition)), "")
}
writeLines(nmd, numbers_path)
message("Wrote ", numbers_path, " (", nrow(numbers), " numbers)")

# Clinically interpretable quantities for the manuscript, from the analysis
# frames and the cached model objects. Nothing is refitted.
#
# A. Table 4 (provisional number): reported effectiveness by stopping because
#    of side effects, per medication, from `dat_long`, overall and by cohort,
#    with unknown values kept as their own row and column; and two derived
#    proportions with participant cluster-bootstrap 95% CIs.
# B. Table 5 (provisional number): predicted probabilities of the outcome by
#    the side-effect measures, from the cached adjusted longitudinal models
#    (`glmm_models_cov`), else the unadjusted ones (`glmm_models`). The
#    predictions average over the model's own rows with the participant random
#    intercept set to 0 (conditional on a typical participant, not marginal over
#    the random effect); 95% CIs are a parametric bootstrap of the fixed effects
#    (1,000 multivariate normal draws from their covariance). See
#    lib/11.clinical_quantities.r for why only the binomial (remission) fits can
#    be used: the cached ordinal (effectiveness) results keep no thresholds,
#    covariate coefficients or covariance.
#
# Output (MANUSCRIPT_OUT_DIR, default docs/):
#   manuscript-tables/table_cross_effectiveness_stopping.html  (Table 4)
#   manuscript-tables/table_predicted_probabilities.html       (Table 5)
#   manuscript-tables-current.md: both tables, in a block between the
#     "BEGIN/END src/06.clinical_quantities.r" markers that is replaced on every
#     run. src/04.manuscript_tables.r rewrites that file in full, so run this
#     script after it.
#   manuscript-numbers-clinical.md: the key numbers, as key / value /
#     definition, in a sidecar beside src/05's manuscript-numbers-current.md
#     (which src/05 rewrites in full and which only reads cached tables).
#
# Checks (the run stops if one fails): the cross-table margins equal the rows
# of `dat_long` by cohort and the medication counts in
# manuscript-numbers-current.md; every predicted contrast has the sign of its
# odds ratio in the cached adjusted table (Tables 3a and 3d).
#
# plyr, attached after dplyr, masks mutate(), summarise() and count(); keep them
# qualified (see "Namespace masking" in docs/analysis-pipeline.md).

require(ProjectTemplate)
require(tidyverse)
reload.project()

# Only the main adjusted and unadjusted sets are used; the sensitivity sets are
# large and not needed here.
rm(list = intersect(c("glmm_models_cov_famhist", "glmm_models_cov_class"), ls(globalenv())), envir = globalenv())
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

numbers <- list()
add <- function(section, key, value, definition) {
  numbers[[length(numbers) + 1]] <<- tibble(
    section = section, key = key, value = as.character(value), definition = definition
  )
}

# ==============================================================================
# A. Effectiveness by stopping because of side effects (Table 4)
# ==============================================================================
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
  add("Effectiveness by stopping because of side effects (Table 4)",
    paste0(x$quantity, "_", cohort_key(x$group)),
    paste0(fmt_pct(x$est), "% [", fmt_pct(x$lo), ", ", fmt_pct(x$hi), "] (", commafy(x$k), " of ", commafy(x$n), ")"),
    paste0(x$text, ", ", x$group, "; of medications with a known stop status, from ", commafy(x$n_id),
      " participants; 95% CI: participant cluster bootstrap (", commafy(N_CLUSTER_BOOT), " resamples, percentile)"))
}
for (g in names(cross_blocks)) {
  cnt <- attr(cross_blocks[[g]], "counts")
  for (e in rownames(cnt)) for (s in colnames(cnt)) {
    add("Effectiveness by stopping because of side effects (Table 4)",
      paste0("cross_", cohort_key(e), "_", cohort_key(s), "_", cohort_key(g)),
      fmt_n_pct(cnt[e, s], sum(cnt[e, ])),
      paste0("Medications (rows of `dat_long`), ", g, ": effectiveness ", e, ", ", s,
        " (row %, of all medications with effectiveness ", e, ")"))
  }
}

cross_header <- c("Effectiveness", unname(STOP_LEVELS), UNKNOWN, "Total")
cross_body <- bind_rows(map(names(cross_blocks), function(g) {
  b <- cross_blocks[[g]]
  bind_rows(
    tibble(Effectiveness = paste0(g, " (n = ", commafy(length(groups[[g]])), ")"),
      !!!setNames(as.list(rep("", ncol(b) - 1)), names(b)[-1])),
    b
  )
}))
cross_italic <- cross_body$Effectiveness %in% paste0(names(groups), " (n = ", commafy(lengths(groups)), ")")
derived_text <- function(q) {
  x <- filter(derived, quantity == q)
  paste0(x$group, " ", x$cell, collapse = "; ")
}
cross_foot <- paste0(
  "Medications (rows of the longitudinal frame: participant x named antidepressant taken). Columns: stopped ",
  "because of side effects. Cells: n (row %); the Total row gives the column distribution. Unknown: ",
  "effectiveness \"don't know\", not answered or not shown; stop status unknown because the number of ",
  "side effects is unknown or the stop item was seen and not answered (a medication with no side ",
  "effects counts as not stopped). Of medications reported as working moderately or very well and with ",
  "a known stop status, stopped because of side effects: ", derived_text("effective_stopped"),
  ". Of medications reported as not working at all and with a known stop status, not stopped: ",
  derived_text("not_effective_not_stopped"), ". 95% CI by a participant cluster bootstrap (",
  commafy(N_CLUSTER_BOOT), " resamples of participants, percentile interval), which respects the ",
  "clustering of medications within participants."
)
writeLines(
  html_table(cross_header, cross_body, cross_foot, italic_rows = cross_italic),
  file.path(out_dir, "table_cross_effectiveness_stopping.html")
)
message("  wrote Table 4 (table_cross_effectiveness_stopping.html, ", nrow(cross_body), " rows)")

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

# The model frame for an outcome x set: the adjusted fit where it can be
# recovered, else the unadjusted one; NULL with the reason otherwise.
locate_frame <- function(outcome, terms) {
  for (obj in c("glmm_models_cov", "glmm_models")) {
    if (!exists(obj, envir = globalenv())) next
    s <- find_set_by_terms(get(obj, envir = globalenv())[[outcome]], terms)
    fr <- cached_fit_frame(s)
    if (!is.null(fr)) {
      return(list(frame = fr, set = s, source = obj,
        adjusted = obj == "glmm_models_cov"))
    }
  }
  NULL
}

# Scenarios for each set, as functions of the model's complete-case data.
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
    # over participants (an observed rating, type 1). The model codes it 1-6
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
SET_LABELS <- c(
  stopped = "Stopped because of side effects (within person)",
  n_se = "Mean number of side effects (between persons; within-person term at 0)",
  se_rating = "Side-effect severity rating (person level)"
)
FOCAL_TERM <- c(stopped = "stopped_due_to_se_within", n_se = "n_se_between", se_rating = "se_rating")
# The labels the cached tables carry for the focal terms, as src/02.regression.r
# builds them ("... (within person)", "... (between persons)").
focal_labels <- setNames(c(labels, within_between_labels(WITHIN_BETWEEN_VARS, labels))[FOCAL_TERM], names(FOCAL_TERM))

pp_rows <- list()
pp_unavailable <- character()
for (outcome in names(pp_outcomes)) {
  for (set_name in names(SE_SETS)) {
    loc <- locate_frame(outcome, SE_SETS[[set_name]])
    if (is.null(loc)) {
      pp_unavailable <- c(pp_unavailable, setNames(paste0(outcome, ": ", SET_LABELS[[set_name]]),
        paste0("pp_", cohort_key(outcome), "_", set_name)))
      message("!! ", outcome, " / ", set_name, ": no recoverable fitted model in the cache; skipped")
      next
    }
    fr <- loc$frame
    m <- fr$m
    if (!(inherits(m, "glmerMod") && stats::family(m)$family == "binomial")) {
      stop("Only binomial glmer fits are supported here; ", outcome, " / ", set_name, " is ", class(m)[1])
    }
    sc <- scenarios_for(set_name, fr$data)
    draws <- draw_fixef(lme4::fixef(m), as.matrix(stats::vcov(m)), N_PARAM_BOOT)
    pr <- predict_scenarios(fr, sc$fns, draws)

    # Check: the contrast has the sign of the focal odds ratio in the cached
    # table (the adjusted column of Table 3d for remission).
    beta <- lme4::fixef(m)[[FOCAL_TERM[[set_name]]]]
    or_row <- loc$set[loc$set$Parameter == unname(focal_labels[[set_name]]), ]
    if (nrow(or_row) != 1) stop("No row ", focal_labels[[set_name]], " in the cached ", outcome, " set")
    diff_est <- pr[[2]]$est - pr[[1]]$est
    if (sign(diff_est) != sign(beta) || sign(log(or_row$Coefficient)) != sign(beta)) {
      stop("Predicted contrast for ", outcome, " / ", set_name, " (", signif(diff_est, 3),
        ") does not follow the sign of its odds ratio")
    }
    message("  check: ", outcome, " / ", set_name, " contrast ", fmt_pct(diff_est), " pp, OR ",
      or_row$Coeff_CI, " (same direction)")

    n_txt <- paste0(commafy(attr(loc$set, "n")[1]), " / ", commafy(attr(loc$set, "n_id")))
    n_long <- paste0(commafy(attr(loc$set, "n")[1]), " observations from ", commafy(attr(loc$set, "n_id")), " participants")
    for (j in seq_along(pr)) {
      pp_rows[[length(pp_rows) + 1]] <- tibble(
        outcome = outcome, set = set_name, value = sc$values[j], key = sc$keys[j],
        est = pr[[j]]$est,
        prob = ci_text(pr[[j]]$est, pr[[j]]$boot),
        diff = if (j == 1) "Reference" else ci_text(pr[[j]]$est - pr[[1]]$est, pr[[j]]$boot - pr[[1]]$boot),
        or = or_row$Coeff_CI,
        n = n_txt, n_long = n_long,
        source = loc$source,
        q = if (!is.null(sc$q)) sc$q[j] else NA_real_
      )
    }
    rm(draws, pr)
    invisible(gc())
  }
}
pp <- bind_rows(pp_rows)

for (r in seq_len(nrow(pp))) {
  x <- pp[r, ]
  add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$key),
    sub("^(\\S+) ", "\\1% ", x$prob),
    paste0("Predicted probability (%, 95% CI) of ", tolower(x$outcome), " for a medication, ",
      SET_LABELS[[x$set]], ": ", x$value, "; ", if (x$source == "glmm_models_cov") "adjusted" else "unadjusted",
      " model (", x$n_long, "), averaged over the model's rows with the random intercept at 0; ",
      "parametric bootstrap CI (", commafy(N_PARAM_BOOT), " draws)"))
  if (x$diff != "Reference") {
    add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$set, "_difference"),
      sub("^(\\S+) ", "\\1 pp ", x$diff),
      paste0("Difference in predicted probability of ", tolower(x$outcome), ", ", SET_LABELS[[x$set]],
        ", second minus first scenario; 95% CI from the same parametric bootstrap draws"))
  }
  if (!is.na(x$q)) {
    add("Predicted probabilities (Table 5)", paste0("pp_", cohort_key(x$outcome), "_", x$key, "_value"),
      formatC(x$q, format = "f", digits = 2),
      paste0("Value of the ", SET_LABELS[[x$set]], " at that percentile over the participants in the model"))
  }
}
for (k in names(pp_unavailable)) {
  add("Predicted probabilities (Table 5)", k, "not available from the cache",
    paste0(pp_unavailable[[k]], ": the cached ordinal (clmm) results keep only exponentiated focal estimates and CIs, ",
      "no thresholds, covariate coefficients or covariance; predicted probabilities need a refit"))
}

pp_header <- c("Side-effect measure", "Value", "Predicted probability, % [95% CI]",
  "Difference, percentage points [95% CI]", "Adjusted OR [95% CI]", "N (observations / participants)")
pp_body <- list()
pp_italic <- logical()
for (outcome in unique(pp$outcome)) {
  pp_body[[length(pp_body) + 1]] <- tibble(a = paste0(outcome, ": probability of ", tolower(outcome), " on the medication"), b = "", c = "", d = "", e = "", f = "")
  pp_italic <- c(pp_italic, TRUE)
  for (s in unique(pp$set[pp$outcome == outcome])) {
    x <- filter(pp, outcome == !!outcome, set == s)
    pp_body[[length(pp_body) + 1]] <- tibble(
      a = c(SET_LABELS[[s]], rep("", nrow(x) - 1)), b = x$value, c = x$prob, d = x$diff,
      e = c(x$or[1], rep("", nrow(x) - 1)), f = c(x$n[1], rep("", nrow(x) - 1))
    )
    pp_italic <- c(pp_italic, rep(FALSE, nrow(x)))
  }
}
pp_body <- bind_rows(pp_body)
names(pp_body) <- pp_header

any_unadjusted <- any(pp$source != "glmm_models_cov")
pp_foot <- paste0(
  "Predicted probability of the outcome for a medication, from the ",
  if (any_unadjusted) "longitudinal models (adjusted where marked, otherwise unadjusted)" else
    "adjusted longitudinal models (sex, starting age, cumulative medication count and PHQ-9)",
  ", each side-effect measure in its own model, with the participant random intercept. ",
  "Predictions are averaged over the observed covariate values of the model's own medication observations ",
  "with the random intercept set to 0, i.e. for a typical participant (conditional, not marginal over ",
  "the random effect). Stopped vs not stopped: the same medication with the participant's own mean ",
  "(between-person term) kept at its observed value, i.e. the within-person contrast. Mean number of side ",
  "effects: the participant's mean at its 25th and 75th percentile over the participants in the model, ",
  "the within-person term at 0. Severity rating (0-5): 25th and 75th percentile over participants. ",
  "95% CI: parametric bootstrap of the fixed effects (", commafy(N_PARAM_BOOT), " multivariate normal ",
  "draws from their estimated covariance; percentile interval); no model was refitted. OR: the adjusted ",
  "estimate of Table 3d.",
  if (length(pp_unavailable)) paste0(
    " Not shown: effectiveness (Table 3a). The cached ordinal effectiveness models keep only the focal odds ",
    "ratios and their CIs, without thresholds, covariate coefficients or covariance, so predicted ",
    "probabilities of each effectiveness category need a refit of those three models."
  ) else ""
)
writeLines(html_table(pp_header, pp_body, pp_foot, italic_rows = pp_italic),
  file.path(out_dir, "table_predicted_probabilities.html"))
message("  wrote Table 5 (table_predicted_probabilities.html, ", nrow(pp_body), " rows)")

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
  paste0("Generated ", format(Sys.Date()), " by `src/06.clinical_quantities.r` from `dat_long` and the cached"),
  "models (no refit). Table numbers are provisional.",
  "",
  "#### Table 4: Reported effectiveness by stopping because of side effects, per medication",
  paste0("`manuscript-tables/table_cross_effectiveness_stopping.html`. ", cross_foot),
  md_table(cross_header, cross_body),
  "",
  "#### Table 5: Predicted probabilities of reported effectiveness and remission by side-effect measures",
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
  "cached longitudinal models (no refit). A sidecar to `manuscript-numbers-current.md`",
  "(`src/05.manuscript_numbers.r`); keys ending in a cohort name or `overall` give that group.",
  ""
)
for (s in unique(numbers$section)) {
  rows <- filter(numbers, section == s)
  nmd <- c(nmd, paste0("## ", s), "", md_table(c("Key", "Value", "Definition"), select(rows, key, value, definition)), "")
}
writeLines(nmd, numbers_path)
message("Wrote ", numbers_path, " (", nrow(numbers), " numbers)")

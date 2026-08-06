# Results tables for the manuscript.
#
# The eight Results tables were previously assembled by hand from the workbooks,
# which is why nothing recorded how the bracket convention or the N footnotes
# were derived. This script regenerates all of them from the cached model
# objects, so the tables in the document can be reproduced and re-checked.
#
# Convention, matching the existing manuscript:
#   - one row per parameter significant after Bonferroni correction in the
#     unadjusted model, the adjusted model, or both
#   - adjusted estimates in parentheses after the unadjusted ones
#   - a row significant only after adjustment appears in parentheses alone
#
# Output: markdown to docs/manuscript-tables-current.md, and one ready-to-paste
# HTML file per table in docs/manuscript-tables/.

require(ProjectTemplate)
require(tidyverse)
reload.project()

out_dir <- "docs/manuscript-tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Both workbook families correct by the number of outcomes they contain, which
# is four in each case. Passing the whole family keeps the tables and the
# figures on the same Bonferroni factor.
glmm_unadj <- multi_adjust(glmm_models)
glmm_adj <- multi_adjust(glmm_models_cov)
legacy <- c(sef_models, eff_models)
legacy_unadj <- multi_adjust(legacy)
legacy_adj <- multi_adjust(c(sef_models_compete, eff_models_compete), n_deps = length(legacy))

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

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
    ifelse(p < 0.001, "<0.001", formatC(round(pmin(p, 1), 3), format = "f", digits = 3))
  )
}

clean_label <- function(x) {
  x <- gsub("v\\.s\\.", "vs.", x)
  x <- gsub("v\\.s\\b", "vs.", x)
  gsub("\\s+", " ", x)
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

commafy <- function(x) formatC(x, format = "d", big.mark = ",")

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
  list(id = "2d", outcome = "Remission", family = "glmm", effect = "OR",
       title = "Variables significantly associated with occurrence of remission"),
  list(id = "2e", outcome = "First improvement duration", family = "legacy", effect = "OR",
       title = "Variables significantly associated with first improvement duration")
)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# The manuscript's existing tables carry formatting that a plain HTML paste does
# not reproduce: Arial 8pt (w:sz 16) in #1f1f1f rather than the 11pt black a
# paste defaults to, a bold header row shaded #efefef, 0.5pt #1f1f1f cell
# borders and 6x9px cell padding. These were read out of the DOCX export of the
# manuscript (w:rPr / w:tcPr on the first table) so that a regenerated table
# drops into the document looking like the ones already there.
CELL_BORDER <- "border:0.5pt solid #1f1f1f;padding:6px 9px;vertical-align:top;"
CELL_FONT <- "font-family:Arial,sans-serif;font-size:8pt;color:#1f1f1f;"
HEADER_FILL <- "background-color:#efefef;"

td_cell <- function(text, head = FALSE, italic = FALSE, span = 1) {
  paste0(
    '<td style="', CELL_BORDER, if (head) HEADER_FILL else "", '"',
    if (span > 1) paste0(' colspan="', span, '"') else "", ">",
    '<p style="margin:0;line-height:1.15;"><span style="', CELL_FONT,
    if (head) "font-weight:700;" else "",
    if (italic) "font-style:italic;" else "", '">',
    esc(text), "</span></p></td>"
  )
}

td_row <- function(cells, head = FALSE) {
  paste0("<tr>", paste0(vapply(cells, td_cell, character(1), head = head), collapse = ""), "</tr>")
}

md <- c(
  "# Results tables — current numbers",
  "",
  paste0("Generated ", format(Sys.Date()), " by `src/04.manuscript_tables.r` from the"),
  "cached model objects, under the **combination** decision: longitudinal (GLMM)",
  "estimates for the four outcomes the GLMM covers, per-participant (legacy)",
  "estimates for the four it does not.",
  "",
  "Adjusted estimates appear in parentheses where the adjusted model is also",
  "significant. Rows shown only in parentheses are significant after adjustment",
  "but not before. The longitudinal models are adjusted for sex, starting age,",
  "cumulative medication count and PHQ-9; the per-participant models for sex,",
  "average starting age and number of relatives with a psychiatric disorder.",
  ""
)

for (spec in TABLES) {
  tab <- if (spec$family == "glmm") {
    build_table(glmm_unadj, glmm_adj, spec$outcome)
  } else {
    build_table(legacy_unadj, legacy_adj, spec$outcome)
  }

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

  header <- paste0(spec$effect, " [95% CI] (adjusted)")

  md <- c(
    md, "",
    paste0("#### Table ", spec$id, ": ", spec$title),
    foot,
    paste0("| Parameter | ", header, " | p (adjusted) |"),
    "|:--|:--|:--|",
    paste0("| ", tab$Parameter, " | ", tab$estimate, " | ", tab$pval, " |")
  )

  html <- paste0(
    '<meta charset="utf-8"><table style="border-collapse:collapse;">',
    td_row(c("Parameter", header, "p (adjusted)"), head = TRUE),
    paste0(
      mapply(function(a, b, c) td_row(c(a, b, c)),
        tab$Parameter, tab$estimate, tab$pval
      ),
      collapse = ""
    ),
    "<tr>", td_cell(foot, span = 3, italic = TRUE), "</tr></table>"
  )
  writeLines(html, file.path(out_dir, paste0("table_", spec$id, ".html")))
  message("  wrote table ", spec$id, " (", nrow(tab), " rows)")
}

writeLines(md, "docs/manuscript-tables-current.md")
message("Wrote docs/manuscript-tables-current.md and ", length(TABLES), " HTML tables")

# Formatting helpers for the manuscript tables and numbers
# (src/04.manuscript_tables.r, src/05.manuscript_numbers.r).

# Always returns character. `ifelse()` on a zero-length or all-logical-NA input
# returns a logical vector, which then makes case_when() below fail with an
# incompatible-type error rather than producing an empty table.
#
# Bonferroni multiplication caps the adjusted p values at 1, and printing that
# cap as "1.000" reads as a three-decimal measurement rather than as "not close
# to significant". Anything that rounds to 1.000 is reported as ">0.999"
# instead, the mirror of the "<0.001" used at the other end; the statement holds
# for a capped value too, since 1 > 0.999.
fmt_p <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  if (!any(ok)) {
    return(out)
  }
  pv <- pmin(p[ok], 1)
  out[ok] <- dplyr::case_when(
    pv < 0.001 ~ "<0.001",
    round(pv, 3) >= 1 ~ ">0.999",
    TRUE ~ formatC(round(pv, 3), format = "f", digits = 3)
  )
  out
}

clean_label <- function(x) {
  x <- gsub("v\\.s\\.", "vs.", x)
  x <- gsub("v\\.s\\b", "vs.", x)
  gsub("\\s+", " ", x)
}

commafy <- function(x) formatC(x, format = "d", big.mark = ",")

# "12 (3.4%)": a count and its share of `n`.
fmt_n_pct <- function(k, n, digits = 1) {
  pct <- ifelse(n > 0, 100 * k / n, NA_real_)
  paste0(commafy(k), " (", formatC(pct, format = "f", digits = digits), "%)")
}

# "12.3 (4.5)": mean and SD of the non-missing values.
fmt_mean_sd <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return("\u2013")
  }
  s <- if (length(x) > 1) stats::sd(x) else NA_real_
  paste0(
    formatC(mean(x), format = "f", digits = digits), " (",
    formatC(s, format = "f", digits = digits), ")"
  )
}

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

td_row <- function(cells, head = FALSE, italic = FALSE) {
  paste0(
    "<tr>",
    paste0(vapply(cells, td_cell, character(1), head = head, italic = italic), collapse = ""),
    "</tr>"
  )
}

# One ready-to-paste HTML table: a bold header row, one row per row of `body`
# (a data frame of character columns), and the footnote in a full-width italic
# cell. `italic_rows` marks body rows to set in italics (group headings).
html_table <- function(header, body, foot, italic_rows = rep(FALSE, nrow(body))) {
  rows <- vapply(seq_len(nrow(body)), function(i) {
    td_row(as.character(unlist(body[i, ], use.names = FALSE)), italic = italic_rows[i])
  }, character(1))
  paste0(
    '<meta charset="utf-8"><table style="border-collapse:collapse;">',
    td_row(header, head = TRUE),
    paste0(rows, collapse = ""),
    "<tr>", td_cell(foot, span = length(header), italic = TRUE), "</tr></table>"
  )
}

# The same table as markdown lines, for the *-current.md review files.
md_table <- function(header, body) {
  cell <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  rows <- if (nrow(body)) {
    apply(as.matrix(body), 1, function(r) paste(cell(r), collapse = " | "))
  } else {
    character()
  }
  c(
    paste0("| ", paste(cell(header), collapse = " | "), " |"),
    paste0("|", paste(rep(":--", length(header)), collapse = "|"), "|"),
    if (length(rows)) paste0("| ", rows, " |")
  )
}

# TRUE for each named object that exists in the global environment, where
# reload.project() puts the cached model objects, and, for a model family,
# was assembled by src/02.regression.r from the current analysis frame
# (model_object_status(), lib/14.model_cache.r). A family cached before the
# stamps existed, or fitted on other data, counts as absent and is reported.
objects_present <- function(names) {
  status <- model_object_status(names)
  report_unusable_models(status)
  status == "current"
}

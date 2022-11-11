require(openxlsx)
write_xlsx_tab <- function(models, ...) {
  # Write models to xlsx file
  wb <- createWorkbook()
  header_sty <- createStyle(textDecoration = "Bold")
  iwalk(models, function(models_indeps, dep) {
    addWorksheet(wb, sheetName = dep)
    setColWidths(wb, sheet = dep, cols = 1, widths = 90)
    setColWidths(wb, sheet = dep, cols = 2:5, widths = 30)
    start_row <- 1
    for (model in models_indeps) {
      # Find positions of significant p-values
      sig_pos <- which(model < 0.05, arr.ind = T)
      # Style for significant p-values
      sig_sty <- createStyle(textDecoration = c("bold", "italic"))
      addStyle(wb,
        sheet = dep, style = sig_sty,
        # Add start_row for tables other than the first
        rows = sig_pos[, 1] + start_row,
        cols = sig_pos[, 2]
      )

      sample_size <- attr(model, "n")

      # Format p-value columns
      pcols_i <- grep("^p", colnames(model))
      model[pcols_i] <- modify(model[pcols_i], ~ scales::pvalue(.x))

      if (start_row == 1) {
        pcolnms_fmt <- paste0(
          colnames(model)[pcols_i],
          "(N = ", sample_size, ")"
        )
        colnames(model)[pcols_i] <- pcolnms_fmt
      } else {
        # Only write the column names of the first table
        # But add sample size
        colnames(model) <- rep("", times = ncol(model))
        colnames(model)[pcols_i] <- paste0("(N = ", sample_size, ")")
      }

      writeData(wb,
        sheet = dep, model,
        startRow = start_row, headerStyle = header_sty
      )
      # Count the header row
      nrows <- nrow(model) + 1
      start_row <- start_row + nrows
    }

    saveWorkbook(wb = wb, ...)
  })

  invisible(wb)
}

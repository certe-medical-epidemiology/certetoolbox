# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Create Excel Workbook Object
#' 
#' The [as_excel()] function relies on the `openxlsx` package for creating an Excel Workbook object in R. These objects can be saved using [save_excel()] or [export_xlsx()].
#' @param ... data sets, use named items for multiple tabs (see *Examples*)
#' @param sheet_names sheet names
#' @param autofilter create autofilter on columns in first row
#' @param autowidth automatically adjust columns widths
#' @param rows_zebra create banded rows
#' @param cols_zebra create banded columns
#' @param freeze_top_row freeze the first row of the sheet
#' @rdname as_excel
#' @importFrom openxlsx createStyle createWorkbook modifyBaseFont addWorksheet writeDataTable addStyle freezePane setColWidths
#' @importFrom tibble rownames_to_column
#' @export
#' @examples
#' # creates a Workbook object
#' xl <- as_excel("this is a sheet" = mtcars,
#'                "another sheet" = anscombe)
#' xl
#' 
#' # save it with save_excel() or export_xlsx()
#' 
#' library(openxlsx)
#' # add a new empty sheet
#' addWorksheet(wb = xl,
#'              sheetName = "new sheet")
#'              
#' some_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
#' addStyle(wb = xl,
#'          sheet = 3,
#'          style = some_style,
#'          rows = 2:6,
#'          cols = 1:6,
#'          gridExpand = TRUE)
as_excel <- function(...,
                     sheet_names = NULL,
                     autofilter = TRUE,
                     autowidth = TRUE,
                     rows_zebra = TRUE,
                     cols_zebra = FALSE,
                     freeze_top_row = TRUE) {
  dots <- list(...)
  if (!is.null(sheet_names)) {
    names(dots) <- sheet_names
  }
  if (is.null(names(dots))) {
    names(dots) <- paste0("Blad", seq_len(length(dots)))
  }
  name_missing <- which(names(dots) == "")
  names(dots)[names(dots) == ""] <- paste0("Blad", name_missing)
  
  options("openxlsx.dateFormat" = "dd-mm-yyyy")
  options("openxlsx.datetimeFormat" = "dd-mm-yyyy hh:MM:ss")
  
  style <- createStyle(fontName = "Calibri",
                       fontSize = 11,
                       halign = "center",
                       valign = "center",
                       wrapText = TRUE)
  wb <- createWorkbook(creator = suppressWarnings(read_secret("department.name")))
  modifyBaseFont(wb, fontSize = 11, fontColour = "black", fontName = "Calibri")
  
  for (i in seq_len(length(dots))) {
    df <- dots[[i]]
    # support row names
    if (!identical(rownames(df), as.character(seq_len(NROW(df))))) {
      df <- rownames_to_column(df)
      warning("Row names added as first column 'rownames'", call. = FALSE)
    }
    col_widths <- double(NCOL(df))
    # no invalid characters in text like clinical properties, then saving won't work
    for (col in seq_len(ncol(df))) {
      if (is.character(df[, col, drop = TRUE])) {
        # \\s = whitespace character such as \n, \t, \r\n, etc.
        df[, col] <- gsub("\\s+", " ", df[, col, drop = TRUE], perl = TRUE)
        # remove trema's and accents
        df[, col] <- iconv(df[, col, drop = TRUE], from = "UTF-8", to = "ASCII//TRANSLIT")
      }
      col_widths[col] <- min(255,
                             max(c(8, nchar(c(colnames(df)[col],
                                              as.character(df[, col, drop = TRUE]))) * 1.2)))
    }
    addWorksheet(wb = wb,
                 sheetName = names(dots)[i])
    writeDataTable(wb = wb,
                   x = df,
                   tableName = paste0("tabel_", i),
                   sheet = i,
                   withFilter = isTRUE(autofilter),
                   bandedRows = isTRUE(rows_zebra),
                   bandedCols = isTRUE(cols_zebra),
                   tableStyle = "TableStyleMedium2",
                   headerStyle = style)
    addStyle(wb = wb,
             sheet = i,
             style = style,
             rows = seq_len(NROW(df) + 1), # voor kopteksten
             cols = seq_len(NCOL(df)),
             gridExpand = TRUE,
             stack = TRUE)
    if (isTRUE(freeze_top_row)) {
      freezePane(wb = wb,
                 sheet = i,
                 firstRow = TRUE)
    }
    if (isTRUE(autowidth)) {
      if (isFALSE(autofilter)) {
        col_widths <- "auto"
      }
      setColWidths(wb = wb,
                   sheet = i,
                   cols = seq_len(NCOL(df)),
                   widths = col_widths)
    }
  }
  
  wb
}

#' @rdname as_excel
#' @param xl Excel object, as created with [as_excel()] (or manually with the `openxlsx` package)
#' @param filename file location to save Excel document to, defaults to a random filename in the current folder
#' @param overwrite overwrite existing file
#' @importFrom openxlsx saveWorkbook
#' @export
save_excel <- function(xl, filename = NULL, overwrite = FALSE) {
  if (!inherits(xl, "Workbook")) {
    stop("This function can only accept class Workbook from the 'openxlsx' package.",
         "\nDid you mean export_xlsx() to export a data set to an Excel file?", call. = FALSE)
  }
  if (is.null(filename)) {
    filename <- paste0("excel_", generate_identifier(8), ".xlsx")
  } else if (filename %unlike% "[.]xlsx?$") {
    filename <- paste0(filename, ".xlsx")
  }
  saveWorkbook(xl, file = filename, overwrite = overwrite)
  if (file.exists(filename)) {
    message(paste0("Workbook object exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
  invisible(xl)
}

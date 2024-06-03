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
#' The [as_excel()] function relies on the `openxlsx2` package for creating an Excel Workbook object in R. These objects can be saved using [save_excel()] or [export_xlsx()].
#' @param ... data sets, use named items for multiple tabs (see *Examples*)
#' @param sheet_names sheet names
#' @param autofilter create autofilter on columns in first row
#' @param autowidth automatically adjust columns widths
#' @param rows_zebra create banded rows
#' @param cols_zebra create banded columns
#' @param freeze_top_row freeze the first row of the sheet
#' @param table_style style(s) for each table, see below. This can also be a vector with the same length as `...`.
#' @param align horizontal alignment of text
#' @param creator name of the creator of the workbook
#' @param department name of the department of the workbook
#' @param project_number project number, to add project ID as the subject of the workbook
#' @section Supported Table Styles:
#' For the argument `table_style`, use one or more of these table styles as character input. The default is **TableStyleMedium2**.
#' 
#' ![table styles](tablestyles.png)
#' @rdname as_excel
#' @importFrom openxlsx2 wb_load wb_set_base_font wb_add_cell_style wb_dims wb_add_worksheet wb_add_data_table wb_freeze_pane wb_set_col_widths wb_remove_worksheet wb_set_properties
#' @importFrom dplyr mutate_if
#' @importFrom tibble rownames_to_column
#' @importFrom certestyle format2
#' @importFrom certeprojects project_get_current_id project_identifier
#' @export
#' @examples
#' # creates a Workbook object
#' xl <- as_excel("this is a sheet" = mtcars,
#'                "another sheet" = anscombe)
#' xl
#' 
#' # then save it with save_excel() or export_xlsx()
as_excel <- function(...,
                     sheet_names = NULL,
                     autofilter = TRUE,
                     autowidth = TRUE,
                     rows_zebra = TRUE,
                     cols_zebra = FALSE,
                     freeze_top_row = TRUE,
                     align = "center",
                     table_style = "TableStyleMedium2",
                     creator = Sys.info()["user"],
                     department = read_secret("department.name"),
                     project_number = project_get_current_id(ask = FALSE)) {
  dots <- list(...)
  if (length(dots) == 1 && is.list(dots[[1]]) && !is.data.frame(dots[[1]])) {
    # was passed on as a list, e.g. from export_xlsx()
    dots <- dots[[1]]
  }
  if (!is.null(sheet_names)) {
    names(dots) <- sheet_names
  }
  if (is.null(names(dots))) {
    names(dots) <- paste0("Blad", seq_len(length(dots)))
  }
  name_missing <- which(names(dots) == "")
  names(dots)[names(dots) == ""] <- paste0("Blad", name_missing)
  
  if (length(table_style) == 1 && length(dots) > 1) {
    table_style <- rep(table_style, length(dots))
  }
  
  wb <- wb_load(file = system.file("exceltemplate", "exceltemplate.xlsx", package = "certetoolbox"))
  
  wb_set_base_font(wb, font_size =  11, font_color = "black", font_name = "Calibri")
  
  style <- wb_add_cell_style(wb,
                             dims = wb_dims(rows = seq_len(NROW(df) + 1), cols = seq_len(NCOL(df))),
                             font_id = "Calibri",
                             horizontal = align,
                             vertical = "center",
                             wrap_text = TRUE)

  for (i in seq_len(length(dots))) {
    df <- dots[[i]]
    # openxlsx2 does not support lists
    df <- df |>
      mutate_if(is.list, function(x) unname(paste0(unlist(x), collapse = ", ")))
    if (!is.data.frame(df)) {
      stop("Object ", i, " (sheet '", names(dots)[i], "') must be a data.frame, not class ", paste0(class(df), collapse = "/"), call. = FALSE)
    }
    # support row names
    if (!identical(rownames(df), as.character(seq_len(NROW(df))))) {
      warning("Row names for object ", i, 
              " (", paste0(format2(dim(df)), collapse = "x"), ", sheet '", names(dots)[i],
              "') added as first column 'rownames'",
              call. = FALSE)
      df <- rownames_to_column(df)
    }
    wb <- wb |> 
      wb_add_worksheet(sheet = names(dots)[i]) |> 
      wb_add_data_table(x = df,
                        table_name = paste0("Tabel", i),
                        sheet = i + 1, # since template sheet is #1
                        with_filter = isTRUE(autofilter),
                        banded_rows = isTRUE(rows_zebra),
                        banded_cols = isTRUE(cols_zebra),
                        table_style = table_style[i]) |> 
      wb_add_cell_style(sheet = i + 1, # since template sheet is #1
                        dims = wb_dims(rows = seq_len(NROW(df) + 1),
                                       cols = seq_len(NCOL(df))),
                        font_id = "Calibri",
                        horizontal = align,
                        vertical = "center",
                        wrap_text = TRUE)
    if (isTRUE(freeze_top_row)) {
      wb <- wb |> 
        wb_freeze_pane(sheet = i + 1, # since template sheet is #1
                       first_row = TRUE)
    }
    if (isTRUE(autowidth)) {
      if (isFALSE(autofilter)) {
        col_widths <- "auto"
      } else {
        col_widths <- double(NCOL(df))
        for (col in seq_len(ncol(df))) {
          # remove invalid characters in text like clinical properties, otherwise saving won't work
          if (is.character(df[, col, drop = TRUE])) {
            # \\s = whitespace character such as \n, \t, \r\n, etc.
            df[, col] <- gsub("\\s+", " ", df[, col, drop = TRUE], perl = TRUE)
            # remove trema's and accents
            df[, col] <- iconv(df[, col, drop = TRUE], from = "UTF-8", to = "ASCII//TRANSLIT")
          }
          col_widths[col] <- min(100,
                                 max(c(8.43, nchar(c(colnames(df)[col],
                                                     as.character(df[, col, drop = TRUE]))) * 1.3),
                                     na.rm = TRUE),
                                 na.rm = TRUE)
        }
      }
      wb <- wb |>
        wb_set_col_widths(sheet = i + 1, # since template sheet is #1
                          cols = seq_len(NCOL(df)),
                          widths = col_widths)
    }
  }
  
  wb <- wb |>
    # remove template worksheet
    wb_remove_worksheet(sheet = "exceltemplate") |> 
    # set Excel properties
    wb_set_properties(creator = creator,
                      company = department,
                      keywords = project_identifier(project_number))
  
  wb
}

#' @rdname as_excel
#' @param xl Excel object, as created with [as_excel()] (or manually with the `openxlsx2` package)
#' @param filename file location to save Excel document to, defaults to a random filename in the current folder
#' @param overwrite overwrite existing file
#' @importFrom openxlsx2 wb_workbook wb_save
#' @export
save_excel <- function(xl, filename = NULL, overwrite = FALSE) {
  if (!inherits(xl, class(wb_workbook()))) {
    stop("This function can only accept class wbWorkbook from the 'openxlsx2' package.",
         "\nDid you mean export_xlsx() to export a data set to an Excel file?", call. = FALSE)
  }
  if (is.null(filename)) {
    filename <- paste0("excel_", generate_identifier(8), ".xlsx")
  } else if (filename %unlike% "[.]xlsx?$") {
    filename <- paste0(filename, ".xlsx")
  }
  wb_save(wb = xl, file = filename, overwrite = overwrite)
  if (file.exists(filename)) {
    message(paste0("Workbook object exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
  invisible(xl)
}

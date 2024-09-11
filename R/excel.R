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
#' @param autofilter create autofilter on columns in first row. This can also be a vector with the same length as `...`.
#' @param autowidth automatically adjust columns widths. This can also be a vector with the same length as `...`.
#' @param rows_zebra create banded rows. This can also be a vector with the same length as `...`.
#' @param cols_zebra create banded columns. This can also be a vector with the same length as `...`.
#' @param freeze_top_row freeze the first row of the sheet. This can also be a vector with the same length as `...`.
#' @param digits number of digits for numeric values (integer values will always be rounded to whole numbers), defaults to `2`
#' @param table_style style(s) for each table, see below. This can also be a vector with the same length as `...`.
#' @param align horizontal alignment of text
#' @param widths width of columns, must be length 1 or `ncol()` of the data set. If set, overrides `autowidth`.
#' @param creator name of the creator of the workbook
#' @param department name of the department of the workbook
#' @param project_number project number, to add project ID as the subject of the workbook
#' @section Supported Table Styles:
#' For the argument `table_style`, use one or more of these table styles as character input. The default is **TableStyleMedium2**.
#' 
#' ![table styles](tablestyles.png)
#' @rdname as_excel
#' @importFrom openxlsx2 create_colors_xml wb_add_cell_style wb_add_data_table wb_add_worksheet wb_dims wb_freeze_pane wb_remove_worksheet wb_set_base_colours wb_set_base_font wb_set_properties wb_workbook wb_add_numfmt wb_set_col_widths
#' @importFrom dplyr mutate_if ungroup select where
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
                     widths = NULL,
                     rows_zebra = TRUE,
                     cols_zebra = FALSE,
                     freeze_top_row = TRUE,
                     digits = 2,
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
  
  
  multiply_if_multiple <- function(arg, reference) {
    if (length(arg) == 1 && length(reference) > 1) {
      arg <- rep(arg, length(reference))
    }
    return(arg)
  }
  autofilter <- multiply_if_multiple(autofilter, dots)
  autowidth <- multiply_if_multiple(autowidth, dots)
  rows_zebra <- multiply_if_multiple(rows_zebra, dots)
  cols_zebra <- multiply_if_multiple(cols_zebra, dots)
  freeze_top_row <- multiply_if_multiple(freeze_top_row, dots)
  table_style <- multiply_if_multiple(table_style, dots)
  
  wb <- wb_workbook(creator = creator,
                    company = department,
                    keywords = project_identifier(project_number))
  
  for (i in seq_len(length(dots))) {
    df <- dots[[i]]
    # openxlsx2 does not support lists
    df <- df |>
      ungroup() |> 
      mutate_if(is.list, function(x) unname(paste0(unlist(x), collapse = ", ")))
    if (!is.data.frame(df)) {
      stop("Object ", i, " (sheet '", names(dots)[i], "') must be a data.frame, not class ", paste0(class(df), collapse = "/"), call. = FALSE)
    }
    # support row names
    if (!identical(rownames(df), as.character(seq_len(NROW(df))))) {
      warning("Row names for object ", i, 
              " (", paste0(format2(dim(df)), collapse = pkg_env$cross_icon), ", sheet '", names(dots)[i],
              "') added as first column 'rownames'",
              call. = FALSE)
      df <- rownames_to_column(df)
    }
    if (nchar(names(dots)[i]) >= 32) {
      # Sheet names can only have 31 characters. Yes, really.
      warning(paste0("The name of sheet ", i, " (",  names(dots)[i], ") is too long and was cut to 30 characters and its sheet number."),
              call. = FALSE,
              immediate. = TRUE)
      names(dots)[i] <- paste0(substr(names(dots)[i], 1, 30), "#", i)
    }
    wb <- wb |> 
      wb_add_worksheet(sheet = names(dots)[i]) |> 
      wb_add_data_table(x = df,
                        table_name = paste0("Tabel", i),
                        sheet = i,
                        with_filter = isTRUE(autofilter[i]),
                        banded_rows = isTRUE(rows_zebra[i]),
                        banded_cols = isTRUE(cols_zebra[i]),
                        table_style = table_style[i],
                        na.strings = "") |> 
      wb_add_cell_style(sheet = i,
                        dims = wb_dims(rows = seq_len(NROW(df) + 1),
                                       cols = seq_len(NCOL(df))),
                        font_id = "Source Sans Pro",
                        horizontal = align,
                        vertical = "center",
                        wrap_text = TRUE)
    
    if (!is.null(digits) && !isFALSE(digits) && !is.infinite(digits)) {
      if (any(vapply(FUN.VALUE = logical(1), df, is.numeric))) {
        # all numbers as numbers with set decimals
        cols_to_update <- vapply(FUN.VALUE = logical(1), df, function(x) is.numeric(x) && !all(x == ceiling(x), na.rm = TRUE))
        if (any(cols_to_update)) {
          wb <- wb |>
            wb_add_numfmt(sheet = i,
                          dims = wb_dims(rows = 2:(NROW(df) + 1),
                                         cols = which(cols_to_update)),
                          numfmt = paste0("0.", strrep("0", times = digits)))
        }
      }
    }
    if (any(vapply(FUN.VALUE = logical(1), df, is.integer))) {
      # all integers as numbers with zero decimals
      cols_to_update <- vapply(FUN.VALUE = logical(1), df, is.integer)
      if (any(cols_to_update)) {
        wb <- wb |> 
          wb_add_numfmt(sheet = i,
                        dims = wb_dims(rows = 2:(NROW(df) + 1),
                                       cols = which(cols_to_update)),
                        numfmt = "0")
      }
    }
    
    if (isTRUE(freeze_top_row[i])) {
      wb <- wb |> 
        wb_freeze_pane(sheet = i,
                       first_row = TRUE)
    }
    
    # data cleaning
    for (col in seq_len(ncol(df))) {
      # remove invalid characters in text like clinical properties, otherwise saving won't work
      if (is.character(df[, col, drop = TRUE])) {
        # \\s = whitespace character such as \n, \t, \r\n, etc.
        df[, col] <- gsub("\\s+", " ", df[, col, drop = TRUE], perl = TRUE)
        # remove tremas and accents
        df[, col] <- iconv(df[, col, drop = TRUE], from = "UTF-8", to = "ASCII//TRANSLIT")
      }
    }
    
    if (isTRUE(autowidth[i]) || !is.null(width)) {
      # wb_set_col_widths() can take `widths = "auto"` as input, but it is not even close to auto-resizing in Excel manually
      if (isTRUE(autowidth[i])) {
        width <- double(NCOL(df))
        for (col in seq_len(ncol(df))) {
          width[col] <- min(100,
                            max(c(8.43,
                                  nchar(c(colnames(df)[col],
                                          as.character(df[, col, drop = TRUE]))) * 1.25),
                                na.rm = TRUE) + 4,
                            na.rm = TRUE)
        }
      }
      wb <- wb |>
        wb_set_col_widths(sheet = i,
                          cols = seq_len(NCOL(df)),
                          widths = width)
    }
  }
  
  wb <- wb |>
    # colour settings
    wb_set_base_colours(xml = create_colors_xml("Certe",
                                                dark = certestyle::colourpicker(c("black", "black", "grey75", "certeblauw")),
                                                accent = certestyle::colourpicker("Certe", 6))) |> 
    # font settings
    wb_set_base_font(font_size =  11,
                     font_color = "black",
                     font_name = "Source Sans Pro")
  
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

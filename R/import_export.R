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

# Helper functions --------------------------------------------------------

#' @importFrom certeprojects project_set_file
parse_file_location <- function(filename, needed_extension, card_number) {
  if (is.null(card_number) || is.na(card_number) || isFALSE(card_number) || card_number %in% c(0, "")) {
    card_number <- NULL
  }
  needed_extension <- gsub("^[.]", "", needed_extension[1L])
  if (filename == ".") {
    filename <- "tbl"
  }
  if (needed_extension != "" & filename %unlike% paste0("[.](", paste0(needed_extension, collapse = "|"), ")$")) {
    filename <- paste0(filename, ".", needed_extension[1L])
  }
  if (!is.null(card_number) && filename %unlike% paste0("p", card_number, "|[A-Z]:/")) {
    # has no valid location yet, so include card number
    filename_proj <- project_set_file(filename, card_number = card_number)
    if (!is.na(filename_proj)) {
      filename <- filename_proj
    }
  }
  # remove invalid characters
  filename <- gsub("[?|<>|*]+", "", filename)
  filename
}

export_exec <- function(object,
                        needed_extension,
                        filename,
                        filename_deparse,
                        card_number,
                        fn = NULL,
                        ...) {
  if (is.null(needed_extension)) {
    needed_extension <- ""
  }
  if (is.null(filename)) {
    filename <- filename_deparse
  }
  needed_extension <- tolower(needed_extension)
  filename <- parse_file_location(filename,
                                  needed_extension = needed_extension,
                                  card_number = card_number)
  needed_extension <- needed_extension[1L]
  if (needed_extension == "") {
    if (filename %unlike% "[.][a-z0-9_-]+$") {
      warning("No (valid) file extension set.", call. = FALSE)
    }
    # custom method
    fn(object, filename, ...)
  } else if (needed_extension == "rds") {
    # R format
    base::saveRDS(object, file = filename, ...)
  } else if (needed_extension == "sav") {
    # SPSS format
    haven::write_sav(object, path = filename, ...)
  } else if (needed_extension == "feather") {
    # Apache's Feather format
    object <- rownames_1st_column(object)
    arrow::write_feather(x = object, sink = filename, ...)
  } else if (needed_extension == "parquet") {
    # Apache's Parquet format
    object <- rownames_1st_column(object)
    arrow::write_parquet(x = object, sink = filename, ...)
  } else if (needed_extension == "xlsx") {
    # Excel format
    if (!inherits(object, "Workbook")) {
      # not yet an openxlsx object (but rather e.g. a data frame)
      object <- suppressMessages(as_excel(object, ...))
    }
    suppressMessages(save_excel(xl = object, filename = filename, overwrite = TRUE))
  } else {
    # flat data file
    object <- rownames_1st_column(object)
    if (needed_extension %in% c("csv", "tsv", "txt")) {
      # arguments such as 'sep' etc. are passed into '...':
      utils::write.table(object, file = filename, ...)
    } else {
      stop("Unknown extension method: ", needed_extension, call. = FALSE)
    }
  }
  if (file.exists(filename)) {
    message(paste0("Data exported to '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
  invisible(object)
}

#' @importFrom readr read_delim locale
#' @importFrom dplyr select
#' @importFrom certeprojects project_get_file
import_exec <- function(filename,
                        filename_deparse,
                        extension,
                        card_number,
                        auto_transform,
                        ...) {
  extension <- extension[1L]
  
  if (!is.character(filename)) {
    filename <- filename_deparse
  }
  
  filename_url <- NULL
  if (filename %like% "^(http|https|ftp|sftp|ftps|ssh)://") {
    # download the file first
    filename_url <- filename
    if (filename_url %like% "git(hub|lab).com/.*/blob/") {
      # get GitHub/GitLab raw URL
      filename_url <- gsub("/blob/", "/raw/", filename_url, fixed = TRUE)
    }
    filename <- tempfile(pattern = "import_", fileext = paste0(".", extension))
    if (.Platform$OS.type == "windows") {
      utils::download.file(url = filename_url, destfile = filename, mode = "wb")
    } else {
      utils::download.file(url = filename_url, destfile = filename)
    }
    if (!file.exists(filename)) {
      stop("Failed to download: ", filename_url, call. = FALSE)
    }
  }
  
  filename <- gsub('\\', '/', filename, fixed = TRUE)
  if (filename %unlike% "[.][a-zA-Z0-9]{1,7}$") {
    # does not have extension yet
    filename <- paste0(filename, ".", gsub("^[.]", "", extension))
  }
  if (!file.exists(filename) && !is.null(card_number)) {
    # try project file using the 'certeprojects' package
    filename <- project_get_file(filename, card_number = card_number)
  }
  
  if (!file.exists(filename)) {
    stop("File not found: ", filename, call. = FALSE)
  }
  
  if (extension == "rds") {
    # R format
    df <- base::readRDS(file = filename)
  } else if (extension %in% c("csv", "tsv", "txt")) {
    # flat files
    df <- read_delim(file = filename,
                     guess_max = 20000,
                     delim = list(...)$sep,
                     na = list(...)$na,
                     progress = interactive(),
                     skip = list(...)$skip,
                     locale = locale(date_names = list(...)$datenames,
                                     date_format = list(...)$dateformat,
                                     time_format = list(...)$timeformat,
                                     tz = list(...)$timezone,
                                     decimal_mark = list(...)$decimal.mark,
                                     grouping_mark = list(...)$big.mark,
                                     encoding = "UTF-8"),
                     show_col_types = FALSE)
    if (isTRUE(auto_transform)) {
      df <- auto_transform(df, decimal.mark = ".", big.mark = "")
      auto_transform <- FALSE
    }
  } else if (extension == "sav") {
    # SPSS format
    df <- haven::as_factor(haven::read_sav(file = filename))
  } else if (extension %like% "xlsx?") {
    # Excel format
    df <- readxl::read_excel(path = filename,
                             guess_max = 200000,
                             sheet = list(...)$sheet,
                             range = list(...)$range,
                             na = list(...)$na,
                             skip = list(...)$skip)
  } else if (extension == "feather") {
    # Apache's Feather format
    df <- arrow::read_feather(file = filename,
                              as_data_frame = TRUE,
                              col_select = list(...)$select)
  } else if (extension == "parquet") {
    # Apache's Parquet format
    df <- arrow::read_parquet(file = filename,
                              as_data_frame = TRUE,
                              col_select = list(...)$select)
  } else {
    # use rio::import which pretty much understands any file type
    check_is_installed("rio")
    df <- rio::import(file = filename, ...)
  }
  
  # force data.frame
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # if row names were saved as first col, set back to row names
  if (colnames(df)[1L] %like% "row.?names?") {
    rownames(df) <- df[, 1, drop = TRUE]
    df <- select(df, -1)
    message("Row names restored from first column.")
  }
  
  if (isTRUE(auto_transform)) {
    df <- auto_transform(df, ...)
  }
  
  if (interactive()) {
    if (is.null(filename_url)) {
      file_src <- tools::file_path_as_absolute(filename)
    } else {
      file_src <- filename_url
      # try to remove downloaded file
      try(unlink(filename), silent = TRUE)
    }
    message(
      paste0(
        "Imported data set (", format2(NROW(df)), "x", format2(NCOL(df)),
        ", ", size_humanreadable(utils::object.size(df)),  ") from '",
        file_src, "'"
      )
    )
  }
  
  df
}


# Export functions --------------------------------------------------------

#' Export Data Sets and Plots
#' 
#' These functions can be used to export data sets and plots. They invisibly return the object itself again, allowing for usage in pipes (except for the plot-exporting functions [export_pdf()], [export_png()] and [export_html()]). The functions work closely together with the `certeprojects` package to support Trello card numbers.
#' @param object,plot the \R object to export
#' @param fn a manual export function, such as `haven::write_sas` to write SAS files. This function has to have the object as first argument and the future file location as second argument.
#' @param filename the full path of the exported file
#' @param card_number a Trello card number
#' @param ... arguments passed on to methods
#' @details The [export()] function can export to any file format, also with a manually set export function when passed on to the `fn` argument. This function `fn` has to have the object as first argument and the future file location as second argument. If `fn` is left blank, the `export_*` function will be used based on the filename.
#' @rdname export
#' @seealso [import()]
#' @export
#' @examples 
#' \dontrun{
#' 
#' library(dplyr)
#' 
#' # export two files: 'whole_file.rds' and 'first_ten_rows.xlsx'
#' starwars |>
#'   export_rds("whole_file") |>
#'   slice(1:10) |>
#'   export_xlsx("first_ten_rows")
#'   
#' # the above is equal to:
#' starwars |>
#'   export("whole_file.rds") |>
#'   slice(1:10) |>
#'   export("first_ten_rows.xlsx")
#' }
export <- function(object,
                   filename = NULL,
                   card_number = project_get_current_id(ask = FALSE),
                   fn = NULL,
                   ...) {
  
  if (!is.null(fn)) {
    # export using manual function
    if (is.character(fn)) {
      fn <- tryCatch(eval(parse(text = fn)), error = function(e) NA_character_)
    }
    if (!is.function(fn)) {
      stop("`fn` must be a function")
    }
    export_exec(object = object,
                needed_extension = NULL,
                filename = filename,
                card_number = card_number,
                fn = fn,
                ...)
  } else {
    if (is.null(filename)) {
      filename <- deparse(substitute(filename))
    }
    if (filename %like% "[.]rds$") {
      export_rds(object = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]csv$") {
      export_csv(object = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]tsv$") {
      export_tsv(object = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]txt$") {
      export_txt(object = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]xlsx?$") {
      export_xlsx(object = object,
                  filename = filename,
                  card_number = card_number,
                  ...)
    } else if (filename %like% "[.]sav$") {
      export_sav(object = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]feather$") {
      export_feather(object = object,
                     filename = filename,
                     card_number = card_number,
                     ...)
    } else if (filename %like% "[.]parquet$") {
      export_parquet(object = object,
                     filename = filename,
                     card_number = card_number,
                     ...)
    } else if (filename %like% "[.]pdf$") {
      export_pdf(plot = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]png$") {
      export_png(plot = object,
                 filename = filename,
                 card_number = card_number,
                 ...)
    } else if (filename %like% "[.]html$") {
      export_html(plot = object,
                  filename = filename,
                  card_number = card_number,
                  ...)
    } else {
      stop("Unknown file format for export: ", filename, call. = FALSE)
    }
  }
}

#' @rdname export
#' @details RDS files as created using [export_rds()] are compatible with R3 and R4.
#' @export
export_rds <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       ...) {
  export_exec(object, "rds",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              compress = "gzip",
              ascii = FALSE,
              version = 2)
}

#' @rdname export
#' @inheritParams as_excel
#' @details The [export_xlsx()] and [export_excel()] functions use [`save_excel(as_excel(...))`][as_excel()] internally. **IMPORTANT**: these two functions can accept more than one [data.frame]. When naming the data sets, the names will become sheet names in the resulting Excel file. For a complete visual overview of supported table styles, see [as_excel()]. If the last value in `...` is a [character] of length 1 and `filename` is `NULL`, this value is assumed to be the filename.
#' @export
export_xlsx <- function(...,
                        filename = NULL,
                        card_number = project_get_current_id(ask = FALSE),
                        sheet_names = NULL,
                        autofilter = TRUE,
                        rows_zebra = TRUE,
                        cols_zebra = FALSE,
                        freeze_top_row = TRUE,
                        table_style = "TableStyleMedium2") {
  object <- list(...)
  if (length(object) > 1 && is.null(filename) &&
      is.character(object[[length(object)]]) && length(object[[length(object)]]) == 1) {
    # unnamed second argument is the filename, like other export functions
    filename <- object[[length(object)]]
    object <- object[seq_len(length(object) - 1)]
  }
  export_exec(object = object, "xlsx",
              filename = filename,
              filename_deparse = ".",
              card_number = card_number,
              sheet_names = sheet_names,
              autofilter = autofilter,
              rows_zebra = rows_zebra,
              cols_zebra = cols_zebra,
              freeze_top_row = freeze_top_row,
              table_style = table_style)
}

#' @rdname export
#' @export
export_excel <- export_xlsx

#' @rdname export
#' @param na replacement character for empty values (default: `""`)
#' @details For [export_csv()], [export_csv2()] and [export_tsv()], files will be saved in UTF-8 encoding and `NA` values will be exported as `""` at default. Like other `*.csv` and `*.csv2` functions, csv is comma (`,`) separated and csv2 is semicolon (`;`) separated.
#' @export
export_csv <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       na = "",
                       ...) {
  export_exec(object, "csv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              append = FALSE,
              quote = TRUE,
              sep = ",",
              eol = "\n",
              na = na,
              dec = ".",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @export
export_csv2 <- function(object,
                        filename = NULL,
                        card_number = project_get_current_id(ask = FALSE),
                        na = "",
                        ...) {
  export_exec(object, "csv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              append = FALSE,
              quote = TRUE,
              sep = ";",
              eol = "\n",
              na = na,
              dec = ",",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @export
export_tsv <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       na = "",
                       ...) {
  export_exec(object, "tsv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              append = FALSE,
              quote = TRUE,
              sep = "\t",
              eol = "\n",
              na = na,
              dec = ".",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @param sep separator for values in a row (default: tab)
#' @details The [export_txt()] function exports to a tab-separated file.
#' @export
export_txt <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       sep = "\t",
                       na = "",
                       ...) {
  export_exec(object, "txt",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              append = FALSE,
              quote = TRUE,
              sep = sep,
              eol = "\n",
              na = na,
              dec = ".",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @details `r doc_requirement("an SPSS file", c("export_sav", "export_spss"), "haven")`.
#' @export
export_sav <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       ...) {
  check_is_installed("haven")
  export_exec(object, "sav",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              compress = FALSE)
}

#' @rdname export
#' @export
export_spss <- export_sav 


#' @rdname export
#' @details `r doc_requirement("a Feather file", "export_feather", "arrow")`. Feather provides efficient binary columnar serialization for data sets, enabling easy sharing data across data analysis languages.
#' @export
export_feather <- function(object,
                           filename = NULL,
                           card_number = project_get_current_id(ask = FALSE),
                           ...) {
  check_is_installed("arrow")
  export_exec(object, "feather",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              ...)
}

#' @rdname export
#' @details `r doc_requirement("a Parquet file", "export_parquet", "arrow")`. 'Parquet' is a columnar storage file format.
#' @export
export_parquet <- function(object,
                           filename = NULL,
                           card_number = project_get_current_id(ask = FALSE),
                           ...) {
  check_is_installed("arrow")
  export_exec(object, "parquet",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              card_number = card_number,
              ...)
}

#' @rdname export
#' @param size paper size, defaults to A5. Can be A0 to A7.
#' @param portrait portrait mode, defaults to `FALSE` (i.e., landscape mode)
#' @details `r doc_requirement("a PDF file", "export_pdf", "ggplot2")`. If the filename is left blank in [export_pdf()], [export_png()] or [export_html()], the title of `plot` will be used if it's available and the `certeplot2` package is installed, and a timestamp otherwise. **NOTE:** All export functions invisibly return `object` again, but the plotting functions invisibly return the file path
#' @importFrom certestyle format2
#' @export
export_pdf <- function(plot,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       size = "A5",
                       portrait = FALSE,
                       ...) {
  check_is_installed("ggplot2")
  if ("certeplot2" %in% rownames(utils::installed.packages())) {
    get_plot_title <- certeplot2::get_plot_title
  } else {
    get_plot_title <- NULL
  }
  
  if (is.null(filename) && !is.null(get_plot_title)) {
    filename <- get_plot_title(plot)
    if (is.na(filename)) {
      filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
    }
    filename <- paste0(filename, ".pdf")
  } else if (is.null(filename)) {
    filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
  }
  filename <- parse_file_location(filename,
                                  needed_extension = "pdf",
                                  card_number = card_number)
  
  a0_height <- sqrt(sqrt(2)) * 1000 # x1000 for millimetres
  a1_height <- a0_height / sqrt(2)
  a2_height <- a1_height / sqrt(2)
  a3_height <- a2_height / sqrt(2)
  a4_height <- a3_height / sqrt(2)
  a5_height <- a4_height / sqrt(2)
  a6_height <- a5_height / sqrt(2)
  a7_height <- a6_height / sqrt(2)
  if (tolower(size) == "a0") {
    height <- a0_height
    width <- a0_height / sqrt(2)
  } else if (tolower(size) == "a1") {
    height <- a1_height
    width <- a1_height / sqrt(2)
  } else if (tolower(size) == "a2") {
    height <- a2_height
    width <- a2_height / sqrt(2)
  } else if (tolower(size) == "a3") {
    height <- a3_height
    width <- a3_height / sqrt(2)
  } else if (tolower(size) == "a4") {
    height <- a4_height
    width <- a4_height / sqrt(2)
  } else if (tolower(size) == "a5") {
    height <- a5_height
    width <- a5_height / sqrt(2)
  } else if (tolower(size) == "a6") {
    height <- a6_height
    width <- a6_height / sqrt(2)
  } else if (tolower(size) == "a7") {
    height <- a7_height
    width <- a7_height / sqrt(2)
  } else {
    message("No valid value for size; using A4 as paper format.")
    height <- a4_height
    width <- a4_height / sqrt(2)
  }
  
  if (portrait == FALSE) {
    height.bak <- height
    height <- width
    width <- height.bak
  }
  
  suppressWarnings(
    ggplot2::ggsave(filename = filename,
                    device = grDevices::cairo_pdf,
                    width = width,
                    height = height,
                    units = "mm",
                    plot = plot,
                    ...)
  )
  
  if (file.exists(filename)) {
    message(paste0("Plot exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    invisible(filename)
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
}

#' @rdname export
#' @param width required width of the PNG file in pixels
#' @param height required height of the PNG file in pixels
#' @param dpi plot resolution
#' @details `r doc_requirement("a PNG file", "export_png", "ggplot2")`.
#' @importFrom certestyle format2
#' @export
export_png <- function(plot,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       width = 1000,
                       height = 800,
                       dpi = showtext::showtext_opts()$dpi,
                       ...) {
  
  if (!"showtext" %in% rownames(utils::installed.packages())) {
    stop("Package 'showtext' not installed")
  }
  
  check_is_installed("ggplot2")
  if ("certeplot2" %in% rownames(utils::installed.packages())) {
    get_plot_title <- certeplot2::get_plot_title
  } else {
    get_plot_title <- NULL
  }
  
  if (is.null(filename) && !is.null(get_plot_title)) {
    filename <- get_plot_title(plot)
    if (is.na(filename)) {
      filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
    }
    filename <- paste0(filename, ".png")
  } else if (is.null(filename)) {
    filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
  }
  filename <- parse_file_location(filename,
                                  needed_extension = "png",
                                  card_number = card_number)
  
  dpi_old <- showtext::showtext_opts()$dpi
  showtext::showtext_opts(dpi = dpi)
  
  suppressWarnings(
    ggplot2::ggsave(filename = filename,
                    dpi = dpi,
                    width = width,
                    height = height,
                    units = "px",
                    plot = plot,
                    ...)
  )
  
  showtext::showtext_opts(dpi = dpi_old)
  
  if (file.exists(filename)) {
    message(paste0("Plot exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    invisible(filename)
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
}


#' @rdname export
#' @details `r doc_requirement("an HTML file", "export_html", c("ggplot2", "htmltools"))`. The arguments put in `...` will be passed on to [plotly::layout()] if `plot` is not yet a Plotly object (but rather a `ggplot2` object), which of course then requires the `plotly` package to be installed as well.
#' @importFrom certestyle format2
#' @export
export_html <- function(plot,
                        filename = NULL,
                        card_number = project_get_current_id(ask = FALSE),
                        ...) {
  check_is_installed(c("ggplot2", "htmltools"))
  if ("certeplot2" %in% rownames(utils::installed.packages())) {
    get_plot_title <- certeplot2::get_plot_title
  } else {
    get_plot_title <- NULL
  }
  
  if (is.null(filename) && !is.null(get_plot_title)) {
    filename <- get_plot_title(plot)
    if (is.na(filename)) {
      filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
    }
    filename <- paste0(filename, ".html")
  } else if (is.null(filename)) {
    filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
  }
  filename <- parse_file_location(filename,
                                  needed_extension = "html",
                                  card_number = card_number)
  
  if (ggplot2::is.ggplot(plot)) {
    # transform to plotly first
    check_is_installed("plotly")
    plot <- plotly::layout(plotly::ggplotly(plot), ...)
  }
  
  suppressWarnings(
    htmltools::save_html(plot,
                         file = filename,
                         lang = "nl",
                         libdir = "library_do_not_delete")
  )
  
  if (file.exists(filename)) {
    message(paste0("Plot exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    invisible(filename)
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
}


# Import functions --------------------------------------------------------

#' Import Data Sets
#' 
#' These functions can be used to import data, from local or remote paths, or from the internet. They work closely with the `certeprojects` package and support Trello card numbers. To support row names and older R versions, `import_*()` functions return plain [data.frame]s, not e.g. [tibble][tibble::tibble()]s.
#' @param filename the full path of the file to be imported, will be parsed to a [character]
#' @param auto_transform transform the imported data with [auto_transform()]
#' @param card_number a Trello card number
#' @param ... arguments passed on to methods
#' @details `r doc_requirement("any unlisted filetype", "import", "rio")`.
#' @rdname import
#' @seealso [export()]
#' @export
import <- function(filename,
                   card_number = project_get_current_id(ask = FALSE),
                   auto_transform = TRUE,
                   ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  if (filename %like% "[.]rds$") {
    import_rds(filename = filename,
               card_number = card_number,
               ...)
  } else if (filename %like% "[.]csv$" && (is.null(list(...)$sep) || identical(list(...)$sep, ","))) {
    import_csv(filename = filename,
               card_number = card_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]csv$" && identical(list(...)$sep, ";")) {
    import_csv2(filename = filename,
                card_number = card_number,
                auto_transform = auto_transform,
                ...)
  } else if (filename %like% "[.]tsv$") {
    import_tsv(filename = filename,
               card_number = card_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]txt$") {
    import_txt(filename = filename,
               card_number = card_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]xlsx?$") {
    import_xlsx(filename = filename,
                card_number = card_number,
                auto_transform = auto_transform,
                ...)
  } else if (filename %like% "[.]sav$") {
    import_sav(filename = filename,
               card_number = card_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]feather$") {
    if (missing(auto_transform)) {
      auto_transform <- FALSE
    }
    import_feather(filename = filename,
                   card_number = card_number,
                   auto_transform = auto_transform,
                   ...)
  } else if (filename %like% "[.]parquet$") {
    if (missing(auto_transform)) {
      auto_transform <- FALSE
    }
    import_parquet(filename = filename,
                   card_number = card_number,
                   auto_transform = auto_transform,
                   ...)
  } else {
    import_exec(filename,
                extension = "",
                card_number = card_number,
                auto_transform = auto_transform,
                ...)
  }
}

#' @rdname import
#' @export
import_rds <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "rds",
              card_number = card_number,
              auto_transform = FALSE)
}

#' @rdname import
#' @param sheet Excel sheet to import, defaults to first sheet
#' @param range a cell range to read from, allows typical Excel ranges such as "B3:D87" and "Budget!B2:G14"
#' @param skip number of first rows to skip
#' @inheritParams auto_transform
#' @details `r doc_requirement("an Excel file", c("import_xlsx", "import_excel"), "readxl")`.
#' @importFrom cleaner format_datetime
#' @export
import_xlsx <- function(filename,
                        card_number = project_get_current_id(ask = FALSE),
                        sheet = 1,
                        range = NULL,
                        auto_transform = TRUE,
                        datenames = "nl",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = ",",
                        big.mark = "",
                        timezone = "UTC",
                        na = c("", "NULL", "NA", "<NA>"),
                        skip = 0,
                        ...) {
  check_is_installed("readxl")
  if (length(sheet) != 1) {
    stop("'sheet' must be a single number or name, since only one sheet can be imported at a time", call. = FALSE)
  }
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = ifelse(filename %like% "[.]xls$", "xls", "xlsx"),
              card_number = card_number,
              sheet = sheet,
              range = range,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

#' @rdname import
#' @export
import_excel <- import_xlsx

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_csv <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "csv",
              sep = ",",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_csv2 <- function(filename,
                        card_number = project_get_current_id(ask = FALSE),
                        auto_transform = TRUE,
                        datenames = "nl",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = ",",
                        big.mark = "",
                        timezone = "UTC",
                        na = c("", "NULL", "NA", "<NA>"),
                        skip = 0,
                        ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "csv",
              sep = ";",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_tsv <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "tsv",
              sep = "\t",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_txt <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       sep = "\t",
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ",",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "txt",
              sep = sep,
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

#' @rdname import
#' @details `r doc_requirement("an SPSS file", c("import_sav", "import_spss"), "haven")`.
#' @export
import_sav <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "en",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       ...) {
  check_is_installed("haven")
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "sav",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = dateformat,
              timeformat = timeformat,
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @export
import_spss <- import_sav

#' @rdname import
#' @details `r doc_requirement("a Feather file", "import_feather", "arrow")`. Feather provides efficient binary columnar serialization for data sets, enabling easy sharing data across data analysis languages. The imported data set will not be transformed automatically, since Feather files contain a data type structure already. Use the `select` argument (which supports the [tidyselect language][tidyselect::language]) for specific data selection.
#' @param select columns to select, supports the [tidyselect language][tidyselect::language])
#' @importFrom dplyr everything
#' @export
import_feather <- function(filename,
                           card_number = project_get_current_id(ask = FALSE),
                           select = everything(),
                           auto_transform = FALSE,
                           datenames = "en",
                           dateformat = "yyyy-mm-dd",
                           timeformat = "HH:MM",
                           decimal.mark = ".",
                           big.mark = "",
                           timezone = "UTC",
                           na = c("", "NULL", "NA", "<NA>"),
                           ...) {
  check_is_installed("arrow")
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "feather",
              card_number = card_number,
              select = select,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = dateformat,
              timeformat = timeformat,
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @details `r doc_requirement("a Parquet file", "import_parquet", "arrow")`. 'Parquet' is a columnar storage file format. The imported data set will not be transformed automatically, since Parquet files contain a data type structure already. Use the `select` argument (which supports the [tidyselect language][tidyselect::language]) for specific data selection.
#' @importFrom dplyr everything
#' @export
import_parquet <- function(filename,
                           card_number = project_get_current_id(ask = FALSE),
                           select = everything(),
                           auto_transform = FALSE,
                           datenames = "en",
                           dateformat = "yyyy-mm-dd",
                           timeformat = "HH:MM",
                           decimal.mark = ".",
                           big.mark = "",
                           timezone = "UTC",
                           na = c("", "NULL", "NA", "<NA>"),
                           ...) {
  check_is_installed("arrow")
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "parquet",
              card_number = card_number,
              select = select,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = dateformat,
              timeformat = timeformat,
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @param url remote location of any data set, can also be a (non-raw) GitHub/GitLab link
#' @details The [import_url()] tries to download the file first, after which it will be imported using the appropriate `import_*()` function.
#' @export
import_url <- function(url,
                       auto_transform = TRUE,
                       sep = ",",
                       datenames = "en",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       ...) {
  url <- url[1]
  if (url %unlike% "://") {
    url <- paste0("http://", url)
  }
  import_exec(url,
              filename_deparse = deparse(substitute(url)),
              extension = gsub(".+[.](.*)$", "\\1", basename(url)),
              sep = sep,
              card_number = NULL,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = dateformat,
              timeformat = timeformat,
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip)
}

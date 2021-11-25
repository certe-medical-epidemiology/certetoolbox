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
    project_set_file <- get_external_function("project_set_file", "certeprojects")
    if (!is.null(project_set_file)) {
      filename_proj <- project_set_file(filename, card_number = card_number)
      if (!is.na(filename_proj)) {
        filename <- filename_proj
      }
    }
  }
  # remove invalid characters
  filename <- gsub("[?|<>|*]+", "", filename)
  filename
}

# Export functions --------------------------------------------------------

# export.query <- function(object, filename) {
#   filename_qry <- gsub("[.][a-zA-Z0-9]+$", ".sql", filename)
#   write(suppressMessages(qry(object)), file = filename_qry, ncolumns = 1, append = FALSE)
#   message(paste0("Query exported as `", tools::file_path_as_absolute(filename_qry), "`."))
# }

#' Export Data and Plots
#' 
#' These functions can be used to export data and plots. They invisibly return the object itself again, allowing for usage in pipes. The functions work closely with the `certeprojects` package and support Trello card numbers.
#' @param object,plot the \R object to export
#' @param fn a manual export function, such as `haven::write_sas` to write SAS files
#' @param filename the full path of the exported file
#' @param card_number a Trello card number
#' @param export_qry export the underlying SQL query as well
#' @param ... arguments passed on to methods
#' @rdname export
#' @details The [export()] function can export to any file format, as long as the export function is passed on to the `fn` argument. This function has to have the object as first argument and the future file location as second argument.
#' @export
#' @examples 
#' \dontrun{
#' library(dplyr)
#' 
#' # export two files: 'whole_file.rds' and 'first_ten_rows.xlsx'
#' starwars %>%
#'   export.R("whole_file") %>%
#'   slice(1:10) %>%
#'   export.excel("first_ten_rows")
#' }
export <- function(object,
                   fn,
                   filename = NULL,
                   card_number = project_get_current_id(ask = FALSE),
                   ...) {
  if (is.character(fn)) {
    fn <- tryCatch(eval(parse(text = fn)), error = function(e) NA_character_)
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object,
              needed_extension = NULL,
              filename = filename,
              card_number = card_number,
              export_qry = FALSE,
              fn = fn,
              ...)
}

#' @rdname export
#' @inheritParams as_excel
#' @details RDS files as created by [export.R()] are compatible with R3 and R4.
#' @export
export.R <- function(object,
                     filename = NULL,
                     card_number = project_get_current_id(ask = FALSE),
                     export_qry = TRUE,
                     ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "rds",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              compress = "gzip",
              ascii = FALSE,
              version = 2)
}

#' @rdname export
#' @inheritParams as_excel
#' @details The [export.excel()] uses [as_excel()] internally.
#' @export
export.excel <- function(object,
                         filename = NULL,
                         card_number = project_get_current_id(ask = FALSE),
                         export_qry = TRUE,
                         sheet_names = NULL,
                         autofilter = TRUE,
                         rows_zebra = TRUE,
                         cols_zebra = FALSE,
                         ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "xlsx",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              sheet_names = "Blad1",
              autofilter = autofilter,
              rows_zebra = rows_zebra,
              cols_zebra = cols_zebra,
              save = TRUE,
              overwrite = TRUE)
}

#' @rdname export
#' @details For [export.csv()], [export.csv2()] and [export.tsv()], files will be saved in UTF-8 encoding and values `NA` will be exported as `""`. Like other `*.csv` and `*.csv2` functions, csv is comma (`,`) separated and csv2 is semicolon (`;`) separated.
#' @export
export.csv <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       export_qry = TRUE,
                       ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "csv",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              append = FALSE,
              quote = TRUE,
              sep = ",",
              eol = "\n",
              na = "",
              dec = ".",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @export
export.csv2 <- function(object,
                        filename = NULL,
                        card_number = project_get_current_id(ask = FALSE),
                        export_qry = TRUE,
                        ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "csv",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              append = FALSE,
              quote = TRUE,
              sep = ";",
              eol = "\n",
              na = "",
              dec = ",",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @export
export.tsv <- function(object,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       export_qry = TRUE,
                       ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "tsv",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              append = FALSE,
              quote = TRUE,
              sep = "\t",
              eol = "\n",
              na = "",
              dec = ".",
              row.names = FALSE,
              col.names = TRUE,
              qmethod = "double",
              fileEncoding = "UTF-8")
}

#' @rdname export
#' @details Exporting as SPSS files with [export.spss()] requires the `haven` package to be installed.
#' @export
export.spss <- function(object,
                        filename = NULL,
                        card_number = project_get_current_id(ask = FALSE),
                        export_qry = TRUE,
                        ...) {
  if (is.null(filename)) {
    filename <- deparse(substitute(object))
  }
  export_exec(object, "sav",
              filename = filename,
              card_number = card_number,
              export_qry = export_qry,
              compress = FALSE)
}

#' @rdname export
#' @param size paper size, defaults to A5. Can be A0 to A7.
#' @param portrait portrait mode, defaults to `FALSE` (i.e., landscape mode)
#' @details If the filename is left blank in [export.pdf()] or [export.png()], the title of `plot` will be used.
#' @importFrom certestyle format2
#' @export
export.pdf <- function(plot,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       size = "A5",
                       portrait = FALSE,
                       ...) {
  
  ggsave <- get_external_function("ggsave", "ggplot2")
  get_plot_title <- get_external_function("get_plot_title", "certeplot2", error_on_fail = FALSE)
  
  if (is.null(filename) && !is.null(get_plot_title)) {
    filename <- get_plot_title(plot)
    if (is.na(filename)) {
      filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
    }
    filename <- paste0(filename, ".pdf")
  } else {
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
    ggsave(filename = filename,
           device = grDevices::cairo_pdf,
           width = width,
           height = height,
           units = 'mm',
           plot = plot,
           ...)
  )
  
  if (file.exists(filename)) {
    message(paste0("Plot exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    invisible(plot)
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
}

#' @rdname export
#' @param width required width of the PNG file in pixels
#' @param height required height of the PNG file in pixels
#' @param text.factor text factor for the exported plot. Defaults to `1.2`, which loosely equals a PDF file in A5 format when it comes to text sizes.
#' @export
export.png <- function(plot,
                       filename = NULL,
                       card_number = project_get_current_id(ask = FALSE),
                       width = 1000,
                       height = 800,
                       text.factor = 1.2,
                       ...) {
  
  ggsave <- get_external_function("ggsave", "ggplot2")
  get_plot_title <- get_external_function("get_plot_title", "certeplot2", error_on_fail = FALSE)
  
  if (is.null(filename) && !is.null(get_plot_title)) {
    filename <- get_plot_title(plot)
    if (is.na(filename)) {
      filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
    }
    filename <- paste0(filename, ".png")
  } else {
    filename <- format2(now(), "yyyy-mm-dd-HHMMSS")
  }
  filename <- parse_file_location(filename,
                                  needed_extension = "png",
                                  card_number = card_number)
  
  suppressWarnings(
    ggsave(filename = filename,
           dpi =  text.factor * 100,
           width = width / (text.factor * 100),
           height = height / (text.factor * 100),
           units = "in",
           plot = plot,
           ...)
  )
  
  if (file.exists(filename)) {
    message(paste0("Plot exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    invisible(plot)
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
}

#' @rdname export
#' @param sep (for [export.clipboard()]) separator for values in a row (default: tab)
#' @param na (for [export.clipboard()]) replacement character for empty values (default: `""`)
#' @param header (for [export.clipboard()]) use column names as header (default: `TRUE`)
#' @param quote (for [export.clipboard()]) use quotation marks (default: `FALSE`)
#' @param decimal.mark (for [export.clipboard()]) character to use for decimal numbers
#' @details The [export.clipboard()] function requires the `clipr` package to be installed. The function allows any object (also other than [data.frame]s) and is only limited to the available amount of RAM memory.
#' @importFrom certestyle format2
#' @export
export.clipboard <- function(object,
                             sep = "\t",
                             na = "",
                             header = TRUE,
                             quote = FALSE,
                             decimal.mark = ",",
                             ...) {
  
  # this will check for installation of 'clipr'
  write_clip <- get_external_function("write_clip", "clipr")
  write_clip(content = object,
             object_type = ifelse(inherits(object, c("data.frame", "matrix")),
                                  "table",
                                  "character"),
             eos = ifelse(inherits(object, c("data.frame", "matrix")),
                          "\n",
                          NULL),
             sep = sep,
             na = as.character(na),
             row.names = FALSE,
             col.names = isTRUE(header),
             dec = decimal.mark,
             allow_non_interactive = TRUE,
             quote = isTRUE(quote))
  message(
    paste0(
      "Object exported to clipboard: ",
      format2(NROW(object)), "x", format2(NCOL(object)),
      " (", size_humanreadable(utils::object.size(object)), ")"
    )
  )
}

#' @importFrom tibble rownames_to_column
export_exec <- function(object,
                        needed_extension,
                        filename,
                        card_number,
                        export_qry,
                        fn = NULL,
                        ...) {
  if (is.null(needed_extension)) {
    needed_extension <- ""
  }
  needed_extension <- tolower(needed_extension)
  filename <- parse_file_location(filename,
                                  needed_extension = needed_extension,
                                  card_number = card_number)
  needed_extension <- needed_extension[1L]
  if (needed_extension == "") {
    if (filename %unlike% "[.][a-z0-9]+$") {
      warning("No file extension set.", call. = FALSE)
    }
    # custom method
    fn(object, filename, ...)
  } else if (needed_extension == "rds") {
    # R format
    base::saveRDS(object, file = filename, ...)
  } else if (needed_extension == "sav") {
    # SPSS format
    check_is_installed("haven")
    haven::write_sav(object, path = filename, ...)
  } else if (needed_extension == "xlsx") {
    # Excel format
    if (inherits(object, "Workbook")) {
      # an openxlsx object
      suppressMessages(save_excel(object, file = filename, ...))
    } else {
      # a data.frame
      suppressMessages(as_excel(object, file = filename, ...))  
    }
  } else {
    # flat data file
    if (!all(rownames(object) == as.character(1:nrow(object)))) {
      object <- rownames_to_column(object, var = "rownames")
      warning("Row names added as first column 'rownames'", call. = FALSE)
    }
    if (needed_extension %in% c("csv", "tsv")) {
      # arguments such as 'sep' etc. are passed into '...':
      utils::write.table(object, file = filename, ...)
    } else {
      stop("Unknown extension method: ", needed_extension, call. = FALSE)
    }
  }
  if (file.exists(filename)) {
    message(paste0("Data exported as '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
  } else {
    stop("Error while saving `", filename, "`.", call. = FALSE)
  }
  # if (export_qry == TRUE && has_qry(object)) {
  #   export.query(object, filename)
  # }
  invisible(object)
}

# Import functions --------------------------------------------------------

#' Import Data and Plots
#' 
#' These functions can be used to import data. They work closely with the `certeprojects` package and support Trello card numbers.
#' @param filename the full path of the file to be imported, will be parsed to a [character]
#' @param auto_transform transform the imported data with [auto_transform()]
#' @param card_number a Trello card number
#' @param ... arguments passed on to methods
#' @details The [import()] function uses the `rio` package to guess the required import function.
#' @rdname import
#' @export
#' @examples 
#' #
import <- function(filename,
                   card_number = project_get_current_id(ask = FALSE),
                   auto_transform = FALSE,
                   ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  # uses rio::import which pretty much understands any file type
  check_is_installed("rio")
  import_exec(filename,
              extension = "",
              card_number = card_number,
              auto_transform = auto_transform,
              ...)
}

#' @rdname import
#' @export
import.R <- function(filename,
                     card_number = project_get_current_id(ask = FALSE),
                     ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
              extension = "rds",
              card_number = card_number,
              auto_transform = FALSE)
}

#' @rdname import
#' @param sheet Excel sheet to import, defaults to first sheet
#' @inheritParams auto_transform
#' @importFrom cleaner format_datetime
#' @export
import.excel <- function(filename,
                         card_number = project_get_current_id(ask = FALSE),
                         sheet = 1,
                         auto_transform = TRUE,
                         datenames = "nl",
                         dateformat = "yyyy-mm-dd",
                         timeformat = "HH:MM",
                         decimal.mark = ",",
                         big.mark = "",
                         timezone = "UTC",
                         na = c("", "NULL", "NA", "<NA>"),
                         ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
              extension = "xlsx",
              card_number = card_number,
              sheet = sheet,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import.csv <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ",",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
              extension = "csv",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import.csv2 <- function(filename,
                        card_number = project_get_current_id(ask = FALSE),
                        auto_transform = TRUE,
                        datenames = "nl",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = ",",
                        big.mark = "",
                        timezone = "UTC",
                        na = c("", "NULL", "NA", "<NA>"),
                        ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
              extension = "csv2",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import.tsv <- function(filename,
                       card_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ",",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
              extension = "tsv",
              card_number = card_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na)
}

#' @rdname import
#' @export
import.spss <- function(filename,
                        card_number = project_get_current_id(ask = FALSE),
                        auto_transform = FALSE,
                        datenames = "en",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = ".",
                        big.mark = "",
                        timezone = "UTC",
                        na = c("", "NULL", "NA", "<NA>"),
                        ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  import_exec(filename,
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
#' @param sep character to separate values in a row
#' @param header use first row as header
#' @param startrow first row to start importing
#' @details The [import.clipboard()] function requires the `clipr` package to be installed.
#' @export
import.clipboard <- function(sep = "\t",
                             header = TRUE,
                             startrow = 1,
                             auto_transform = TRUE,
                             datenames = "nl",
                             dateformat = "yyyy-mm-dd",
                             timeformat = "HH:MM",
                             decimal.mark = ",",
                             big.mark = "",
                             timezone = "UTC",
                             na = c("", "NULL", "NA", "<NA>"),
                             ...) {
  
  # this will check for installation of 'clipr'
  read_clip_tbl <- get_external_function("read_clip_tbl", "clipr")
  df <- read_clip_tbl(sep = sep,
                      header = header,
                      row.names = NULL,
                      quote = '"',
                      fill = TRUE,
                      comment.char = "",
                      strip.white = TRUE,
                      dec = decimal.mark,
                      na.strings = na,
                      # fileEncoding = 'UTF-8',
                      encoding = "UTF-8",
                      stringsAsFactors = FALSE)
  if (startrow > 1) {
    # otherwise column headers will be lost
    df <- df[c(startrow:nrow(df)), , drop = FALSE]
  }
  colnames(df) <- gsub("[.]+", "_", colnames(df))
  if (auto_transform == TRUE) {
    df <-  auto_transform(df, ...)
  }
  df
}

#' @importFrom readr read_delim locale
#' @importFrom dplyr select
import_exec <- function(filename,
                        extension,
                        card_number,
                        auto_transform, ...) {
  extension <- extension[1L]
  csv_delim <- ","
  if (extension == "csv2") {
    csv_delim <- ";"
    extension <- "csv"
  } else if (extension == "tsv") {
    csv_delim <- "\t"
  }
  
  filename <- gsub('\\', '/', filename, fixed = TRUE)
  if (filename %unlike% paste0(extension, "$")) {
    filename <- paste0(filename, ".", gsub("^[.]", "", extension))
  }
  if (!file.exists(filename) && !is.null(card_number)) {
    # try project file using the 'certeprojects' package
    project_get_file <- get_external_function("project_get_file", "certeprojects", error_on_fail = FALSE)
    if (!is.null(project_get_file)) {
      filename <- project_get_file(filename, card_number = card_number)
    }
  }
  if (!file.exists(filename)) {
    filename <- read_secret("path.refmap")
    if (!file.exists(filename)) {
      stop("File not found: ", filename, call. = FALSE)
    }
  }
  if (!file.exists(filename)) {
    stop("File not found: ", filename, call. = FALSE)
  }
  
  if (extension == "rds") {
    # R format
    df <- base::readRDS(file = filename)
  } else if (extension == "sav") {
    # SPSS format
    read_sav <- get_external_function("read_sav", "haven", error_on_fail = TRUE)
    as_factor <- get_external_function("as_factor", "haven", error_on_fail = TRUE)
    df <- as_factor(read_sav(file = filename))
  } else if (extension == "xlsx") {
    read_excel <- get_external_function("read_excel", "readxl", error_on_fail = TRUE)
    # Excel format
    df <- read_excel(path = filename, progress = interactive())
  } else if (extension %in% c("csv", "tsv")) {
    # flat files
    df <- read_delim(file = filename,
                     guess_max = 200000,
                     delim = csv_delim,
                     na = list(...)$na,
                     progress = interactive(),
                     locale = locale(date_names = list(...)$datenames,
                                     date_format = list(...)$dateformat,
                                     time_format = list(...)$timeformat,
                                     tz = list(...)$timezone,
                                     decimal_mark = list(...)$decimal.mark,
                                     grouping_mark = list(...)$big.mark,
                                     encoding = "UTF-8"))
  } else {
    import <- get_external_function("import", "rio", error_on_fail = TRUE)
    df <- import(file = filename, ...)
  }
  
  # force data.frame
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # if row names were saved as first col, set back to row names
  if (colnames(df)[1L] %like% "row.names?") {
    rownames(df) <- df[, 1, drop = TRUE]
    df <- select(df, -1)
    message("Row names restored from first column.", call. = FALSE)
  }
  
  if (auto_transform == TRUE) {
    df <- auto_transform(df, ...)
  }
  
  if (interactive()) {
    message(
      paste0(
        "Imported data set (", format2(NROW(df)), "x", format2(NCOL(df)),
        ", ", size_humanreadable(utils::object.size(df)),  ") from '",
        tools::file_path_as_absolute(filename), "'"
      )
    )
  }
  
  df
}


#' Create Excel Document
#' 
#' This function relies on the `openxlsx` package for creating Excel documents.
#' @param ... data objects, use named items for multiple tabs (see *Examples*) zie Examples
#' @param sheet_names sheet names
#' @param autofilter create autofilter on columns in first row
#' @param autowidth automatically adjust columns widths
#' @param rows_zebra create banded rows
#' @param cols_zebra create banded columns
#' @param save save the file directly, defaults to `TRUE` if used after a pipe (`%>%`)
#' @param file file location to save Excel document to, defaults to a random filename
#' @param freeze_top_row freeze the first row of the sheet
#' @param overwrite overwrite existing file
#' @rdname as_excel
#' @importFrom openxlsx createStyle createWorkbook modifyBaseFont addWorksheet writeDataTable addStyle freezePane setColWidths
#' @importFrom tibble rownames_to_column
#' @export
#' @examples
#' # does not save immediately, but returns the workbook object
#' xl <- as_excel("this is a sheet" = mtcars,
#'                "another sheet" = anscombe)
#' xl
#' 
#' 
#' # save with save_excel() or export.excel()
#' export.excel(xl)
#'          
#' \dontrun{
#' # saves immediately
#' if (require("dplyr")) {
#'   mtcars %>%
#'     as_excel()
#' }
#' }
as_excel <- function(...,
                     sheet_names = NULL,
                     autofilter = TRUE,
                     autowidth = TRUE,
                     rows_zebra = TRUE,
                     cols_zebra = FALSE,
                     save = identical(deparse(substitute(...)), "."),
                     file = NULL,
                     freeze_top_row = TRUE,
                     overwrite = FALSE) {
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
  wb <- createWorkbook(creator = read_secret("department.name"))
  modifyBaseFont(wb, fontSize = 11, fontColour = "black", fontName = "Calibri")
  
  for (i in seq_len(length(dots))) {
    df <- dots[[i]]
    # support row names
    if (!identical(rownames(df), as.character(seq_len(NROW(df))))) {
      df <- rownames_to_column(df)
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
  
  if (isTRUE(save)) {
    save_excel(wb, file = file, overwrite = overwrite)
    if (file.exists(tools::file_path_as_absolute(file))) {
      message(paste0("Exported as '", tools::file_path_as_absolute(file), "' (", size_humanreadable(file.size(file)), ")."))
    } else {
      stop("Error while saving '", tools::file_path_as_absolute(file), "'.")
    }
  } else {
    wb
  }
}

#' @rdname as_excel
#' @param xl Excel object, as created with the `openxlsx` package
#' @importFrom openxlsx saveWorkbook
#' @export
save_excel <- function(xl, file = NULL, overwrite = FALSE, ...) {
  if (!inherits(xl, "Workbook")) {
    stop("this function can only accept class Workbook from the 'openxlsx' package", call. = FALSE)
  }
  if (is.null(file)) {
    file <- paste0("excel_", generate_identifier(8), ".xlsx")
  } else if (file %unlike% "[.]xlsx?$") {
    file <- paste0(file, ".xlsx")
  }
  saveWorkbook(xl, file = file, overwrite = overwrite)
}

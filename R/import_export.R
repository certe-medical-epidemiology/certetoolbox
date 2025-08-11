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

export_exec <- function(object,
                        needed_extension,
                        filename,
                        filename_deparse,
                        project_number,
                        overwrite,
                        fn = NULL,
                        encrypt = FALSE,
                        key = "",
                        ...) {
  if (!is.data.frame(object) && !is.list(object) && needed_extension %unlike% "xls" ) {
    # maybe it's a name of an object? Try to get it:
    object <- tryCatch(eval(parse(text = object)),
                       error = function(e) NULL)
    if (!is.data.frame(object) && needed_extension != "rds") {
      stop("'object' must be a data.frame", call. = FALSE)
    }
  }
  if (is.null(needed_extension)) {
    needed_extension <- ""
  }
  filename_deparse <- gsub('"', "", filename_deparse, fixed = TRUE)
  if (is.null(filename)) {
    filename <- filename_deparse
  }
  needed_extension <- tolower(needed_extension)
  filename <- parse_file_location(filename,
                                  needed_extension = needed_extension,
                                  project_number = project_number)
  filename_old <- paste0(filename, ".certetoolbox_export")
  if (!file_can_be_overwritten(overwrite, filename)) {
    return(invisible(object))
  }
  
  if (any(colnames(object) %like% "bsn|burger.*service") && encrypt == FALSE) {
    warning("Data set might contain social security numbers (BSN) - these must never be shared.", call. = FALSE)
  }
  
  object.bak <- object
  
  if (encrypt == TRUE) {
    check_is_installed("openssl")
    # transform into raw vector
    object <- serialize(object, connection = NULL, xdr = TRUE)
    # compress vector, similar to base::saveRDS
    object <- memCompress(object, type = "gzip")
    # encrypt these bytes
    object <- openssl::aes_gcm_encrypt(object,
                                       key = openssl::sha256(charToRaw(key)),
                                       iv = openssl::rand_bytes(12))
  }
  
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
    object.bak <- object
    arrow::write_feather(x = object, sink = filename, ...)
    # } else if (needed_extension == "parquet") {
    #   # Apache's Parquet format
    #   object <- rownames_1st_column(object)
    #   arrow::write_parquet()
    #   arrow::write_parquet(x = object, sink = filename, ...)
  } else if (needed_extension == "xlsx") {
    xl_object <- object
    # Excel format
    if (!inherits(xl_object, class(wb_workbook()))) {
      # not yet an openxlsx2 object (but rather e.g. a data frame)
      xl_object <- suppressMessages(as_excel(xl_object, project_number = project_number, ...))
    }
    suppressMessages(save_excel(xl = xl_object, filename = filename, overwrite = TRUE))
    if (is.list(object)) {
      object <- object[[1]]
      object.bak <- object
    }
  } else {
    # flat data file
    object <- rownames_1st_column(object)
    object.bak <- object
    if (needed_extension %in% c("csv", "tsv", "txt")) {
      # arguments such as 'sep' etc. are passed into '...':
      utils::write.table(object, file = filename, ...)
    } else {
      stop("Unknown extension method: ", needed_extension, call. = FALSE)
    }
  }
  
  if (file.exists(filename)) {
    message(paste0("Exported data set (",
                   format2(NROW(object.bak)), pkg_env$cross_icon, format2(NCOL(object.bak)),
                   ") to '",
                   tools::file_path_as_absolute(filename), 
                   "' (", size_humanreadable(file.size(filename)), ")."))
    if (encrypt == TRUE) {
      message("NOTE: This file was AES-GCM encrypted and is unreadable without the correct `key`.")
    }
  } else {
    warning("Error while exporting to `", filename, "`.", call. = FALSE)
    # revert the old existing file (see the file_can_be_overwritten() function)
    if (file.exists(filename_old)) {
      file.copy(filename_old, filename, copy.mode = TRUE, copy.date = TRUE)
    }
  }
  if (file.exists(filename_old)) {
    try(file.remove2(filename_old), silent = TRUE)
  }
  invisible(structure(object.bak, filename = tools::file_path_as_absolute(filename)))
}

#' @importFrom readr read_delim locale problems
#' @importFrom dplyr select
#' @importFrom certeprojects project_get_file
import_exec <- function(filename,
                        filename_deparse,
                        extension,
                        project_number,
                        auto_transform,
                        key = "",
                        ...) {
  extension <- tolower(extension[1L])
  
  if (!is.character(filename)) {
    filename <- filename_deparse
  }
  
  filename_url <- NULL
  if (filename %like% "^(http|https|ftp|sftp|ftps|ssh)://") {
    # download the file first
    filename_url <- filename
    if (filename_url %like% "git(hub|lab)[.]com/.*/blob/") {
      # get GitHub/GitLab raw URL
      filename_url <- gsub("/blob/", "/raw/", filename_url, fixed = TRUE)
    }
    if (extension == "") {
      extension <- tools::file_ext(basename(filename_url))
    }
    filename <- tempfile(pattern = "import_", fileext = paste0(".", extension))
    if (.Platform$OS.type == "windows") {
      utils::download.file(url = filename_url, destfile = filename, mode = "wb")
    } else {
      utils::download.file(url = filename_url, destfile = filename)
    }
    if (!file.exists(filename)) {
      stop("Failed to download: ", filename_url, call. = FALSE)
    } else {
      message("Downloaded file: ", size_humanreadable(file.size(filename), decimal.mark = "."), " (", file.size(filename), " bytes)")
    }
  }
  
  filename <- gsub('\\', '/', filename, fixed = TRUE)
  if (filename %unlike% "[.][a-zA-Z0-9]{1,7}$") {
    # does not have extension yet
    filename <- paste0(filename, ".", gsub("^[.]", "", extension))
  }
  if (!file.exists(filename) && !is.null(project_number)) {
    # try project file using the 'certeprojects' package
    filename <- project_get_file(filename, project_number = project_number)
  }
  
  if (!file.exists(filename)) {
    stop("File not found: ", filename, call. = FALSE)
  }
  
  if (extension == "rds") {
    # R format
    df <- base::readRDS(file = filename, ...)
  } else if (extension == "rda") {
    env <- new.env()
    load(filename, envir = env)
    df <- get(names(env), envir = env)
    rm(env)
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
                                     encoding = list(...)$encoding),
                     show_col_types = FALSE)
    probs <- problems(df)
    if (nrow(probs) > 0) {
      warning("Contents of `readr::problems()`:\n", paste0(format(probs), collapse = "\n"), call. = FALSE)
    }
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
    # } else if (extension %in% c("feather", "parquet")) {
  } else if (extension == "feather") {
    # Apache's Feather and Parquet format
    fun <- eval(parse(text = paste0("read_", extension)),
                envir = asNamespace("arrow"))
    df <- fun(file = filename,
              # always include first column, might be rownames
              col_select = c(1, list(...)$col_select),
              as_data_frame = TRUE)
    if (colnames(df)[1] != "rownames" && !1 %in% list(...)$col_select) {
      # 1st column is not rownames and 1st column was not in original selection, so:
      df <- df |> select(-1)
    }
    
  } else {
    # use rio::import which pretty much understands any file type
    check_is_installed("rio")
    df <- rio::import(file = filename, ...)
  }
  
  if (is.raw(df)) {
    # object was encrypted using openssl::aes_gcm_encrypt()
    df <- decrypt_object(df, key = key)
  }
  
  if (!inherits(df, c("sf", "raw")) && !extension == "rds") {
    # force plain data.frame if type is not map data
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    auto_transform <- FALSE
  }
  
  # if row names were saved as first col, set back to row names
  if (is.data.frame(df) && colnames(df)[1L] %like% "row.?names?") {
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
      try(file.remove2(filename), silent = TRUE)
    }
    message(
      paste0(
        "Imported data set (",
        format2(NROW(df)), pkg_env$cross_icon, format2(NCOL(df)), ") from '",
        file_src, "'"
      )
    )
  }
  
  df
}

#' @importFrom certeprojects project_set_file
parse_file_location <- function(filename, needed_extension, project_number) {
  if (is.null(project_number) || is.na(project_number) || isFALSE(project_number) || project_number %in% c(0, "")) {
    project_number <- NULL
  }
  needed_extension <- gsub("^[.]", "", needed_extension[1L])
  if (filename == ".") {
    filename <- "tbl"
  }
  if (needed_extension != "" & filename %unlike% paste0("[.](", paste0(needed_extension, collapse = "|"), ")$")) {
    filename <- paste0(filename, ".", needed_extension[1L])
  }
  if (!is.null(project_number) && filename %unlike% paste0("p", project_number, "|[A-Z]:/")) {
    # has no valid location yet, so include project number
    filename_proj <- project_set_file(filename, project_number = project_number)
    if (!is.na(filename_proj)) {
      filename <- filename_proj
    }
  }
  # remove invalid characters
  filename <- gsub("[?|<>|*]+", "", filename)
  filename
}

doc_requirement <- function(filetype, fn, pkg) {
  fn <- paste0("[", fn, "()]", collapse = " or ")
  paste0(ifelse(fn %like% "import", "Importing", "Exporting to"), " ", 
         filetype, " using ", fn, " requires the ", paste0("`", pkg, "`", collapse = " and "), 
         " package", ifelse(length(pkg) > 1, "s", ""), " to be installed")
}

#' @importFrom tibble rownames_to_column
rownames_1st_column <- function(object) {
  if (!all(rownames(object) == as.character(seq_len(NROW(object))))) {
    object <- rownames_to_column(object, var = "rownames")
    message("Note: Row names added as first column 'rownames'")
  }
  object
}

#' @importFrom certestyle format2
file_can_be_overwritten <- function(overwrite, filename) {
  if (!file.exists(filename)) {
    return(TRUE)
  }
  if (is.logical(overwrite)) {
    return(overwrite)
  }
  
  file_info <- file.info(filename)
  file_text <- paste0("Created: ", format2(file_info$ctime, "yyyy-mm-dd HH:MM:ss"), "\n",
                      "Changed: ", format2(file_info$mtime, "yyyy-mm-dd HH:MM:ss"), "\n",
                      "Size: ", size_humanreadable(file_info$size, decimal.mark = "."))
  if (base::interactive()) {
    q_text <- paste0("The file '", filename, "' already exists:\n",
                     file_text, "\n\n",
                     "Overwrite this file?")
    q <- tryCatch(rstudioapi::showQuestion(title = paste("File", basename(filename), "exists"),
                                           message = q_text,
                                           ok = "Yes",
                                           cancel = "No"),
                  error = function(e) NULL)
    if (is.null(q)) {
      q <- utils::askYesNo(q_text, default = TRUE, prompts = c("Yes", "No", "Cancel"))
    }
    if (isTRUE(q)) {
      # so the file exists - create a backup so it can be reverted back if export fails
      file.copy(from = filename, to = paste0(filename, ".certetoolbox_export"))
      # and remove the existing file
      try(file.remove2(filename), silent = TRUE)
    }
    return(isTRUE(q))
  } else {
    # non-interactive mode, make a permanent copy
    filename_new <- gsub("[.]([a-zA-Z0-9_-]+)$", paste0("_", format2(now(), "yyyymmdd-HHMMSS"), ".\\1"), filename)
    file.copy(from = filename, to = filename_new)
    # and remove the existing file
    try(file.remove2(filename), silent = TRUE)
    message("Original file ", filename, " existed, this file was renamed to ", filename_new, " before overwriting the original file.")
    return(TRUE)
  }
}

plot_export_result <- function(filename) {
  filename_old <- paste0(filename, ".certetoolbox_export")
  if (file.exists(filename)) {
    message(paste0("Exported plot to '",
                   tools::file_path_as_absolute(filename),
                   "' (", size_humanreadable(file.size(filename)), ")."))
  } else {
    # revert the old existing file (see the file_can_be_overwritten() function)
    if (file.exists(filename_old)) {
      file.copy(filename_old, filename, copy.mode = TRUE, copy.date = TRUE)
    }
    warning("Error while saving `", filename, "`.", call. = FALSE)
  }
  
  try(file.remove2(filename_old), silent = TRUE)
  if (file.exists(filename)) {
    return(invisible(tools::file_path_as_absolute(filename)))
  } else {
    return(invisible(NULL))
  }
}


# Export functions --------------------------------------------------------

#' Export Data Sets and Plots
#' 
#' These functions can be used to export data sets and plots. They invisibly return the object itself again, allowing for usage in pipes (except for the plot-exporting functions [export_pdf()], [export_png()] and [export_html()]). The functions work closely together with the `certeprojects` package to support Microsoft Planner project numbers.
#' @param object,plot the \R object to export
#' @param fn a manual export function, such as `haven::write_sas` to write SAS files. This function has to have the object as first argument and the future file location as second argument.
#' @param filename the full path of the exported file
#' @param project_number a Microsoft Planner project number
#' @param overwrite a [logical] value to indicate if an existing file must be overwritten. In [interactive mode][base::interactive()], this will be asked if the file exists. In non-interactive mode, this has a special default behaviour: the original file will be copied to `filename_datetime.ext` before overwriting the file. Exporting with existing files is always non-destructive: if exporting fails, the original, existing file will not be altered.
#' @param encrypt a [logical] value to indicate if the object must be encrypted before exporting using AES-GCM via [openssl::aes_gcm_encrypt()], providing authenticated encryption. Default is `TRUE` for `rds` files. This guarantees both confidentiality and integrity: the file cannot be read without the correct key, and any tampering will be detected automatically during decryption. The initialization vector (iv) will be a length-12 random raw vector.
#' @param key a character to be used as the encryption key. Internally, this is converted using [openssl::sha256()] to ensure a [raw] high-entropy key of length 32, suitable for AES-GCM. The default is `read_secret("tools.encryption_password")`.
#' @param ... arguments passed on to methods
#' @details The [export()] function can export to any file format, also with a manually set export function when passed on to the `fn` argument. This function `fn` has to have the object as first argument and the future file location as second argument. If `fn` is left blank, the `export_*` function will be used based on the filename.
#' @rdname export
#' @seealso [import()]
#' @export
#' @examples 
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' # export to two files: 'whole_file.rds' and 'first_ten_rows.xlsx'
#' starwars |>
#'   export_rds("whole_file") |>
#'   slice(1:10) |>
#'   export_xlsx("first_ten_rows")
#'   
#' # the above is equal to:
#' # starwars |>
#' #   export("whole_file.rds") |>
#' #   slice(1:10) |>
#' #   export("first_ten_rows.xlsx")
#' 
#' # RDS files are encrypted by default
#' x <- base::readRDS("whole_file.rds")
#' head(x)
#' # use decrypt_object() for manual decryption
#' x <- decrypt_object(x)
#' head(x)
#' 
#' # this goes automatically with import_rds():
#' x <- import_rds("whole_file.rds")
#' head(x)
#' 
#' 
#' # Apache's Feather format is column-based
#' # and allow for cross-language specific and fast file reading
#' starwars |> export_feather()
#' import("starwars.feather",
#'        col_select = starts_with("h")) |> 
#'   head()
#'   
#' 
#' # (cleanup)
#' file.remove("whole_file.rds")
#' file.remove("first_ten_rows.xlsx")
#' file.remove("starwars.feather")
export <- function(object,
                   filename = NULL,
                   project_number = project_get_current_id(ask = FALSE),
                   overwrite = NULL,
                   encrypt = !is.null(filename) && filename %like% "[.]rds$",
                   key = read_secret("tools.encryption_password"),
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
                filename_deparse = deparse(substitute(filename)),
                project_number = project_number,
                overwrite = overwrite,
                fn = fn,
                ...)
  } else {
    if (is.null(filename)) {
      filename <- deparse(substitute(filename))
    }
    if (filename %like% "[.]rds$") {
      export_rds(object = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 encrypt = encrypt,
                 key = key,
                 ...)
    } else if (filename %like% "[.]csv$") {
      export_csv(object = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]tsv$") {
      export_tsv(object = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]txt$") {
      export_txt(object = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]xlsx?$") {
      export_xlsx(object = object,
                  filename = filename,
                  project_number = project_number,
                  overwrite = overwrite,
                  ...)
    } else if (filename %like% "[.]sav$") {
      export_sav(object = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]feather$") {
      export_feather(object = object,
                     filename = filename,
                     project_number = project_number,
                     overwrite = overwrite,
                     ...)
      # } else if (filename %like% "[.]parquet$") {
      #   export_parquet(object = object,
      #                  filename = filename,
      #                  project_number = project_number,
      #                  overwrite = overwrite,
      #                  ...)
    } else if (filename %like% "[.]pdf$") {
      export_pdf(plot = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]png$") {
      export_png(plot = object,
                 filename = filename,
                 project_number = project_number,
                 overwrite = overwrite,
                 ...)
    } else if (filename %like% "[.]html$") {
      export_html(plot = object,
                  filename = filename,
                  project_number = project_number,
                  overwrite = overwrite,
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
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       encrypt = TRUE,
                       key = read_secret("tools.encryption_password"),
                       ...) {
  export_exec(object, "rds",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
              encrypt = encrypt,
              key = key,
              ...)
}

#' @rdname export
#' @inheritParams as_excel
#' @details The [export_xlsx()] and [export_excel()] functions use [`save_excel(as_excel(...))`][as_excel()] internally. **IMPORTANT**: these two functions can accept more than one [data.frame]. When naming the data sets, the names will become sheet names in the resulting Excel file. For a complete visual overview of supported table styles, see [as_excel()]. If the last value in `...` is a [character] of length 1 and `filename` is `NULL`, this value is assumed to be the filename.
#' @export
export_xlsx <- function(...,
                        filename = NULL,
                        project_number = project_get_current_id(ask = FALSE),
                        overwrite = NULL,
                        sheet_names = NULL,
                        autofilter = TRUE,
                        rows_zebra = TRUE,
                        cols_zebra = FALSE,
                        freeze_top_row = TRUE,
                        table_style = "TableStyleMedium2",
                        align = "center") {
  object <- list(...)
  if (length(object) > 1) {
    object_name <- "."
    # check if second value is filename
    if (is.null(filename) && is.character(object[[2]]) && length(object[[2]]) == 1) {
      filename <- object[[2]]
      object <- object[-2]
    }
    # check if third value is project number
    if (length(object) > 1 && is.null(project_number) && is.numeric(object[[length(object)]]) && length(object[[length(object)]]) == 1) {
      project_number <- object[[length(object)]]
      object <- object[-length(object)]
    }
  } else if (length(object) == 1) {
    object_name <- tryCatch(paste0(trimws(deparse(substitute(...))), collapse = ""), error = function(x) ".")
  }
  export_exec(object = object, "xlsx",
              filename = filename,
              filename_deparse = object_name,
              project_number = project_number,
              overwrite = overwrite,
              sheet_names = sheet_names,
              autofilter = autofilter,
              rows_zebra = rows_zebra,
              cols_zebra = cols_zebra,
              freeze_top_row = freeze_top_row,
              table_style = table_style,
              align = align)
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
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       na = "",
                       ...) {
  export_exec(object, "csv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
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
                        project_number = project_get_current_id(ask = FALSE),
                        overwrite = NULL,
                        na = "",
                        ...) {
  export_exec(object, "csv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
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
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       na = "",
                       ...) {
  export_exec(object, "tsv",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
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
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       sep = "\t",
                       na = "",
                       ...) {
  export_exec(object, "txt",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
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
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       ...) {
  check_is_installed("haven")
  export_exec(object, "sav",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
              compress = FALSE)
}

#' @rdname export
#' @export
export_spss <- export_sav 

#' @rdname export
#' @details `r doc_requirement("a Feather file", "export_feather", "arrow")`. [Apache Feather](https://arrow.apache.org/docs/python/feather.html) provides efficient binary columnar serialization for data sets, enabling easy sharing data across data analysis languages (such as between Python and R).
#' @export
export_feather <- function(object,
                           filename = NULL,
                           project_number = project_get_current_id(ask = FALSE),
                           overwrite = NULL,
                           ...) {
  check_is_installed("arrow")
  export_exec(object, "feather",
              filename = filename,
              filename_deparse = deparse(substitute(object)),
              project_number = project_number,
              overwrite = overwrite,
              ...)
}

#' #' @rdname export
#' #' @details `r doc_requirement("a Parquet file", "export_parquet", "arrow")`. [Apache Parquet](https://parquet.apache.org) is an open source, column-oriented data file format designed for efficient data storage and retrieval.
#' #' @export
#' export_parquet <- function(object,
#'                            filename = NULL,
#'                            project_number = project_get_current_id(ask = FALSE),
#'                            overwrite = NULL,
#'                            ...) {
#'   check_is_installed("arrow")
#'   export_exec(object, "parquet",
#'               filename = filename,
#'               filename_deparse = deparse(substitute(object)),
#'               project_number = project_number,
#'               overwrite = overwrite,
#'               ...)
#' }

#' @rdname export
#' @param size paper size, defaults to A5. Can be A0 to A7.
#' @param portrait portrait mode, defaults to `FALSE` (i.e., landscape mode)
#' @details `r doc_requirement("a PDF file", "export_pdf", "ggplot2")`. If the filename is left blank in [export_pdf()], [export_png()] or [export_html()], the title of `plot` will be used if it's available and the `certeplot2` package is installed, and a timestamp otherwise. **NOTE:** All export functions invisibly return `object` again, but the plotting functions invisibly return the file path
#' @importFrom certestyle format2
#' @export
export_pdf <- function(plot,
                       filename = NULL,
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
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
                                  project_number = project_number)
  if (!file_can_be_overwritten(overwrite, filename)) {
    return(invisible(NULL))
  }
  
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
    message("No valid value for size - using A4 as paper format.")
    height <- a4_height
    width <- a4_height / sqrt(2)
  }
  
  if (portrait == FALSE) {
    height.bak <- height
    height <- width
    width <- height.bak
  }
  
  ggplot2::ggsave(filename = filename,
                  device = grDevices::cairo_pdf,
                  width = width,
                  height = height,
                  units = "mm",
                  plot = plot,
                  ...)
  
  plot_export_result(filename)
}

#' @rdname export
#' @param width required width of the PNG file in pixels
#' @param height required height of the PNG file in pixels
#' @param dpi plot resolution, defaults to DPI set in `showtext` package
#' @details `r doc_requirement("a PNG file", "export_png", c("ggplot2", "showtext"))`.
#' @importFrom certestyle format2
#' @export
export_png <- function(plot,
                       filename = NULL,
                       project_number = project_get_current_id(ask = FALSE),
                       overwrite = NULL,
                       width = 1000,
                       height = 800,
                       dpi = NULL,
                       ...) {
  
  check_is_installed(c("ggplot2", "showtext"))
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
                                  project_number = project_number)
  if (!file_can_be_overwritten(overwrite, filename)) {
    return(invisible(NULL))
  }
  
  if (is.null(dpi)) {
    dpi <- showtext::showtext_opts()$dpi
  }
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
  
  plot_export_result(filename)
}

#' @rdname export
#' @details `r doc_requirement("an HTML file", "export_html", c("ggplot2", "htmltools"))`. The arguments put in `...` will be passed on to [plotly::layout()] if `plot` is not yet a Plotly object (but rather a `ggplot2` object), which of course then requires the `plotly` package to be installed as well.
#' @importFrom certestyle format2
#' @export
export_html <- function(plot,
                        filename = NULL,
                        project_number = project_get_current_id(ask = FALSE),
                        overwrite = NULL,
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
                                  project_number = project_number)
  if (!file_can_be_overwritten(overwrite, filename)) {
    return(invisible(NULL))
  }
  
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
  
  plot_export_result(filename)
}


# Import functions --------------------------------------------------------

#' Import Data Sets
#' 
#' These functions can be used to import data, from local or remote paths, or from the internet. They work closely with the `certeprojects` package to support Microsoft Planner project numbers. To support row names and older R versions, `import_*()` functions return plain [data.frame]s, not e.g. [tibble][tibble::tibble()]s.
#' @param filename the full path of the file to be imported, will be parsed to a [character], can also be a remote location (from http/https/ftp/ssh, GitHub/GitLab)
#' @param auto_transform transform the imported data with [auto_transform()]
#' @param project_number a Microsoft Planner project number
#' @param key a character to decrypt the file, see [export()] for explanation of `key`. For manual decryption, run [decrypt_object()].
#' @param ... arguments passed on to methods
#' @details `r doc_requirement("any unlisted filetype", "import", "rio")`.
#' @rdname import
#' @seealso [export()]
#' @export
#' @examples 
#' export_csv(iris)
#' import_csv("iris") |> head()
#' 
#' # the above is equal to:
#' # export(iris, "iris.csv")
#' # import("iris.csv") |> head()
#' 
#' 
#' # row names are also supported
#' export_csv(mtcars)
#' import_csv("mtcars") |> head()
#' 
#' 
#' # Apache's Feather format is column-based
#' # and allow for specific and fast file reading
#' library(dplyr, warn.conflicts = FALSE)
#' starwars |> export_feather()
#' import("starwars.feather",
#'        col_select = starts_with("h")) |> 
#'   head()
#'   
#' 
#' # (cleanup)
#' file.remove("iris.csv")
#' file.remove("mtcars.csv")
#' file.remove("starwars.feather")
import <- function(filename,
                   project_number = project_get_current_id(ask = FALSE),
                   auto_transform = TRUE,
                   encoding = "UTF-8",
                   key = read_secret("tools.encryption_password"),
                   ...) {
  if (!is.character(filename)) {
    filename <- deparse(substitute(filename))
  }
  if (filename %like% "[.]rds$") {
    import_rds(filename = filename,
               project_number = project_number,
               key = key,
               ...)
  } else if (filename %like% "[.]csv$" && (is.null(list(...)$sep) || identical(list(...)$sep, ","))) {
    import_csv(filename = filename,
               project_number = project_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]csv$" && identical(list(...)$sep, ";")) {
    import_csv2(filename = filename,
                project_number = project_number,
                auto_transform = auto_transform,
                ...)
  } else if (filename %like% "[.]tsv$") {
    import_tsv(filename = filename,
               project_number = project_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]txt$") {
    import_txt(filename = filename,
               project_number = project_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]xlsx?$") {
    import_xlsx(filename = filename,
                project_number = project_number,
                auto_transform = auto_transform,
                ...)
  } else if (filename %like% "[.]sav$") {
    import_sav(filename = filename,
               project_number = project_number,
               auto_transform = auto_transform,
               ...)
  } else if (filename %like% "[.]feather$") {
    import_feather(filename = filename,
                   project_number = project_number,
                   ...)
    # } else if (filename %like% "[.]parquet$") {
    #   import_parquet(filename = filename,
    #                  project_number = project_number,
    #                  ...)
  } else {
    import_exec(filename,
                extension = "",
                project_number = project_number,
                auto_transform = auto_transform,
                ...)
  }
}

#' @rdname import
#' @export
import_rds <- function(filename,
                       project_number = project_get_current_id(ask = FALSE),
                       key = read_secret("tools.encryption_password"),
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "rds",
              project_number = project_number,
              key = key,
              auto_transform = FALSE,
              ...)
}

#' @rdname import
#' @export
readRDS <- import_rds

#' @rdname import
#' @param sheet Excel sheet to import, defaults to first sheet
#' @param range a cell range to read from, allows typical Excel ranges such as "B3:D87" and "Budget!B2:G14"
#' @param skip number of first rows to skip
#' @inheritParams auto_transform
#' @details `r doc_requirement("an Excel file", c("import_xlsx", "import_excel"), "readxl")`.
#' @importFrom cleaner format_datetime
#' @export
import_xlsx <- function(filename,
                        project_number = project_get_current_id(ask = FALSE),
                        sheet = 1,
                        range = NULL,
                        auto_transform = TRUE,
                        datenames = "nl",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = dec_mark(),
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
              project_number = project_number,
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
              skip = skip,
              ...)
}

#' @rdname import
#' @export
import_excel <- import_xlsx

#' @rdname import
#' @param encoding Default encoding. This only affects how the file is read.
#' @importFrom cleaner format_datetime
#' @export
import_csv <- function(filename,
                       project_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       encoding = "UTF-8",
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "csv",
              sep = ",",
              project_number = project_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip,
              encoding = encoding,
              ...)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_csv2 <- function(filename,
                        project_number = project_get_current_id(ask = FALSE),
                        auto_transform = TRUE,
                        datenames = "nl",
                        dateformat = "yyyy-mm-dd",
                        timeformat = "HH:MM",
                        decimal.mark = ",",
                        big.mark = "",
                        timezone = "UTC",
                        na = c("", "NULL", "NA", "<NA>"),
                        skip = 0,
                        encoding = "UTF-8",
                        ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "csv",
              sep = ";",
              project_number = project_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip,
              encoding = encoding,
              ...)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_tsv <- function(filename,
                       project_number = project_get_current_id(ask = FALSE),
                       auto_transform = TRUE,
                       datenames = "nl",
                       dateformat = "yyyy-mm-dd",
                       timeformat = "HH:MM",
                       decimal.mark = ".",
                       big.mark = "",
                       timezone = "UTC",
                       na = c("", "NULL", "NA", "<NA>"),
                       skip = 0,
                       encoding = "UTF-8",
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "tsv",
              sep = "\t",
              project_number = project_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip,
              encoding = encoding,
              ...)
}

#' @rdname import
#' @importFrom cleaner format_datetime
#' @export
import_txt <- function(filename,
                       project_number = project_get_current_id(ask = FALSE),
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
                       encoding = "UTF-8",
                       ...) {
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "txt",
              sep = sep,
              project_number = project_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = format_datetime(dateformat),
              timeformat = format_datetime(timeformat),
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              skip = skip,
              encoding = encoding,
              ...)
}

#' @rdname import
#' @details `r doc_requirement("an SPSS file", c("import_sav", "import_spss"), "haven")`.
#' @export
import_sav <- function(filename,
                       project_number = project_get_current_id(ask = FALSE),
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
              project_number = project_number,
              auto_transform = auto_transform,
              datenames = datenames,
              dateformat = dateformat,
              timeformat = timeformat,
              decimal.mark = decimal.mark,
              big.mark = big.mark,
              timezone = timezone,
              na = na,
              ...)
}

#' @rdname import
#' @export
import_spss <- import_sav

#' @rdname import
#' @details `r doc_requirement("a Feather file", "import_feather", "arrow")`. [Apache Feather](https://arrow.apache.org/docs/python/feather.html) provides efficient binary columnar serialization for data sets, enabling easy sharing data across data analysis languages (such as between Python and R). Use the `col_select` argument (which supports the [tidyselect language][tidyselect::language]) for specific data selection to improve importing speed.
#' @param col_select columns to select, supports the [tidyselect language][tidyselect::language])
#' @importFrom dplyr everything
#' @export
import_feather <- function(filename,
                           project_number = project_get_current_id(ask = FALSE),
                           col_select = everything(),
                           ...) {
  check_is_installed("arrow")
  import_exec(filename,
              filename_deparse = deparse(substitute(filename)),
              extension = "feather",
              project_number = project_number,
              auto_transform = FALSE,
              col_select = col_select,
              ...)
}

#' @rdname import
#' @param object object to decrypt
#' @export
decrypt_object <- function(object, key = read_secret("tools.encryption_password")) {
  if (is.raw(object) && !is.null(attr(object, "iv"))) {
    # object was encrypted using openssl::aes_gcm_encrypt() in an export_* function
    check_is_installed("openssl")
    object <- openssl::aes_gcm_decrypt(object,
                                       key = openssl::sha256(charToRaw(key)),
                                       iv = attr(object, "iv"))
    tryCatch({
      object <- memDecompress(object, type = "gzip")
      object <- unserialize(object)
      message("AES-GCM encryption removed using provided `key`.")
    }, error = function(e) {
      warning("AES-GCM encryption could not be removed ('", conditionMessage(e), "'), returning raw vector.", call. = FALSE, immediate. = TRUE)
    })
  }
  object
}

#' #' @rdname import
#' #' @details `r doc_requirement("a Parquet file", "import_parquet", "arrow")`. [Apache Parquet](https://parquet.apache.org) is an open source, column-oriented data file format designed for efficient data storage and retrieval. Use the `col_select` argument (which supports the [tidyselect language][tidyselect::language]) for specific data selection to improve importing speed.
#' #' @importFrom dplyr everything
#' #' @export
#' import_parquet <- function(filename,
#'                            project_number = project_get_current_id(ask = FALSE),
#'                            col_select = everything(),
#'                            ...) {
#'   check_is_installed("arrow")
#'   import_exec(filename,
#'               filename_deparse = deparse(substitute(filename)),
#'               extension = "parquet",
#'               project_number = project_number,
#'               auto_transform = FALSE,
#'               col_select = col_select)
#' }

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

#' @rdname export
#' @param na replacement character for empty values (default: `""`)
#' @param header (for [export_clipboard()]) use column names as header (default: `TRUE`)
#' @param quote (for [export_clipboard()]) use quotation marks (default: `FALSE`)
#' @param decimal.mark (for [export_clipboard()]) character to use for decimal numbers, defaults to [dec_mark()]
#' @details `r doc_requirement("the clipboard", "export_clipboard", "clipr")`. The function allows any object (also other than [data.frame]s) and is only limited to the available amount of RAM memory.
#' @importFrom certestyle format2
#' @export
export_clipboard <- function(object,
                             sep = "\t",
                             na = "",
                             header = TRUE,
                             quote = FALSE,
                             decimal.mark = dec_mark(),
                             ...) {
  check_is_installed("clipr")
  clipr::write_clip(content = object,
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

#' @rdname import
#' @param sep character to separate values in a row
#' @param header use first row as header
#' @param startrow first row to start importing
#' @details `r doc_requirement("the clipboard", "import_clipboard", "clipr")`.
#' @export
import_clipboard <- function(sep = "\t",
                             header = TRUE,
                             startrow = 1,
                             auto_transform = TRUE,
                             datenames = "nl",
                             dateformat = "yyyy-mm-dd",
                             timeformat = "HH:MM",
                             decimal.mark = dec_mark(),
                             big.mark = "",
                             timezone = "UTC",
                             na = c("", "NULL", "NA", "<NA>"),
                             ...) {
  check_is_installed("clipr")
  df <- clipr::read_clip_tbl(sep = sep,
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
  message(
    paste0(
      "Imported data set (",
      format2(NROW(df)), "x", format2(NCOL(df)), ") from clipboard")
  )
  df
}

#' @rdname import
#' @details `r doc_requirement("mail attachments", "import_mail_attachment", "certemail")`. It calls [`download_mail_attachment()`][certemail::download_mail_attachment()] internally and saves the attachment to a temporary folder.
#' @param search see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param search_subject see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param search_from see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param search_when see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param search_attachment see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param n see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @inheritParams certemail::download_mail_attachment
#' @param sort see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @export
import_mail_attachment <- function(search = "hasattachment:yes",
                                   search_subject = NULL,
                                   search_from = NULL,
                                   search_when = NULL,
                                   search_attachment = NULL,
                                   n = 5,
                                   sort = "received desc",
                                   account = certemail::connect_outlook(),
                                   auto_transform = TRUE,
                                   sep = ",",
                                   ...) {
  check_is_installed("certemail")
  path <- suppressMessages(certemail::download_mail_attachment(
    path = tempdir(),
    filename = "{original}",
    search = search,
    search_subject = search_subject,
    search_from = search_from,
    search_when = search_when,
    search_attachment = search_attachment,
    n = n,
    sort = sort,
    overwrite = TRUE,
    account = account))
  
  if (file.exists(path)) {
    if (path %like% "[.]csv$") {
      import(path, auto_transform = auto_transform, sep = sep, ...)
    } else {
      import(path, auto_transform = auto_transform, ...)
    }
  } else {
    stop("Importing attachment failed")
  }
}

#' @rdname import
#' @param url remote location of any data set, can also be a (non-raw) GitHub/GitLab link
#' @details The [import_url()] function tries to download the file first, after which it will be imported using the appropriate `import_*()` function.
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
              extension = tools::file_ext(url),
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

#' @rdname import
#' @param account a Teams account from Azure or an `AzureAuth` Microsoft 365 token, e.g. retrieved with [certeprojects::connect_teams()]
#' @inheritParams certeprojects::teams_download_file
#' @importFrom certeprojects teams_download_file connect_teams
#' @details The [import_teams()] function uses [certeprojects::teams_download_file()] to provide an interactive way to select a file in any Team, to download the file, and to import the file using the appropriate `import_*()` function.
#' @export
import_teams <- function(full_teams_path = NULL,
                         account = connect_teams(),
                         auto_transform = TRUE,
                         sep = ",",
                         datenames = "en",
                         dateformat = "yyyy-mm-dd",
                         timeformat = "HH:MM",
                         decimal.mark = ".",
                         big.mark = "",
                         timezone = "UTC",
                         na = c("", "NULL", "NA", "<NA>"),
                         skip = 0) {
  
  temp <- paste0(tempfile(), "/")
  dir.create(temp)
  
  teams_download_file(full_teams_path = full_teams_path, account = account, destination_dir = temp, overwrite = TRUE)
  filename <- list.files(temp, full.names = TRUE)[1]
  
  import_exec(filename,
              filename_deparse = basename(filename),
              extension = tools::file_ext(filename),
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

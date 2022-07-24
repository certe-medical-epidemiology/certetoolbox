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
#' @param decimal.mark (for [export_clipboard()]) character to use for decimal numbers
#' @details `r doc_requirement("the clipboard", "export_clipboard", "clipr")`. The function allows any object (also other than [data.frame]s) and is only limited to the available amount of RAM memory.
#' @importFrom certestyle format2
#' @export
export_clipboard <- function(object,
                             sep = "\t",
                             na = "",
                             header = TRUE,
                             quote = FALSE,
                             decimal.mark = ",",
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
                             decimal.mark = ",",
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
#' @param sort see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @param account see [`download_mail_attachment()`][certemail::download_mail_attachment()]
#' @export
import_mail_attachment <- function(search = "hasattachment:yes",
                                   search_subject = NULL,
                                   search_from = NULL,
                                   search_when = NULL,
                                   search_attachment = NULL,
                                   n = 5,
                                   sort = "received desc",
                                   account = NULL,
                                   auto_transform = TRUE,
                                   sep = ",",
                                   ...) {
  check_is_installed("certemail")
  if (missing(account)) {
    account <- certemail::connect_outlook365()
  }
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
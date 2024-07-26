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
#' @details `r doc_requirement("the clipboard", "export_clipboard", "clipr")`. The function allows any object (also other than [data.frame]s) to be exported to the clipboard and is only limited to the available amount of RAM memory.
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
  message(paste0("Exported object (",
                 format2(NROW(object)), pkg_env$cross_icon, format2(NCOL(object)),
                 ") to clipboard."))
  return(invisible(object))
}

#' @rdname export
#' @param full_teams_path path in Teams to export object to. Can be left blank to use interactive folder picking mode in the console.
#' @param account a Teams account from Azure or an `AzureAuth` Microsoft 365 token, e.g. retrieved with [certeprojects::connect_teams()]
#' @details `r doc_requirement("Microsoft Teams", "export_teams", "AzureGraph")`. The function allows any object (also other than [data.frame]s) to be exported to any Team channel. The filename set in `filename` will determine the exported file type and defaults to an [RDS file][saveRDS()].
#' @importFrom certeprojects teams_upload_file connect_teams
#' @importFrom certestyle format2
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' # ---- Microsoft Teams support -------------------------------------------
#' 
#' # IMPORTING
#' 
#' # import from Teams by picking a folder interactively from any Team
#' x <- import_teams()
#' 
#' # to NOT pick a Teams folder (e.g. in non-interactive mode), set `full_teams_path`
#' x <- import_teams(full_teams_path = "MyTeam/MyChannel/MyFolder/MyFile.xlsx")
#' 
#' 
#' # EXPORTING
#' 
#' # export to Teams by picking a folder interactively from any Team
#' mtcars |> export_teams()
#' 
#' # the default is RDS, but you can set `filename` to specify yourself
#' mtcars |> export_teams("mtcars.xlsx")
#' 
#' # to NOT pick a Teams folder (e.g. in non-interactive mode), set `full_teams_path`
#' mtcars |> export_teams("mtcars.xlsx", full_teams_path = "MyTeam/MyChannel/MyFolder")
#' mtcars |> export_teams(full_teams_path = "MyTeam/MyChannel/MyFolder")
#' 
#' }
export_teams <- function(object,
                         filename = NULL,
                         full_teams_path = NULL,
                         account = connect_teams(),
                         ...) {
  if (!is.null(filename)) {
    ext <- tools::file_ext(filename)
    filename <- paste0(tempdir(), "/", filename)
  } else {
    ext <- "rds"
    if (deparse(substitute(object)) == ".") {
      filename <- paste0(tempdir(), "/tbl.rds")
    } else {
      filename <- paste0(tempdir(), "/", deparse(substitute(object)))
    }
  }
  out <- export_exec(object, ext,
                     filename = filename,
                     filename_deparse = deparse(substitute(object)),
                     project_number = NULL,
                     overwrite = TRUE,
                     ...)
  file_path <- attributes(out)$filename
  if (!file.exists(file_path)) {
    stop("Object not exported to Microsoft Teams.")
  }
  
  teams_upload_file(file_path = file_path,
                    full_teams_path = full_teams_path,
                    account = account,
                    file_name = basename(file_path))
  message(paste0("Exported object (",
                 format2(NROW(object)), pkg_env$cross_icon, format2(NCOL(object)),
                 ") to Microsoft Teams."))
  return(invisible(structure(object, filename = tools::file_path_as_absolute(file_path))))
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
      format2(NROW(df)), pkg_env$cross_icon, format2(NCOL(df)), ") from clipboard")
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
              project_number = NULL,
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
#' @examples
#' 
#' \dontrun{
#' 
#' # ---- Microsoft Teams support -------------------------------------------
#' 
#' # IMPORTING
#' 
#' # import from Teams by picking a folder interactively from any Team
#' x <- import_teams()
#' 
#' # to NOT pick a Teams folder (e.g. in non-interactive mode), set `full_teams_path`
#' x <- import_teams(full_teams_path = "MyTeam/MyChannel/MyFolder/MyFile.xlsx")
#' 
#' 
#' # EXPORTING
#' 
#' # export to Teams by picking a folder interactively from any Team
#' mtcars |> export_teams()
#' 
#' # the default is RDS, but you can set `filename` to specify yourself
#' mtcars |> export_teams("mtcars.xlsx")
#' 
#' # to NOT pick a Teams folder (e.g. in non-interactive mode), set `full_teams_path`
#' mtcars |> export_teams("mtcars.xlsx", full_teams_path = "MyTeam/MyChannel/MyFolder")
#' mtcars |> export_teams(full_teams_path = "MyTeam/MyChannel/MyFolder")
#' 
#' }
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
              project_number = NULL,
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

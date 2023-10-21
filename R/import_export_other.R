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

#' @rdname import
#' @param teams_path a full path in Teams, **including the Team name and the channel name**. Leave blank to use interactive mode, which allows file picking from a list in the console.
#' @param account a Teams account from Azure or an `AzureAuth` Microsoft 365 token, e.g. retrieved with [certeprojects::connect_teams()]
#' @importFrom certeprojects connect_teams
#' @importFrom dplyr case_when
#' @details The [import_teams()] function provides an interactive way to select a file in any Team, any channel. It then tries to download the file, after which it will be imported using the appropriate `import_*()` function.
#' @export
import_teams <- function(teams_path = NULL,
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
  if (inherits(account, "ms_team")) {
    token <- account$token
  } else if (inherits(account, "AzureToken")) {
    token <- account
  }
  if (!inherits(token, "AzureToken")) {
    stop("Class of `token` is not 'AzureToken'")
  }
  
  if (!is.null(teams_path)) {
    path_parts <- strsplit(teams_path, split = "/", fixed = TRUE)[[1]]
    message("Retrieving Team '", path_parts[1], "'...", appendLF = FALSE)
    login <- AzureGraph::create_graph_login(token = token)
    group <- login$get_group(name = path_parts[1])
    drive <- group$get_drive()
    item <<- drive$get_item(paste0(path_parts[2:length(path_parts)], collapse = "/"))
    message("OK.")
    message("Downloading file '", item$properties$name, "' (", size_humanreadable(item$properties$size), ") to temporary folder...", appendLF = FALSE)
    filename <- paste0(tempdir(), "/", path_parts[length(path_parts)])
    drive$download_file(srcid = item$properties$id, dest = filename, overwrite = TRUE)
    message("OK.")
    
  } else {
    
    if (!interactive()) {
      stop("File picking only works in interactive mode. Set `teams_path` in non-interactive mode")
    }
    
    message("Retrieving list of Teams within ", token$tenant, "...", appendLF = FALSE)
    login <- AzureGraph::create_graph_login(token = token)
    groups <- login$list_groups()
    groups_names <- sapply(groups, function(g) g$properties$displayName)
    message("OK, n = ", length(groups), ".")
    continue <- FALSE
    while (!isTRUE(continue)) {
      searchterm <- readline("Search for Team name (allows regex): ")
      found_groups <- which(groups_names %like% searchterm)
      if (length(found_groups) == 0) {
        levensthein <- as.double(adist(searchterm, groups_names, counts = FALSE, ignore.case = TRUE))
        found_groups <- order(levensthein)[1]
      }
      continue <- utils::askYesNo(paste0("Found Team '", groups_names[found_groups[1]], "'. Continue?"))
      if (is.na(continue)) {
        # has chosen 'cancel'
        return(invisible())
      }
    }
    get_icon <- function(ff) {
      case_when(ff$isdir ~ "🗂️",  # "📁",
                ff$name %like% "xlsx?$" ~ "📊",
                ff$name %like% "(docx?|pdf)$" ~ "📝",
                ff$name %like% "pptx?$" ~ "📉",
                ff$name %like% "csv$" ~ "🧾",
                ff$name %like% "zip$" ~ "📦",
                ff$name %like% "(jpe?g|bmp|png|gif)$" ~ "🖼️",
                ff$name %like% "(eml|msg)$" ~ "✉️️",
                TRUE ~ "📄")
    }
    message("Retrieving file list...", appendLF = FALSE)
    group <- groups[[found_groups[1]]]
    drive <- group$get_drive()
    files <- drive$list_files() 
    file_choices <- paste0(get_icon(files), " ",
                           trimws(files$name),
                           " (", size_humanreadable(files$size, decimal.mark = "."), ")")
    message("OK.")
    picked <- utils::menu(choices = file_choices,
                          graphics = FALSE,
                          title = "Choose a file or folder (0 to Cancel):")
    if (picked == 0) {
      return(invisible())
    }
    picked <- files[picked, ]
    dive_levels <- picked$name
    is_file <- !picked$isdir
    
    while (!is_file) {
      cat("Current folder:\n  ", groups_names[found_groups[1]], sep = "")
      if (length(dive_levels) > 0) {
        cat(paste0("\n", strrep("   ", seq_len(length(dive_levels))), "↳ ", dive_levels), "\n\n")
      } else {
        cat("\n\n")
      }
      searchpath <- paste0(dive_levels, collapse = "/")
      files <- drive$list_files(searchpath) 
      file_choices <- paste0(get_icon(files), " ",
                             trimws(files$name),
                             " (", size_humanreadable(files$size, decimal.mark = "."), ")")
      if (searchpath != "") {
        file_choices <- c(file_choices, "↩ Go back to previous folder...")
      }
      picked <- utils::menu(choices = file_choices,
                            graphics = FALSE,
                            title = "Choose a file or folder (0 to Cancel):")
      if (picked == 0) {
        # has chosen Cancel
        return(invisible())
      }
      if (searchpath != "" && picked == length(file_choices)) {
        # return one level
        dive_levels <- dive_levels[seq_len(length(dive_levels) - 1)]
      } else {
        picked <- files[picked, ]
        dive_levels <- c(dive_levels, picked$name)
        is_file <- !picked$isdir
      }
    }
    message("Full path for reference: ", paste0(groups_names[found_groups[1]], "/", paste0(dive_levels, collapse = "/")))
    message("Downloading file '",  picked$name, "' (", size_humanreadable(picked$size), ") to temporary folder...", appendLF = FALSE)
    filename <- paste0(tempdir(), "/", picked$name)
    drive$download_file(srcid = picked$id, dest = filename, overwrite = TRUE)
    message("OK.")
  }
  
  import_exec(filename,
              filename_deparse = basename(filename),
              extension = gsub(".+[.](.*)$", "\\1", basename(filename)),
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

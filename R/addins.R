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

# No export, no Rd
#' @importFrom rstudioapi getActiveDocumentContext insertText modifyRange document_range document_position
addin_insert_like <- function() {
  # we want Shift + Ctrl/Cmd + L to iterate over %like%, %unlike%, %like_case%, and %unlike_case%
  
  context <- getActiveDocumentContext()
  current_row <- context$selection[[1]]$range$end[1]
  current_col <- context$selection[[1]]$range$end[2]
  current_row_txt <- context$contents[current_row]
  if (is.null(current_row) || current_row_txt %unlike% "%(un)?like") {
    insertText(" %like% ")
    return(invisible())
  }
  
  pos_preceded_by <- function(txt) {
    if (tryCatch(substr(current_row_txt, current_col - nchar(trimws(txt, which = "right")), current_col) == trimws(txt, which = "right"),
                 error = function(e) FALSE)) {
      return(TRUE)
    }
    tryCatch(substr(current_row_txt, current_col - nchar(txt), current_col) %like% paste0("^", txt),
             error = function(e) FALSE)
  }
  replace_pos <- function(old, with) {
    modifyRange(document_range(document_position(current_row, current_col - nchar(old)),
                               document_position(current_row, current_col)),
                text = with,
                id = context$id)
  }
  
  if (pos_preceded_by(" %like% ")) {
    replace_pos(" %like% ", with = " %unlike% ")
  } else if (pos_preceded_by(" %unlike% ")) {
    replace_pos(" %unlike% ", with = " %like_case% ")
  } else if (pos_preceded_by(" %like_case% ")) {
    replace_pos(" %like_case% ", with = " %unlike_case% ")
  } else if (pos_preceded_by(" %unlike_case% ")) {
    replace_pos(" %unlike_case% ", with = " %like% ")
  } else {
    insertText(" %like% ")
  }
}

# No export, no Rd
#' @importFrom rstudioapi insertText
addin_insert_in <- function() {
  insertText(" %in% ")
}

# No export, no Rd
#' @importFrom rstudioapi showPrompt insertText
addin_insert_section <- function() {
  lbl <- showPrompt(title = "Label of section",
                    message = "Section label:")
  if (is.null(lbl)) {
    return(invisible())
  }
  width <- min(options()$width, 100)
  user <- paste("#" , format2(today(), "yyyy-mm-dd"), "/", Sys.info()[["user"]])
  lbl <- trimws(lbl)
  label <- paste("#", lbl, strrep("-", times = width - nchar(lbl) - 4))
  insertText(paste0("\n", label, "\n", user, "\n\n"))
}

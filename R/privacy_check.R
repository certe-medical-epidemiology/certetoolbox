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

#' Check Privacy of Plain Files
#' 
#' Checks if files contain privacy sensitive data and moves them to a 'vault folder' if this is the case.
#' @param path path to check 
#' @param vault path to vault
#' @importFrom readr read_tsv write_tsv
#' @importFrom dplyr left_join mutate distinct
#' @importFrom certestyle format2
#' @export
privacy_check <- function(path = getwd(), vault = paste0(path, "/vault"), suspicion = "(postcode|bsn|geboortedatum)") {
  vault <- tools::file_path_as_absolute(vault)
  if (!dir.exists(vault)) {
    dir.create(vault)
  }
  
  log_path <- paste0(vault, "/_privacy_log.txt")
  if (!file.exists(log_path)) {
    log <- data.frame(file = character(0),
                      last_checked = Sys.time()[0])
  } else {
    log <- suppressWarnings(suppressMessages(read_tsv(log_path, show_col_types = FALSE)))
  }
  
  message("Vault folder: '", vault, "'\n",
          "Log path:     '", tools::file_path_as_absolute(log_path), "'")
  
  suspects <- list.files(path = path,
                         pattern = "[.](csv|tsv|txt|xlsx?|rds)$",
                         recursive = TRUE,
                         ignore.case = TRUE,
                         full.names = TRUE)
  suspects <- vapply(FUN.VALUE = character(1), suspects, tools::file_path_as_absolute, USE.NAMES = FALSE)
  suspects <- suspects[suspects %unlike% vault]
  
  to_check <- data.frame(file = suspects) |>
    left_join(log, by = "file") |>
    mutate(modified = file.mtime(file)) |> 
    filter(is.na(last_checked) | last_checked < modified)
  
  for (i in seq_len(nrow(to_check))) {
    f <- to_check[i, "file", drop = TRUE]
    
    df <- tryCatch(suppressWarnings(suppressMessages(import(filename = f, card_number = NULL, auto_transform = FALSE))),
                   error = function(x) NULL)
    
    is_ok <<- TRUE
    if (!is.null(df)) {
      suspicious_cols <- which(colnames(df) %like% suspicion)
      if (length(suspicious_cols) > 0) {
        for (susp in suspicious_cols) {
          if (!all(is.na(df[, susp, drop = TRUE]))) {
            is_ok <<- FALSE
          }
        }
      }
    } else {
      message("** FILE ", f, " CANNOT BE READ **")
    }
    
    if (!is_ok) {
      file.rename(from = f, to = paste0(vault, "/", format2(Sys.Date(), "yymmdd"), "/", basename(f)))
      message("** MOVING FILE ", f, " TO VAULT **")
    }
    
    # add to log file
    log <- log |> 
      bind_rows(data.frame(file = tools::file_path_as_absolute(f), last_checked = Sys.time()))
  }
  
  log <- log |> 
    arrange(file, desc(last_checked)) |> 
    distinct(file, .keep_all = TRUE)
  
  write_tsv(log, tools::file_path_as_absolute(log_path), append = FALSE)
  message("Done.")
}

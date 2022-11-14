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
#' Checks if files contain privacy sensitive data and moves them to a 'vault folder' it this is the case.
#' @param path path to check 
#' @param vault path to vault
#' @importFrom readr read_csv
#' @export
privacy_check <- function(path = getwd(), vault = paste0(path, "/vault")) {
  if (!dir.exists(vault)) {
    dir.create(vault)
  }
  
  log_path <- paste0(vault, "/_privacy_log.txt")
  if (!file.exists(log_path)) {
    log <- data.frame(file = character(0),
                      last_checked = Sys.time()[0])
  } else {
    log <- readr::read_tsv(log_path)
  }
  
  suspects <- list.files(path = path,
                         pattern = "[.](csv|tsv|txt|xlsx?|rds)$",
                         recursive = TRUE,
                         ignore.case = TRUE,
                         full.names = TRUE)
  suspects <- suspects[suspects %unlike% vault]
  df <- data.frame(file = suspects) |>
    left_join(log) |>
    mutate(modified = file.mtime(file))
  df
}

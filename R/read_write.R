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

#' Read Certe Secret From File
#' 
#' This function reads from a local or remote YAML file, as set in the environmental variable `"secrets_file"`.
#' @param property the property to read, case-sensitive
#' @details This function will return a vector of length > 1 if the property value contains comma's. The property name and value have to be separated with a colon (`:`).
#' @importFrom readr read_lines
#' @export
#' @examples 
#' # for this example, a temporary 'secrets' file
#' my_secrets_file <- tempfile(fileext = ".yaml")
#' Sys.setenv(secrets_file = my_secrets_file)
#' writeLines("tenant_id: 8fb3c03060e02e89", my_secrets_file)
#' writeLines("default_users: user_1, user_2", my_secrets_file)
#' 
#' read_secret("tenant_id")
#' read_secret("default_users")
read_secret <- function(property) {
  file <- Sys.getenv("secrets_file")
  if (file == "" || !file.exists(file)) {
    stop("Environmental variable 'secrets_file' not set")
  }
  file_lines <- lapply(strsplit(read_lines(Sys.getenv("secrets_file")), ":"), trimws)
  # set name to list item
  names(file_lines) <- sapply(file_lines, function(l) l[1])
  # strip name from vector
  file_lines <- lapply(file_lines, function(l) l[c(2:length(l))])
  # get property
  out <- file_lines[[property]]
  if (is.null(out)) {
    return(NULL)
  }
  # split on comma
  trimws(strsplit(out, ",")[[1]])
}

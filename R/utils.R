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

globalVariables(c(".",
                  ".row_index",
                  ".row_original",
                  "id_col",
                  "Identifier",
                  "last_checked",
                  "modified",
                  "names_col",
                  "SearchPriority",
                  "ShortTitle",
                  "Summary",
                  "Title",
                  "transmute",
                  "values_col"))

pkg_env <- new.env(hash = FALSE)
pkg_env$temp <- list()
pkg_env$cross_icon <- if (isTRUE(base::l10n_info()$`UTF-8`)) " \u00d7 " else " x "

file.remove2 <- function(x, ...) {
  if (file.exists(x)) {
    file.remove(x)
  }
}

#' @importFrom lubridate today
#' @export
lubridate::today

#' @importFrom lubridate now
#' @export
lubridate::now

#' @importFrom certestyle dec_mark
#' @export
certestyle::dec_mark

#' @importFrom certestyle big_mark
#' @export
certestyle::big_mark

#' @importFrom certeprojects project_get_current_id
#' @export
certeprojects::project_get_current_id

#' @importFrom magrittr extract
#' @export
magrittr::extract

#' @importFrom magrittr extract2
#' @export
magrittr::extract2

#' @importFrom magrittr add
#' @export
magrittr::add

#' @importFrom magrittr subtract
#' @export
magrittr::subtract

#' @importFrom magrittr multiply_by
#' @export
magrittr::multiply_by

#' @importFrom magrittr raise_to_power
#' @export
magrittr::raise_to_power

#' @importFrom magrittr divide_by
#' @export
magrittr::divide_by


is_installed <- function(pkgs) {
  isTRUE(check_is_installed(pkgs))
}

check_is_installed <- function(pkgs) {
  to_install <- pkgs[which(!pkgs %in% rownames(utils::installed.packages(.libPaths())))]
  if (length(to_install) > 0) {
    if (interactive()) {
      # ask to install
      choice <- utils::askYesNo(paste0("Package(s) required but not installed: ",
                                       paste0("'", to_install, "'", collapse = ", "), ". ",
                                       "Install now?"))
    } else {
      choice <- FALSE
    }
    if (isTRUE(choice)) {
      utils::install.packages(to_install,
                              repos = c(options()$repos,
                                        "https://certe-medical-epidemiology.r-universe.dev"))
      # try again:
      is_installed(pkgs)
    } else {
      stop("Required package(s) ",
           paste0("'", to_install, "'", collapse = ", "), 
           " not installed", call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}

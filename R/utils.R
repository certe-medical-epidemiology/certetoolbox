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

get_external_function <- function(name, pkg, error_on_fail = TRUE) {
  # This function is a hacky way to NOT depend on another package, but DO use its functions.
  # For example, export.pdf() uses a 'certeplot2' function, but it's not possible to depend
  # on it because that package already depend on this package. 
  if (error_on_fail == TRUE) {
    check_is_installed(pkg)
  }
  tryCatch(base::getExportedValue(name = name, ns = base::asNamespace(pkg)), 
           error = function(e) {
             if (isTRUE(error_on_fail)) {
               stop(pkg, "::", name, "() does not exist.", 
                    call. = FALSE)
             }
             else {
               return(NULL)
             }
           })
}

is_installed <- function(pkgs) {
  isTRUE(check_is_installed(pkgs))
}

check_is_installed <- function(pkgs) {
  to_install <- pkgs[which(!pkgs %in% rownames(utils::installed.packages()))]
  if (length(to_install) > 0) {
    # ask to install
    choice <- utils::askYesNo(paste0("Package(s) required but not installed: ",
                                     paste0("'", to_install, "'", collapse = ", "), ". ",
                                     "Install now?"))
    if (isTRUE(choice)) {
      utils::install.packages(to_install)
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

project_get_current_id <- get_external_function("project_get_current_id",
                                                "certeprojects",
                                                error_on_fail = FALSE)
if (is.null(project_get_current_id)) {
  project_get_current_id <- function(...) NULL
}

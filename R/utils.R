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

globalVariables(c("."))
pkg_env <- new.env(hash = FALSE)
pkg_env$temp <- list()

#' @importFrom dplyr `%>%`
#' @export
dplyr::`%>%`

get_external_function <- function(name, pkg, error_on_fail = TRUE) {
  # This function is a hacky way to NOT depend on another package, but DO use its functions.
  # For example, export.pdf() uses a 'certeplot2' function, but it's not possible to depend
  # on it because that package already depends on this package. 
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

#' @importFrom rstudioapi getSourceEditorContext showPrompt
project_get_current_id <- function(ask = NULL) {

  # this function was copied from certeprojects, but try the installed certeprojects first
  from_certeprojects <- get_external_function("project_get_current_id", "certeprojects", error_on_fail = FALSE)
  if (!is.null(from_certeprojects)) {
    return(from_certeprojects(ask = ask))
  }

  # first try project number from full file location:
  # /folder/p123 Name.Rmd
  # /p123 folder/Name.Rmd
  fix_id <- function(id) {
    id <- gsub("[^0-9]", "", id)
    if (all(is.na(id) | length(id) == 0) || identical(id, "")) {
      NULL
    } else {
      id
    }
  }
  asked <- FALSE
  if (interactive()) {
    path <- getSourceEditorContext()$path
    if (is.null(path) && is.null(ask)) {
      id <- showPrompt("Project Number", "Enter Project Number:")
      return(fix_id(id))
    }
  } else {
    # for markdown
    path <- getwd()
  }
  parts <- tryCatch(unlist(strsplit(path, "[^a-zA-Z0-9]")), error = function(e) NULL)
  if (is.null(parts)) {
    return(NULL)
  }
  id <- parts[parts %like% "^p[0-9]+$"][1]
  if (all(length(id) == 0 | is.na(id)) && interactive() && is.null(ask)) {
    id <- showPrompt("Project Number", "Enter Project Number:")
    asked <- TRUE
  }
  
  if (identical(ask, TRUE) && asked == FALSE) {
    id <- showPrompt("Project Number", "Enter Project Number:", ifelse(length(id) > 0, paste0("p", fix_id(id)), ""))
    if (is.null(id) || all(is.na(id))) {
      return(NULL)
    }
  }
  fix_id(id)
}

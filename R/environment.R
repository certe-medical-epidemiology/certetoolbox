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
# ===========================================================

#' Temporarily Remember Objects
#'
#' Can be used in dplyr-syntax to remember values and objects for later use. Objects are (temporarily) stored in the `certetoolbox` package environment.
#' @rdname remember_recall
#' @param .data [data.frame]
#' @param ... value(s) to be remembered
#' @param x value to be recalled
#' @param delete a [logical] to indicate whether the delete value after recalling
#' @details values can be saved with **[remember()]** and recalled (and deleted) with **[recall()]**.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' x <- mtcars %>% remember(nrow(.)) 
#' recall()
#' recall() # value removed
#' x <- mtcars %>% remember(n = nrow(.)) 
#' recall(n)
#' recall(n) # value removed
#' 
#' \dontrun{
#'  tbl %>%
#'    filter(...) %>%
#'    remember(rows = nrow(.)) %>%
#'    group_by(...) %>%
#'    summarise(...) %>%
#'    plot2(title = "Test",
#'          subtitle = paste("n =", recall(rows)))
#' }
remember <- function(.data, ...) {
  if (!is.data.frame(.data)) {
    stop("remember() must start with a data set (e.g., after a pipe)")
  }
  dots <- list(...)
  dots_names <- names(dots)
  if (is.null(dots_names)) {
    dots_names <- ""
  }
  if (sum(dots_names == "") > 1) {
    stop("The length of objects in remember() must be of length 1 if not named", call. = FALSE)
  }
  dots_names[dots_names == ""] <- "remember_temp"
  names(dots) <- dots_names
  pkg_env$temp <- c(pkg_env$temp, dots)
  .data
}

#' @rdname remember_recall
#' @export
recall <- function(x = NULL, delete = TRUE) {
  x_deparsed <- deparse(substitute(x))
  if (identical(x_deparsed, "NULL") && is.null(x)) {
    x_name <- "remember_temp"
  } else {
    x_name <- x_deparsed
  }
  x_val <- pkg_env$temp[[x_name]]
  if (isTRUE(delete)) {
    pkg_env$temp <- pkg_env$temp[which(names(pkg_env$temp) != x_name)]
  }
  x_val
}

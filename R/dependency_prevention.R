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

# These functions are to prevent dependency on larger packages.
# They all start with a dot to prevent namespace collisions.

#' Case When
#' 
#' Inspired by [dplyr::case_when()], but allows different lengths of outcomes (just like [ifelse()]).
#' @param ... a sequence of two-sided formulas
#' @rdname case_when
#' @source Code copied from [poorman::case_when()] under same license.
#' @export
#' @examples
#' x <- 1:50
#' .case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
.case_when <- function (...) {
  fs <- list(...)
  lapply(fs, function(x) if (class(x) != "formula") 
    stop("`case_when()` requires formula inputs."))
  n <- length(fs)
  if (n == 0L) 
    stop("No cases provided.")
  
  validate_case_when_length <- function (query, value, fs) {
    lhs_lengths <- lengths(query)
    rhs_lengths <- lengths(value)
    all_lengths <- unique(c(lhs_lengths, rhs_lengths))
    if (length(all_lengths) <= 1L) 
      return(all_lengths[[1L]])
    non_atomic_lengths <- all_lengths[all_lengths != 1L]
    len <- non_atomic_lengths[[1L]]
    if (length(non_atomic_lengths) == 1L) 
      return(len)
    inconsistent_lengths <- non_atomic_lengths[-1L]
    lhs_problems <- lhs_lengths %in% inconsistent_lengths
    rhs_problems <- rhs_lengths %in% inconsistent_lengths
    problems <- lhs_problems | rhs_problems
    if (any(problems)) {
      stop("The following formulas must be length ", len, " or 1, not ", 
           paste(inconsistent_lengths, collapse = ", "), ".\n    ", 
           paste(fs[problems], collapse = "\n    "),
           call. = FALSE)
    }
  }
  
  replace_with <- function (x, i, val, arg_name) {
    if (is.null(val)) 
      return(x)
    i[is.na(i)] <- FALSE
    if (length(val) == 1L) {
      x[i] <- val
    }
    else {
      x[i] <- val[i]
    }
    x
  }
  
  query <- vector("list", n)
  value <- vector("list", n)
  default_env <- parent.frame()
  for (i in seq_len(n)) {
    query[[i]] <- eval(fs[[i]][[2]], envir = default_env)
    value[[i]] <- eval(fs[[i]][[3]], envir = default_env)
    if (!is.logical(query[[i]])) 
      stop(fs[[i]][[2]], " does not return a `logical` vector.")
  }
  m <- validate_case_when_length(query, value, fs)
  out <- value[[1]][rep(NA_integer_, m)]
  replaced <- rep(FALSE, m)
  for (i in seq_len(n)) {
    out <- replace_with(out, query[[i]] & !replaced, value[[i]], 
                        NULL)
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }
  out
}

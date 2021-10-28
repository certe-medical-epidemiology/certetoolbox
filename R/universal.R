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

# These functions are universal; they are internal functions in every Certe R package.
# This is to prevent dependency on one another.

#
# This is the 'certetoolbox' package and the functions are exported here.
#

#' Vectorised Pattern Matching with Keyboard Shortcut
#'
#' Convenient wrapper around [grepl()] to match a pattern: `x %like% pattern`. It always returns a [`logical`] vector and is always case-insensitive (use `x %like_case% pattern` for case-sensitive matching). Also, `pattern` can be as long as `x` to compare items of each index in both vectors, or they both can have the same length to iterate over all cases.
#' @param x a [character] vector where matches are sought, or an object which can be coerced by [as.character()] to a [character] vector.
#' @param pattern a [character] vector containing regular expressions (or a [character] string for `fixed = TRUE`) to be matched in the given [character] vector. Coerced by [as.character()] to a [character] string if possible.
#' @param ignore.case if `FALSE`, the pattern matching is *case sensitive* and if `TRUE`, case is ignored during matching.
#' @return A [logical] vector
#' @name like
#' @rdname like
#' @export
#' @details
#' These [like()] and `%like%`/`%unlike%` functions:
#' * Are case-insensitive (use `%like_case%`/`%unlike_case%` for case-sensitive matching)
#' * Support multiple patterns
#' * Check if `pattern` is a valid regular expression and sets `fixed = TRUE` if not, to greatly improve speed (vectorised over `pattern`)
#' * Always use compatibility with Perl unless `fixed = TRUE`, to greatly improve speed
#' 
#' Using RStudio? The `%like%`/`%unlike%` functions can also be directly inserted in your code from the Addins menu and can have its own keyboard shortcut like `Shift+Ctrl+L` or `Shift+Cmd+L` (see menu `Tools` > `Modify Keyboard Shortcuts...`). If you keep pressing your shortcut, the inserted text will be iterated over `%like%` -> `%unlike%` -> `%like_case%` -> `%unlike_case%`.
#' @source Idea from the [`like` function from the `data.table` package](https://github.com/Rdatatable/data.table/blob/ec1259af1bf13fc0c96a1d3f9e84d55d8106a9a4/R/like.R), although altered as explained in *Details*.
#' @seealso [grepl()]
#' @examples
#' a <- "This is a test"
#' b <- "TEST"
#' a %like% b
#' b %like% a
#' 
#' # also supports multiple patterns
#' a <- c("Test case", "Something different", "Yet another thing")
#' b <- c(     "case",           "diff",      "yet")
#' a %like% b
#' a %unlike% b
#' 
#' a[1] %like% b
#' a %like% b[1]
like <- function(x, pattern, ignore.case = TRUE) {
  
  if (all(is.na(x))) {
    return(rep(FALSE, length(x)))
  }
  
  is_valid_regex <- function(x) {
    regex_at_all <- tryCatch(vapply(FUN.VALUE = logical(1),
                                    X = strsplit(x, ""),
                                    FUN = function(y) any(y %in% c("$", "(", ")", "*", "+", "-",
                                                                   ".", "?", "[", "]", "^", "{", 
                                                                   "|", "}", "\\"),
                                                          na.rm = TRUE),
                                    USE.NAMES = FALSE),
                             error = function(e) rep(TRUE, length(x)))
    regex_valid <- vapply(FUN.VALUE = logical(1),
                          X = x,
                          FUN = function(y) !"try-error" %in% class(try(grepl(y, "", perl = TRUE),
                                                                        silent = TRUE)),
                          USE.NAMES = FALSE)
    regex_at_all & regex_valid
  }
  
  # set to fixed if no valid regex (vectorised)
  fixed <- !is_valid_regex(pattern)
  
  if (ignore.case == TRUE) {
    # set here, otherwise if fixed = TRUE, this warning will be thrown: argument `ignore.case = TRUE` will be ignored
    x <- tolower(x)
    pattern <- tolower(pattern)
  }
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  if (length(pattern) == 1) {
    grepl(pattern, x, ignore.case = FALSE, fixed = fixed, perl = !fixed)
  } else {
    if (length(x) == 1) {
      x <- rep(x, length(pattern))
    } else if (length(pattern) != length(x)) {
      stop("arguments `x` and `pattern` must be of same length, or either one must be 1 ",
           "(`x` has length ", length(x), " and `pattern` has length ", length(pattern), ")", call. = FALSE)
    }
    unlist(
      mapply(FUN = grepl,
             x = x,
             pattern = pattern,
             fixed = fixed,
             perl = !fixed,
             MoreArgs = list(ignore.case = FALSE),
             SIMPLIFY = FALSE,
             USE.NAMES = FALSE)
    )
  }
}

#' @rdname like
#' @export
"%like%" <- function(x, pattern) {
  like(x, pattern, ignore.case = TRUE)
}

#' @rdname like
#' @export
"%unlike%" <- function(x, pattern) {
  !like(x, pattern, ignore.case = TRUE)
}

#' @rdname like
#' @export
"%like_case%" <- function(x, pattern) {
  like(x, pattern, ignore.case = FALSE)
}

#' @rdname like
#' @export
"%unlike_case%" <- function(x, pattern) {
  !like(x, pattern, ignore.case = FALSE)
}

#' Paste Character Vectors Together
#'
#' This function is identical to `paste(c(...), sep = "", collapse = "")`.
#' @param ... elements to be pasted together, can also be vectors
#' @export
#' @examples 
#' concat("a", "b", "c")
#' 
#' concat(c("a", "b"), "c")
concat <- function(...) {
  paste(c(...), collapse = "", sep = "")
}
#' Read Certe Secret From File
#' 
#' This function reads from a local or remote YAML file, as set in the environmental variable `"secrets_file"`.
#' @param property the property to read, case-sensitive
#' @inheritParams yaml::read_yaml
#' @details In the secrets file, the property name and value have to be separated with a colon (`:`), as is intended in YAML files.
#' 
#' The default value for `file` is the environmental variable `"secrets_file"`.
#' 
#' The file will be read using [yaml::read_yaml()], which allows almost any path or remote connection (such as websites).
#' @importFrom yaml read_yaml
#' @export
#' @examples 
#' # for this example, create a temporary 'secrets' file
#' my_secrets_file <- tempfile(fileext = ".yaml")
#' Sys.setenv(secrets_file = my_secrets_file)
#' writeLines(c("tenant_id: 8fb3c03060e02e89",
#'              "default_users: user_1"),
#'            my_secrets_file)
#' 
#' read_secret("tenant_id")
#' read_secret("default_users")
read_secret <- function(property, file = Sys.getenv("secrets_file")) {
  if (file == "" && identical(file, Sys.getenv("secrets_file"))) {
    warning("In read_secret(): environmental variable 'secrets_file' not set", call. = FALSE)
    return("")
  }
  contents <- read_yaml(file)
  if (!property %in% names(contents)) {
    warning("In read_secret(): property '", property, "' not found", call. = FALSE)
    return("")
  }
  contents[[property]]
}

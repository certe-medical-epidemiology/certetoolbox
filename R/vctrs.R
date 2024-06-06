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

# the 'vctrs' package (used by numerous tidyverse functions) is extremely strict,
# it does not allow e.g. joins on numeric/character vectors. These will allow this:

#' @method vec_cast.character double
#' @importFrom vctrs vec_cast.character
#' @noRd
#' @export
vec_cast.character.double <- function(x, to, ...) {
  warning("Coercing <double> to <character>", call. = FALSE)
  as.character(x)
}

#' @method vec_cast.double character
#' @importFrom vctrs vec_cast.double
#' @noRd
#' @export
vec_cast.double.character <- function(x, to, ...) {
  warning("Coercing <character> to <double>", call. = FALSE)
  as.double(x)
}

#' @method vec_cast.character integer
#' @importFrom vctrs vec_cast.character
#' @noRd
#' @export
vec_cast.character.integer <- function(x, to, ...) {
  warning("Coercing <integer> to <character>", call. = FALSE)
  as.character(x)
}

#' @method vec_cast.integer character
#' @importFrom vctrs vec_cast.integer
#' @noRd
#' @export
vec_cast.integer.character <- function(x, to, ...) {
  warning("Coercing <character> to <integer>", call. = FALSE)
  as.integer(x)
}

#' @method vec_ptype2.character double
#' @importFrom vctrs vec_ptype2.character
#' @noRd
#' @export
vec_ptype2.character.double <- function(x, y, ...) {
  # vec_cast() makes sure this double will be become a character
  x
}

#' @method vec_ptype2.double character
#' @importFrom vctrs vec_ptype2.double
#' @noRd
#' @export
vec_ptype2.double.character <- function(x, y, ...) {
  # vec_cast() makes sure this character will be become a double
  x
}

#' @method vec_ptype2.character integer
#' @importFrom vctrs vec_ptype2.character
#' @noRd
#' @export
vec_ptype2.character.integer <- function(x, y, ...) {
  # vec_cast() makes sure this integer will be become a character
  x
}

#' @method vec_ptype2.integer character
#' @importFrom vctrs vec_ptype2.integer
#' @noRd
#' @export
vec_ptype2.integer.character <- function(x, y, ...) {
  # vec_cast() makes sure this character will be become an integer
  x
}

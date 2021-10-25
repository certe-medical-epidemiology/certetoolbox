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

#' Human-readable File Size
#' 
#' Formats bytes into human-readable units, from "kB" (10^3) to "YB" (10^23).
#' @param bytes number of bytes
#' @param decimals precision, not used for bytes and kilobytes
#' @param decimal.mark decimal mark to use
#' @export
#' @examples 
#' size_humanreadable(c(12, 1234, 123456, 12345678))
#' 
#' size_humanreadable(1024 ^ c(0:4))
size_humanreadable <- function(bytes, decimals = 1, decimal.mark = ",") {
  bytes <- as.double(bytes)
  # Adapted from:
  # http://jeffreysambells.com/2012/10/25/human-readable-filesize-php
  size <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  factor <- floor((nchar(formatC(bytes, format = "f", digits = 0)) - 1) / 3)
  factor[factor > length(size) - 1] <- length(size) - 1
  # added slight improvement; no decimals for B and kB:
  decimals <- rep(decimals, length(bytes))
  decimals[size[factor + 1]  %in% c("B", "kB")] <- 0
  out <- paste(sprintf(paste0("%.", decimals, "f"), bytes / (1024 ^ factor)), size[factor + 1])
  out <- trimws(gsub(".", decimal.mark, out, fixed = TRUE))
  out
}

#' Create Random Identifier
#' 
#' This function creates unique identifier (IDs) using [sample()].
#' @param id_length character length of ID
#' @param n number of IDs to generate
#' @param chars characters to use for generation, defaults to hexadecimal characters (0-9 and a-f)
#' @export
#' @examples 
#' generate_identifier(8)
#' generate_identifier(6, 3)
generate_identifier <- function(id_length = 6, n = 1, chars = c(0:9, letters[1:6])) {
  vapply(FUN.VALUE = character(1), 
         X = seq_len(n),
         FUN = function(xx) {
           concat(sample(x = chars,
                         size = id_length,
                         replace = TRUE))
         })
}

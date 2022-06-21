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
#' @details If using [object.size()] on an object, this function is equal to using [`format2()`][certestyle::format2()] to format the object size.
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

#' Return Reference Directory
#' 
#' Returns the relative reference directory for non-projects.
#' @param sub relative subfolder or file
#' @details This function returns the absolute path using [tools::file_path_as_absolute()].
#' @export
ref_dir <- function(sub = "") {
  if (Sys.info()['sysname'] %in% c("Linux", "Darwin")) {
    r <- read_secret("path.refmap")
  } else {
    r <- gsub('\\', '/', read_secret("path.refmap"), fixed = TRUE)
  }
  
  if (r == "") {
    stop("Secret 'path.refmap' not set.", call. = FALSE)
  }
  if (r %unlike% '[/]$') {
    r <- paste0(r, '/')
  }
  sub <- trimws(sub, "both")
  r <- paste0(r, sub)
  if (r %like% '[/]$') {
    r <- substr(r, 1, nchar(r) - 1)
  }
  if (tools::file_ext(r) == "" && !dir.exists(r)) {
    dir.create(r, recursive = TRUE)
    message("Created folder: ", r)
  }
  tools::file_path_as_absolute(r)
}

#' Hospitalname
#'
#' Hospitalname and/or location, with support for all hospitals in Northern Netherlands, including Meppel, Hardenberg and Zwolle.
#' @param x text to be transformed
#' @param format default is `"{naamkort}, {plaats}"`. Attributes like `x` to be returned in '`glue`'-format (in curly brackets).
#' @importFrom glue glue
#' @export
#' @examples
#' hospital_name(c("MCL", "MCL", "Martini"))
#' hospital_name(c("Antonius", "WZA", "Martini"), format = "{naam} te {plaats}")
#'
#' # special case for GGD
#' hospital_name(c("Martini", "GGD Groningen", "GGD Drenthe"), format = "{naam}")
#' hospital_name(c("Martini", "GGD Groningen", "GGD Drenthe"), format = "{naamkort}")
#' hospital_name("ggd friesland", "{naam}")
hospital_name <- function(x, format = "{naamkort}, {plaats}") {
  x_trans <- rep(NA_character_, length(x))
  x_trans[x %like% "MZH?|Martini"] <- glue(format, naam = "Martini Ziekenhuis", naamkort = "MZH", plaats = "Groningen")
  x_trans[x %like% "MCL|Leeuwarden"] <- glue(format, naam = "Medisch Centrum Leeuwarden", naamkort = "MCL", plaats = "Leeuwarden")
  x_trans[x %like% "Tjongerschans|Heerenveen"] <- glue(format, naam = "Tjongerschans Ziekenhuis", naamkort = "TZH", plaats = "Heerenveen")
  x_trans[x %like% "Antonius|Sneek"] <- glue(format, naam = "Antonius Ziekenhuis", naamkort = "AZS", plaats = "Sneek")
  x_trans[x %like% "Smellinghe|Drachten"] <- glue(format, naam = "Ziekenhuis Nij Smellinghe", naamkort = "NSD", plaats = "Drachten")
  x_trans[x %like% "WZA|Wilhelmina|Assen"] <- glue(format, naam = "Wilhelmina Ziekenhuis", naamkort = "WZA", plaats = "Assen")
  x_trans[x %like% "OZG|Ommeland|Scheemda"] <- glue(format, naam = "Ommelander Ziekenhuis Groningen", naamkort = "OZG", plaats = "Scheemda")
  x_trans[x %like% "Refaja|Stadskanaal"] <- glue(format, naam = "Refaja Ziekenhuis", naamkort = "Treant", plaats = "Stadskanaal")
  x_trans[x %like% "(^SE$|Scheper|Emmen)"] <- glue(format, naam = "Scheper Ziekenhuis", naamkort = "Treant", plaats = "Emmen")
  x_trans[x %like% "BH|Bethesda|Hoogeveen"] <- glue(format, naam = "Bethesda Ziekenhuis", naamkort = "Treant", plaats = "Hoogeveen")
  x_trans[x %like% "UMCG|Universitair"] <- glue(format, naam = "Universitair Medisch Centrum Groningen", naamkort = "UMCG", plaats = "Groningen")
  x_trans[x %like% "MCL|Leeuwarden"] <- glue(format, naam = "Medisch Centrum Leeuwarden", naamkort = "MCL", plaats = "Leeuwarden")
  x_trans[x %like% "Isala|Zwolle"] <- glue(format, naam = "Isala Zwolle", naamkort = "Isala", plaats = "Zwolle")
  x_trans[x %like% "Dia[ck]on|Meppel"] <- glue(format, naam = "Isala Diaconessenhuis Meppel", naamkort = "Isala", plaats = "Meppel")
  x_trans[x %like% "SH|R.p[ck]+e|Harde.?berg"] <- glue(format, naam = "R\\u00f6pcke-Zweers Ziekenhuis", naamkort = "RZH", plaats = "Hardenberg")
  # modification for GGD, location is province
  x_trans[x %like% "GGD.*fr.*sl.*n"] <- glue(format, naam = "GGD Frysl\u00e2n", naamkort = "GGD", plaats = "Frysl\u00e2n")
  x_trans[x %like% "GGD.*gronin"] <- glue(format, naam = "GGD Groningen", naamkort = "GGD", plaats = "Groningen")
  x_trans[x %like% "GGD.*drent"] <- glue(format, naam = "GGD Drenthe", naamkort = "GGD", plaats = "Drenthe")
  x_trans
}

#' P-symbol format as asterisk
#' @param p numeric value between 0 and 1
#' @param emptychar sign to be displayed for 0.1 < p < 1.0
#' @export
p_symbol <- function(p, emptychar = " ") {
  
  p <- as.double(p)
  s <- rep(NA_character_, length(p))
  
  s[p <= 1] <- emptychar
  s[p <= 0.100] <- "."
  s[p <= 0.050] <- "*"
  s[p <= 0.010] <- "**"
  s[p <= 0.001] <- "***"
  
  s
}

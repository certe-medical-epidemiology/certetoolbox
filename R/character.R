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
#' @param decimal.mark decimal mark to use, defaults to [dec_mark()]
#' @details If using [object.size()] on an object, this function is equal to using [`format2()`][certestyle::format2()] to format the object size.
#' @export
#' @examples 
#' size_humanreadable(c(12, 1234, 123456, 12345678))
#' 
#' size_humanreadable(1024 ^ c(0:4))
size_humanreadable <- function(bytes, decimals = 1, decimal.mark = dec_mark()) {
  bytes <- as.double(bytes)
  # Adapted from:
  # http://jeffreysambells.com/2012/10/25/human-readable-filesize-php
  size <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  factor <- floor((nchar(formatC(bytes, format = "f", digits = 0)) - 1) / 3)
  factor[factor > length(size) - 1] <- length(size) - 1
  # added slight improvement; no decimals for B and kB:
  decimals_bak <- decimals[1]
  decimals <- rep(decimals[1], length(bytes))
  decimals[size[factor + 1] %in% c("B", "kB")] <- 0
  # but do set decimal for kB under 100 kB
  decimals[size[factor + 1] == "kB" & (bytes / 1024) < 100] <- decimals_bak
  # format
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
  if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
    r <- read_secret("path.refmap")
  } else {
    r <- gsub("\\", "/", read_secret("path.refmap"), fixed = TRUE)
  }
  
  if (r == "") {
    stop("Secret 'path.refmap' not set.", call. = FALSE)
  }
  if (r %unlike% "[/]$") {
    r <- paste0(r, "/")
  }
  sub <- trimws(sub, "both")
  r <- paste0(r, sub)
  if (r %like% "[/]$") {
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

#' Microorganisms Code from GLIMS10
#' 
#' This function is analogous to all `mo_*` functions of the AMR package, see [AMR::mo_property()].
#' @inheritParams AMR::mo_property
#' @export
#' @examples
#' mo_glims("E. coli")
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' data.frame(mo = c("ESCCOL", "Staph aureus")) |>
#'   mutate(glims = mo_glims())
#'   
#' # even works for non-existing entries in AMR package
#' mo_glims("Streptococcus mitis/oralis")
#' 
#' if (require(AMR, warn.conflicts = FALSE)) {
#'   as.mo("Streptococcus mitis/oralis")
#' }
#' if (require("AMR")) {
#'   mo_genus("Streptococcus mitis/oralis")
#' }
#' if (require("AMR")) {
#'   mo_gramstain("Streptococcus mitis/oralis")
#' }
mo_glims <- function (x, language = AMR::get_AMR_locale(), keep_synonyms = getOption("AMR_keep_synonyms", FALSE), ...) {
  nm <- AMR::mo_name(x, language = language, keep_synonyms = keep_synonyms, ...)
  mo_table_glims$mnemonic[match(nm, mo_table_glims$fullname)]
  
  # updaten van mo_table_glims:
  # mo_table_glims <- readxl::read_excel("mo-tabel-2023-07-21-TOTAAL-MB.xlsx") |>
  #   filter(genus != "") |>
  #   mutate(fullname = gsub(" species", "", naam)) |>
  #   select(fullname, mnemonic, genus, species, subspecies) |>
  #   distinct() |>
  #   mutate(manual_add = !fullname %in% AMR::microorganisms$fullname) |>
  #   mutate_all(cleaner::na_replace)
  # usethis::use_data(mo_table_glims, internal = TRUE, overwrite = TRUE)
}

#' Retrieve AGB Property
#' 
#' Download properties from the [AGB Register](https://www.vektis.nl/agb-register).
#' @param agb_code AGB codes
#' @param property property of the AGB code. Will return a [data.frame] if left blank.
#' @importFrom certestyle toproper
#' @importFrom dplyr tibble bind_rows summarise_all
#' @importFrom rvest read_html html_node html_text2 html_elements html_table
#' @importFrom cleaner clean_Date
#' @rdname agb_property
#' @details
#' The AGB (Algemeen GegevensBeheer) register is a national database in the Netherlands that assigns a unique AGB code to healthcare providers and institutions. This code is used for identification and administration within the healthcare sector, such as for billing insurance companies, contracting, and data exchange between healthcare providers.
#' 
#' The AGB code is managed by VECOZO and plays a crucial role in ensuring transparency and efficiency in Dutch healthcare administration.
#' @export
#' @examples
#' agb_property(03033048)
#' agb_property(c(03033048, 01102504))
#' 
#' agb_property(03033048, property = "last_name")
#' agb_property(c(03033048, 01102504), "specialty")
agb_property <- function(agb_code, property = NULL) {
  properties <- tibble()
  
  agb_code <- formatC(as.integer(agb_code), width = 8, flag = "0", format = "d")
  
  for (a in agb_code) {
    url <- paste0("https://www.vektis.nl/agb-register/zorgverlener-", a)
    page <- read_html(url)
    
    personal <- page |> html_node(".basic-info") |> html_text2()
    personal <- paste0(personal, "\n")
    full_name <- gsub(".*Naam.*?\n(.*?)\n.*", "\\1", personal)
    initials <- gsub(" .*", "", full_name)
    last_name <- toproper(gsub("^[A-Z.]+ ", "", full_name))
    sex <- gsub(".*Geslacht.*?\n(.*?)\n.*", "\\1", personal)
    sex <- toupper(substr(sex, 1, 1))
    title <- gsub(".*Academische titel.*?\n(.*?)\n.*", "\\1", personal)
    if (tolower(title) == "doctor") {
      title <- "dr."
    } else if (tolower(title) == "doctorandus") {
      title <- "drs."
    } else {
      # can also be "Bachelor", we'll ignore that
      title <- ""
    }
    full_name <- trimws(paste(toproper(title, every_word = TRUE), full_name))
    
    competences <- page |> html_node(".competence-list") |> html_text2()
    specialty <- trimws(gsub("^(.*?)[0-9]+.*", "\\1", competences))
    specialty_start <- gsub(".*Start\n(.*)\nEinde.*", "\\1", competences)
    specialty_start <- clean_Date(specialty_start, format = "dd-mm-yyyy")
    specialty_end <- gsub(".*\nEinde\n([0-9-]+).*", "\\1", competences)
    specialty_end <- clean_Date(specialty_end, format = "dd-mm-yyyy")
    
    employer <- page |> html_elements(css = ".card-table") |> html_table()
    employer_int <- which(vapply(FUN.VALUE = logical(1), employer, function(x) any(x$`AGB-code` %like% "[0-9]+", na.rm = TRUE)))
    employer <- employer[[employer_int[1]]]
    employer$Start <- clean_Date(employer$Start, format = "dd-mm-yyyy")
    employer$Einde <- clean_Date(employer$Einde, format = "dd-mm-yyyy")
    if (NROW(employer) > 1) {
      employer <- employer |> summarise_all(paste, collapse = "; ")
      if (NROW(properties) > 0) {
        properties$employer_agb <- as.character(properties$employer_agb)
        properties$employee_since <- as.character(properties$employee_since)
        properties$employee_until <- as.character(properties$employee_until)
      }
    }
    
    properties <- properties |>
      bind_rows(tibble(agb = a,
                       title = title,
                       initials = initials,
                       last_name = last_name,
                       full_name = full_name,
                       sex = sex,
                       specialty = specialty,
                       specialty_since = specialty_start,
                       specialty_until = specialty_end,
                       employed_by = employer$Naam,
                       employer_agb = employer$`AGB-code`,
                       employee_since = employer$Start,
                       employee_until = employer$Einde,
                       ))
  }
  
  if (!is.null(property)) {
    properties[[property]]
  } else {
    properties
  }
}

# #' @rdname agb_property
# #' @details [agb_lookup()] looks up the AGB code, and returns a [menu][utils::menu()] in an interactive session, and the first hit in a non-interactive session.
# #' @export
# agb_lookup <- function(search_term) {
#   url <- "https://www.vektis.nl/agb-register/zoeken"
#   form <- url |> rvest::read_html() |> rvest::html_node("main form") |> rvest::html_form()
#   form <- form |> rvest::html_form_set(agbcode = "Jansen",
#                                        zorgpartijtype = "zorgverlener",
#                                        zorgsoort = "00 - Alle zorgsoorten")
#   click <- form |> rvest::html_form_submit(submit = 3)
#   s <- rvest::session(url)
#   click2 <- rvest::session_submit(s, form, submit = 2)
# }

#' Quick SHA Hash
#' 
#' Generates hashes with or without an extra key/salt. This key defaults to the [project identifier][certeprojects::project_get_current_id()].
#' @param x input, will be converted to character
#' @param key key or salt. Use `TRUE` (default) to use the [project identifier][certeprojects::project_get_current_id()] that will be printed to the Console, or any character to use as key/salt, or use `NULL` or `FALSE` to not use a key.
#' @param bits number of bits, often one of: 224, 256, 384, 512
#' @details This function uses the [openssl::sha2()] function for the hashing, but always returns a [character] vector.
#' @importFrom certeprojects project_identifier project_get_current_id
#' @export
#' @examples
#' generate_hash("a")
#' generate_hash(c("a", "b", "c"))
#' 
#' generate_hash("a", "secret")
#' generate_hash(c("a", "b", "c"), "secret")
generate_hash <- function(x, key = TRUE, bits = 256) {
  check_is_installed("openssl")
  x <- as.character(x)
  if (is.null(key) || isFALSE(key)) {
    key <- NULL
  } else if (isTRUE(key)) {
    # generate random key
    key <- as.character(project_identifier(project_number = project_get_current_id(ask = FALSE)))
    message("Using {certeprojects} project identifier as hash key: \"", key, "\".")
  } else {
    key <- paste(as.character(key), collapse = "")
  }
  out <- openssl::sha2(x = x, key = key, size = as.integer(bits[1]))
  as.character(out)
}

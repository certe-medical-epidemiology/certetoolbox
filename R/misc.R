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

#' Refmap for certetoolbox
#' @export
.R_REFMAP <- function(sub = "") {
  if (Sys.info()['sysname'] %in% c("Linux", "Darwin")) {
    r <- Sys.getenv("R_REFMAP")
  } else {
    r <- gsub('\\', '/', Sys.getenv("R_REFMAP"), fixed = TRUE)
  }
  
  if (r == "") {
    stop("Environmental user variable `R_REFMAP` not set.", call. = FALSE)
  }
  if (r %unlike% '[/]$') {
    r <- paste0(r, '/')
  }
  sub <- trimws(sub, "both")
  r <- paste0(r, sub)
  if (r %like% '[/]$') {
    r <- substr(r, 1, nchar(r) - 1)
  }
  if (tools::file_ext(r) == "" & !dir.exists(r)) {
    dir.create(r, recursive = TRUE)
  }
  tools::file_path_as_absolute(r)
}

#' Name of antibiotic
#'
#' Transforms a code to a antibiotic name or ATC-code.
#' @param abcode antibiotic code or name.
#' @param from type to transform. Valid options are all variables of \code{\link[AMR]{antibiotics}}.
#' @param to type to transform to. Valid options are all variables of \code{\link[AMR]{antibiotics}}.
#' @param textbetween text between two or more antibiotics.
#' @param tolower result in lower case.
#' @keywords ab antibiotics
#' @export
#' @examples
#' abname_molis("amcl")
#' # "Amoxicilline/clavulaanzuur"
#'
#' abname_molis("amcl+gent")
#' # "Amoxicilline/clavulaanzuur + gentamicine"
#'
#' abname_molis(c("amox", "amcl"))
#' # "Amoxicilline" "Amoxicilline/clavulaanzuur"
#' @source \code{\link[AMR]{antibiotics}}
abname_molis <- function(abcode, textbetween = " + ", tolower = FALSE, ...) {
  codes <- suppressWarnings(AMR::ab_name(abcode, language = "nl", tolower = tolower))
  if (sum(abcode %like% "[+]") > 0) {
    for (i in 1:length(abcode)) {
      if (abcode[i] %like% "[+]") {
        codes[i] <- paste0(AMR::ab_name(strsplit.select(abcode[i], 1, "[+]"),
                                        language = "nl",
                                        tolower = tolower),
                           textbetween,
                           AMR::ab_name(strsplit.select(abcode[i], 2, "[+]"),
                                        language = "nl",
                                        tolower = TRUE))
      }
    }
  }
  codes
}


#' Check micro-organism from early warning mail
#'
#' Use this function to check results and resistance pattern of a strain using the ID provided in the early warning mail.
#' @param ordernr_testcode_mtrlcode_stam strain ID
#' @param view direct view in RStudio viewer.
#' @param info show query information.
#' @param ... arguments passed on to \code{\link{certedb_query}}.
#' @export
check_mo <- function(ordernr_testcode_mtrlcode_stam, view = TRUE, info = FALSE, ...) {
  if (length(ordernr_testcode_mtrlcode_stam) > 1) {
    stop("Only 1 ordernr_testcode_mtrlcode_stam allowed")
  }
  
  ordernr_testcode_mtrlcode_stam <- unlist(strsplit(ordernr_testcode_mtrlcode_stam, "_"))
  ordernr <- ordernr_testcode_mtrlcode_stam[1]
  testcode <- ordernr_testcode_mtrlcode_stam[2]
  mtrlcode <- ordernr_testcode_mtrlcode_stam[3]
  stam <- ordernr_testcode_mtrlcode_stam[4]
  
  res <- certedb_query(info = info,
                       query =
                         paste0(
                           "SELECT
    CONCAT(ordernr,
            '_',
            anamc,
            '_',
            mtrlcode,
            '_',
            stamteller) AS ordernr_testcode_mtrlcode_stam,
    ordernr,
    ontvangstdatum,
    mtrlcode,
    anamc AS testcode,
    stamteller AS stam,
    b.volledigenaam AS mo,
    abmc AS ab,
    res_rsi,
    res_diam,
    res_num,
    protect_typ
FROM
    certemm_abres AS ab
        LEFT JOIN
    temporary_certemm_bacterienlijst AS b ON CONCAT(ab.famnb,
            '_',
            ab.genrnb,
            '_',
            ab.espnb,
            '_',
            ab.sespnb) = b.bacteriecode_oud
WHERE
    ordernr = '", ordernr,
                           "' AND anamc = '",
                           testcode, "' AND mtrlcode = '",
                           mtrlcode, "' AND stamteller = '",
                           stam, "'"), ...)
  ordernr <- paste0("00-", substr(ordernr, 3, 6), "-", substr(ordernr, 7, 10))
  message("Ordernr ", ordernr, ", mtrlcode ", mtrlcode, ", testcode ", testcode, ", stam ", stam, ": ", crayon::italic(res$mo[1]))
  check_mo_result <- res %>%
    select(ab, starts_with("res_"), protect_typ) %>%
    mutate_all(as.character) %>%
    # E en X niet op rapport
    mutate(op_rapport = ifelse(protect_typ == "I", "X", "-")) %>%
    pivot_longer(-ab) %>%
    pivot_wider(names_from = ab, values_from = value) %>%
    slice(1, 5, 3, 2, 4)
  if (view == TRUE) {
    View(check_mo_result)
  } else {
    check_mo_result
  }
}

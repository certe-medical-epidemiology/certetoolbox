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



#' Temporarily Remember Object
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
  if (is.null(dots_names) || any(dots_names == "")) {
    stop("Values must be named in remember()")
  }
  pkg_env$temp <- c(pkg_env$temp, dots)
  .data
}

#' @rdname remember_recall
#' @export
recall <- function(x, delete = TRUE) {
  x_name <- deparse(substitute(x))
  x_val <- pkg_env$temp[[x_name]]
  if (isTRUE(delete)) {
    pkg_env$temp <- pkg_env$temp[which(names(pkg_env$temp) != x_name)]
  }
  x_val
}

#' Create contingency table of [data.frame]
#'
#' Creates a contingency table. Output is a [matrix].
#' @param data [data.frame] or \code{tibble} containing data, where columns \code{column1} and \code{column2} occur.
#' @param column1 Column with values.
#' @param column2 Column with values.
#' @param condition1 Condition to seperate \code{column1} from "Rest".
#' @param condition2 Condition to seperate \code{column2} from "Rest".
#' @param expected Default is \code{FALSE}. Return expected instead of observed (\code{E_i = N / n}, where discrete uniforme distribution is to be expected).
#' @param totals Default is \code{FALSE}. Adds row- and columntotals.
#' @keywords contingency table chi squared
#' @export
#' @return \code{matrix}
crosstab <- function(data, column1, condition1, column2, condition2, expected = FALSE, totals = FALSE) {
  
  # voor later
  # a1 <- data %>% filter(any(condition1 %in% data[, column1]) & any(condition2 %in% data[, column2])) %>% as.data.frame()
  # a2 <- data %>% filter(any(condition1 %in% data[, column1]) & !(condition2 %in% data[, column2])) %>% as.data.frame()
  # b1 <- data %>% filter(!(condition1 %in% data[, column1]) & any(condition2 %in% data[, column2])) %>% as.data.frame()
  # b2 <- data %>% filter(!(condition1 %in% data[, column1]) & !(condition2 %in% data[, column2])) %>% as.data.frame()
  
  # ondersteuning voor maar 1 voorwaarde per kolom:
  a1 <- data %>% filter(data[, column1] == condition1 & data[, column2] == condition2) %>% as.data.frame()
  a2 <- data %>% filter(data[, column1] == condition1 & data[, column2] != condition2) %>% as.data.frame()
  b1 <- data %>% filter(data[, column1] != condition1 & data[, column2] == condition2) %>% as.data.frame()
  b2 <- data %>% filter(data[, column1] != condition1 & data[, column2] != condition2) %>% as.data.frame()
  
  column1.rest <- sort(unique(b2[, column1]))
  condition1.rest <- 'Rest'
  if (length(column1.rest) < 3) {
    condition1.rest <- concat(column1.rest, "/")
  }
  column2.rest <- sort(unique(b2[, column2]))
  condition2.rest <- 'Rest'
  if (length(column2.rest) < 3) {
    condition2.rest <- concat(column2.rest, "/")
  }
  
  x <- matrix(data = c(nrow(a1),
                       nrow(a2),
                       nrow(b1),
                       nrow(b2)),
              nrow = 2,
              ncol = 2,
              byrow = TRUE,
              dimnames = list(c(concat(condition1, "/"), condition1.rest),
                              c(concat(condition2, "/"), condition2.rest)))
  
  if (expected == TRUE) {
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*") / sum(x)
    x <- E
  }
  
  if (totals == TRUE) {
    x <- rbind(cbind(x, rowSums(x)), colSums(cbind(x, rowSums(x))))
  }
  
  x
  
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

#' Susceptibility table between hospitals
#'
#' Creates a susceptibility comparison table between hospitals. Runs a G-test at >1000 observations or an Exact-test when less.
#' @param ab_list list of antibiotics. See [AMR::antibiotics].
#' @param hospitalname name of the hospital to be compared to other hospitals.
#' @param df_all [data.frame] with all data.
#' @param df_thishospital [data.frame] with all data of the to be tested hospital.
#' @param df_otherhospitals  [data.frame] with all data of the other hospitals.
#' @seealso [AMR::g.test()] runs at > 1000 observations; [fisher.test()] runs at <= 1000 observations
#' @importFrom tidyr tibble
#' @importFrom dplyr mutate
#' @importFrom AMR g.test 
#' @importFrom stats fisher.test
#' @export
#' @examples
#' \dontrun{
#' rsi_table(ab_list, 'MZ', df)
#'
#' septic_patients %>%
#'   mutate(zkhgroep = hospital_id) %>%
#'   rsi_table(ab_list = c("amox", "amcl"),
#'             hospitalname = "A",
#'             df_all = .)
#' }
rsi_table <- function(ab_list, hospitalname, df_all, df_thishospital = NULL, df_otherhospitals = NULL) {
  
  tbl.rsi <- tibble(antibioticum = character(0),
                    rsi.dit = double(0),
                    rsi.rest = double(0),
                    p = double(0),
                    psym = character(0),
                    size = double(0),
                    meth = character(0))
  
  if (is.null(df_thishospital)) {
    if (!"zkhgroep" %in% colnames(df_all)) {
      stop("Variable 'zkhgroep' is missing from `df_all`.", call. = FALSE)
    }
    df_thishospital <- df_all %>% filter(.$zkhgroep == hospitalname)
  }
  if (is.null(df_otherhospitals)) {
    if (!"zkhgroep" %in% colnames(df_all)) {
      stop("Variable 'zkhgroep' is missing from `df_all`.", call. = FALSE)
    }
    if (!"zkhgroep_code" %in% colnames(df_all)) {
      warning("Variable 'zkhgroep_code' is missing from `df_all`. Are these all isolates from hospitals?")
      df_all <- df_all %>% mutate(zkhgroep_code = 1)
    }
    df_otherhospitals <- df_all %>% filter(.$zkhgroep != hospitalname, .$zkhgroep_code != 0)
  }
  
  for (i in 1:length(ab_list)) {
    ab <- ab_list[i]
    abnaam <- ab %>% abname_molis()
    
    susceptibility_hospitalname <- AMR::proportion_S(df_thishospital %>% pull(ab))
    susceptibility_rest <- AMR::proportion_S(df_otherhospitals %>% pull(ab))
    
    cont.tbl <- crosstab(data = df_all,
                         column1 = ab,
                         condition1 = 'S',
                         column2 = 'zkhgroep',
                         condition2 = hospitalname)
    
    # source for using 1000: http://www.biostathandbook.com/gtestind.html
    if (sum(cont.tbl) >= 1000) {
      susceptibility_pwaarde <- g.test(cont.tbl)$p.value
      method <- 'Onafh. G-toets'
    } else {
      susceptibility_pwaarde <- fisher.test(cont.tbl)$p.value
      method <- "Fisher's Exact-toets"
    }
    
    tbl.rsi <- tbl.rsi %>%
      tibble::add_row(antibioticum = abnaam,
                      rsi.dit = susceptibility_hospitalname,
                      rsi.rest = susceptibility_rest,
                      p = susceptibility_pwaarde,
                      psym = p_symbol(susceptibility_pwaarde),
                      size = sum(cont.tbl),
                      meth = method)
    
  }
  
  colnames(tbl.rsi) <- c('Antibioticum', hospitalname, 'Rest', 'p-waarde',
                         'Significantie', 'Grootte', 'Methode')
  
  tbl.rsi
}

#' Splits text and select element
#' @inheritParams base::strsplit
#' @param element The nth element that should be returned.
#' @seealso \code{\link[base]{strsplit}}
#' @export strsplit.select
#' @examples
#' \dontrun{
#' tbl %>%
#'   mutate(genus = strsplit.select(microorganisme, 1),
#'          species = strsplit.select(microorganisme, 2))
#' }
strsplit.select <- function(x, element, split = " ", fixed = FALSE, perl = FALSE, useBytes = FALSE) {
  sapply(strsplit(x,
                  split,
                  fixed = fixed,
                  perl = perl,
                  useBytes = useBytes
  ),
  "[",
  element)
}

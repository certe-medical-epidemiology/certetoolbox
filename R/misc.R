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



#' Temporarily save value in Global Environment
#'
#' Can be used in dplyr-syntax to remember for later use. Values are temporarily saved in the Global Environment.
#' @rdname remember_recall
#' @param .data table to be passes through unchanged. 
#' @param ... value(s) to be remembered.
#' @param x value to be recalled.
#' @param delete delete value after.
#' @param envir default is Global Environment. The \link{environment} where values are saved.
#' @details values can be saved with \strong{\code{remember()}} and recalled (and deleted) with \strong{\code{recall()}}.
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
remember <- function(.data, ..., envir = globalenv()) {
  dots <- list(...)
  for (i in seq_len(length(dots))) {
    if (is.null(names(dots)[i])) {
      name <- "tmp_"
    } else {
      name <- names(dots)[i]
    }
    assign(x = name,
           value = dots[[i]],
           envir = envir)
  }
  .data
}


#' @rdname remember_recall
#' @export
recall <- function(x = NULL, delete = TRUE, envir = globalenv()) {
  if (is.null(x)) {
    x_name <- "tmp_"
  } else {
    x_name <- gsub('(^"|"$)', "", deparse(substitute(x)))
  }
  tryCatch(
    x_val <- eval(parse(text = x_name), envir = envir),
    error = function(e) {
      if (x_name == "tmp_") {
        stop("Temporary value for recall() not found in global environment. Did you name the value in remember()?", call. = FALSE)
      } else {
        stop("Value '", x_name, "' for recall() not found in global environment", call. = FALSE)
      }
    })
  
  if (delete == TRUE) {
    rm(list = x_name, envir = envir)
  }
  x_val
}

#' Create contingency table of \code{data.frame}
#'
#' Creates a contingency table. Output is a \code{\link{matrix}}.
#' @param data \code{data.frame} or \code{tibble} containing data, where columns \code{column1} and \code{column2} occur.
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
#' @param ab_list list of antibiotics. See \code{\link[AMR]{antibiotics}}.
#' @param hospitalname name of the hospital to be compared to other hospitals.
#' @param df_all \code{data.frame} with all data.
#' @param df_thishospital \code{data.frame} with all data of the to be tested hospital.
#' @param df_otherhospitals  \code{data.frame} with all data of the other hospitals.
#' @seealso \code{\link{g.test}} runs at > 1000 observations; \code{\link{fisher.test}} runs at <= 1000 observations
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
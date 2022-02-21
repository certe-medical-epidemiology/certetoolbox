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

#' Downloading CBS-data
#'
#' Download data from CBS Open data Statline. See \url{https://www.cbs.nl/nl-nl/onze-diensten/open-data/statline-als-open-data}.
#' @param identifier As provided in \code{cbs_topics()}, or a tracking number (1 to \code{max_print}) in \code{cbs_search()}.
#' @param clean_cols Clean column names. 
#' @param topic Subject to search for.
#' @param max_print Maximum number of subjects to print. 
#' @details \code{cbs_topics()} Retrieves all subjects.
#'
#' \code{cbs_search()} searches for a specific subject.  
#'
#' \code{cbs_download()} downloads tables. Input has to be a CBS Identifier (printed in red in \code{cbs_search()}), or a tracking number of \code{cbs_search()}, or the result of \code{cbs_search()}.
#'
#' \code{cbs_moreinfo()} gives a detailed explanation for the table. Input can also be a dataset downloaded with \code{cbs_download()}.
#' @rdname cbs
#' @export
#' @examples
#' cbs_search("Inwoners")
#'
#' x <- cbs_download(2) # 2e hit van cbs_search()
#'
#' cbs_moreinfo(x)
#' cbs_moreinfo(2)
#'
#' cbs_search("Overledenen; doodsoorzaak, kwartaal en jaar overlijden") %>%
#'   cbs_download() %>%
#'   filter(perioden %like% "J00") %>%
#'   mutate(perioden = clean_integer(gsub("J00", "", perioden))) %>%
#'   select(-matches("totaal")) %>%
#'   set_names(label(.)) %>% # CBS-kolommen hebben labels met netjes opgemaakte tekst
#'   pivot_longer(-perioden) %>%
#'   plot2.line(x = perioden, x.category = name, y = value, sort.x = NULL)
cbs_topics <- function() {
  check_is_installed("cbsodataR")
  cbsodataR::cbs_get_toc(Language = "nl")
}

#' @rdname cbs
#' @export
cbs_search <- function(topic, max_print = 25) {
  check_is_installed("cbsodataR")
  
  topics <- cbs_topics() %>%
    filter(Title %like% topic | Summary %like% topic) %>%
    arrange(SearchPriority, ShortTitle)
  
  if (nrow(topics) == 0) {
    message("No topics found")
    return(invisible())
  }
  
  for (i in 1:min(max_print, nrow(topics))) {
    cat(paste0(i, ". ", crayon::bold(topics[i, ]$Title), " (", crayon::red(topics[i, ]$Identifier), ")\n",
               "Periode: ", topics[i, ]$Period, "\n",
               "Grootte: ", format2(topics[i, ]$RecordCount), " x ", topics[i, ]$ColumnCount, "\n",
               "Aangepast: ", format2(topics[i, ]$Modified, " dddd d mmmm yyyy"), "\n",
               topics[i, ]$Summary, "\n\n"))
  }
  
  if (nrow(topics) > max_print) {
    message("...", nrow(topics))
  }
  
  options(cbs_identifiers = topics$Identifier)
  invisible(topics)
}

#' @rdname cbs
#' @export
cbs_download <- function(identifier, clean_cols = TRUE) {
  check_is_installed("cbsodataR")
  if (is.data.frame(identifier)) {
    if ("Identifier" %in% colnames(identifier)) {
      identifier <- identifier$Identifier[1L]
    } else {
      stop("Invalid identifier")
    }
  } else if (is.numeric(identifier)) {
    cbs_identifiers <- getOption("cbs_identifiers")
    if (is.null(cbs_identifiers)) {
      stop("Invalid identifier")
    } else {
      identifier <- cbs_identifiers[identifier]
    }
  }
  df <- cbsodataR::cbs_get_data(identifier)
  if (clean_cols == TRUE) {
    cols <- colnames(df)
    # laatste _47 verwijderen
    cols <- gsub("_[0-9]+$", "", cols)
    # snake case van maken
    cols <- gsub("[^a-zA-Z0-9]+", "_", cols)
    cols <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", cols)
    cols <- gsub("([a-zA-Z])([0-9])", "\\1_\\2", cols)
    cols <- tolower(cols)
    cols <- tolower(cols)
    colnames(df) <- cols
  }
  attr(df, "identifier") <- identifier
  df
}


#' @rdname cbs
#' @export
cbs_moreinfo <- function(identifier) {
  check_is_installed("cbsodataR")
  
  if (is.data.frame(identifier)) {
    identifier <- attributes(identifier)$identifier
    if (is.null(identifier)) {
      stop("Invalid identifier")
    }
  } else if (is.numeric(identifier)) {
    cbs_identifiers <- getOption("cbs_identifiers")
    if (is.null(cbs_identifiers)) {
      stop("Invalid identifier")
    } else {
      identifier <- cbs_identifiers[identifier]
    }
  }
  topic <- cbs_topics() %>%
    filter(Identifier == identifier)
  
  descr <- topic$ShortDescription
  descr <- gsub("Status van de cijfers.*?(\n| )", crayon::underline("Status van de cijfers:\n"), descr)
  descr <- gsub("Wijzigingen per(.*?):?\n", crayon::underline("Wijzigingen per\\1:\n"), descr)
  descr <- gsub("Wanneer komen er nieuwe cijfers.*?\n", crayon::underline("Nieuwe cijfers:\n"), descr)
  cat(paste0(crayon::bold(topic$Title), " (", crayon::red(topic$Identifier), ")",
             descr))
}

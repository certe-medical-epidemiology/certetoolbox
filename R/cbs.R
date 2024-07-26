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

#' Download CBS-data
#'
#' Download data from [CBS Open data Statline](https://www.cbs.nl/nl-nl/onze-diensten/open-data/statline-als-open-data).
#' @param identifier tracking number (1 to `max_print`) in [cbs_search()]
#' @param clean_cols Clean column names.
#' @param topic topics to search for
#' @param max_print maximum number of subjects to print
#' @details [cbs_topics()] retrieves all topics.
#'
#' [cbs_search()] searches for a specific subject.  
#'
#' [cbs_download()] downloads tables. Input has to be a CBS Identifier (printed in red in [cbs_search()]), or a tracking number of [cbs_search()], or the result of [cbs_search()].
#'
#' [cbs_moreinfo()] gives a detailed explanation for the table. Input can also be a dataset downloaded with [cbs_download()].
#' @rdname cbs
#' @export
#' @examples
#' \dontrun{
#' cbs_search("Inwoners")
#'
#' x <- cbs_download(2) # 2nd hit of cbs_search()
#' str(x)
#'
#' cbs_moreinfo(x)
#' cbs_moreinfo(2)
#' }
cbs_topics <- function() {
  check_is_installed("cbsodataR")
  cbsodataR::cbs_get_toc(Language = "nl")
}

#' @importFrom dplyr filter arrange
#' @rdname cbs
#' @export
cbs_search <- function(topic, max_print = 25) {
  check_is_installed(c("cbsodataR", "crayon"))
  
  topics <- cbs_topics() |>
    filter(Title %like% topic | Summary %like% topic) |>
    arrange(SearchPriority, ShortTitle)
  
  if (nrow(topics) == 0) {
    message("No topics found")
    return(invisible())
  }
  
  for (i in seq_len(min(max_print, nrow(topics)))) {
    cat(paste0(i, ". ", crayon::bold(topics[i, ]$Title), " (", crayon::red(topics[i, ]$Identifier), ")\n",
               "Periode: ", topics[i, ]$Period, "\n",
               "Grootte: ", format2(topics[i, ]$RecordCount), pkg_env$cross_icon, topics[i, ]$ColumnCount, "\n",
               "Aangepast: ", format2(topics[i, ]$Modified, " dddd d mmmm yyyy"), "\n",
               topics[i, ]$Summary, "\n\n"))
  }
  
  if (nrow(topics) > max_print) {
    message("...", nrow(topics))
  }
  
  pkg_env$cbs_identifiers <- topics$Identifier
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
    cbs_identifiers <- pkg_env$cbs_identifiers
    if (is.null(cbs_identifiers)) {
      stop("Invalid identifier")
    } else {
      identifier <- cbs_identifiers[identifier]
    }
  }
  df <- cbsodataR::cbs_get_data(identifier)
  if (clean_cols == TRUE) {
    cols <- colnames(df)
    # remove last _47
    cols <- gsub("_[0-9]+$", "", cols)
    # make it snake case
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

#' @importFrom dplyr filter
#' @rdname cbs
#' @export
cbs_moreinfo <- function(identifier) {
  check_is_installed(c("cbsodataR", "crayon"))
  
  if (is.data.frame(identifier)) {
    identifier <- attributes(identifier)$identifier
    if (is.null(identifier)) {
      stop("Invalid identifier")
    }
  } else if (is.numeric(identifier)) {
    cbs_identifiers <- pkg_env$cbs_identifiers
    if (is.null(cbs_identifiers)) {
      stop("Invalid identifier")
    } else {
      identifier <- cbs_identifiers[identifier]
    }
  }
  topic <- cbs_topics() |>
    filter(Identifier == identifier)
  
  descr <- topic$ShortDescription
  descr <- gsub("Status van de cijfers.*?(\n| )", crayon::underline("Status van de cijfers:\n"), descr)
  descr <- gsub("Wijzigingen per(.*?):?\n", crayon::underline("Wijzigingen per\\1:\n"), descr)
  descr <- gsub("Wanneer komen er nieuwe cijfers.*?\n", crayon::underline("Nieuwe cijfers:\n"), descr)
  cat(paste0(crayon::bold(topic$Title), " (", crayon::red(topic$Identifier), ")",
             descr))
}

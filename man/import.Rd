% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_export.R
\name{import}
\alias{import}
\alias{import_rds}
\alias{import_xlsx}
\alias{import_excel}
\alias{import_csv}
\alias{import_csv2}
\alias{import_tsv}
\alias{import_sav}
\alias{import_spss}
\alias{import_clipboard}
\alias{import_mail_attachment}
\title{Import Data and Plots}
\usage{
import(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = FALSE,
  ...
)

import_rds(filename, card_number = project_get_current_id(ask = FALSE), ...)

import_xlsx(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  sheet = 1,
  range = NULL,
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_excel(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  sheet = 1,
  range = NULL,
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_csv(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_csv2(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_tsv(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_sav(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = FALSE,
  datenames = "en",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ".",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_spss(
  filename,
  card_number = project_get_current_id(ask = FALSE),
  auto_transform = FALSE,
  datenames = "en",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ".",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_clipboard(
  sep = "\\t",
  header = TRUE,
  startrow = 1,
  auto_transform = TRUE,
  datenames = "nl",
  dateformat = "yyyy-mm-dd",
  timeformat = "HH:MM",
  decimal.mark = ",",
  big.mark = "",
  timezone = "UTC",
  na = c("", "NULL", "NA", "<NA>"),
  ...
)

import_mail_attachment(
  search = "hasattachment:yes",
  search_subject = NULL,
  search_from = NULL,
  search_when = NULL,
  search_attachment = NULL,
  n = 5,
  sort = "received desc",
  account = NULL,
  auto_transform = TRUE,
  ...
)
}
\arguments{
\item{filename}{the full path of the file to be imported, will be parsed to a \link{character}}

\item{card_number}{a Trello card number}

\item{auto_transform}{transform the imported data with \code{\link[=auto_transform]{auto_transform()}}}

\item{...}{arguments passed on to methods}

\item{sheet}{Excel sheet to import, defaults to first sheet}

\item{range}{a cell range to read from, allows typical Excel ranges such as "B3:D87" and "Budget!B2:G14"}

\item{datenames}{language of the date names, such as weekdays and months}

\item{dateformat}{expected date format, will be coerced with \code{\link[cleaner:format_datetime]{format_datetime()}}}

\item{timeformat}{expected time format, will be coerced with \code{\link[cleaner:format_datetime]{format_datetime()}}}

\item{decimal.mark}{separator for decimal numbers}

\item{big.mark}{separator for thousands}

\item{timezone}{expected time zone}

\item{na}{values to interpret as \code{NA}}

\item{sep}{character to separate values in a row}

\item{header}{use first row as header}

\item{startrow}{first row to start importing}

\item{search}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{search_subject}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{search_from}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{search_when}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{search_attachment}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{n}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{sort}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}

\item{account}{see \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}}}
}
\description{
These functions can be used to import data. They work closely with the \code{certeprojects} package and support Trello card numbers.
}
\details{
Importing from any unlisted filetype using \code{\link[=import]{import()}} requires the \code{rio} package to be installed.

Importing from an Excel file using \code{\link[=import_xlsx]{import_xlsx()}} or \code{\link[=import_excel]{import_excel()}} requires the \code{readxl} package to be installed.

Importing from an SPSS file using \code{\link[=import_sav]{import_sav()}} or \code{\link[=import_spss]{import_spss()}} requires the \code{haven} package to be installed.

Importing from the clipboard using \code{\link[=import_clipboard]{import_clipboard()}} requires the \code{clipr} package to be installed.

Importing from mail attachments using \code{\link[=import_mail_attachment]{import_mail_attachment()}} requires the \code{certemail} package to be installed. It calls \code{\link[certemail:download_mail_attachment]{download_mail_attachment()}} internally and saves the attachment to a temporary folder.
}
\seealso{
\code{\link[=export]{export()}}
}
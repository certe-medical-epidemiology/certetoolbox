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

#' Force Time as UTC
#' @param x a vector of datetime values
#' @export
as.UTC <- function(x, ...) {
  UseMethod("as.UTC")
}

#' @method as.UTC data.frame
#' @rdname as.UTC
#' @export
as.UTC.data.frame <- function(x, ...) {
  POSIXct_cols <- which(sapply(x, inherits, "POSIXct"))
  for (i in POSIXct_cols) {
    x[, i] <- as.UTC(x[, i, drop = TRUE])
  }
  x
}

#' @method as.UTC POSIXct
#' @rdname as.UTC
#' @export
as.UTC.POSIXct <- function(x, ...) {
  attr(x, "tzone") <- "UTC"
  x
}

#' @rdname as.UTC
#' @export
as.UTC.default <- function(x, ...) {
  if (inherits(x, "POSIXct")) {
    attr(x, "tzone") <- "UTC"
  }
  x
}


#' Dates around Today
#'
#' @param ref reference date (defaults to today)
#' @param only_start_end logical to indicate whether only the first and last value of the resulting vector should be returned
#' @param day day to return (0 are 7 are Sunday, 1 is Monday, etc.)
#' @param wk week to search for
#' @param yr year to search for, defaults to current year
#' @details All functions return a vector of dates, except for [yesterday()], [today()], [tomorrow()], [week2date()], and the `start_of_*()`, `end_of_*()` and `nth_*()` functions; these return 1 date.
#' 
#' Week ranges always start on Mondays and end on Sundays.
#'
#' [year()] always returns an [integer].
#' @rdname days_around_today
#' @name days_around_today
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter
#' @importFrom lubridate as_date dweeks dmonths dyears floor_date ceiling_date
#' @export
#' @examples
#' today() %in% this_month()
#' 
#' # 2nd Monday of last month:
#' last_month() %>% nth_monday(2)
#'
#' df <- data.frame(date = sample(seq.Date(start_of_last_year(),
#'                                         end_of_this_year(),
#'                                         by = "day"),
#'                                size = 500))
#' df$time <- as.POSIXct(paste(df$date, "12:00:00"))
#' 
#' # these are equal:
#' df %>%
#'   filter(date %>% between(start_of_last_week(),
#'                           end_of_last_week()))
#' df %>%
#'   filter(date %in% last_week())
#'
#'
#' # be sure to transform times to dates in certain filters
#' df %>%
#'   filter(as.Date(time) %>% between(start_of_last_week(),
#'                                    end_of_last_week()))
#' df %>%
#'   filter(as.Date(time) %in% last_week())
#'   
#' \dontrun{
#' 
#' data <- certedb_getmmb(dates = last_week(only_start_end = TRUE))
#' data <- certedb_getmmb(where = db$o.ontvangstdatum %in% last_week())
#' data <- certedb_getmmb(where = db$dlt.modbac_datumtijd > start_of_this_month())
#' }
yesterday <- function(ref = today()) {
  ref <- as_date(ref)
  ref - 1
}

#' @rdname days_around_today
#' @importFrom lubridate today
#' @export
lubridate::today

#' @rdname days_around_today
#' @export
tomorrow <- function(ref = today()) {
  ref <- as_date(ref)
  ref + 1
}

#' @rdname days_around_today
#' @export
last_week <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_week(ref - dweeks(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
this_week <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- seq(from = floor_date(ref, unit = "week", week_start = 1),
             to = floor_date(ref, unit = "week", week_start = 1) + 6,
             by = "1 day")
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
next_week <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_week(ref + dweeks(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
last_month <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_month((start_of_this_month(ref) + 7) - dmonths(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
this_month <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- seq(from = floor_date(ref, "month"),
             to = ceiling_date(ref, "month") - 1,
             by = "1 day")
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
next_month <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_month((start_of_this_month(ref) + 7) + dmonths(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}


#' @rdname days_around_today
#' @export
last_quarter <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_quarter((start_of_this_month(ref) + 7) - dmonths(3))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
this_quarter <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- seq(from = floor_date(ref, unit = "quarter"),
             to = ceiling_date(ref, unit= "quarter") - 1,
             by = "1 day")
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
next_quarter <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_quarter((start_of_this_month(ref) + 7) + dmonths(3))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}


#' @rdname days_around_today
#' @export
last_year <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_year(ref - dyears(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
this_year <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- seq(from = floor_date(ref, "year"),
             to = ceiling_date(ref, "year") - 1,
             by = "1 day")
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
next_year <- function(ref = today(), only_start_end = FALSE) {
  ref <- as_date(ref)
  out <- this_year(ref + dyears(1))
  if (only_start_end == TRUE) {
    c(out[1], out[length(out)])
  } else {
    out
  }
}

#' @rdname days_around_today
#' @export
start_of_last_week <- function(ref = today(), day = 1) {
  ref <- as_date(ref)
  if (day == 0) day <- 7
  floor_date(ref, unit = "week", week_start = 0) - 7 + day
}

#' @rdname days_around_today
#' @export
end_of_last_week <- function(ref = today(), day = 7) {
  ref <- as_date(ref)
  if (day == 0) day <- 7
  floor_date(ref, unit = "week", week_start = 0) - 7 + day
}

#' @rdname days_around_today
#' @export
start_of_this_week <- function(ref = today(), day = 1) {
  ref <- as_date(ref)
  if (day == 0) day <- 7
  floor_date(ref, unit = "week", week_start = 0) + day
}

#' @rdname days_around_today
#' @export
end_of_this_week <- function(ref = today(), day = 7) {
  ref <- as_date(ref)
  if (day == 0) day <- 7
  floor_date(ref, unit = "week", week_start = 0) + day
}

#' @rdname days_around_today
#' @export
start_of_last_month <- function(ref = today()) {
  ref <- as_date(ref)
  last_month(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_last_month <- function(ref = today()) {
  ref <- as_date(ref)
  out <- last_month(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @export
start_of_this_month <- function(ref = today()) {
  ref <- as_date(ref)
  this_month(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_this_month <- function(ref = today()) {
  ref <- as_date(ref)
  out <- this_month(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @export
start_of_last_quarter <- function(ref = today()) {
  ref <- as_date(ref)
  last_quarter(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_last_quarter <- function(ref = today()) {
  ref <- as_date(ref)
  out <- last_quarter(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @export
start_of_this_quarter <- function(ref = today()) {
  ref <- as_date(ref)
  this_quarter(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_this_quarter <- function(ref = today()) {
  ref <- as_date(ref)
  out <- this_quarter(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @export
start_of_last_year <- function(ref = today()) {
  ref <- as_date(ref)
  last_year(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_last_year <- function(ref = today()) {
  ref <- as_date(ref)
  out <- last_year(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @export
start_of_this_year <- function(ref = today()) {
  ref <- as_date(ref)
  this_year(ref)[1]
}

#' @rdname days_around_today
#' @export
end_of_this_year <- function(ref = today()) {
  ref <- as_date(ref)
  out <- this_year(ref)
  out[length(out)]
}

#' @rdname days_around_today
#' @importFrom stringr str_detect str_match
#' @export
week2date <- function(wk, yr = year(Sys.Date()), day = 1) {
  if (day == 0) day <- 7
  
  # taken from ISOweek::ISOweek2date
  fn <- function(weekdate) {
    kPattern <- "^([0-9]{4})-W([0-9]{2})-([0-9]{1})$"
    stopifnot(all(is.na(weekdate) | str_detect(weekdate, kPattern)))
    wd_ywd <- str_match(weekdate, kPattern)
    if (all(is.na(weekdate))) {
      return(rep(as.Date(NA_character_), length.out = length(weekdate)))
    }
    stopifnot(ncol(wd_ywd) == 4)
    year <- wd_ywd[, 2]
    week <- as.integer(wd_ywd[, 3])
    weekday <- as.integer(wd_ywd[, 4])
    stopifnot(all(is.na(week) | (1 <= week & week <= 53)))
    stopifnot(all(is.na(weekday) | (1 <= weekday & weekday <=
                                      7)))
    january04 <- as.Date(ifelse(is.na(year), NA, paste(year, "01", "04", sep = "-")))
    
    thursday0 <- function(date) {
      date <- as.Date(date)
      ISOweekday <- function (date) {
        date <- as.Date(date)
        return(as.integer((as.integer(format(date, "%w")) + 6)%%7 + 1))
      }
      weekday0 <- function(date) ISOweekday(date) - 1L
      return(date - weekday0(date) + 3)
    }
    
    first_thursday <- thursday0(january04)
    nearest_thursday <- first_thursday + 7 * (week - 1)
    return(nearest_thursday - 4 + weekday)
  }
  
  # that function expects something like "2019-W10-1", so:
  fn(paste0(yr, "-W", formatC(wk, width = 2, flag = 0), "-", day))
  
}

nth_weekday <- function(ref = today(), n, weekday) {
  ref <- as_date(ref)
  out <- floor_date(ref[1], unit = "week", week_start = weekday)
  if (out < ref[1]) {
    out <- out + 7
  }
  out + ((n - 1) * 7)
}

#' @rdname days_around_today
#' @export
nth_monday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 1)
}

#' @rdname days_around_today
#' @export
nth_tuesday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 2)
}

#' @rdname days_around_today
#' @export
nth_wednesday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 3)
}

#' @rdname days_around_today
#' @export
nth_thursday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 4)
}

#' @rdname days_around_today
#' @export
nth_friday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 5)
}

#' @rdname days_around_today
#' @export
nth_saturday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 6)
}

#' @rdname days_around_today
#' @export
nth_sunday <- function(ref = today(), n = 1) {
  nth_weekday(ref = ref, n = n, weekday = 7)
}

#' @rdname days_around_today
#' @export
year <- function(x) {
  as.integer(lubridate::year(x))
}

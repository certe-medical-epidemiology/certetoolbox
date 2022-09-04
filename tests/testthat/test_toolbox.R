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

# test_that("cbs works", {
#   expect_true(is.data.frame(cbs_topics()))
#   expect_output(cbs_search("test"))
#   expect_message(cbs_search("certe"))
#   expect_message(cbs_search("inwoners"))
#   expect_true(is.data.frame(cbs_search("test") |> cbs_download()))
#   expect_error(cbs_download(mtcars))
#   cbs_search("test")
#   expect_true(is.data.frame(cbs_download(identifier = 3)))
#   pkg_env$cbs_identifiers <- NULL
#   expect_error(cbs_download(identifier = 3))
#   expect_output(cbs_search("test") |> cbs_download() |> cbs_moreinfo())
#   expect_error(cbs_moreinfo(mtcars))
#   cbs_search("test")
#   expect_output(cbs_moreinfo(identifier = 3))
#   pkg_env$cbs_identifiers <- NULL
#   expect_error(cbs_moreinfo(identifier = 3))
# })

test_that("character works", {
  expect_true(size_humanreadable(1234) |> is.character())
  expect_true(generate_identifier() |> is.character())
  expect_true(hospital_name("MZ") |> is.character())
  expect_true(is.na(hospital_name("test")))
  expect_true(p_symbol(0.05) |> is.character())
})

test_that("data.frame works", {
  expect_s3_class(tbl_flextable("test"), "flextable")
  expect_s3_class(tbl_flextable(mtcars), "flextable")
  expect_s3_class(tbl_flextable(mtcars, rows.height = 2), "flextable")
  expect_s3_class(tbl_flextable(mtcars, row.extra.header = list(values = letters[1:12], widths = rep(1, 12))), "flextable")
  expect_s3_class(tbl_flextable(mtcars, row.extra.footer = list(values = letters[1:12], widths = rep(1, 12))), "flextable")
  expect_warning(tbl_flextable(mtcars, align = letters))
  expect_warning(tbl_flextable(mtcars, align = "ll"))
  expect_warning(tbl_flextable(mtcars[, character(0)]))
  expect_s3_class(tbl_flextable(mtcars, vline = c(2, 4), rows.zebra = TRUE, columns.width = c(1, 3), autofit.fullpage = FALSE), "flextable")
  expect_warning(mtcars |> filter(cyl == 0) |> tbl_flextable())
  expect_s3_class(tbl_flextable(mtcars, column.total = TRUE), "flextable")
  expect_s3_class(tbl_flextable(mtcars, row.total = TRUE), "flextable")
  expect_s3_class(tbl_flextable(mtcars, row.total = TRUE, columns.percent = 9), "flextable")
  expect_s3_class(tbl_flextable(mtcars, column.names = c("1" = "column 1", "2" = "column 2")), "flextable")
  expect_warning(tbl_flextable(mtcars, column.names = letters))
  expect_warning(tbl_flextable(mtcars, column.names = c("a", "b", "5" = "c")))
  expect_s3_class(tbl_flextable(Sys.Date()), "flextable")
  expect_s3_class(data.frame(a = c(TRUE, FALSE), b = c(FALSE, TRUE)) |>
                    tbl_flextable(logicals = c(TRUE, FALSE)), "flextable")
  expect_s3_class(data.frame(a = c(TRUE, FALSE), b = c(FALSE, TRUE)) |>
                    tbl_flextable(logicals = c(TRUE, FALSE), values.colour = "FALSE", values.fill = "TRUE", values.bold = "FALSE", values.italic = "TRUE", values.fill.picker = "green"), "flextable")
  expect_s3_class(mtcars |> tbl_flextable(columns.percent = 8), "flextable")
  expect_true(inherits(tbl_markdown("test"), "list"))
  expect_true(inherits(tbl_markdown(Sys.Date()), "list"))
  expect_invisible(tbl_flextable(mtcars, print = TRUE))
  expect_true(inherits(data.frame(a = c(TRUE, FALSE),
                                  b = c(FALSE, TRUE)) |>
                         tbl_markdown(logicals = c(TRUE, FALSE)),
                       "list"))
  expect_true(inherits(mtcars |> tbl_markdown(columns.percent = 8, 9), "list"))
  expect_true(is.data.frame(auto_transform(as.data.frame("test"))))
  
  expect_s3_class(iris |> tbl_gtsummary(), "gtsummary")
  expect_s3_class(iris |> tbl_gtsummary(digits = list(Sepal.Length = 1)), "gtsummary")
  expect_s3_class(iris |> tbl_gtsummary(Species), "gtsummary")
  expect_s3_class(iris |> tbl_gtsummary(Species, digits = list(Sepal.Length = 1)), "gtsummary")
  expect_s3_class(iris |> tbl_gtsummary(Species, 
                                        add_n = TRUE,
                                        add_p = TRUE,
                                        add_ci = TRUE,
                                        add_overall = TRUE,
                                        statistic = dplyr::everything() ~ "{mean}"),
                  "gtsummary")
  iris2 <- iris
  iris2$Category <- sample(LETTERS[1:2], size = 150, replace = TRUE)
  expect_s3_class(iris2 |> tbl_gtsummary(c(Category, Species)), "gtsummary")
  expect_s3_class(iris2 |> tbl_gtsummary(c(Category, Species), digits = list(Sepal.Length = 1)), "gtsummary")
  
  expect_s3_class(iris |> tbl_gtsummary(Species) |> tbl_flextable(), "flextable")
  expect_s3_class(iris2 |> tbl_gtsummary(c(Category, Species)) |> tbl_flextable(), "flextable")
  
  expect_warning(auto_transform("test"))
})

test_that("datetime works", {
  expect_true(as.UTC(Sys.time()) |> inherits("POSIXct"))
  expect_true(mtcars |> as.UTC() |> is.data.frame())
  expect_identical(as.UTC(123), 123)
  expect_equal(yesterday(), Sys.Date()-1)
  expect_equal(tomorrow(), Sys.Date()+1)
  expect_true(week() |> is.numeric())
  expect_true(year() |> is.numeric())
  expect_true(last_week() |> inherits("Date"))
  expect_true(this_week() |> inherits("Date"))
  expect_true(next_week() |> inherits("Date"))
  expect_true(last_month() |> inherits("Date"))
  expect_true(this_month() |> inherits("Date"))
  expect_true(next_month() |> inherits("Date"))
  expect_true(last_quarter() |> inherits("Date"))
  expect_true(this_quarter() |> inherits("Date"))
  expect_true(next_quarter() |> inherits("Date"))
  expect_true(last_year() |> inherits("Date"))
  expect_true(this_year() |> inherits("Date"))
  expect_true(next_year() |> inherits("Date"))
  expect_true(start_of_last_week() |> inherits("Date"))
  expect_true(end_of_last_week() |> inherits("Date"))
  expect_true(start_of_this_week() |> inherits("Date"))
  expect_true(end_of_this_week() |> inherits("Date"))
  expect_true(start_of_last_month() |> inherits("Date"))
  expect_true(end_of_last_month() |> inherits("Date"))
  expect_true(start_of_this_month() |> inherits("Date"))
  expect_true(end_of_this_month() |> inherits("Date"))
  expect_true(start_of_next_month() |> inherits("Date"))
  expect_true(end_of_next_month() |> inherits("Date"))
  expect_true(start_of_last_quarter() |> inherits("Date"))
  expect_true(end_of_last_quarter() |> inherits("Date"))
  expect_true(start_of_this_quarter() |> inherits("Date"))
  expect_true(end_of_this_quarter() |> inherits("Date"))
  expect_true(start_of_next_quarter() |> inherits("Date"))
  expect_true(end_of_next_quarter() |> inherits("Date"))
  expect_true(start_of_last_year() |> inherits("Date"))
  expect_true(end_of_last_year() |> inherits("Date"))
  expect_true(start_of_this_year() |> inherits("Date"))
  expect_true(end_of_this_year() |> inherits("Date"))
  expect_true(start_of_next_year() |> inherits("Date"))
  expect_true(end_of_next_year() |> inherits("Date"))
  expect_true(nth_weekday(n = 1, weekday = 1) |> inherits("Date"))
  expect_true(nth_monday() |> inherits("Date"))
  expect_true(nth_tuesday() |> inherits("Date"))
  expect_true(nth_wednesday() |> inherits("Date"))
  expect_true(nth_thursday() |> inherits("Date"))
  expect_true(nth_friday() |> inherits("Date"))
  expect_true(nth_saturday() |> inherits("Date"))
  expect_true(nth_sunday() |> inherits("Date"))
  expect_true(week2date(1) |> inherits("Date"))
  expect_true(week2resp_season(1) |> is.ordered())
})

test_that("environment works", {
  library(dplyr)
  expect_identical(mtcars %>% remember(rows = nrow(.)), mtcars)
  expect_identical(pkg_env$temp$rows, recall(rows))
  # unnamed:
  expect_identical(mtcars %>% remember(nrow(.)), mtcars)
  expect_identical(pkg_env$temp$remember_temp, recall())
})

test_that("import_export works", {
  # check location parser
  expect_identical(basename(parse_file_location("test", "csv", card_number = 0)), "test.csv")
  expect_identical(basename(suppressWarnings(parse_file_location(".", "csv", card_number = 123))), "tbl.csv")
  
  # helper function for import/export checking:
  identical_import_export <- function(import_fn, export_fn, fileext,
                                      check_factors = TRUE, check_posix = TRUE,
                                      digits = Inf, ...) {
    # generate temporary file location:
    templocation <- tempfile(fileext = paste0(".", fileext))
    # generate data:
    old_df <- data.frame(doubles = round(runif(10, 5, 10), digits = digits),
                         integers = as.integer(runif(10, 5, 10)),
                         dates = Sys.Date() - c(1:10),
                         posix = as.UTC(as.POSIXct(Sys.Date() - c(1:10))),
                         characters = LETTERS[1:10],
                         factors = as.factor(LETTERS[1:10]),
                         stringsAsFactors = FALSE)
    if (!isTRUE(check_factors)) {
      old_df$factors <- as.character(old_df$factors)
    }
    if (!isTRUE(check_posix)) {
      old_df$posix <- as.Date(old_df$posix)
    }
    # export and import:
    suppressMessages(export_fn(old_df, templocation))
    new_df <- suppressMessages(import_fn(templocation, ...))
    if (!isTRUE(check_posix)) {
      new_df$dates <- as.Date(new_df$dates)
      new_df$posix <- as.Date(new_df$posix)
    }
    for (i in seq_len(ncol(new_df))) {
      # SPSS
      vct <- new_df[, i, drop = TRUE]
      attributes(vct)$format.spss <- NULL
      new_df[, i] <- vct
    }
    # clean up:
    unlink(templocation)
    # test:
    same <- identical(old_df, new_df)
    if (!isTRUE(same)) {
      cat("Non-identical columns for *", fileext, "*:", sep = "")
      non_identical <- which(!mapply(identical, old_df, new_df))
      for (i in seq_len(length(non_identical))) {
        cat("\n------------\n")
        cat(names(non_identical)[i], "\n")
        old <- old_df[, non_identical[i], drop = TRUE]
        new <- new_df[, non_identical[i], drop = TRUE]
        cat("\nBefore export/import (class: ", paste0(class(old), collapse = "/"), "):\n", sep = "")
        print(old)
        cat("\nAfter export/import (class: ", paste0(class(new), collapse = "/"), "):\n", sep = "")
        print(new)
      }
    }
    isTRUE(same)
  }
  # R
  expect_true(identical_import_export(import_rds, export_rds, "rds"))
  expect_true(identical_import_export(import, export, "rds"))
  # Text files
  expect_true(identical_import_export(import_csv, export_csv, "csv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import_csv2, export_csv2, "csv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import_tsv, export_tsv, "tsv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import_txt, export_txt, "txt",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "csv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "csv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "tsv",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "txt",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  # Excel
  expect_true(identical_import_export(import_xlsx, export_xlsx, "xlsx",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "xlsx",
                                      check_factors = FALSE, check_posix = FALSE, digits = 10))
  # SPSS
  expect_true(identical_import_export(import_sav, export_sav, "sav",
                                      check_factors = FALSE, digits = 10))
  expect_true(identical_import_export(import, export, "sav",
                                      check_factors = FALSE, digits = 10))
  # Apache
  expect_true(identical_import_export(import_feather, export_feather, "feather"))
  expect_true(identical_import_export(import, export, "feather"))
  expect_true(identical_import_export(import_parquet, export_parquet, "parquet"))
  expect_true(identical_import_export(import, export, "parquet"))
  export_feather(iris)
  export_feather(mtcars)
  expect_equal("iris" |> 
                 import_feather(col_select = dplyr::matches("d")) |> 
                 dim(),
               c(150, 2))
  expect_equal("mtcars" |> 
                 import_feather(col_select = dplyr::matches("d")) |> 
                 dim(),
               c(32, 2))
  unlink("iris.feather")
  unlink("mtcars.feather")
  
  # check overwrite function
  export_csv(iris, "iris_overwrite")
  expect_true(file_can_be_overwritten(TRUE, "iris_overwrite.csv"))
  expect_false(file_can_be_overwritten(FALSE, "iris_overwrite.csv"))
  if (!interactive()) {
    expect_message(export_csv(iris, "iris_overwrite"))
    expect_true(file_can_be_overwritten(NULL, "iris_overwrite.csv"))
  }
  mtime_old <- file.mtime("iris_overwrite.csv")
  Sys.sleep(1)
  export_csv(iris, "iris_overwrite", overwrite = TRUE)
  mtime_new <- file.mtime("iris_overwrite.csv")
  expect_lt(mtime_old, mtime_new)
  unlink("iris_overwrite.csv")
  
  # remote files
  expect_equal(dim(import_url("https://filesamples.com/samples/document/csv/sample1.csv")), c(8, 13))
  expect_equal(dim(import_url("https://filesamples.com/samples/document/xlsx/sample1.xlsx")), c(390, 5))
  expect_equal(dim(import_url("github.com/tidyverse/dplyr/blob/8abb54b60e40ef7c619156a12b14872cb2eb7989/data-raw/starwars.csv")), dim(dplyr::starwars))
  
  # export of graphical functions
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
  temp_pdf <- tempfile(fileext = ".pdf")
  
  if (Sys.info()["sysname"] %in% c("Windows", "Linux")) {
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a0"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a1"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a2"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a3"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a4"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a5"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a6"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a7"))))
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export_pdf(p, filename = temp_pdf, size = "a999"))))
    temp_png <- tempfile(fileext = ".png")
    expect_true(file.exists(suppressMessages(export_png(p, filename = temp_png))))
    
    temp_html <- tempfile(fileext = ".html")
    expect_true(file.exists(suppressMessages(export_html(p, filename = temp_html))))
    
    unlink(temp_pdf)
    expect_true(file.exists(suppressMessages(export(p, filename = temp_pdf))))
    unlink(temp_png)
    expect_true(file.exists(suppressMessages(export(p, filename = temp_png))))
    unlink(temp_html)
    expect_true(file.exists(suppressMessages(export(p, filename = temp_html))))
  }
  
  # importing a data.frame with rownames as first column should be transformed right
  temp_csv <- tempfile(fileext = ".csv")
  expect_message(export_csv(mtcars, temp_csv))
  expect_identical(rownames(import_csv(temp_csv)), rownames(mtcars))
  
  # test manual export function
  unlink(temp_csv)
  suppressWarnings(export(mtcars, temp_csv, fn = utils::write.table))
  expect_true(file.exists(temp_csv))
  unlink(temp_csv)
  suppressWarnings(export(mtcars, temp_csv, fn = "utils::write.table"))
  expect_true(file.exists(temp_csv))
  expect_error(export(mtcars, temp_csv, fn = "non-existing-function"))
  expect_error(export(mtcars, "file.nocluehowtosave"))
  
  expect_error(as_excel("text"))
})

test_that("universal works", {
  expect_identical(c("a", "b", "c") %like% "a", c(TRUE, FALSE, FALSE))
  expect_identical("a" %like% c("a", "b", "c") , c(TRUE, FALSE, FALSE))
  expect_identical(c("a", "b", "c") %like% c("a", "b", "c") , c(TRUE, TRUE, TRUE))
  expect_error(c("a", "b", "c") %like% c("a", "b"))
  expect_true("a" %like% "A")
  expect_false("a" %like_case% "A")
  expect_false("a" %unlike% "A")
  expect_true("a" %unlike_case% "A")
})

test_that("vctrs work", {
  library(dplyr, warn.conflicts = FALSE)
  df1 <- tibble(postcode = c(2,4,6))
  df2 <- tibble(postcode = as.character(c(1:10)),
                letter = letters[1:10])
  expect_warning(df1 |> left_join(df2))
  expect_warning(df2 |> left_join(df1))
  df1$postcode <- as.integer(df1$postcode)
  expect_warning(df1 |> left_join(df2))
  expect_warning(df2 |> left_join(df1))
})

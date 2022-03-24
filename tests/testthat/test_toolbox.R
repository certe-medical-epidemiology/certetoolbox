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

test_that("cbs works", {
  expect_true(is.data.frame(cbs_topics()))
  expect_true(is.data.frame(cbs_search("test")))
  expect_message(cbs_search("certe"))
  expect_message(cbs_search("inwoners"))
  expect_true(is.data.frame(cbs_search("test") %>% cbs_download()))
  expect_error(cbs_download(mtcars))
  cbs_search("test")
  expect_true(is.data.frame(cbs_download(identifier = 3)))
  options(cbs_identifiers = NULL)
  expect_error(cbs_download(identifier = 3))
  expect_output(cbs_search("test") %>% cbs_download() %>% cbs_moreinfo())
  expect_error(cbs_moreinfo(mtcars))
  cbs_search("test")
  expect_output(cbs_moreinfo(identifier = 3))
  options(cbs_identifiers = NULL)
  expect_error(cbs_moreinfo(identifier = 3))
})

test_that("character works", {
  expect_true(size_humanreadable(1234) %>% is.character())
  expect_true(generate_identifier() %>% is.character())
  expect_true(hospital_name("MZ") %>% is.character())
  expect_true(is.na(hospital_name("test")))
  expect_true(p_symbol(0.05) %>% is.character())
})

test_that("data.frame works", {
  expect_true(class(tbl_flextable("test")) == 'flextable')
  expect_true(class(tbl_markdown("test")) == 'list')
  expect_true(is.data.frame(auto_transform(as.data.frame("test"))))
})

test_that("datetime works", {
  expect_true(as.UTC(Sys.time()) %>% inherits("POSIXct"))
  expect_true(mtcars %>% as.UTC() %>% is.data.frame())
  expect_identical(as.UTC(123), 123)
  expect_equal(yesterday(), Sys.Date()-1)
  expect_equal(tomorrow(), Sys.Date()+1)
  expect_true(week() %>% is.numeric())
  expect_true(year() %>% is.numeric())
  expect_true(last_week() %>% inherits('Date'))
  expect_true(this_week() %>% inherits('Date'))
  expect_true(next_week() %>% inherits('Date'))
  expect_true(last_month() %>% inherits('Date'))
  expect_true(this_month() %>% inherits('Date'))
  expect_true(next_month() %>% inherits('Date'))
  expect_true(last_quarter() %>% inherits('Date'))
  expect_true(this_quarter() %>% inherits('Date'))
  expect_true(next_quarter() %>% inherits('Date'))
  expect_true(last_year() %>% inherits('Date'))
  expect_true(this_year() %>% inherits('Date'))
  expect_true(next_year() %>% inherits('Date'))
  expect_true(start_of_last_week() %>% inherits('Date'))
  expect_true(end_of_last_week() %>% inherits('Date'))
  expect_true(start_of_this_week() %>% inherits('Date'))
  expect_true(end_of_this_week() %>% inherits('Date'))
  expect_true(start_of_last_month() %>% inherits('Date'))
  expect_true(end_of_last_month() %>% inherits('Date'))
  expect_true(start_of_this_month() %>% inherits('Date'))
  expect_true(end_of_this_month() %>% inherits('Date'))
  expect_true(start_of_next_month() %>% inherits('Date'))
  expect_true(end_of_next_month() %>% inherits('Date'))
  expect_true(start_of_last_quarter() %>% inherits('Date'))
  expect_true(end_of_last_quarter() %>% inherits('Date'))
  expect_true(start_of_this_quarter() %>% inherits('Date'))
  expect_true(end_of_this_quarter() %>% inherits('Date'))
  expect_true(start_of_next_quarter() %>% inherits('Date'))
  expect_true(end_of_next_quarter() %>% inherits('Date'))
  expect_true(start_of_last_year() %>% inherits('Date'))
  expect_true(end_of_last_year() %>% inherits('Date'))
  expect_true(start_of_this_year() %>% inherits('Date'))
  expect_true(end_of_this_year() %>% inherits('Date'))
  expect_true(start_of_next_year() %>% inherits('Date'))
  expect_true(end_of_next_year() %>% inherits('Date'))
  expect_true(nth_weekday(n = 1, weekday = 1) %>% inherits('Date'))
  expect_true(nth_monday() %>% inherits('Date'))
  expect_true(nth_tuesday() %>% inherits('Date'))
  expect_true(nth_wednesday() %>% inherits('Date'))
  expect_true(nth_thursday() %>% inherits('Date'))
  expect_true(nth_friday() %>% inherits('Date'))
  expect_true(nth_saturday() %>% inherits('Date'))
  expect_true(nth_sunday() %>% inherits('Date'))
  expect_true(week2date(1) %>% inherits('Date'))
  expect_true(week2resp_season(1) %>% is.ordered())
})

test_that("environment works", {
  mtcars %>% remember(rows = nrow(.))
  expect_identical(pkg_env$temp$rows, recall(rows))
})

test_that("import_export works", {
  tmp <- tempdir()
  tmp_file <-  paste0(tmp, "/test.rds")
  export_rds(mtcars, filename = tmp_file, card_number = NULL)
  imp <- import_rds(tmp_file)
  expect_identical(mtcars, imp)
  
  
})

test_that("vctrs work", {
  library(dplyr, warn.conflicts = FALSE)
  df1 <- tibble(postcode = c(2,4,6))
  df2 <- tibble(postcode = as.character(c(1:10)),
                letter = letters[1:10])
  expect_warning(df1 %>% left_join(df2))
  expect_warning(df2 %>% left_join(df1))
  df1$postcode <- as.integer(df1$postcode)
  expect_warning(df1 %>% left_join(df2))
  expect_warning(df2 %>% left_join(df1))
})

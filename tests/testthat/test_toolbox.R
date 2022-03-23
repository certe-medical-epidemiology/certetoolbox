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
  
})

test_that("character works", {
  
})

test_that("data.frame works", {
  
})

test_that("datetime works", {
  
})

test_that("environment works", {
  
})

test_that("import_export works", {
  
})

test_that("utils work", {
  
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
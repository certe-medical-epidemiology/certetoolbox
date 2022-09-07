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

#' Update Data Set Based on Row Numbers
#' 
#' Update a [data.frame] using specific integers for row numbers or a vectorised filter. Also supports dplyr groups. see *Examples*.
#' @method update data.frame
#' @param object a [data.frame]
#' @param rows row numbers or a [logical] vector
#' @param ... arguments passed on to [mutate()]
#' @importFrom dplyr mutate row_number filter pull bind_rows arrange select
#' @name update
#' @aliases update
#' @export
#' @examples 
#' iris |> 
#'   update(3:4, Species = c("A", "B")) |> 
#'   head()
#'   
#' iris |> 
#'   update(Species == "setosa" & Sepal.Length > 5,
#'          Species = "something else") |> 
#'   head()
#'   
#' if (require("dplyr")) {
#' 
#'   # also supports dplyr groups:
#'   iris |> 
#'     group_by(Species) |>
#'     # update every 2nd to 4th row in group
#'     update(2:4, Species = "test") |> 
#'     # groups will be updated automatically
#'     count()
#' 
#' }
update.data.frame <- function(object, rows, ...) {
  
  # original sort (with or without dplyr groups)
  object$.row_original <- seq_len(nrow(object))
  # sort within dplyr groups
  object <- object |> 
    mutate(.row_index = row_number())
  
  # support filters given in `rows`
  if (!isTRUE(try(is.numeric(rows), silent = TRUE))) {
    rows <- object |> 
      filter({{ rows }}) |> 
      pull(.row_original)
  }
  
  object |> 
    filter(.row_index %in% rows) |> 
    mutate(...) |> 
    bind_rows(object |> filter(!.row_index %in% rows)) |> 
    # return to original sort
    arrange(.row_original) |> 
    select(-c(.row_index, .row_original))
}

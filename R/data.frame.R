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

#' Format Data Set as Flextable
#'
#' Format a [data.frame] as [flextable()] with Certe style, bold headers and Dutch number formats. This function can also transform existing `flextable` and `gtsummary` objects to allow the formatting provided in this [tbl_flextable()] function.
#' @param x a [data.frame] or a [`flextable`][flextable::flextable()] object or a [`gtsummary`][gtsummary::tbl_summary()] object
#' @param rows.height height of the rows in centimetres
#' @param row.names.bold display row names in bold
#' @param rows.italic column indexes of rows in italics
#' @param rows.bold column indexes of rows in bold
#' @param rows.fill the column indices of rows to be shaded
#' @param rows.zebra banded rows in the body - equivalent to `rows.fill = seq(2, nrow(x), 2)`
#' @param row.total add a row total (at the bottom of the table)
#' @param row.total.name name of the row total
#' @param row.total.function function used to calculate all numeric values per column (non-numeric columns are skipped)
#' @param row.total.widths cell width in row total
#' @param row.total.bold bold formatting of row total
#' @param row.extra.header an extra header to be displayed above the table
#' @param row.extra.footer an extra footer to show below the table
#' @param columns.width width of columns. For `autofit.fullpage = TRUE`, these are proportions to `autofit.fullpage.width`. For `autofit.fullpage = FALSE`, these are centimeters
#' @param column.names.bold display column names in bold
#' @param columns.italic column indices of columns to be displayed in italics
#' @param columns.bold column indices of columns in bold
#' @param columns.fill the column indices of rows to be shaded
#' @param columns.zebra banded columns - equivalent to `columns.fill = seq(2, ncol(x), 2)`
#' @param column.total adding a column total (to the right of the table)
#' @param column.total.name name of the column total
#' @param column.total.function function used to calculate all numeric values per row
#' @param column.total.bold bold formatting of column total
#' @param decimal.mark decimal separator, defaults to [dec_mark()]
#' @param big.mark thousands separator, defaults to [big_mark()]
#' @param font.family table font family
#' @param font.size table font size
#' @param font.size.header font size of header
#' @param values.colour,values.fill,values.bold,values.italic values to be formatted
#' @param autofit format table in width automatically. This will apply [`autofit()`][flextable::autofit()].
#' @param autofit.fullpage display table across width of page
#' @param autofit.fullpage.width set number of centimetres to width of table
#' @param vline indices of columns to have a vertical line to their right
#' @param vline.part part of the table where the vertical lines should be placed ("all", "header", "body", "footer")
#' @param ... not used
#' @param row.names row names to be displayed. Will be `1:nrow(x)` if set to TRUE, but can be a vector of values.
#' @param column.names column names to be displayed. Can also be a named vector where the names are existing columns, or indices of columns. When this vector is smaller than `ncol(x)`, only the first `length(column.names)` are replaced. When this vector is longer than `ncol(x)`, all column names are replaced
#' @param align default is "c", which aligns everything centrally. Use "r", "l", "c" and "j"/"u" (justify/align) to change alignment. Can be a vector or a character (like "lrrrcc")
#' @param align.part part of the table where the alignment should take place ("all", "header", "body", "footer")
#' @param caption table caption
#' @param na text for missing values
#' @param format.dates see [`format2()`][certestyle::format2()]
#' @param logicals vector with two values that replace `TRUE` and `FALSE`
#' @param columns.percent display the column indices as percentages using [`format2()`][certestyle::format2()] - example: `columns.percent = c(2, 3)`
#' @param round.numbers number of decimal places to round up for numbers
#' @param round.percent number of decimal places to round to when using `columns.percent`
#' @param theme a Certe colour theme, defaults to [`current_markdown_colour()`][certestyle::current_markdown_colour()] which determines the Certe colour based on a markdown YAML header and defaults to `"certeblauw"`. Can also be `"certeroze"`, `"certegroen"`, etc. This will set the list in `colours` and will be ignored if `colours` is set manually. Can be set to "white" for a clean look.
#' @param colours a [list] with the following named character values: `rows.fill.even`, `rows.fill.odd`, `columns.fill`, `values.fill`, and `values.colour`. All values will be evaluated with [`colourpicker()`][certestyle::colourpicker()].
#' @param split.across.pages a [logical] whether tables are allowed to split across page. This argument only has effect for PDF output.
#' @param print forced printing (required in a `for` loop), default is `TRUE` in non-interactive sessions
#' @details Run [tbl_markdown()] on a `flextable` object to transform it into markdown for use in Quarto or R Markdown reports. If `print = TRUE` in non-interactive sessions (Quarto or R Markdown), the `flextable` object will also be printed in markdown.
#' 
#' The value for `theme` is dependent on whether a colour is set in the markdown YAML header. Otherwise, use `theme` to set a Certe colour theme, defaults to `"certeblauw"`:
#' 
#' ```r
#' # from the example below
#' tbl_flextable(df)
#' ````
#' 
#' ![tbl_flextable_certeblauw](flextableblauw.png)
#' 
#' ```r
#' tbl_flextable(df, theme = "certeroze")
#' ````
#' 
#' ![tbl_flextable_certeroze](flextableroze.png)
#' 
#' ```r
#' tbl_flextable(df, theme = "certegeel")
#' ````
#' 
#' ![tbl_flextable_certegeel](flextablegeel.png)
#' 
#' ```r
#' tbl_flextable(df, theme = "certegroen", vline = c(2:3))
#' ````
#' 
#' ![tbl_flextable_certegroen](flextablegroen.png)
#' 
#' ```r
#' tbl_flextable(
#'   df,
#'   theme = "certelila",
#'   row.total = TRUE,
#'   row.total.function = median,
#'   round.numbers = 4,
#'   row.extra.header = list(values = LETTERS[1:5])
#' )
#' ```
#' 
#' ![tbl_flextable_certelila](flextablelila.png)
#' @seealso [flextable()]
#' @return [flextable] object
#' @rdname tbl_flextable
#' @importFrom certestyle colourpicker format2 current_markdown_colour
#' @importFrom dplyr bind_cols pull
#' @importFrom flextable flextable border fp_border_default add_footer_row color bg bold italic set_header_labels fontsize font width height flextable_dim autofit align set_caption hline vline add_header_row set_flextable_defaults hline_top hline_bottom
#' @importFrom cleaner as.percentage
#' @export
#' @examples
#' \dontrun{
#' 
#' # generate a data.frame
#' df <- data.frame(text = LETTERS[1:10],
#'                  `decimal numbers` = runif(10, 0, 10),
#'                  `whole numbers` = as.integer(runif(10, 0, 10)),
#'                  `logical values` = as.logical(round(runif(10, 0, 1))),
#'                  dates = today() - runif(10, 200, 2000),
#'                  stringsAsFactors = FALSE)
#'
#' # default
#' tbl_flextable(df)      # dataset has no row names
#' tbl_flextable(mtcars)  # dataset has row names
#' 
#' # print in markdown
#' df |> 
#'   tbl_flextable() |> 
#'   tbl_markdown()
#'   
#' # transform a gtsummary to a flextable
#' iris |>
#'   tbl_gtsummary(Species, add_p = TRUE) |>
#'   tbl_flextable()
#'   
#' # extra formatting
#' tbl_flextable(df,
#'               logicals = c("X", "-"),     # replaces TRUE en FALSE
#'               values.colour = "X",
#'               values.fill = "X",
#'               row.names = "S. aureus",
#'               columns.italic = 1,
#'               format.dates = "ddd dd-mm-yy",
#'               round.numbers = 3)
#'
#' # row totals
#' tbl_flextable(df,
#'               row.total = TRUE,           # add row total
#'               row.total.function = max,   # instead of sum()
#'               row.total.name = "Maximum", # also works with dates
#'               columns.percent = 2,        # 2nd column as percentages
#'               round.percent = 0)          # rounding percentages
#'
#' # column names
#' tbl_flextable(df,
#'               column.names = c("1" = "Column 1",
#'                                "2" = "Column 2",
#'                                dates = "DATES!"))
#' tbl_flextable(df,
#'               column.names = LETTERS)
#'
#' # vertical lines, alignment and row names
#' tbl_flextable(df,
#'               align = "lrrcc", # also works: c("l", "r", "r", "c", "c")
#'               font.size = 12,
#'               vline = c(2, 4),
#'               vline.part = "all",
#'               row.names = paste("Experiment", 1:10))
#'
#' # width of cells and table
#' tbl_flextable(data.frame(test1 = "A", test2 = "B"),
#'               vline = 1,
#'               autofit.fullpage.width = 16, # default values in cm
#'               columns.width = c(1, 3))     # ratio; cells become 4 and 12 cm
#'
#' tbl_flextable(data.frame(test1 = "A", test2 = "B"),
#'               vline = 1,
#'               autofit.fullpage = FALSE,    # no fullpage autofit
#'               columns.width = c(1, 3))     # cells become 1 and 3 cm
#'               
#' # adding extra header or footer
#' tbl_flextable(data.frame(test1 = "A", test2 = "B"),
#'               row.extra.header = list(values = c("Header", "Header"),
#'                                       widths = c(1, 1)),
#'               row.extra.footer = list(values = c("Footer", "Footer"),
#'                                       widths = c(1, 1)))
#' }
tbl_flextable <- function(x,
                          row.names = rownames(x),
                          row.names.bold = TRUE,
                          rows.italic = NULL,
                          rows.bold = NULL,
                          rows.height = NULL,
                          rows.fill = NULL,
                          rows.zebra = TRUE,
                          row.total = FALSE,
                          row.total.name = "Totaal",
                          row.total.function = sum,
                          row.total.widths = NULL,
                          row.total.bold = TRUE,
                          row.extra.header = list(values = NULL, widths = 1),
                          row.extra.footer = list(values = NULL, widths = 1),
                          column.names = colnames(x),
                          column.names.bold = TRUE,
                          columns.width = NULL,
                          columns.percent = NULL,
                          columns.italic = NULL,
                          columns.bold = NULL,
                          columns.fill = NULL,
                          columns.zebra = FALSE,
                          column.total = FALSE,
                          column.total.name = "Totaal",
                          column.total.function = sum,
                          column.total.bold = TRUE,
                          align = "c",
                          align.part = "all",
                          caption = "",
                          na = "",
                          logicals = c("X", ""),
                          round.numbers = 2,
                          round.percent = 1,
                          format.dates = "d mmm yyyy",
                          decimal.mark = dec_mark(),
                          big.mark = big_mark(),
                          font.family = "Calibri",
                          font.size = 9,
                          font.size.header = font.size + 1,
                          values.colour = NULL,
                          values.fill = NULL,
                          values.bold = NULL,
                          values.italic = NULL,
                          autofit = is.null(columns.width) & is.null(rows.height),
                          autofit.fullpage = TRUE,
                          autofit.fullpage.width = 16,
                          vline = NULL,
                          vline.part = c("body", "footer"),
                          theme = current_markdown_colour(),
                          colours = list(
                            rows.fill.even = paste0(theme, "6"),
                            rows.fill.odd = paste0(theme, "5"),
                            columns.fill = paste0(theme, "5"),
                            values.fill = paste0(theme, "3"),
                            values.colour = theme,
                            vline.colour = theme,
                            hline.colour = theme,
                            header.fill = theme,
                            header.colour = "white",
                            vline.header.colour = "white"),
                          split.across.pages = NROW(x) > 37,
                          print = !interactive(),
                          ...) {
  
  if (any(c("rows.fill.picker", "values.colour.picker", "values.fill.picker", "columns.fill.picker", "vline.border") %in% names(list(...)))) {
    stop("Set a tbl_flextable() theme now with the arguments `theme` or `colours`.", call. = FALSE)
  }
  if (identical(row.names, as.character(seq_len(NROW(x)))) || is.null(row.names)) {
    row.names <- FALSE
  } else if (isTRUE(row.names)) {
    row.names <- as.character(seq_len(NROW(x)))
  }
  
  if (theme == "white") {
    colours <- list(
      rows.fill.even = "#eeeeee",
      rows.fill.odd = "white",
      columns.fill = "#eeeeee",
      values.fill = "#cccccc",
      values.colour = "#999999",
      vline.colour = "black",
      hline.colour = "black",
      header.fill = "white",
      header.colour = "black",
      vline.header.colour = "black")
    if (missing(rows.zebra)) {
      # default zebra rows FALSE
      rows.zebra <- FALSE
    }
  } else if (theme %unlike% "^certe[a-z]+$") {
    stop("`theme` cannot be \"", theme, "\" - it must be \"white\" or a Certe colour theme, either ",
         toString(paste0('"', unique(gsub("[^a-z]" , "", names(certestyle::certe.colours))), '"')),
         call. = FALSE)
  } else if (theme %in% c("certegeel", "certezachtlila") && missing(colours)) {
    colours$header.colour <- "black"
  }
  
  if (inherits(x, "gtsummary")) {
    check_is_installed("gtsummary")
    gt <- x
    # get the flextable calls
    gt_flex <- gt |> gtsummary::as_flex_table(return_calls = TRUE)
    # remove formatting functions
    gt_flex <- gt_flex[names(gt_flex) %unlike% "border|padding"]
    # apply to gtsummary object
    .eval_list_of_exprs <- get(".eval_list_of_exprs", envir = asNamespace("gtsummary"))
    x <- .eval_list_of_exprs(gt_flex)
    if (length(unique(gt$table_styling$header$spanning_header)) > 1) {
    # fix for double header
    x <- x |>
      border(i = 1:2, j = 1,
             border.bottom = fp_border_default(NA),
             border.top = fp_border_default(NA),
             part = "header") |>
      border(i = 1, j = 1,
             border.top = fp_border_default(width = 2),
             part = "header") |>
      border(i = 2, j = 1,
             border.bottom = fp_border_default(width = 2),
             part = "header")
    }
  }
  
  if (inherits(x, "flextable")) {
    ft <- x
    colnames_bak <- NULL
    x <- x$body$dataset
    x.bak <- x
    
  } else {
    
    if (NROW(x) == 0) {
      warning("`x` has no rows")
    }
    if (NCOL(x) == 0) {
      warning("`x` has no columns")
    }
    
    # be nice in case of typing errors
    dots <- list(...)
    if (is.null(vline) && missing(vline)) {
      vline <- dots$vline.columns
    }
    if (!is.null(dots$vline.parts)) {
      vline.part <- dots$vline.parts
    }
    if (!is.null(dots$col.names)) {
      column.names <- dots$col.names
    }
    if (!is.null(dots$row.height)) {
      rows.height <- dots$row.height
    }
    
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    
    if (identical(row.names, as.character(c(seq_len(NROW(x))))) & missing(row.names)) {
      # rownames not set, are thus 1:nrow(x)
      row.names <- FALSE
    }
    
    if (!isFALSE(row.names)) {
      if (isTRUE(row.names)) {
        row.names <- seq_len(NROW(x))
      } else if (length(row.names) == 1) {
        row.names <- rep(row.names, NROW(x))
      } else if (length(row.names) != NROW(x)) {
        stop("length of row.names is not equal to number of rows")
      }
      row.names <- as.character(row.names)
      x <- bind_cols(data.frame(`col_temp_flextbl_` = row.names, stringsAsFactors = FALSE), x)
    }
    
    if (is.null(ncol(x))) {
      col_count <- 1
    } else {
      col_count <- ncol(x)
    }
    
    if (column.total == TRUE) {
      column.total.df <- x[, vapply(FUN.VALUE = logical(1), x, is.numeric), drop = FALSE]
      column.total.values <- apply(column.total.df,
                                   1,
                                   function(x,
                                            FUN = column.total.function) {
                                     if (is.numeric(x)) {
                                       FUN(x)
                                     } else {
                                       ""
                                     }
                                   })
      if (!all(x == "")) {
        x$col_temp_flextbl_2 <- column.total.values
        colnames(x)[ncol(x)] <- column.total.name
      }
    }
    
    x.bak <- x
    
    columns.percent <- columns.percent + as.integer(!isFALSE(row.names))
    columns.italic <- columns.italic + as.integer(!isFALSE(row.names))
    columns.bold <- columns.bold + as.integer(!isFALSE(row.names))
    columns.fill <- columns.fill + as.integer(!isFALSE(row.names))
    for (i in seq_len(col_count)) {
      if (inherits(x[1, i, drop = TRUE], c("Date", "POSIXct", "POSIXlt"))) {
        x[, i] <- x |>
          pull(i) |> 
          as.Date() |> 
          format2(format = format.dates)
      }
      if (is.logical(x[1, i, drop = TRUE])) {
        x[, i] <- gsub("TRUE", logicals[1],
                       gsub("FALSE", logicals[2],
                            x |>
                              pull(i) |> 
                              as.character()))
      }
      if (inherits(x[1, i, drop = TRUE], c("double", "integer", "numeric", "single")) &
          !i %in% columns.percent) {
        x[, i] <- x |>
          pull(i) |> 
          format2(round = round.numbers, decimal.mark = decimal.mark, big.mark = big.mark)
      }
      if (i %in% columns.percent) {
        x[, i] <- x |> 
          pull(i) |>
          as.percentage() |> 
          format2(round = round.percent, decimal.mark = decimal.mark, big.mark = big.mark)
      }
    }
    
    # replace NAs
    colnames.bak <- colnames(x)
    x <- as.data.frame(lapply(x,
                              function(x, na_val = na) {
                                x <- as.character(x)
                                x[is.na(x)] <- na_val
                                x
                              }),
                       stringsAsFactors = FALSE)
    colnames(x) <- colnames.bak
    
    # colnames BEFORE the flextable (set_header_labels is hard...)
    colnames_bak <- NULL
    if (length(column.names) > 0) {
      if (!is.null(names(column.names))) {
        # NAMED COLUMNS
        if (length(column.names) != ncol(x) && any(names(column.names) == "", na.rm = TRUE)) {
          warning("Not all columns are named")
        }
        for (i in seq_len(length(column.names))) {
          col.name <- names(column.names)[i]
          if (col.name != "") {
            if (as.character(col.name) %in% as.character(seq_len(ncol(x)))) {
              # is an index of a column
              colnames(x)[as.integer(col.name)] <- column.names[i]
            } else {
              colnames(x)[colnames(x) == col.name] <- column.names[i]
            }
          } else {
            colnames(x)[i] <- column.names[i]
          }
        }
      } else {
        # UNNAMED COLUMNS
        if (length(column.names) != ncol(x)) {
          if (length(column.names) < ncol(x)) {
            warning("Only the first ", length(column.names), " column names will be replaced")
            colnames(x)[seq_len(length(column.names))] <- column.names
          } else {
            warning("Only the first ", ncol(x), " items of column.names will be used")
            colnames(x) <- column.names[seq_len(ncol(x))]
          }
        } else {
          colnames(x) <- column.names
        }
        if (colnames(x)[1] == "col_temp_flextbl_") {
          colnames(x)[1] <- " "
        }
      }
      # support duplicate colnames, so this will work:
      # data.frame(col1 = 1, col2 = 2) |>
      #   tbl_flextable(column.names = c(col1 = "test", col2 = "test"))
      colnames_bak <- colnames(x)
      # A to ZZ (total 26 + 26 * 26 = 702)
      letter_vector <- c(LETTERS, unlist(lapply(LETTERS, function(x) paste0(x, LETTERS))))
      colnames(x) <- letter_vector[seq_len(ncol(x))]
      names(colnames_bak) <- colnames(x) # this will make set_header_labels work, further down
    }
    
    ft <- flextable(x)
  }
  
  # row and column names
  if (row.total == TRUE) {
    ind <- 0
    row.total.values <- vapply(FUN.VALUE = character(1),
                               x.bak,
                               function(x,
                                        FUN = row.total.function,
                                        dec = decimal.mark,
                                        big = big.mark,
                                        date_format = format.dates,
                                        round = round.numbers,
                                        round_pct = round.percent) {
                                 ind <<- ind + 1
                                 if (ind %in% columns.percent) {
                                   x |> 
                                     FUN() |>
                                     as.percentage() |>
                                     format2(round = round.percent, decimal.mark = dec, big.mark = big)
                                 } else if (!identical(FUN, sum) & inherits(x, c("Date", "POSIXt"))) {
                                   x |>
                                     FUN() |>
                                     format2(format = date_format)
                                 } else if (is.numeric(x)) {
                                   x |> 
                                     FUN() |>
                                     format2(round = round, decimal.mark = dec, big.mark = big)
                                 } else {
                                   ""
                                 }
                               })
    if (!all(row.total.values == "")) {
      # add name to first col
      row.total.values[1] <- row.total.name
      if (is.null(row.total.widths)) {
        row.total.widths <- 1
      }
      if (length(row.total.widths) == 1) {
        row.total.widths <- rep(row.total.widths, ncol(x))
      }
      ft <- ft |>
        add_footer_row(values = row.total.values,
                       colwidths = row.total.widths)
      if (row.total.bold == TRUE) {
        ft <- ft |>
          bold(part = "footer", i = NROW(ft$footer$dataset))
      }
    }
  }
  if (is.list(row.extra.header)) {
    if (!is.null(row.extra.header$values)) {
      if (length(row.extra.header$widths) == 1) {
        row.extra.header$widths <- rep(row.extra.header$widths, ncol(x))
      }
      ft <- ft |>
        add_header_row(values = row.extra.header$values,
                       colwidths = row.extra.header$widths)
    }
  }
  if (is.list(row.extra.footer)) {
    if (!is.null(row.extra.footer$values)) {
      if (length(row.extra.footer$widths) == 1) {
        row.extra.footer$widths <- rep(row.extra.footer$widths, ncol(x))
      }
      ft <- ft |>
        add_footer_row(values = row.extra.footer$values,
                       colwidths = row.extra.footer$widths)
    }
  }
  
  # format specific values
  if (!is.null(values.colour)) {
    ind <- which(as.matrix(x) == as.character(values.colour), arr.ind = TRUE)
    for (row in seq_len(NROW(ind))) {
      ft <- ft |> color(i = ind[row, "row"],
                        j = ind[row, "col"],
                        color = colourpicker(colours$values.colour))
    }
  }
  if (!is.null(values.fill)) {
    ind <- which(as.matrix(x) == as.character(values.fill), arr.ind = TRUE)
    for (row in seq_len(NROW(ind))) {
      ft <- ft |> bg(i = ind[row, "row"],
                     j = ind[row, "col"],
                     bg = colourpicker(colours$values.fill))
    }
  }
  if (!is.null(values.bold)) {
    ind <- which(as.matrix(x) == as.character(values.bold), arr.ind = TRUE)
    for (row in seq_len(NROW(ind))) {
      ft <- ft |> bold(i = ind[row, "row"],
                       j = ind[row, "col"])
    }
  }
  if (!is.null(values.italic)) {
    ind <- which(as.matrix(x) == as.character(values.italic), arr.ind = TRUE)
    for (row in seq_len(NROW(ind))) {
      ft <- ft |> italic(i = ind[row, "row"],
                         j = ind[row, "col"])
    }
  }
  
  # format column total
  if (column.total == TRUE & column.total.bold == TRUE) {
    ft <- ft |> bold(j = ncol(x.bak), part = "all")
  }
  
  # vertical lines
  if (!is.null(vline)) {
    for (i in seq_len(length(vline.part))) {
      ft <- ft |> vline(border = fp_border_default(colourpicker(colours$vline.colour)),
                        j = vline,
                        part = vline.part[i])
    }
  }
  # horizontal lines on top and bottom
  ft <- ft |>
    border(part = "all",
           # this is to remove border between multiline headers
           border.top = fp_border_default(NA),
           border.bottom = fp_border_default(NA)) |> 
    hline_top(part = "all",
              border = fp_border_default(colourpicker(colours$hline.colour), width = 2)) |> 
    hline_bottom(part = "all",
                 border = fp_border_default(colourpicker(colours$hline.colour), width = 2)) |> 
    hline_bottom(part = "footer",
                 border = fp_border_default(NA))
  # header colours
  ft <- ft |> 
    bg(part = "header",
       bg = colourpicker(colours$header.fill)) |> 
    color(part = "header",
          color = colourpicker(colours$header.colour))
  if (!is.null(vline)) {
    ft <- ft |> vline(part = "header",
                      border = fp_border_default(colourpicker(colours$vline.header.colour)),
                      j = vline)
  }
  
  # zebra print
  if (isTRUE(rows.zebra)) {
    rows.fill <- seq_len(NROW(x))
  }
  if (isTRUE(columns.zebra)) {
    columns.fill <- seq(2, ncol(x), 2)
  }
  
  # Certe theme
  ft <- ft |>
    font(fontname = font.family, part = "all") |>
    fontsize(size = font.size, part = "all")
  if (caption != "") {
    ft <- ft |> set_caption(caption)
  }
  # bold headers
  if (column.names.bold == TRUE) {
    ft <- ft |> bold(part = "header")
  }
  # bold row names
  if (!isFALSE(row.names) & row.names.bold == TRUE) {
    ft <- ft |> bold(j = 1)
  }
  if (font.size.header != font.size) {
    ft <- ft |> fontsize(size = font.size.header, part = "header")
  }
  if (length(columns.italic) > 0) {
    ft <- ft |> italic(j = columns.italic)
  }
  if (length(columns.bold) > 0) {
    ft <- ft |> bold(j = columns.bold)
  }
  if (!is.null(rows.italic)) {
    ft <- ft |> italic(i = rows.italic)
  }
  if (!is.null(rows.bold)) {
    ft <- ft |> bold(i = rows.bold)
  }
  if (!is.null(colnames_bak)) {
    ft <- ft |> set_header_labels(values = colnames_bak)
  }
  if (!is.null(rows.fill)) {
    ft <- ft |>
      bg(i = rows.fill[which(rows.fill %% 2 == 0)],
         bg = colourpicker(colours$rows.fill.even),
         part = "body")
    ft <- ft |>
      bg(i = rows.fill[which(rows.fill %% 2 != 0)],
         bg = colourpicker(colours$rows.fill.odd),
         part = "body")
  }
  if (length(columns.fill) > 0) {
    ft <- ft |>
      bg(j = columns.fill,
         bg = colourpicker(colours$columns.fill),
         part = "body")
  }
  
  # set width
  if (!is.null(columns.width)) {
    if (length(columns.width) == 1) {
      columns.width <- rep(columns.width, ncol(x))
    }
    if (autofit.fullpage.width == TRUE) {
      # if autofit.fullpage = TRUE, then widths must not be centimetres,
      # but ratios of autofit.fullpage.width
      columns.width <- (columns.width / sum(columns.width)) * autofit.fullpage.width
    } else {
      # otherwise it's centimetres, but flextable() works with inches, so:
      columns.width <- columns.width / 2.54
    }
    for (j in seq_len(ncol(x))) {
      ft <- ft |> width(j = j, width = columns.width[j])
    }
  }
  # set height
  if (!is.null(rows.height)) {
    if (length(rows.height) == 1) {
      rows.height <- rep(rows.height, NROW(x))
    }
    rows.height <- rows.height / 2.54
    for (i in seq_len(NROW(x))) {
      ft <- ft |> height(i = i, height = rows.height[i])
    }
  }
  
  if (autofit == TRUE) {
    ft <- ft |> autofit()
  }
  if (autofit.fullpage == TRUE) {
    # width as inch
    ft <- ft |> width(width = dim(ft)$widths * (autofit.fullpage.width / 2.54) / (flextable_dim(ft)$widths))
  }
  
  # alignment
  if (!is.null(align)) {
    align_setting <- substr(unlist(strsplit(align, "")), 1, 1) # now a vector with "c", "l", "r" etc.
    if (length(align_setting) == 1) {
      align_setting <- rep(align_setting, ncol(x))
    }
    if (!isFALSE(row.names) && length(align_setting) == ncol(x) - 1) {
      align_setting <- c("l", align_setting)
    }
    if (length(align_setting) != ncol(x)) {
      if (length(align_setting) < ncol(x)) {
        warning("Only the first ", length(align_setting), " columns have set alignment - the remaining ",
                ncol(x) - length(align_setting), " will be centred")
        align_setting <- c(align_setting, rep("c", ncol(x) - length(align_setting)))
      } else {
        warning("Only the first ", ncol(x), " alignment settings will be used")
        align_setting <- align_setting[seq_len(ncol(x))]
      }
    }
    if (!isFALSE(row.names)) {
      # first column are rownames, always left aligned
      align_setting[1] <- "l"
    }
    align_setting[align_setting == "u"] <- "j" # 'justify' instead of 'uitlijnen'
    if (any(align_setting == "l")) {
      ft <- ft |> align(align = "left", j = which(align_setting == "l"), part = align.part)
    }
    if (any(align_setting == "c")) {
      ft <- ft |> align(align = "center", j = which(align_setting == "c"), part = align.part)
    }
    if (any(align_setting == "r")) {
      ft <- ft |> align(align = "right", j = which(align_setting == "r"), part = align.part)
    }
    if (any(align_setting == "j")) {
      ft <- ft |> align(align = "justify", j = which(align_setting == "j"), part = align.part)
    }
  }
  
  attr(ft, "split.across.pages") <- isTRUE(split.across.pages)
  ft <- structure(ft, class = c("certetoolbox_flextable", class(ft)))
  
  if (isTRUE(print)) {
    # run print.certetoolbox_flextable()
    print(ft)
  } else {
    ft
  }
}

#' @param use_knitr use the `knitr` package for printing. Ignored when in an interactive session. If `FALSE`, an internal certetoolbox function will be used to convert the LaTeX `longtable` that would print across multiple PDF pages. If in a non-interactive session where the output is non-LaTeX, the `knitr` package will always be used.
#' @rdname tbl_flextable
#' @export
#' @importFrom knitr is_latex_output asis_output opts_chunk opts_current raw_latex knit_print
print.certetoolbox_flextable <- function(x, use_knitr = !is_latex_output(), ...) {
  class(x) <- class(x)[!class(x) == "certetoolbox_flextable"]
  
  if (interactive()) {
    # use flextable:::print.flextable()
    print(x, ...)
    
  } else if (isTRUE(use_knitr) || isFALSE(is_latex_output())) {
    knit_print(x, ...)
    
  } else {
    knitr_update_properties <- get("knitr_update_properties", envir = asNamespace("flextable"))
    is_in_bookdown <- get("is_in_bookdown", envir = asNamespace("flextable"))
    is_in_quarto <- get("is_in_quarto", envir = asNamespace("flextable"))
    knit_to_latex <- get("knit_to_latex", envir = asNamespace("flextable"))
    
    out <- x |>
      knitr_update_properties(bookdown = is_in_bookdown(), quarto = is_in_quarto()) |>
      knit_to_latex(bookdown = is_in_bookdown(), quarto = is_in_quarto())
    
    # remove very weird lines generated by flextable about setting length of some \Oldarrayrulewidth
    out <- gsub("(\n|^)[\\]global[\\]setlength.*?\n", "", out)
    
    if (!isTRUE(attributes(x)$split.across.pages) && grepl("longtable", paste(out, collapse = ""))) {
      # this replace flextable's longtable to tabular
      if (grepl("endfirsthead", out)) {
        out <- gsub("[\\]begin[{]longtable[}][[].[]](.*?)[}][}].*[\\]endfirsthead", "\\\\begin{table}[H]\n\\\\centering\n\\\\begin{tabular}\\1}}", out)
      } else {
        out <- gsub("[\\]begin[{]longtable[}][[].[]]", "\\\\begin{table}[H]\n\\\\centering\n\\\\begin{tabular}", out, fixed = TRUE)
      }
      out <- gsub("\\end{longtable}", "\\end{tabular}\n\\end{table}", out, fixed = TRUE)
      out <- gsub("\\endhead", "", out, fixed = TRUE)
      out <- gsub("\\endfoot", "", out, fixed = TRUE)
      out <- gsub("\\endfirsthead", "", out, fixed = TRUE)
      out <- gsub("\\endlastfoot", "", out, fixed = TRUE)
    }
    
    if (opts_chunk$get("results") == "asis") {
      cat(raw_latex(out))
      return(invisible(""))
    } else if (opts_current$get("results") == "asis") {
      # required for Quarto
      cat(raw_latex(out))
      return(invisible(""))
    } else {
      return(asis_output(out))
    }
  }
}

#' Summarise Table as `gtsummary`
#'
#' Summarise a [data.frame] as [`gtsummary`][gtsummary::tbl_summary()] with Dutch defaults. These objects are based on the `gt` package by RStudio. To provide Certe style and compatibility with MS Word, use [tbl_flextable()] to transform the [`gtsummary`][gtsummary::tbl_summary()] object.
#' @inheritParams gtsummary::tbl_summary
#' @param x a [data.frame]
#' @param ... Arguments passed on to [gtsummary::tbl_summary()]
#' @param language the language to use, defaults to Dutch
#' @param column1_name name to use for the first column
#' @param add_n add the overall N using [gtsummary::add_n()]
#' @param add_p add the p values [gtsummary::add_p()] (tests will be determined automatcally)
#' @param add_ci add the confidence interval using [gtsummary::add_ci()]
#' @param add_overall add the overall statistics using [gtsummary::add_overall()]
#' @param decimal.mark decimal separator, defaults to [dec_mark()]
#' @param big.mark thousands separator, defaults to [big_mark()]
#' @inheritParams gtsummary::tbl_summary
#' @details [tbl_gtsummary()] creates a summary table with [gtsummary::tbl_summary()], to which different extra columns can be added e.g. with `add_p = TRUE` and `add_overall = TRUE`.
#' @importFrom dplyr select everything %>%
#' @export
#' @examples 
#' # These examples default to the Dutch language
#' 
#' iris |>
#'   tbl_gtsummary()
#' 
#' iris |> 
#'   tbl_gtsummary(Species, add_p = TRUE)
#'   
#' iris |> 
#'   tbl_gtsummary(Species, add_n = TRUE)
#'   
#' # support strata by providing 
#' iris2 <- iris
#' iris2$Category <- sample(LETTERS[1:2], size = 150, replace = TRUE)
#' head(iris2)
#' 
#' iris2 |> 
#'   tbl_gtsummary(c(Category, Species))
#' 
#' # transform to flextable 
#' # (formats to Certe style and allows rendering to Word)
#' iris |> 
#'   tbl_gtsummary(Species) |> 
#'   tbl_flextable()
tbl_gtsummary <- function(x,
                          by = NULL,
                          label = NULL,
                          digits = 1,
                          ...,
                          language = "nl",
                          column1_name = "Eigenschap",
                          add_n = FALSE,
                          add_p = FALSE,
                          add_ci = FALSE,
                          add_overall = FALSE,
                          decimal.mark = dec_mark(),
                          big.mark = big_mark()) {
  check_is_installed("gtsummary")
  
  if (isTRUE(add_ci) && isTRUE(add_overall)) {
    message("'add_ci' and 'add_overall' cannot both be true, ignoring 'add_overall'")
    add_overall <- FALSE
  }
  
  # set the theme with the chosen decimal marks
  theme_old <- gtsummary::get_gtsummary_theme()
  gtsummary::theme_gtsummary_language(language = language,
                                      decimal.mark = decimal.mark,
                                      big.mark = big.mark,
                                      iqr.sep = "\u2009\u2013\u2009", # small space, 'en' dash, small space
                                      ci.sep = "\u2009\u2013\u2009",
                                      set_theme = FALSE) |>
    gtsummary::set_gtsummary_theme(quiet = TRUE)
  
  # determine the 'by'
  by_select <- x |>
    select({{ by }}) |>
    colnames()
  
  if (length(by_select) == 0) {
    by_select <- NULL
  }
  
  if (length(by_select) <= 1) {
    # at most 1 variable to split data by
    if (tryCatch(is.numeric(digits) && length(digits) == 1, error = function(e) FALSE)) {
      # support for tbl_gtsummary(..., digits = 1)
      out <- x |> 
        gtsummary::tbl_summary(by = by_select[1], label = label, digits = everything() ~ digits, ...)
    } else {
      out <- x |> 
        gtsummary::tbl_summary(by = by_select[1], label = label, digits = digits, ...)
    }
    
  } else if (length(by_select) == 2) {
    # 2 variables to split data by
    if (tryCatch(is.numeric(digits) && length(digits) == 1, error = function(e) FALSE)) {
      # support for tbl_gtsummary(..., digits = 1)
      out <- gtsummary::tbl_strata(x,
                                   strata = by_select[1],
                                   .tbl_fun = gtsummary::tbl_summary,
                                   by = by_select[2],
                                   label = label,
                                   digits = everything() ~ digits,
                                   ...)
    } else {
      out <- gtsummary::tbl_strata(x,
                                   strata = by_select[1],
                                   .tbl_fun = gtsummary::tbl_summary,
                                   by = by_select[2],
                                   label = label,
                                   digits = digits,
                                   ...)
    }
  } else {
    stop("'by' can only be 1 or 2 variables")
  }
  
  if (isTRUE(add_n)) {
    out <- out |> 
      gtsummary::add_n(last = TRUE)
  }
  if (isTRUE(add_overall)) {
    out <- out |> 
      gtsummary::add_overall(last = FALSE)
  }
  if (isTRUE(add_ci)) {
    out <- out |> 
      gtsummary::add_ci()
  }
  
  if (isTRUE(add_p)) {
    out <- out |> 
      gtsummary::add_p()
  }
  
  gtsummary::set_gtsummary_theme(theme_old, quiet = TRUE)
  out
}

#' Print Table as Markdown, LaTeX of HTML
#'
#' Prints a [data.frame] as Markdown, LaTeX or HTML using [knitr::kable()], with bold headers and Dutch number formats.
#' @param x a [data.frame] or a [`flextable`][flextable::flextable()] object or a [`gtsummary`][gtsummary::tbl_summary()] object
#' @param row.names row names to be displayed
#' @param column.names column names to be displayed
#' @param align alignment of columns (default: numbers to the right, others to the left)
#' @param padding extra cell padding
#' @param caption caption of table
#' @param na text for missing values (default: `""`)
#' @param type type of formatting the table - valid options are `"latex"`, `"html"`, `"markdown"`, `"pandoc"` and `"rst"`
#' @param format.dates formatting of dates, will be evaluated with [`format2()`][certestyle::format2()]
#' @param newlines.leading number of white lines to print before the table
#' @param newlines.trailing number of white lines to print after the table
#' @param print only useful when input is a Flextable: force printing
#' @inheritParams tbl_flextable
#' @details When in an R Markdown rapport a table is printed using this function, column headers only print well if `newlines.leading` >= 2, or by manually using `cat("\\n\\n")` before printing the table.
#' @seealso [knitr::kable()]
#' @return character
#' @importFrom dplyr pull
#' @importFrom knitr kable
#' @export
#' @examples 
#' tbl_markdown(mtcars[1:6, 1:6], padding = 1)
tbl_markdown <- function(x,
                         row.names = rownames(x),
                         column.names = colnames(x),
                         align = NULL,
                         padding = 2,
                         caption = "",
                         na = "",
                         type = "markdown",
                         format.dates = "dd-mm-yyyy",
                         decimal.mark = dec_mark(),
                         big.mark = big_mark(),
                         logicals = c("X", ""),
                         columns.percent = NA,
                         column.names.bold = TRUE,
                         round.numbers = 2,
                         round.percent = 1,
                         newlines.leading = 0,
                         newlines.trailing = 2,
                         print = TRUE) {
  
  if (inherits(x, "gtsummary")) {
    # first transform to flextable
    x <- tbl_flextable(x)
  }
  
  if (inherits(x, "certetoolbox_flextable")) {
    return(invisible(print(x)))
  }
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x_name <- deparse(substitute(x))
  
  if (identical(row.names, as.character(seq_len(NROW(x)))) || is.null(row.names)) {
    row.names <- FALSE
  }
  if (caption == "") {
    caption <- NULL
  }
  
  if (column.names.bold == TRUE) {
    column.names <- paste0("**", column.names, "**")
  }
  
  if (is.null(ncol(x))) {
    col_count <- 1
  } else {
    col_count <- ncol(x)
  }
  
  for (i in seq_len(col_count)) {
    if (class(x[1, i])[1] %in% c("Date", "POSIXct", "POSIXlt")) {
      x[, i] <- x |> 
        pull(i) |>
        format2(format = format.dates)
    }
    if (class(x[1, i])[1] %in% "logical") {
      x[, i] <- gsub("TRUE", logicals[1],
                     gsub("FALSE", logicals[2],
                          x |>
                            pull(i) |> 
                            as.character()))
      
    }
    if (class(x[1, i])[1] %in% c("double", "integer", "numeric", "single") &
        !i %in% columns.percent) {
      x[, i] <- x |>
        pull(i) |>
        format2(round = round.numbers, decimal.mark = decimal.mark, big.mark = big.mark)
    }
    if (i %in% columns.percent) {
      x[, i] <- x |>
        pull(i) |>
        as.percentage() |>
        format2(round = round.percent, decimal.mark = decimal.mark, big.mark = big.mark)
    }
  }
  
  # nolint start
  opt.old <- options()$knitr.kable.NA
  options(knitr.kable.NA = na)
  # nolint end
  
  cat(strrep("\n", newlines.leading))
  print(kable(
    x,
    table.attr = ifelse(type == "html", "", paste0("id=\"", x_name, "\"")),
    col.names = column.names,
    row.names = !isFALSE(row.names),
    align = align,
    format = type,
    padding = padding,
    caption = caption))
  cat(strrep("\n", newlines.trailing))
  
  # nolint start
  options(knitr.kable.NA = opt.old)
  # nolint end
}

#' Automatically Transform Data Set
#' 
#' This function transforms a [data.frame] by guessing the right data classes and applying them, using [readr::parse_guess()] and `cleaner` functions such as [cleaner::clean_Date()].
#' @param x a [data.frame]
#' @param datenames language of the date names, such as weekdays and months
#' @param dateformat expected date format, will be coerced with [`format_datetime()`][cleaner::format_datetime()]
#' @param timeformat expected time format, will be coerced with [`format_datetime()`][cleaner::format_datetime()]
#' @param decimal.mark separator for decimal numbers
#' @param big.mark separator for thousands
#' @param timezone expected time zone
#' @param na values to interpret as `NA`
#' @param snake_case apply [snake case](https://en.wikipedia.org/wiki/Snake_case) to the column names
#' @param ... not used as the time, allows for future extension
#' @importFrom cleaner format_names format_datetime clean_Date clean_POSIXct clean_logical
#' @importFrom readr parse_guess locale
#' @importFrom hms as_hms
#' @importFrom progress progress_bar
#' @export
auto_transform <- function(x,
                           datenames = "en",
                           dateformat = "yyyy-mm-dd",
                           timeformat = "HH:MM",
                           decimal.mark = ".",
                           big.mark = "",
                           timezone = "",
                           na = c("", "NULL", "NA", "<NA>"),
                           snake_case = FALSE,
                           ...) {
  if (!inherits(x, "data.frame")) {
    warning("auto_transform() can only transform a 'data.frame', not ",
            paste0("'", class(x), "'", collapse = "/"),
            " (data left unchanged)", call. = FALSE)
    return(x)
  }
  
  if (isTRUE(snake_case)) {
    x <- format_names(x, snake_case = TRUE)
  }
  
  dateformat <- format_datetime(dateformat)
  timeformat <- format_datetime(timeformat)
  cat("\n") # will otherwise overwrite existing line in console, such as in certedb
  pb <- progress_bar$new(total = ncol(x),
                         format = "Auto-transform: :what [:bar] :percent",
                         show_after = 0)
  col_names <- format(colnames(x))
  
  try_convert <- function(object, backup, col) {
    tryCatch(object,
             error = function(e) {
               msg <- paste0("NOTE: Ignoring column ", trimws(col_names[col]), " because of error: ",
                             gsub("\n", " ", e$message))
               pb$message(msg)
               return(backup)
             },
             warning = function(e) {
               msg <- paste0("Column ", trimws(col_names[col]), ": ",
                             gsub("\n", " ", e$message))
               pb$message(msg)
               return(suppressWarnings(object))
             })
  }
  
  for (i in seq_len(ncol(x))) {
    pb$tick(tokens = list(what = col_names[i]))
    
    # 2023-02-13 fix for Diver, logicals/booleans seem corrupt
    if (is.logical(x[, i, drop = TRUE])) {
      x[, i] <- as.logical(as.character(x[, i, drop = TRUE]))
    }
    if (inherits(x[, i, drop = TRUE], "integer64")) {
      # int64 relies on the bit64 pkg, just use base R here
      if (all(abs(x[, i, drop = TRUE]) <= base::.Machine$integer.max, na.rm = TRUE)) {
        x[, i] <- as.integer(x[, i, drop = TRUE])
      } else {
        x[, i] <- as.double(x[, i, drop = TRUE])
      }
    }
    
    col_data <- x[, i, drop = TRUE]
    col_data_unique <- unique(col_data)
    col_name <- tolower(colnames(x)[i])
    if (!inherits(col_data, c("list", "matrix")) &&
        # no faeces (F) or tips (T)
        !all(unique(col_data) %in% c("T", "F"))) {
      parsed <- NULL
      if (length(col_data_unique) > 5000) {
        # check if there's no difference for the first 5000 unique values
        parsed <- parse_guess(x = as.character(col_data_unique)[1:5000],
                              na = na,
                              guess_integer = TRUE,
                              trim_ws = TRUE,
                              locale = locale(date_names = datenames,
                                              date_format = dateformat,
                                              time_format = timeformat,
                                              decimal_mark = decimal.mark,
                                              grouping_mark = big.mark,
                                              encoding = "UTF-8",
                                              tz = timezone,
                                              asciify = FALSE))
        if (!any(class(parsed) %in% class(col_data))) {
          # class is different - it has to run for all values unfortunately
          parsed <- NULL
        }
      }
      if (is.null(parsed)) {
        # run for all unique values
        parsed_unique <- parse_guess(x = as.character(col_data_unique),
                                     na = na,
                                     guess_integer = TRUE,
                                     trim_ws = TRUE,
                                     locale = locale(date_names = datenames,
                                                     date_format = dateformat,
                                                     time_format = timeformat,
                                                     decimal_mark = decimal.mark,
                                                     grouping_mark = big.mark,
                                                     encoding = "UTF-8",
                                                     tz = timezone,
                                                     asciify = FALSE))
        # insert into data
        x[, i] <- parsed_unique[match(col_data, col_data_unique)]
      }
      if (is.double(col_data) && !is.double(x[, i, drop = TRUE]) && decimal.mark != ".") {
        # exception for csv2 (semi-colon separated) export and import
        x[, i] <- parse_guess(x = as.character(col_data), guess_integer = TRUE)
      } else if (inherits(col_data, "integer64")) {
        x[, i] <- col_data
      }
      if (!is.double(col_data) && is.double(x[, i, drop = TRUE]) && decimal.mark == ".") {
        col_data <- x[, i, drop = TRUE]
      }
      if (all(col_data_unique %like% "[0-3][0-9]-[0-1][0-9]-[12][09][0-9][0-9]", na.rm = TRUE)) {
        x[, i] <- try_convert(clean_Date(col_data, format = "dd-mm-yyyy"),
                              backup = x[, i, drop = TRUE], col = i)
      }
      if (all(tolower(col_data_unique) %in% c("nee", "no", "niet", "ja", "yes", "wel"), na.rm = TRUE)) {
        x[, i] <- try_convert(clean_logical(col_data, true = "(ja|yes|wel)", false = "(nee|no|niet)", fixed = FALSE),
                              backup = x[, i, drop = TRUE], col = i)
      }
    }
    if (inherits(col_data, "POSIXct") && timezone == "UTC") {
      x[, i] <- try_convert(as.UTC(col_data),
                            backup = x[, i, drop = TRUE], col = i)
    }
    col_data <- x[, i, drop = TRUE]
    col_data_unique <- unique(col_data)
    if (inherits(col_data, c("factor", "character"))) {
      # remove ASCII escape character: https://en.wikipedia.org/wiki/Escape_character#ASCII_escape_character
      x[, i] <- tryCatch(gsub("\033", " ", col_data, fixed = TRUE),
                         error = function(e) {
                           warning(e$message)
                           return(col_data)})
    }
    
    # set times/dates
    if (col_name %like% "_(tijd|time)$") {
      hms_data <- col_data
      # hms::as_hms() only supports HH:MM:SS since recent versions, but we want to keep support for HH:MM, so:
      hms_data[hms_data %like% "^[0-9]?[0-9]:[0-9][0-9]$"] <- paste0(hms_data[hms_data %like% "^[0-9]?[0-9]:[0-9][0-9]$"], ":00")
      x[, i] <- try_convert(as_hms(hms_data),
                            backup = x[, i, drop = TRUE], col = i)
    } else if (col_name %like% "_(datum|date)$") {
      x[, i] <- try_convert(clean_Date(col_data, guess_each = TRUE),
                            backup = x[, i, drop = TRUE], col = i)
    } else if (col_name %like% "_(timestamp|tijd.?stempel|datum.?tijd)$") {
      x[, i] <- try_convert(clean_POSIXct(col_data),
                            backup = x[, i, drop = TRUE], col = i)
    }
    
    if ("AMR" %in% rownames(utils::installed.packages())) {
      # check for SIR
      if (col_name %like% "_(rsi|sir)$" ||
          (inherits(col_data, c("factor", "character")) &&
           !all(col_data_unique[!is.na(col_data_unique)] == "") &&
           all(col_data_unique[!is.na(col_data_unique)] %in% c("", "I", "I;I", "R", "R;R", "S", "S;S")))) {
        x[, i] <- try_convert(AMR::as.sir(col_data),
                              backup = x[, i, drop = TRUE], col = i)
      }
      # set Minimum Inhibitory Concentration (MIC)
      if (col_name %like% "_mic$") {
        x[, i] <- try_convert(AMR::as.mic(col_data),
                              backup = x[, i, drop = TRUE], col = i)
      }
      # set disk diffusion values
      if (col_name %like% "_disk$") {
        x[, i] <- try_convert(AMR::as.disk(col_data),
                              backup = x[, i, drop = TRUE], col = i)
      }
    }
  }
  x
}


#' Create a Crosstab
#' 
#' Transform a data set into an *n* x *m* table, e.g. to be used in [certestats::confusion_matrix()].
#' @param df a [data.frame]
#' @param identifier a column name to use as identifier, such as a patient ID or an order ID
#' @param compare a column name for the two axes of the table: the labels between the outcomes must be compared
#' @param outcome a column name containing the outcome values to compare
#' @param positive a [regex] to match the values in `outcome` that must be considered as the Positive class, use `FALSE` to not use a Positive class
#' @param negative a [regex] to match the values in `outcome` that must be considered as the Negative class, use `FALSE` to not use a Negative class
#' @param ... manual [regex]es for classes if not using `positive` and `negative`, such as `Class1 = "c1", Class2 = "c2", Class3 = "c3"`
#' @param na.rm a [logical] to indicate whether empty values must be removed before forming the table
#' @param ignore_case a [logical] to indicate whether the case in the values of `positive`, `negative` and `...` must be ignored
#' @importFrom dplyr mutate case_when select pull transmute
#' @importFrom tidyr pivot_wider
#' @export
#' @examples 
#' df <- data.frame(
#'   order_nr = sort(rep(LETTERS[1:20], 2)),
#'   test_type = rep(c("Culture", "PCR"), 20),
#'   result = sample(c("pos", "neg"),
#'                   size = 40,
#'                   replace = TRUE,
#'                   prob = c(0.3, 0.9))
#' )
#' head(df)
#' 
#' out <- df |> crosstab(order_nr, test_type, result)
#' out
#' 
#' 
#' df$result <- gsub("pos", "#p", df$result)
#' df$result <- gsub("neg", "#n", df$result)
#' head(df)
#' # gives a warning that pattern matching failed:
#' df |> crosstab(order_nr, test_type, result)
#' 
#' # define the pattern yourself in such case:
#' df |> crosstab(order_nr, test_type, result,
#'                positive = "#p",
#'                negative = "#n")
#'                              
#'                              
#' # defining classes manually, can be more than 2:
#' df |> crosstab(order_nr, test_type, result,
#'                ClassA = "#p", Hello = "#n")
#'                              
#' if ("certestats" %in% rownames(utils::installed.packages())) {
#'   certestats::confusion_matrix(out)
#' }
crosstab <- function(df,
                     identifier,
                     compare,
                     outcome,
                     positive = "^pos.*",
                     negative = "^neg.*",
                     ...,
                     na.rm = TRUE,
                     ignore_case = TRUE) {
  
  dots <- list(...)
  
  out <- df |> 
    transmute(id_col = {{ identifier }},
              names_col = {{ compare }},
              values_col = {{ outcome }})
  
  if (length(dots) == 0) {
    # use default 'positive' and 'negative' arguments
    out <- out |> 
      mutate(values_col = case_when(grepl(pattern = positive, x = values_col, ignore.case = ignore_case) ~ "Positive",
                                    grepl(pattern = negative, x = values_col, ignore.case = ignore_case) ~ "Negative",
                                    TRUE ~ NA_character_))
    if (!all(c("Positive", "Negative") %in% out$values_col, na.rm = TRUE)) {
      warning("Check the regular expressions in the 'positive' and 'negative' arguments - they are not both matched",
              call. = FALSE)
    }
    out <- out |> 
      mutate(values_col = factor(values_col, levels = c("Positive", "Negative"), ordered = TRUE))
    
  } else {
    # use manual list
    if (is.null(names(dots)) || any(names(dots) == "")) {
      stop("All manual classes in ... must be named")
    }
    if (!missing(positive)) {
      warning("ignoring argument 'positive' since manual values are defined: ", paste0(names(dots), collapse = ", "), call. = FALSE)
    }
    if (!missing(negative)) {
      warning("ignoring argument 'negative' since manual values are defined: ", paste0(names(dots), collapse = ", "), call. = FALSE)
    }
    # reverse the list so we will end with the top one
    values <- out$values_col
    out$values_col <- NA_character_
    for (i in rev(seq_len(length(dots)))) {
      out$values_col <- ifelse(grepl(pattern = dots[[i]], x = values, ignore.case = ignore_case),
                               names(dots)[i],
                               out$values_col)
    }
  }
  
  out |> 
    pivot_wider(id_cols = id_col, names_from = names_col, values_from = values_col) |> 
    select(-id_col) |> 
    table(useNA = ifelse(isTRUE(na.rm), "no", "always"))
}


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

#' Format Table as Flextable
#'
#' Format a [data.frame] as [flextable()] with Certe style, bold headers and Dutch number formats.
#' @param x a [data.frame]
#' @param rows.height height of the rows in centimetres
#' @param row.names.bold display row names in bold
#' @param rows.italic column indexes of rows in italics
#' @param rows.bold column indexes of rows in bold
#' @param rows.fill the column indices of rows to be shaded
#' @param rows.fill.picker the colour for values in `rows.fill`, is evaluated with [`colourpicker()`][certestyle::colourpicker()]
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
#' @param columns.fill.picker the colour for values in `columns.fill`, is evaluated with [`colourpicker()`][certestyle::colourpicker()]
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
#' @param values.colour.picker,values.fill.picker the colour for values in `values.colour` or `values.fill`, is evaluated with [`colourpicker()`][certestyle::colourpicker()]
#' @param autofit format table in width automatically
#' @param autofit.fullpage display table across width of page
#' @param autofit.fullpage.width set number of centimetres to width of table
#' @param vline indices of columns to have a vertical line to their right
#' @param vline.border function to define vertical line
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
#' @param print forced printing (required in a `for`` loop)
#' @details Run [tbl_markdown()] on a `flextable` object to transform it into R Markdown and use it for Quarto or R Markdown reports.
#' @seealso [flextable()]
#' @return [flextable] object
#' @rdname tbl_flextable
#' @importFrom certestyle colourpicker format2
#' @importFrom dplyr bind_cols pull
#' @importFrom flextable flextable fp_border_default add_footer_row color bg bold italic set_header_labels fontsize font width height flextable_dim autofit align set_caption hline vline flextable_to_rmd add_header_row
#' @importFrom cleaner as.percentage
#' @export
#' @examples
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
#' # create a gtsummary as flextable
#' iris |> 
#'   tbl_gtsummary_flextable(Species, decimal.mark = ".")
#' iris |> 
#'   tbl_gtsummary_flextable(Species, decimal.mark = ",")
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
#'                                       
tbl_flextable <- function(x,
                          row.names = rownames(x),
                          row.names.bold = TRUE,
                          rows.italic = NULL,
                          rows.bold = NULL,
                          rows.height = NULL,
                          rows.fill = NULL,
                          rows.fill.picker = "certeblauw5",
                          rows.zebra = FALSE,
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
                          columns.fill.picker = "certeblauw5",
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
                          values.colour.picker = "certeroze",
                          values.fill = NULL,
                          values.fill.picker = "certeroze3",
                          values.bold = NULL,
                          values.italic = NULL,
                          autofit = is.null(columns.width) & is.null(rows.height),
                          autofit.fullpage = TRUE,
                          autofit.fullpage.width = 16,
                          vline = NULL,
                          vline.border = fp_border_default("black"),
                          vline.part = c("body", "footer"),
                          print = FALSE,
                          ...) {
  
  if (inherits(x, "flextable")) {
    ft <- x
    colnames_bak <- NULL
    x <- x$body$dataset
    
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
    
    if (identical(row.names, as.character(c(seq_len(nrow(x))))) & missing(row.names)) {
      # rownames not set, are thus 1:nrow(x)
      row.names <- FALSE
    }
    
    if (!isFALSE(row.names)) {
      if (isTRUE(row.names)) {
        row.names <- seq_len(nrow(x))
      } else if (length(row.names) == 1) {
        row.names <- rep(row.names, nrow(x))
      } else if (length(row.names) != nrow(x)) {
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
      # A to Z, then AA to ZZ (total 26 + 26 * 26 = 702)
      letter_vector <- c(LETTERS, unlist(lapply(LETTERS, function(x) paste0(x, LETTERS))))
      colnames(x) <- letter_vector[seq_len(ncol(x))]
      names(colnames_bak) <- colnames(x) # this will make set_header_labels work, furter down
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
          bold(part = "footer", i = nrow(ft$footer$dataset))
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
    for (row in seq_len(nrow(ind))) {
      ft <- ft |> color(i = ind[row, "row"],
                        j = ind[row, "col"],
                        color = colourpicker(values.colour.picker))
    }
  }
  if (!is.null(values.fill)) {
    ind <- which(as.matrix(x) == as.character(values.fill), arr.ind = TRUE)
    for (row in seq_len(nrow(ind))) {
      ft <- ft |> bg(i = ind[row, "row"],
                     j = ind[row, "col"],
                     bg = colourpicker(values.fill.picker))
    }
  }
  if (!is.null(values.bold)) {
    ind <- which(as.matrix(x) == as.character(values.bold), arr.ind = TRUE)
    for (row in seq_len(nrow(ind))) {
      ft <- ft |> bold(i = ind[row, "row"],
                       j = ind[row, "col"])
    }
  }
  if (!is.null(values.italic)) {
    ind <- which(as.matrix(x) == as.character(values.italic), arr.ind = TRUE)
    for (row in seq_len(nrow(ind))) {
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
      ft <- ft |> vline(border = vline.border,
                        j = vline,
                        part = vline.part[i])
    }
  }
  
  # zebra print
  if (isTRUE(rows.zebra)) {
    rows.fill <- seq(2, nrow(x), 2)
  }
  if (isTRUE(columns.zebra)) {
    columns.fill <- seq(2, ncol(x), 2)
  }
  
  # Certe theme
  ft <- ft |>
    font(fontname = font.family, part = "all") |>
    fontsize(size = font.size, part = "all")
  if (caption != "") {
    ft <- ft |>
      set_caption(caption)
  }
  # bold headers
  if (column.names.bold == TRUE) {
    ft <- ft |>
      bold(part = "header")
  }
  # bold row names
  if (!isFALSE(row.names) & row.names.bold == TRUE) {
    ft <- ft |>
      bold(j = 1)
  }
  if (font.size.header != font.size) {
    ft <- ft |>
      fontsize(size = font.size.header, part = "header")
  }
  if (length(columns.italic) > 0) {
    ft <- ft |>
      italic(j = columns.italic)
  }
  if (length(columns.bold) > 0) {
    ft <- ft |>
      bold(j = columns.bold)
  }
  if (!is.null(rows.italic)) {
    ft <- ft |>
      italic(i = rows.italic)
  }
  if (!is.null(rows.bold)) {
    ft <- ft |>
      bold(i = rows.bold)
  }
  if (!is.null(colnames_bak)) {
    ft <- ft |>
      set_header_labels(values = colnames_bak)
  }
  if (!is.null(rows.fill)) {
    ft <- ft |>
      bg(i = rows.fill,
         bg = colourpicker(rows.fill.picker),
         part = "body")
  }
  if (length(columns.fill) > 0) {
    ft <- ft |>
      bg(j = columns.fill,
         bg = colourpicker(columns.fill.picker),
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
      rows.height <- rep(rows.height, nrow(x))
    }
    rows.height <- rows.height / 2.54
    for (i in seq_len(nrow(x))) {
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
  
  if (isTRUE(print)) {
    if (interactive()) {
      print(ft)
    } else {
      # not interactive like in R Markdown - print as markdown table
      flextable_to_rmd(ft, print = TRUE)
    }
  } else {
    ft
  }
}

#' @rdname tbl_flextable
#' @inheritParams gtsummary::tbl_summary
#' @param ... Arguments passed on to [gtsummary::tbl_summary()]
#' @details [tbl_gtsummary_flextable()] creates a summary table with [gtsummary::tbl_summary()], transforms it to a flextable and runs [tbl_flextable()] to apply the Certe style. The table will be Dutch if `decimal.mark` is a comma (default on Dutch systems). To turn a manual `gtsummary` into a 'Certe flextable', run [gtsummary_as_flextable()] on a `gtsummary` object.
#' @export
tbl_gtsummary_flextable <- function(data, by, label = NULL, digits = 2, ..., decimal.mark = dec_mark()) {
  check_is_installed("gtsummary")
  gt <- data |> 
    gtsummary::tbl_summary(by = {{ by }}, label = label, digits = dplyr::everything() ~ digits, ...)
  
  if (decimal.mark == ",") {
    stat_cols <- names(gt$table_body)[names(gt$table_body) %like% "stat_"]
    for (col in stat_cols) {
      gt$table_body[, col] <- gsub(",", ";", gt$table_body[, col, drop = TRUE], fixed = TRUE)
      gt$table_body[, col] <- gsub(".", decimal.mark, gt$table_body[, col, drop = TRUE], fixed = TRUE)
      # Dutch language
      gt$table_body[, col] <- gsub(" to ", " t/m ", gt$table_body[, col, drop = TRUE], fixed = TRUE)
    }
    # Dutch language
    gt$table_styling$header$label <- gsub("Characteristic", "Eigenschap", gt$table_styling$header$label, fixed = TRUE)
    gt$table_styling$header$label <- gsub("**TRUE**", "**Wel**", gt$table_styling$header$label, fixed = TRUE)
    gt$table_styling$header$label <- gsub("**FALSE**", "**Niet**", gt$table_styling$header$label, fixed = TRUE)
    footnote <- gt$table_styling$footnote$footnote
    footnote <- gsub("CI = Credible Interval", "CI = geloofwaardigheidsinterval", footnote, fixed = TRUE)
    footnote <- gsub("Credible Interval", "Geloofwaardigheidsinterval", footnote, fixed = TRUE)
    footnote <- gsub("CI = Confidence Interval", "CI = betrouwbaarheidsinterval", footnote, fixed = TRUE)
    footnote <- gsub("Confidence Interval", "Betrouwbaarheidsinterval", footnote, fixed = TRUE)
    footnote <- gsub("HR = Hazard Ratio", "HR = Hazard-ratio", footnote, fixed = TRUE)
    footnote <- gsub("IRR = Incidence Rate Ratio", "IRR = Incidence Rate Ratio", footnote, fixed = TRUE)
    footnote <- gsub("RR = Relative Risk", "RR = Relatieve risico", footnote, fixed = TRUE)
    footnote <- gsub("SE = Standard Error", "SE = Standard Error", footnote, fixed = TRUE)
    footnote <- gsub("% missing", "% ontbrekend", footnote, fixed = TRUE)
    footnote <- gsub("% Missing (unweighted)", "% ontbrekend (ongewogen)", footnote, fixed = TRUE)
    footnote <- gsub("% not missing", "% niet ontbrekend", footnote, fixed = TRUE)
    footnote <- gsub("% not Missing (unweighted)", "% niet ontbrekend (ongewogen)", footnote, fixed = TRUE)
    footnote <- gsub("Characteristic", "Karakteristiek", footnote, fixed = TRUE)
    footnote <- gsub("Mean", "gemiddelde", footnote, fixed = TRUE)
    footnote <- gsub("Median", "mediaan", footnote, fixed = TRUE)
    footnote <- gsub("N missing", "N ontbrekend", footnote, fixed = TRUE)
    footnote <- gsub("N Missing", "N ontbrekend", footnote, fixed = TRUE)
    footnote <- gsub("N Missing (unweighted)", "N ontbrekend (ongewogen)", footnote, fixed = TRUE)
    footnote <- gsub("N not Missing", "N niet ontbrekend", footnote, fixed = TRUE)
    footnote <- gsub("N not Missing (unweighted)", "N niet ontbrekend (ongewogen)", footnote, fixed = TRUE)
    footnote <- gsub("No. obs.", "Aantal obs.", footnote, fixed = TRUE)
    footnote <- gsub("Range", "bereik", footnote, fixed = TRUE)
    footnote <- gsub("SD", "SD", footnote, fixed = TRUE)
    footnote <- gsub("Statistics presented", "Getoonde statistieken", footnote, fixed = TRUE)
    footnote <- gsub("Sum", "som", footnote, fixed = TRUE)
    footnote <- gsub("Total N", "Totaal N", footnote, fixed = TRUE)
    footnote <- gsub("Total N (unweighted)", "Totaal N (ongewogen)", footnote, fixed = TRUE)
    footnote <- gsub("Unknown", "onbekend", footnote, fixed = TRUE)
    footnote <- gsub("Variance", "variantie", footnote, fixed = TRUE)
    gt$table_styling$footnote$footnote <- footnote
  }
  
  gt |>
    gtsummary_as_flextable()
}

#' @rdname tbl_flextable
#' @param gtsummary a `gtsummary` object created with [gtsummary::tbl_summary()]
#' @export
gtsummary_as_flextable <- function(gtsummary) {
  gtsummary |> 
    gtsummary::as_flex_table() |>
    tbl_flextable()
}

#' Print Table as Markdown, LaTeX of HTML
#'
#' Prints a [data.frame] as Markdown, LaTeX or HTML using [knitr::kable()], with bold headers and Dutch number formats.
#' @param x a [data.frame]
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
#' @importFrom flextable flextable_to_rmd
#' @importFrom knitr kable
#' @export
#' @examples 
#' tbl_markdown(mtcars[1:6, 1:6], padding = 1)
tbl_markdown <- function(x,
                         row.names = !is.null(rownames(x)),
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
  
  if (inherits(x, "flextable")) {
    return(invisible(flextable_to_rmd(x, print = print)))
  }
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  x_name <- deparse(substitute(x))
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
    table.attr = paste0("id=\"", x_name, "\""),
    col.names = column.names,
    row.names = row.names,
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
#' This function transforms a [data.frame] by guessing the right data classes and applying them, using [readr::parse_guess()].
#' @param x a [data.frame]
#' @param datenames language of the date names, such as weekdays and months
#' @param dateformat expected date format, will be coerced with [`format_datetime()`][cleaner::format_datetime()]
#' @param timeformat expected time format, will be coerced with [`format_datetime()`][cleaner::format_datetime()]
#' @param decimal.mark separator for decimal numbers
#' @param big.mark separator for thousands
#' @param timezone expected time zone
#' @param na values to interpret as `NA`
#' @param ... not used as the time, allows for future extension
#' @importFrom cleaner format_datetime clean_Date
#' @importFrom readr parse_guess locale
#' @export
auto_transform <- function(x,
                           datenames = "en",
                           dateformat = "yyyy-mm-dd",
                           timeformat = "HH:MM",
                           decimal.mark = ".",
                           big.mark = "",
                           timezone = "",
                           na = c("", "NULL", "NA", "<NA>"),
                           ...) {
  if (!inherits(x, "data.frame")) {
    warning("auto_transform() can only transform a 'data.frame', not ",
            paste0("'", class(x), "'", collapse = "/"),
            " (data left unchanged)", call. = FALSE)
    return(x)
  }
  dateformat <- format_datetime(dateformat)
  timeformat <- format_datetime(timeformat)
  for (i in seq_len(ncol(x))) {
    col_data <- x[, i, drop = TRUE]
    col_data_unique <- unique(col_data)
    if (!inherits(col_data, c("list", "matrix")) &&
        # no faeces (F) or tips (T)
        !all(unique(col_data) %in% c("T", "F"))) {
      x[, i] <- parse_guess(x = as.character(col_data),
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
      if (is.double(col_data) && !is.double(x[, i, drop = TRUE]) && decimal.mark != ".") {
        # exception for csv2 (semi-colon separated) export and import
        x[, i] <- parse_guess(x = as.character(col_data), guess_integer = TRUE)
      } else if (inherits(col_data, "integer64")) {
        x[, i] <- col_data
      }
      if (!is.double(col_data) && is.double(x[, i, drop = TRUE]) && decimal.mark == ".") {
        col_data <- x[, i, drop = TRUE]
      }
      if (all(col_data %like% "[0-3][0-9]-[0-1][0-9]-[12][09][0-9][0-9]", na.rm = TRUE)) {
        x[, i] <- clean_Date(col_data, format = "dd-mm-yyyy")
      }
    }
    if (inherits(col_data, "POSIXct") && timezone == "UTC") {
      x[, i] <- as.UTC(col_data)
    }
    
    if (inherits(col_data, c("factor", "character"))) {
      # remove ASCII escape character: https://en.wikipedia.org/wiki/Escape_character#ASCII_escape_character
      x[, i] <- tryCatch(gsub("\033", " ", col_data, fixed = TRUE),
                         error = function(e) {
                           warning(e$message)
                           return(col_data)})
    }
    if ("AMR" %in% rownames(utils::installed.packages())) {
      # check for RSI
      if (inherits(col_data, c("factor", "character")) &&
          !all(col_data_unique[!is.na(col_data_unique)] == "") &&
          all(col_data_unique[!is.na(col_data_unique)] %in% c("", "I", "I;I", "R", "R;R", "S", "S;S"))) {
        x[, i] <- AMR::as.rsi(col_data)
      }
      # set Minimum Inhibitory Concentration (MIC)
      if (colnames(x)[i] %like% "_mic$") {
        x[, i] <- AMR::as.mic(col_data)
      }
    }
  }
  x
}

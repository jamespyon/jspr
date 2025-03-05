#' Flextable Footnotes based on ATSDR Style Manual (2019)
#'
#'@description Create flextable footnotes according to ATSDR Style Manual (2019). This is a wrapper for the atsdr_footnote_symbol() function.
#'
#' @param flextable A flextable object.
#' @param data A dataset with comment, i, j variables.
#'
#' @return A flextable class object.
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(flextable)
#'
#'example_dataset <- head(iris) |>
#'  flextable::flextable() |>
#'  flextable::set_caption(flextable::as_paragraph("Iris Title"))
#'
#'add_footnotes <- data.frame(
#'  comment = c("column 3", "location (2,3)", "location (1,1)", "column 2", "title"),
#'  i = c(NA, 2, 1, NA, NA),
#'  j = c(3, 3, 1, 2, NA)
#')
#'
#'example_dataset |> flextable_atsdr_footnote(add_footnotes)

flextable_atsdr_footnote <- function(flextable, data) {
  # flextable = example_dataset
  # data = add_footnotes

  #find title footnotes
  title_fn <- data[is.na(data$i) & is.na(data$j),]

  #find column footnotes and order
  column_fn <- data[is.na(data$i) & !is.na(data$j),]
  column_fn <- column_fn[with(column_fn, order(j)),]

  #find cell footnotes and order
  cell_fn <- data[!is.na(data$i),]
  cell_fn <- cell_fn[with(cell_fn, order(j,i)),]

  #initialize
  current_title <- flextable$caption$value
  c_index <- max(current_title$.chunk_index)
  fn_index <- 0
  temp <- flextable

  #start with title footnotes
  for(t in 1:nrow(title_fn)) {

    #current title info
    title_info <- title_fn[t,]
    format_info <- current_title[1,]

    #increase index
    fn_index <- fn_index+1
    c_index <- c_index+1


    if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

      format_info$txt <- atsdr_footnote_symbol(fn_index)
      format_info$vertical.align <- "baseline"
      format_info$.chunk_index <- c_index

      temp$caption$value <- dplyr::bind_rows(temp$caption$value,
                                             format_info)

      temp <- flextable::add_footer_lines(temp,
                                          flextable::as_paragraph(atsdr_footnote_symbol(fn_index),
                                                                  title_info$comment))

    } else {

      format_info$txt <- atsdr_footnote_symbol(fn_index)
      format_info$vertical.align <- "superscript"
      format_info$.chunk_index <- c_index

      temp$caption$value <- dplyr::bind_rows(temp$caption$value,
                                             format_info)

      temp <- flextable::add_footer_lines(temp,
                                          flextable::as_paragraph(flextable::as_sup(atsdr_footnote_symbol(fn_index)),
                                                                  title_info$comment))

    }

  }

  #start with only column footnotes
  for(c in 1:nrow(column_fn)) {

    #current column info
    column_info <- column_fn[c,]

    #increase index
    fn_index <- fn_index+1

    if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

      temp <- flextable::footnote(
        temp,
        j = column_info$j,
        part = "header",
        ref_symbols = atsdr_footnote_symbol(fn_index),
        value = flextable::as_paragraph(column_info$comment))

    } else {

      temp <- flextable::footnote(
        temp,
        j = column_info$j,
        part = "header",
        ref_symbols = atsdr_footnote_symbol(fn_index),
        value = flextable::as_paragraph(column_info$comment))

    }

  }

  #start with only cell footnotes
  for(d in 1:nrow(cell_fn)) {

    #current cell info
    cell_info <- cell_fn[d,]

    #increase index
    fn_index <- fn_index+1

    if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

      temp <- flextable::footnote(
        temp,
        i = cell_info$i, j = cell_info$j,
        part = "body",
        ref_symbols = atsdr_footnote_symbol(fn_index),
        value = flextable::as_paragraph(cell_info$comment))

    } else {

      temp <- flextable::footnote(
        temp,
        i = cell_info$i, j = cell_info$j,
        part = "body",
        ref_symbols = atsdr_footnote_symbol(fn_index),
        value = flextable::as_paragraph(cell_info$comment))

    }

  }

  output <- temp

  return(output)

}

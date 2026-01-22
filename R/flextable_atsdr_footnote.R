#' Flextable Footnotes based on ATSDR Style Manual (2019)
#'
#' @description Create flextable footnotes according to ATSDR Style Manual (2019). This is a wrapper for the atsdr_footnote_symbol() function.
#'
#' @param flextable A flextable object.
#' @param data A dataset with comment, i, j variables.
#'
#' @details
#' The data argument needs a data.frame object that consists of three vectors labeled as comment, i, and j.
#'
#' The i and j column should be a single number which represent the row (i) and column (j) of where the footnote is placed. If both are NA, then the footnote will be for the title, or caption in flextable terms. If the row (i) is NA but not the column (j), then the footnote will be for the column name, or header in flextable terms.
#'
#' The comment column should be a character string and have the description for the footnote.
#'
#' @return A flextable class object.
#'
#' @examples
#' library(tidyverse)
#' library(flextable)
#'
#' example_dataset <- head(iris) |>
#'   flextable::flextable() |>
#'   flextable::set_caption(flextable::as_paragraph("Iris Title"))
#'
#' add_footnotes <- data.frame(
#'   comment = c("col", "cell 1", "cell 1", "col", "title", "cell 2", "col 2", "cell 3", "cell 4"),
#'   i = c(NA, 2, 1, NA, NA, 3, NA, 4, 6),
#'   j = c(3, 3, 1, 2, NA, 3, 4, 4, 4)
#' )
#'
#' #example_dataset |> flextable_atsdr_footnote(add_footnotes)
#'

flextable_atsdr_footnote <- function(flextable, data) {

  # find title footnotes
  title_fn <- data[is.na(data$i) & is.na(data$j),]

  # find column footnotes and order
  column_fn <- data[is.na(data$i) & !is.na(data$j),]
  column_fn <- column_fn[with(column_fn, order(j)),]

  # find cell footnotes and order
  cell_fn <- data[!is.na(data$i),]
  cell_fn <- cell_fn[with(cell_fn, order(j,i)),]

  # initialize
  current_title <- flextable$caption$value
  c_index <- max(current_title$.chunk_index)
  fn_index <- 0
  temp <- flextable
  save_data <- dplyr::distinct(data, comment)

  # start with title footnotes
  for(t in 1:nrow(title_fn)) {

    # current title info
    title_info <- title_fn[t,]
    format_info <- current_title[1,]

    # increase index
    fn_index <- fn_index+1
    c_index <- c_index+1

    # initial save comment and index (there shouldn't be the same comment on the title, i assume?)
    save_data$fn_index[save_data$comment == title_info$comment] <- fn_index

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

  # start with only column footnotes
  for(c in 1:nrow(column_fn)) {

    # current column info
    column_info <- column_fn[c,]

    # increase index
    fn_index <- fn_index+1

    # check and save comment and index
    save_data_index <- match(column_info$comment, save_data$comment)

    if(is.na(save_data[save_data_index,]$fn_index)) { #same

      save_data$fn_index[save_data$comment == column_info$comment] <- fn_index
      fn_index <- fn_index

      if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

        temp <- flextable::append_chunks(temp,
                                         j = column_info$j,
                                         part = "header",
                                         flextable::as_chunk(atsdr_footnote_symbol(fn_index)))

        temp <- flextable::add_footer_lines(temp,
                                            flextable::as_paragraph(atsdr_footnote_symbol(fn_index),
                                                                    column_info$comment))

      } else {

        temp <- flextable::append_chunks(temp,
                                         j = column_info$j,
                                         part = "header",
                                         flextable::as_sup(atsdr_footnote_symbol(fn_index)))

        temp <- flextable::add_footer_lines(temp,
                                            flextable::as_paragraph(flextable::as_sup(atsdr_footnote_symbol(fn_index)),
                                                                    column_info$comment))

      }

    } else {

      fn_index <- save_data$fn_index[save_data$comment == column_info$comment]

      if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

        temp <- flextable::append_chunks(temp,
                                         j = column_info$j,
                                         part = "header",
                                         atsdr_footnote_symbol(fn_index))

      } else {

        temp <- flextable::append_chunks(temp,
                                         j = column_info$j,
                                         part = "header",
                                         flextable::as_sup(atsdr_footnote_symbol(fn_index)))

      }

    }

    # reset footnote index
    fn_index <- max(save_data$fn_index, na.rm = TRUE)

  }

  # start with only cell footnotes
  for(d in 1:nrow(cell_fn)) {

    # current cell info
    cell_info <- cell_fn[d,]

    # increase index
    fn_index <- fn_index+1

    # check and save comment and index
    save_data_index <- match(cell_info$comment, save_data$comment)

    if(is.na(save_data[save_data_index,]$fn_index)) {

      save_data$fn_index[save_data$comment == cell_info$comment] <- fn_index
      fn_index <- fn_index

      if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

        temp <- flextable::append_chunks(temp,
                                         i = cell_info$i, j = cell_info$j,
                                         part = "body",
                                         flextable::as_chunk(atsdr_footnote_symbol(fn_index)))

        temp <- flextable::add_footer_lines(temp,
                                            flextable::as_paragraph(atsdr_footnote_symbol(fn_index),
                                                                    cell_info$comment))

      } else {

        temp <- flextable::append_chunks(temp,
                                         i = cell_info$i, j = cell_info$j,
                                         part = "body",
                                         flextable::as_sup(atsdr_footnote_symbol(fn_index)))

        temp <- flextable::add_footer_lines(temp,
                                            flextable::as_paragraph(flextable::as_sup(atsdr_footnote_symbol(fn_index)),
                                                                    cell_info$comment))

      }

    } else {

      fn_index <- save_data$fn_index[save_data$comment == cell_info$comment]

      if(grepl("\\*", atsdr_footnote_symbol(fn_index))) { #do not superscript star symbol

        temp <- flextable::append_chunks(temp,
                                         i = cell_info$i, j = cell_info$j,
                                         part = "body",
                                         atsdr_footnote_symbol(fn_index))

      } else {

        temp <- flextable::append_chunks(temp,
                                         i = cell_info$i, j = cell_info$j,
                                         part = "body",
                                         flextable::as_sup(atsdr_footnote_symbol(fn_index)))

      }

    }

    # reset footnote index
    fn_index <- max(save_data$fn_index, na.rm = TRUE)

  }

  output <- temp

  return(output)

}

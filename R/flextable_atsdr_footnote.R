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
#'
#' example_dataset <- tibble::tribble(
#'   ~col_1, ~col_2, ~col_3, ~col_4,
#'   "1,1", "1,2", "1,3", "1,4",
#'   "2,1", "2,2", "2,3", "2,4",
#'   "3,1", "3,2", "3,3", "3,4",
#'   "4,1", "4,2", "4,3", "4,4"
#' ) %>% flextable::flextable() %>%
#'    flextable::set_caption(flextable::as_paragraph("Title"))
#'
#'add_footnotes <- data.frame(
#'comment = c("column 3", "location (2,3)", "location (1,1)", "column 2", "title"), i = c(NA, 2, 1, NA, NA), j = c(3, 3, 1, 2, NA))
#'
#'example_dataset %>% flextable_atsdr_footnote(add_footnotes)

example_dataset <- tibble::tribble(
  ~col_1, ~col_2, ~col_3, ~col_4,
  "1,1", "1,2", "1,3", "1,4",
  "2,1", "2,2", "2,3", "2,4",
  "3,1", "3,2", "3,3", "3,4",
  "4,1", "4,2", "4,3", "4,4"
  ) %>% flextable::flextable() %>%
  flextable::set_caption(flextable::as_paragraph("Title"))

add_footnotes <- data.frame(
  comment = c("column 3", "location (2,3)", "location (1,1)", "column 2", "title"),
  i = c(NA, 2, 1, NA, NA),
  j = c(3, 3, 1, 2, NA))


flextable_atsdr_footnote <- function(flextable, data) {
  # flextable = example_dataset
  # data = add_footnotes

  #find title footnotes
  title_fn <- data %>% filter(is.na(i) & is.na(j))

  #find column footnotes and order
  column_fn <- data %>% filter(is.na(i) & !is.na(j)) %>% arrange(j)

  #find column footnotes and order
  cell_fn <- data %>% filter(!is.na(i)) %>% arrange(j) %>% arrange(i)

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

      initial_formatting$txt <- atsdr_footnote_symbol(fn_index)
      initial_formatting$vertical.align <- "baseline"
      initial_formatting$.chunk_index <- c_index

      temp$caption$value <- bind_rows(temp$caption$value,
                                      initial_formatting)

      temp <- temp %>%
        flextable::add_footer_lines(flextable::as_paragraph(atsdr_footnote_symbol(fn_index),
                                                            title_info$comment))

    } else {

      initial_formatting$txt <- atsdr_footnote_symbol(fn_index)
      initial_formatting$vertical.align <- "superscript"
      initial_formatting$.chunk_index <- c_index

      temp$caption$value <- bind_rows(temp$caption$value,
                                      initial_formatting)

      temp <- temp %>%
        flextable::add_footer_lines(flextable::as_paragraph(flextable::as_sup(atsdr_footnote_symbol(fn_index)),
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

      temp <- temp %>%
        flextable::footnote(
          j = column_info$j,
          part = "header",
          ref_symbols = atsdr_footnote_symbol(fn_index),
          value = flextable::as_paragraph(column_info$comment))

    } else {

      temp <- temp %>%
        flextable::footnote(
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

      temp <- temp %>%
        flextable::footnote(
          i = cell_info$i, j = cell_info$j,
          part = "body",
          ref_symbols = atsdr_footnote_symbol(fn_index),
          value = flextable::as_paragraph(cell_info$comment))

    } else {

      temp <- temp %>%
        flextable::footnote(
          i = cell_info$i, j = cell_info$j,
          part = "body",
          ref_symbols = atsdr_footnote_symbol(fn_index),
          value = flextable::as_paragraph(cell_info$comment))

    }

  }

  output <- temp

  return(output)

}

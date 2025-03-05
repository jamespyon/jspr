library(tidyverse)

example_dataset <- tibble::tribble(
  ~col_1, ~col_2, ~col_3, ~col_4,
  "1,1", "1,2", "1,3", "1,4",
  "2,1", "2,2", "2,3", "2,4",
  "3,1", "3,2", "3,3", "3,4",
  "4,1", "4,2", "4,3", "4,4"
) %>% flextable::flextable() %>%
  flextable::set_caption("Title")

add_footnotes <- data.frame(
  comment = c("column 3", "location (2,3)", "location (1,1)", "column 2", "title"),
  i = c(NA, 2, 1, NA, NA),
  j = c(3, 3, 1, 2, NA)
)

flextable_atsdr_footnote <- function(flextable, data) {
  # flextable = example_dataset
  # data = add_footnotes

  #find title footnotes
  title_fn <- data %>% filter(is.na(i) & is.na(j))

  #find column footnotes and order
  column_fn <- data %>% filter(is.na(i) & !is.na(j)) %>% arrange(j)

  #find column footnotes and order
  cell_fn <- data %>% filter(!is.na(i)) %>% arrange(j) %>% arrange(i)

  #start with only column footnotes
  #for() {  }

  #start with only column footnotes
  #for() {  }

  flextable


}

example_dataset %>% flextable_atsdr_footnote(add_footnotes)

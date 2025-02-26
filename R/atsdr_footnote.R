library(tidyverse)

example_dataset <- tibble::tribble(
  ~col_1, ~col_2, ~col_3, ~col_4,
  "1,1", "1,2", "1,3", "1,4",
  "2,1", "2,2", "2,3", "2,4",
  "3,1", "3,2", "3,3", "3,4",
  "4,1", "4,2", "4,3", "4,4"
) %>% flextable::flextable()

add_footnotes <- data.frame(
  comment = c("column 3", "location (2,3)", "location (1,1)"),
  i = c(NA, 2, 1),
  j = c(3, 3, 1)
)

atsdr_footnote <- function(flextable, data) {
  # flextable = example_dataset
  # data = add_footnotes

  col_fn <- data %>% filter(is.na(i))

  flextable


}

example_dataset %>% atsdr_footnote(add_footnotes)

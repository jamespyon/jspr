# epc tool example

import <- readxl::read_excel("data-raw/contaminants.xlsx")

contaminants <- janitor::clean_names(import)

usethis::use_data(contaminants, overwrite = TRUE)

# analyte to cas-rn linkage

import <- readxl::read_excel("data-raw/casrn_data.xlsx")

casrn_data <- janitor::clean_names(import)

usethis::use_data(casrn_data, overwrite = TRUE)

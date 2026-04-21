# recommended cvs downloaded 2026-04-21

import <- readxl::read_excel("data-raw/atsdr_recommended_cv.xlsx", skip = 2)

raw_data <- import |>
  janitor::clean_names()

#find phast version and database rev
raw_info <- strsplit(unname(unlist(raw_data[nrow(raw_data),1])), split = " ")[[1]]
version <- gsub(",", "", raw_info[grep("PHAST", raw_info)+2])
rev <- raw_info[grep("rev", raw_info)+1]

raw_data <- raw_data[1:(nrow(raw_data)-2),]

#create dataframe
atsdr_recommended_cv <- lapply(c("drinking_water", "shower", "soil", "air", "svi_soil_gas", "svi_groundwater"), function(x) {

  #pull selected names
  select_names <- names(raw_data)[grepl(paste0(x,"_[0-9]"), names(raw_data))]

  #create subset dataset
  raw_matrix <- raw_data[,c("contaminant_name", "casrn", select_names)]

  #new names
  new_names <- raw_matrix |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names(replace = janitor:::mu_to_u)

  #add media column and remove NAs
  media <- unique(gsub("_[0-9]+$", "", select_names))
  new_column <- new_names |>
    dplyr::mutate(media = media) |>
    dplyr::filter(!is.na(rec_cv_type))

  output <- new_column |>
    tidyr::pivot_longer(cols = matches("m3|ppb|ppm"),
                        names_to = "unit",
                        values_to = "cv") |>
    dplyr::mutate(unit = dplyr::case_when(unit == "rec_cv_ug_m3" ~ "μg/m3",
                                          unit == "rec_cv_ppb" ~ "ppb",
                                          unit == "rec_cv_ppm" ~ "ppm")) |>
    dplyr::transmute(contaminant = contaminant_name,
                     casrn, cv, unit,
                     cv_type = rec_cv_type, media)

  return(output)

}) |> dplyr::bind_rows() |>
  dplyr::mutate(phast_version = version,
                database_rev = rev)

usethis::use_data(atsdr_recommended_cv, overwrite = TRUE)

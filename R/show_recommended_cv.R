#' ATSDR Recommended Screening CVs
#'
#' @description Produces recommended screening CVs given the compound and m. Last updated 04/21/2026.
#'
#' @param casrn A character vector representing the CAS-RN for each compound.
#' @param media A character vector representing the media. should be one of the following: "drinking_water", "shower", "soil", "air", "svi_soil_gas", "svi_groundwater"
#' @param unit Optional character vector representing the units. Default NULL.
#'
#' @returns A data.frame class object.
#' * `casrn`: CAS reference number for the associated recommended CV.
#' * `media`: Media associated to the CV.
#' * `contaminant`: Contaminant name as in PHAST.
#' * `cv`: numeric value of the recommended CV.
#' * `unit`: Unit for CV.
#' * `cv_type`: CV type of the recommended CV.
#' * `phast_version`: PHAST version the data is pulled from.
#' * `database_rev`: database revision for the PHAST database.
#'
#' @export
#'
#' @examples
#' #example of individual inputs
#' show_recommended_cv(casrn = c("7783-06-4", "71-43-2", "67-56-1", "7664-41-7", "67-64-1"),
#'                     media = "air", unit = "ppb")
#'

show_recommended_cv <- function(casrn, media = c("drinking_water", "shower", "soil", "air", "svi_soil_gas", "svi_groundwater"), unit = c("\u00B5g/m3", "ug/m3", "ppb", "ppm")) {

  #fix casrn
  casrn <- fix_casrn(casrn, format = "xx-xx-x")

  #check media
  if(identical(media, c("drinking_water", "shower", "soil", "air", "svi_soil_gas", "svi_groundwater"))) {
    stop("media argument should be one of the following: drinking_water, shower, soil, air, svi_soil_gas, svi_groundwater")
  } else {
    media <- match.arg(media)
  }

  #check unit
  if(identical(unit, c("\u00B5g/m3", "ug/m3", "ppb", "ppm"))) {
    unit <- NULL
  } else {
    if(unit == "ug/m3") {unit <- "\u00B5g/m3"}
    unit <- match.arg(unit)
  }

  if(!is.null(unit)) {

    matrix <- media
    units <- unit

    unit_data <- jspr::atsdr_recommended_cv |>
      dplyr::distinct(media, unit) |>
      dplyr::filter(media == matrix)

    if(!any(units == unit_data$unit)) {

      stop(paste("Units must be", paste(unit_data$unit, collapse = " or "), "for media type", matrix))

    } else {

      output <- data.frame(casrn, media) |>
        dplyr::left_join(jspr::atsdr_recommended_cv, by = c("casrn", "media")) |>
        dplyr::filter(unit == units)

    }

  } else {

    output <- data.frame(casrn, media) |>
      dplyr::left_join(jspr::atsdr_recommended_cv, by = c("casrn", "media"))

  }

  return(output)

}

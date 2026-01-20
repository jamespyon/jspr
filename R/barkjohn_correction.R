#' Barkjohn Correction Factor for PurpleAir monitors
#'
#' @description Applies the EPA standardized version of the Barkjohn correction factor for PurpleAir devices.
#'
#' @param x A value representing PurpleAir PM2.5 output.
#' @param rh A value representing PurpleAir relative humidity.
#' @param warnings A logical value. Change to remove warnings.
#'
#' @returns A numeric class object
#'
#' @export
#'
#' @examples
#' barkjohn_correction(0.5, 30)
#'

barkjohn_correction <- function(x, rh, warnings = TRUE) {

  jspr_warning("Relative Humidity units should be in %.\nPM2.5 units should be in ug/m3.", warnings)

	if(any(is.na(c(x, rh)))){

		y <- NA

	} else {

		y <- dplyr::case_when(x < 30 ~ (0.524 * x) - (0.0862 * rh) + 5.75,
					   x >= 30 & x < 50 ~ ((0.786 * ((x / 20) - (3/2))) + (0.524 * (1 - ((x / 20) - (3/2))))) * x - (0.0862 * rh) + 5.75,
					   x >= 50 & x < 210 ~ 	(0.786 * x) - (0.0862 * rh) + 5.75,
					   x >= 210 & x < 260 ~ (((0.69 * ((x / 50) - (21/5))) + (0.786 * (1 - ((x / 50) - (21/5))))) * x) - (0.0862 * rh * (1 - ((x / 50) - (21/5)))) + (2.966 * ((x / 50) - (21/5))) + (5.75 * (1 - ((x / 50) - (21/5)))) + (8.84 * (10^(-4)) * x^2 * ((x / 50) - (21/5))),
					   x >= 260 ~ 2.966 + (0.69 * x) + (8.84 * (10^(-4)) * x^2))

	}

	return(y)

}

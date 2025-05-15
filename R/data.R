
#' Example Dataset from EPCTool
#'
#' SVI and Air contamination dataset used as example template for EPC calculation.
#'
#' @format An 262 x 8 data frame:
#' \describe{
#'   \item{exposure_unit}{Exposure Unit}
#'   \item{contaminant, casrn}{Contaminant name with respective identifying CAS number}
#'   \item{media}{Sample Media}
#'   \item{concentration, unit, detected_flag}{Sample concentration with respective unit of measurement and detection flag (0 if detected, 1 if not)}
#'   \item{sample_id}{Unique Sample Identifier}
#' }
#'
"contaminants"

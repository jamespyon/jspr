#' Example Dataset from EPCTool
#'
#' SVI and Air contamination dataset used as example template for EPC calculation.
#'
#' @format An 262 x 8 data frame:
#' \describe{
#'   \item{exposure_unit}{Exposure Unit}
#'   \item{contaminant, casrn}{Contaminant name with respective identifying CAS-RN}
#'   \item{media}{Sample Media}
#'   \item{concentration, unit, detected_flag}{Sample concentration with respective unit of measurement and detection flag (0 if detected, 1 if not)}
#'   \item{sample_id}{Unique Sample Identifier}
#' }
#'
"contaminants"

#' TEF List
#'
#' TEF list
#'
#' @format An 29 x 4 data frame:
#' \describe{
#'   \item{casrn}{CAS-RN}
#'   \item{congener_class}{Congener class}
#'   \item{full_congener_name}{Dioxin and dioxin-like coupound chemical name}
#'   \item{shorthand_congener_name}{Dioxin and dioxin-like coupound chemical shorthand name}
#'   \item{tef}{TEF value}
#' }
#'
"tef_congener"

#' PEF List
#'
#' PEF list
#'
#' @format An 25 x 3 data frame:
#' \describe{
#'   \item{casrn}{CAS-RN}
#'   \item{pah}{PAH name}
#'   \item{pef}{PEF value}
#' }
#'
"pef_congener"



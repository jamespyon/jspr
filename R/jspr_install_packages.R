#' Install All Necessary Packages
#'
#' @description Installs all packages required for many statistical analysis done at ATSDR.
#'
#' @param x A character vector with all names of packages to be added to the list
#'
#' @returns An installation of an array of packages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' jspr_install_packages()
#' }
#'
jspr_install_packages <- function(x = NULL) {

  package_list <- c("bayestestR",
                    "boot",
                    "brms",
                    "EnvStats",
                    "flextable",
                    "frictionless",
                    "ggplot2",
                    "gratia",
                    "janitor",
                    "magrittr",
                    "mgcv",
                    "NADA",
                    "NADA2",
                    "openair",
                    "openairmaps",
                    "readxl",
                    "sf",
                    "stringr",
                    "tidybayes",
                    "tidyverse",
                    "tmaptools",
                    "worldmet")

  if(is.null(x)) {x <- package_list}
  if(class(x) != "character") {stop("Must be a valid character vector.")} else {x <- c(package_list, x)}

  utils::install.packages(x)

}

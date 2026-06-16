#' Calculate Benzo(a)pyrene (BaP) Equivalents for Polycyclic Aromatic Hydrocarbons
#'
#' @description Calculate BaP equivalents according to Guidance for Calculating Benzo(a)pyrene Equivalents for Cancer Evaluations of Polycyclic Aromatic Hydrocarbons (2022). Last updated 11/13/2024.
#'
#' @details
#' The function can be used in conjunction with dplyr `group_by()` and `summarise()` functions to output values not dependent on the CAS-RN inputted. Whether or not `casrn` argument has only BaP or a mix with other compounds, the function will only use values concerning BaP equivalent compounds, as long as the arguments are represented well.
#'
#'
#' @param obs A numeric vector
#' @param cen A logical vector pertaining to censoring of obs. TRUE if obs is censored.
#' @param casrn A character vector representing the CAS-RN for each compound.
#' @param sigfig A numeric value for the number of significant figures for the outputs of the function. Default is 4.
#' @param message Do you want mesasge?
#'
#' @returns A list class object.
#' * `bap`: numeric value of the BaP Equivalent
#' * `bap_lower`: numeric value of the lower bound BaP Equivalent when at least one compound has all nondetects.
#' * `bap_upper`: numeric value of the upper bound BaP Equivalent when at least one compound has all nondetects.
#' * `contaminant`: compound the BaP Equivalent should be compared to.
#' * `casrn`: CAS-RN of the compound the BaP Equivalent should be compared to.
#' * `notes`: character string of the underlying process that outputted the returned values.
#' * `qcontrol`: character string of the potential errors related to the data in relation to the returned values.#'
#'
#'
#' @export
#'
#' @examples
#' #example of individual inputs
#' results = rexp(n = 20, rate = 1)
#' nondetects <- results<0.5
#' congener <- rep(pef_congener$casrn, length.out = 20)
#'
#' calculate_bap_equivalents(obs = results, cen = nondetects, casrn = congener)
#'
#' #example of dplyr verbs with dataframe
#' library(tidyverse)
#'
#' congener <- rep(pef_congener$casrn, each = 2, length.out = 20)
#' data.frame(results, nondetects, congener) |>
#'   summarise(bap = calculate_bap_equivalents(obs = results,
#'                                             cen = nondetects,
#'                                             casrn = congener)$bap)
#'

calculate_bap_equivalents <- function(obs, cen, casrn, sigfig = 4, message = TRUE) {

  #warnings
  if(message == TRUE) {
    message(paste("This function is for Benzo(a)pyrene (CAS-RN: 50-32-8) and Benzo(a)pyrene equivalent compounds only.\n",
                  "BaP Equivalent should be compared to Benzo(a)pyrene.\n",
                  "Note: Naphthalene (CAS-RN: 91-20-3) should not be included in the the BaP equivalent compounds and should be evaluated separately.\n",
                  "This function should be used:",
                  " * to calculate BaP equivalents for each sample during screening (grouped by SAMPLE);",
                  " * to calculate EPCs for BaP equivalents (grouped by SAMPLING UNIT)",
                  sep = "\n"))
  }

  #format tef_congener
  pef_data <- jspr::pef_congener

  # check cas for BaP
  casrn <- fix_casrn(casrn)
  bap_casrns <- intersect(pef_data$casrn, casrn)
  bap_present <- length(bap_casrns) > 0

  # final output
  output <- list(bap = NA,
                 bap_lower = NA,
                 bap_upper = NA,
                 contaminant = "BaP Equivalent",
                 casrn = "NULL-CAS-020",
                 notes = "",
                 qcontrol = "")

  # check to see if there are more than just BaP
  if(bap_present) {

    if(all(casrn == "50-32-8")) {

      output$qcontrol <- message_breaks(output$qcontrol, "The imported data contained only Benzo(a)pyrene.")

    }

  }

  #Reduce the group to just the PAHs
  input_data <- data.frame(obs, cen, casrn)
  bap_data <- merge(input_data, pef_data[,c("casrn", "pef")], by = "casrn")
  bap_data <- bap_data[!is.na(bap_data$pef),]

  # pre-processing after filtering
  loop_bap_data <- lapply(unique(bap_data$casrn), function(x) {

    loop_data <- bap_data[casrn == x,]

    nondetect_count <- sum(loop_data$cen)
    obs_count <- length(loop_data$obs)
    detected_count <- sum(!loop_data$cen)
    nondetect_percent <- nondetect_count/obs_count

    if(detected_count > 0){
      unique_detected_values <- unique(loop_data$obs[!loop_data$cen])
      unique_detected_count <- length(unique_detected_values)
      max_detected_value <- signif(max(unique_detected_values), sigfig)
    } else {
      unique_detected_values <- unique(loop_data$obs)
      unique_detected_count <- length(unique_detected_values)
    }

    loop_data$nondetect_count <- nondetect_count
    loop_data$obs_count <- obs_count
    loop_data$detected_count <- detected_count
    loop_data$nondetect_percent <- nondetect_percent


    return(loop_data)

  })

  bap_data <- dplyr::bind_rows(loop_bap_data)

  unique_cas <- unique(bap_data$casrn)

  if(nrow(bap_data) == length(unique_cas)) { # when screening

    if(message == TRUE) {
      message(paste("Distinct CAS-RNs detected. Method is Screening.",
                    sep = "\n"))
    }

    # calculate BaP equivalents
    bap_data$bec <- bap_data$obs*bap_data$pef
    output$bap <- sum(bap_data$bec, na.rm = TRUE)

  } else { # when doing epcs

    if(message == TRUE) {
      message(paste("Replicative CAS-RNs detected. Method is EPC Calculation",
                    sep = "\n"))
    }

    # get highest reported limit for each contaminant
    epc_high_report <- lapply(unique(bap_data$casrn), function(x) {

      loop_data <- bap_data[casrn == x,]
      highest_report_limit <- max(loop_data$obs, na.rm = TRUE)

      # calculate BaP EPC
      epc <- jspr::calculate_epc(loop_data$obs, loop_data$cen, message = FALSE)$epc
      notes <- jspr::calculate_epc(loop_data$obs, loop_data$cen, message = FALSE)$notes
      pef <- unique(loop_data$pef)
      detected_count <- unique(loop_data$detected_count)

      bec_lower <- if(detected_count == 0) {pef*0} else {pef*epc}
      bec_upper <- if(detected_count == 0) {pef*highest_report_limit} else {pef*epc}
      bec <- pef*epc
      has_detects <- if(detected_count > 0) {TRUE} else {FALSE}

      loop_out <- data.frame(casrn = x, highest_report_limit, bec, bec_lower, bec_upper, has_detects)

      return(loop_out)

    })

    bec_epc <- dplyr::bind_rows(epc_high_report)

    # check if any BaP congeners are all non detects
    each_congener_has_detects <- all(bec_epc$has_detects)

    if(!each_congener_has_detects) { # if there is a congener that has all non detects

      # lower bound BaP
      output$bap_lower <- sum(bec_epc$bec_lower, na.rm = TRUE)

      # upper bound BaP
      output$bap_upper <- sum(bec_epc$bec_upper, na.rm = TRUE)
      output$notes <- message_breaks(output$notes, "This EPC represents a lower-bound benzo(a)pyrene (BaP) equivalent concentration for polycyclic aromatic hydrocarbons. See this dataset's quality control flag for further information about the calculated EPC.")
      output$notes <- message_breaks(output$notes, "This EPC represents an upper-bound benzo(a)pyrene (BaP) equivalent concentration for polycyclic aromatic hydrocarbons. See this dataset's quality control flag for further information about the calculated EPC.")

      output$qcontrol <- message_breaks(output$qcontrol, "At least one polycyclic aromatic hydrocarbon congener associated with this BaP equivalent concentration came from a dataset with all nondetects. See Section 3.2.1 of ATSDR's Guidance for Calculating Benzo(a)pyrene Equivalents for Cancer Evaluations of Polycyclic Aromatic Hydrocarbons for further information.")

    } else {

      output$bap <- sum(bec_epc$epc, na.rm = TRUE)

      output$notes <- message_breaks("Maximum detected value and EPC are derived from benzo(a)pyrene (BaP) equivalent concentrations for polycyclic aromatic hydrocarbons. The maximum detected value should be used for cancer screening and the EPC should be used for evaluation of cancer risk.")

    }

  }

  return(output)

}

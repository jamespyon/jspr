#' Calculate Toxic Equivalents (TEQ) for Dioxin and Dioxin-Like Compounds
#'
#' @description Calculate TEQs according to ATSDR Toxic Equivalents Procedure for Dioxin and Dioxin-like Compounds Evaluation (2025). Last updated 07/25/2025.
#'
#' @details
#' The function can be used in conjunction with dplyr `group_by()` and `summarise()` functions to output values not dependent on the CAS-RN inputted. Whether or not `casrn` argument has exactly only dioxin and DLC or a mix with other compounds, the function will only use values concerning dioxin and DLC, as long as the arguments are represented well.
#'
#'
#' @param obs A numeric vector
#' @param cen A logical vector pertaining to censoring of obs. TRUE if obs is censored.
#' @param casrn A character vector representing the CAS-RN for each compound.
#' @param sigfig A numeric value for the number of significant figures for the outputs of the function. Default is 4.
#' @param warnings Logical. Do you want warnings?
#'
#' @returns A list class object.
#' * `teq`: numeric value of the TEQ.
#' * `teq_cen`: logical value of the censoring of the TEQ. TRUE for non-detect.
#' * `rpd`: numeric value for the relative percent difference between the lower-bound TEQ estimate and the upper-bound TEQ estimate.
#' * `rpd_flag`: flag for "RPD > 50%".
#' * `contaminant`: compound the TEQ should be compared to.
#' * `casrn`: CAS-RN of the compound the TEQ should be compared to.
#' * `notes`: character string of the underlying process that outputted the returned values.
#' * `qcontrol`: character string of the potential errors related to the data in relation to the returned values.#'
#'
#' @export
#'
#' @examples
#' results = rexp(n = 20, rate = 1)
#' nondetects <- results<0.5
#' congener <- rep(tef_congener$casrn, length.out = 20)
#'
#' calculate_teq(obs = results, cen = nondetects, casrn = congener)
#'
#' library(tidyverse)
#'
#' congener <- rep(tef_congener$casrn, each = 2, length.out = 20)
#'
#' data.frame(results, nondetects, congener, group = rep(c("A", "B"), 20)) |>
#'   group_by(group) |>
#'   summarise(teq = calculate_teq(obs = results, cen = nondetects, casrn = congener)$teq)
#'

calculate_teq <- function(obs, cen, casrn, sigfig = 4, warnings = TRUE) {

  #warnings
  if(warnings == TRUE) {
    warning(paste("This function is for dioxin and dioxin-like compounds only.",
                  "TEQs should be compared as to 2,3,7,8-tetrachlorodibenzo-p-dioxin (2,3,7,8-TCDD).",
                  "Note: specific dioxins should be calculated individually with calculate_epc() along with calculate_teq().",
                  "Namely:",
                  "*2,3,7,8-Tetrachloro dibenzo-p-dioxin (CASRN: 1746-01-6)",
                  "*1,2,3,6,7,8-Hexachloro dibenzo-p-dioxin (CASRN: 57653-85-7)",
                  "*1,2,3,7,8,9-Hexachloro dibenzo-p-dioxin (CASRN: 19408-74-3) ",
                  "*2,3,4,7,8-Pentachloro dibenzofuran (CASRN: 57117-31-4)",
                  sep = "\n"))
  }

  # check cas for dioxins
  casrn <- fix_casrn(casrn)
  dioxin_casrns <- intersect(jspr::tef_congener$casrn, casrn)
  dioxin_present <- length(dioxin_casrns) > 0

  # final output
  output <- list(teq = NA,
                 teq_cen = NA,
                 rpd = NA,
                 rpd_flag = "",
                 contaminant = "2,3,7,8-TCDD TEQ",
                 casrn = "NULL-CAS-017",
                 notes = "",
                 qcontrol = "")

  # Only do the calculation if there's something besides just dioxin present
  if(dioxin_present) {

    if(all(casrn == "001746-01-6")) {

      df$qcontrol <- message_breaks(df$qcontrol, "The imported data contained only 2,3,7,8-TCDD.")

    }

  }

  # continue analysis
  if(dioxin_present) {

    # create dataframe
    # filter down to just dioxins (contaminants with TECs)
    input_data <- data.frame(obs, cen, casrn)
    dioxin_data <- merge(input_data, jspr::tef_congener[,c("casrn", "tef")], by = "casrn")
    dioxin_data <- dioxin_data[!is.na(dioxin_data$tef),]
    dioxin_data$tec <- dioxin_data$obs*dioxin_data$tef

    # pre-processing after filtering
    obs_count <- length(dioxin_data$obs)
    nondetect_count <- sum(dioxin_data$cen)
    detected_count <- sum(!dioxin_data$cen)
    nondetect_percent <- nondetect_count/obs_count
    if(detected_count > 0){
      unique_detected_values <- unique(dioxin_data$obs[!dioxin_data$cen])
      unique_detected_count <- length(unique_detected_values)
      max_detected_value <- signif(max(unique_detected_values), sigfig)
    } else {
      unique_detected_values <- unique(dioxin_data$obs)
      unique_detected_count <- length(unique_detected_values)
    }


    # Sum the TECs and determine the fraction that comes from nondetects.
    dioxin_data_nd <- dioxin_data[dioxin_data$cen,]
    sum_tec_nd <- sum(dioxin_data_nd$tec)
    sum_tec_all <- sum(dioxin_data$tec)
    nd_tec_ratio <- sum_tec_nd/sum_tec_all

    # If >80%, assume the resultant TEQ is nondetect. Otherwise assume it is a detect.
    if (nd_tec_ratio > 0.8) {
      output$teq_cen <- TRUE
    } else {
      output$teq_cen <- FALSE
    }

    # Evaluate min and max TECs:
    tec_min <- min(dioxin_data$tec, na.rm = TRUE)
    tec_max <- max(dioxin_data$tec, na.rm = TRUE)

    #Set initial flags for KM call for all records
    dioxin_data$cen_for_km <- dioxin_data$cen

    #if the max or the min TEC is a non-detect, convert them to a detect for calcs with the KM method
    #only perform this check if there are at least three detected congeners
    if(detected_count >= 3) {

      #Address minimum as nondetect
      #if statement confirms that no detected records match the min TEC.
      #If they don't, the minimum value is a non-detect and one instance of it should be changed
      #If they do, no need to change anything even if a minimum value is also a nondetect
      any(dioxin_data[!dioxin_data$cen,]$tec == tec_min)

      if(any(dioxin_data[!dioxin_data$cen,]$tec == tec_min)) {

        #Find the first instance of the TEC equal to the min (should be all nondetects at this stage)s
        dioxin_data[min(which(dioxin_data$tec == tec_min)), ]$cen_for_km <- FALSE # should be false?

      }

      #Address maximum as nondetect
      #if statement confirms that no detected records match the max TEC.
      #If they don't, the maximum value is a non-detect and one instance of it should be changed
      #If they do, no need to change anything even if a maximum value is also a nondetect
      if(any(dioxin_data[!dioxin_data$cen,]$tec == tec_max)) {

        #Find the first instance of the TEC equal to the max (should be all nondetects at this stage)s
        dioxin_data[min(which(dioxin_data$tec == tec_max)), ]$cen_for_km <- FALSE # should be false?

      }

    }

    # if all are detects or there are less than three detects, sum TECs to get the TEQ
    if(sum(dioxin_data$cen) == 0 | detected_count < 3) {

      teq <- sum(dioxin_data$tec, na.rm = TRUE)

      output$teq <- signif(teq, sigfig)

      # otherwise, use the Kaplan Meier approach
    } else if(detected_count >= 3 & sum(dioxin_data$cen) >= 1) {

      #There's a chance that all the nondetects are now detects after the flag switching above. If so do calculations assuming all detects but still test sensitivity
      if(sum(dioxin_data$cen_for_km) == 0){

        teq <- sum(dioxin_data$tec, na.rm = TRUE)

        output$teq <- signif(teq, sigfig)

      } else {

        #else we still have at least one nondetect, so enparCensored shouldn't throw an error
        teq <- EnvStats::enparCensored(dioxin_data$tec, dioxin_data$cen_for_km)$parameters[[1]]*nrow(dioxin_data)

        output$teq <- signif(teq, sigfig)

      }

      # sensitivity analysis
      teq_lwr <- dioxin_data$tec
      teq_lwr <- sum(teq_lwr, na.rm = TRUE)
      teq_uppr <- sum(dioxin_data$tec, na.rm = TRUE)

      output$rpd <- abs((teq_uppr - teq_lwr) / ((teq_uppr + teq_lwr)/2)) |> signif(sigfig)

      if(output$rpd > 0.5) {

        output$rpd_flag <- "RPD > 50%"

      }

    }

    output$notes <- "Maximum detected value and EPC are derived from toxic equivalents for dioxins and dioxin-like compounds."

    no_rpdflags <- all(unique(output$rpd_flag) == "") # check if any RPD flags

    if (no_rpdflags) { # if no samples had an RPD flag

      output$qcontrol = ""

    } else { # if at least one of the samples had an RPD flag

      # check if RPD was the max for the exposure unit/media group
      rpd_max <- max(output$rpd, na.rm = TRUE)
      teq_max <- max(output$teq, na.rm = TRUE)

      if (rpd_max == teq_max) {

        output$qcontrol <- "The method used to replace nondetects had an unacceptably large influence on at least one of the dioxin TEQs used to calculate this EPC and on the dioxin TEQ selected as the maximum detected value. See Section 3.6 of ATSDR's Toxic Equivalents Guidance for Dioxin and Dioxin-like Compounds for further information."

      } else{

        output$qcontrol <- "The method used to replace nondetects had an unacceptably large influence on at least one of the dioxin TEQs used to calculate this EPC. See Section 3.6 of ATSDR's Toxic Equivalents Guidance for Dioxin and Dioxin-like Compounds for further information."

      }

      output$notes <- message_breaks(output$notes, "One or more samples had a relative percent difference (RPD) greater than 50%; see this dataset's quality control flag for further information.")


    }

  } else {

    output$qcontrol <- ""
    output$notes <- ""

  }

  return(output)

}

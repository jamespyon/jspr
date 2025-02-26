#' Calculate Exposure Point Concentrations (EPCs)
#'
#'@description Calculate exposure point concentrations according to ATSDR Division of Community Health Investigations Exposure Point Concentration Guidance for Discrete Sampling (2023). Last updated 09/21/2023.
#'
#' @param obs A numeric vector
#' @param cen A logical vector pertaining to censoring of obs. TRUE if obs is censored.
#' @param conf.level A numeric vector from 0 to 1 for confidence level. Default is 0.90 for computing 95UCL.
#' @param sigfig A numeric value for the number of significant figures for the outputs of the function. Default is 4.
#' @param testForNormal Logical. If you want to test for normal distribution of your obs. Default is TRUE
#' @param useDefaultSeed Logical. The Default TRUE uses custom seed similar to EPCTool, which is sum of all concentrations. For testing, either upload separate files of individual data subsets to EPCTool or sum all obs in clean dataset and set to seed.
#'
#' @return A data.frame class object.
#' * `function_used`: the character string representing the type of calculation used for the EPC.
#' * `mean`: numeric value of the estimated mean based on `function_used`.
#' * `sd`: numeric value of the estimated standard deviation based on `function_used`.
#' * `median`: numeric value of the estimated median based on `function_used`, if available.
#' * `epc`: numeric value of the EPC based on `function_used`.
#' * `mean_lci`: numeric value of the lower confidence interval based on `conf.level`, based on `function_used`.
#' * `mean_uci`: numeric value of the upper confidence interval based on `conf.level`, based on `function_used`. Technically the EPC.
#' * `notes`: character string of the underlying process that outputted the returned values.
#' * `qcontrol`: character string of the potential errors related to the data in relation to the returned values.
#' * `normal_dist`: numeric value of the Cox-value for normal distribution.
#' * `lognorm_dist`: numeric value of the Cox-value for log-normal distribution.
#' * `gamma_dist`: numeric value of the Cox-value for gamma distribution.
#' * `best_dist`: either Normal, Gamma, or Lognormal, depending on what is chosen.
#' * `dist_iqr`: character string of the IQR (interquartile range, i.e. 25th and 75th percentile) of the distribution, if avaliable.
#' * `mean_ci`: character string of the confidence interval based on `conf.level`.
#'
#' @export
#'
#' @examples
#' results <- rexp(n = 15, rate = 1)
#' nondetects <- results<0.5
#' calculate_epc(obs = results, cen = nondetects)
#'

calculate_epc <- function(obs = NULL, cen = NULL, conf.level = 0.90, sigfig = 4, testForNormal = TRUE, useDefaultSeed = TRUE) {

  #pre-processing
  obs_count <- length(obs)
  nondetect_count <- sum(cen)
  detected_count <- sum(!cen)
  nondetect_percent <- nondetect_count/obs_count
  if(detected_count > 0){
    unique_detected_values <- unique(obs[!cen])
    unique_detected_count <- length(unique_detected_values)
    max_detected_value <- signif(max(unique_detected_values), sigfig)
  }

  #warnings
  #rlang::warn("This function should not be used for the following:\n*asbestos/lead,\n*radiological contaminants,\n*dioxins/polycyclic aromatic hydrocarbons (PAHs),\n*non-discrete sampling data,\n*dependent data.")
  #rlang::abort("abort")

  #set dataset seed.
  if(useDefaultSeed){set.seed(sum(obs))}

  # final output
  df <- data.frame(
    function_used = "",
    mean = NA,
    sd = NA,
    median = NA,
    epc = NA,
    mean_lci = NA,
    mean_uci = NA,
    notes = "",
    qcontrol = "",
    normal_dist = NA,
    lognorm_dist = NA,
    gamma_dist = NA,
    best_dist = "NA",
    dist_iqr = "NA",
    mean_ci = ""
  )

  #perform initial screening for conditions that don't allow for 95UCL calculation

  if(detected_count == 0){ #if not detected, we use maximum censoring limit

    df$epc <- signif(max(obs), sigfig)
    df$function_used <- "no_detections"
    df$notes <- "This dataset did not contain any detected results, so an EPC could not be calculated. Use the maximum censoring limit."

  } else if(detected_count < 4) { #if less than 4 detected, we use max detected

    df$epc <- max_detected_value
    df$function_used <- "max_less_than_4_detections"
    df$notes <- message_breaks(df$notes, "This dataset contained less than four detections, so the EPC is equal to the maximum detected value.")

  } else if(nondetect_percent >= 0.8){ #if equal/more than 80% non-detected, we use max detected

    df$epc <- max_detected_value
    df$function_used <- "max_80_percent_or_more_non_detects"
    df$notes <- message_breaks(df$notes, "This dataset contained 80% or more non-detects, so the EPC is equal to the maximum detected value.")

  }

  #perform 95UCL calculation

  if(obs_count < 8){ #if less than 8 obs, we use the max

    df$epc <- max_detected_value
    df$function_used <- "max_less_than_8_records"
    df$notes <- message_breaks(df$notes, "This dataset contained less than eight records, so the EPC is equal to the maximum detected value.")

  } else if(unique_detected_count < 3){ #if less than 3 unique detected, we use the max

    df$epc <- max_detected_value
    df$function_used <- "max_fewer_than_3_unique_detections"
    df$notes <- message_breaks(df$notes, "This dataset contained fewer than three unique detected values, so the EPC is equal to the maximum detected value.")

  } else if(obs_count >= 20){ #20 or more values requires bootstrapping

    if(nondetect_count > 0){

      dataCensoringSummary <- NADA::censummary(obs, cen)
      pexceed <- dataCensoringSummary$limits$pexceed

      if(length(pexceed) == 1 | stats::var(pexceed) == 0) {

        distData <- EnvStats::elnormAltCensored(obs, cen, method = "rROS", ci = TRUE, ci.type = "two-sided", ci.method = "bootstrap" , n.bootstraps = 5000, conf = conf.level)
        df$function_used <- "lognormalBootstrap_95ucl"

      } else {

        distData <- EnvStats::enparCensored(obs, cen, ci = TRUE, ci.type = "two-sided", ci.method = "bootstrap", n.bootstraps = 5000, conf = conf.level)
        df$function_used <- "bootstrap_95ucl"

      }

      df$epc <- as.numeric(distData$interval$limits["Pct.UCL"])
      df$mean <- as.numeric(distData$parameters["mean"])
      df$mean_lci <- signif(as.numeric(distData$interval$limits["Pct.LCL"]), sigfig)

      if(df$function_used == "lognormalBootstrap_95ucl") {
        cv <- distData$parameters[[2]]
        df$sd <- df$mean * cv # Standard deviation, since it isn't directly reported
        df$median <- EnvStats::qlnormAlt(0.5, df$mean, cv) # Median, since it isn't directly reported

        firstQuartile <- signif(EnvStats::qlnormAlt(0.25, df$mean, cv), sigfig)
        thirdQuartile <- signif(EnvStats::qlnormAlt(0.75, df$mean, cv), sigfig)
        df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","), "–", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum(signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")
      }
      if(df$function_used == "bootstrap_95ucl") {
        cv <- distData$parameters[[2]]
        df$sd <- df$mean * cv # Standard deviation, since it isn't directly reported
        df$median <- EnvStats::qlnormAlt(0.5, df$mean, cv) # keep as normal dist for now

        firstQuartile <- signif(EnvStats::qlnormAlt(0.25, df$mean, cv), sigfig) # keep as normal dist for now
        thirdQuartile <- signif(EnvStats::qlnormAlt(0.75, df$mean, cv), sigfig) # keep as normal dist for now
        df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","), "–", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum(signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")
      }

    } else {

      bootoutput <- boot::boot(obs, function(x, index) mean(x[index]), 5000)
      df$epc <- boot::boot.ci(bootoutput, conf = conf.level, type = "perc")$percent[[5]]
      df$mean <- mean(bootoutput$t)
      df$mean_lci <- signif(boot::boot.ci(bootoutput, conf = conf.level, type = "perc")$percent[[4]], sigfig)
      df$median <- stats::median(bootoutput$t)
      firstQuartile <- signif(stats::quantile(bootoutput$t, 0.25), sigfig)
      thirdQuartile <- signif(stats::quantile(bootoutput$t, 0.75), sigfig)
      df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","), "–", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum(signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")
      df$function_used <- "bootstrap_95ucl"

    }

    # mean confidence interval
    df$mean_uci <- signif(df$epc, sigfig)

  } else { #between 8 and 19

    if(nondetect_count > 0){

      bCox <- EnvStats::boxcoxCensored(obs,cen,lambda = seq(0,1,0.1))
      ppcc <- bCox$objective

      lognorm_dist <- ppcc[1]
      gamma_dist <- ppcc[4]
      normal_dist <- ppcc[11]

      df$lognorm_dist <- signif(lognorm_dist, sigfig)
      df$gamma_dist <- signif(gamma_dist, sigfig)
      df$normal_dist <- signif(normal_dist, sigfig)

    } else {

      lognorm_w <- EnvStats::gofTest(obs, dist="lnorm")
      lognorm_dist <- lognorm_w$statistic

      gamma_w <- EnvStats::gofTest(obs, dist="gamma")
      gamma_dist <- gamma_w$statistic

      normal_w <- EnvStats::gofTest(obs, dist="norm")
      normal_dist <- normal_w$statistic

      p_lognorm <- lognorm_w$p.value
      p_gamma <- gamma_w$p.value
      p_normal <- normal_w$p.value

      df$lognorm_dist <- paste0(signif(lognorm_dist, sigfig), " (p = ", signif(p_lognorm, sigfig), ")")
      df$gamma_dist <- paste0(signif(gamma_dist, sigfig), " (p = ", signif(p_gamma, sigfig), ")")
      df$normal_dist <- paste0(signif(normal_dist, sigfig), " (p = ", signif(p_normal, sigfig), ")")

    }

    roundedDistStats <- signif(c(normal_dist, gamma_dist, lognorm_dist), 8)
    normal_dist <- roundedDistStats[[1]]
    gamma_dist <- roundedDistStats[[2]]
    lognorm_dist <- roundedDistStats[[3]]

    if(testForNormal){
      org_dist <- sort(c(normal_dist, gamma_dist, lognorm_dist), decreasing = TRUE)
    } else {
      org_dist <- sort(c(gamma_dist, lognorm_dist), decreasing = TRUE)
    }

    success <- FALSE
    iterations <- 0

    checkedNormalDistribution <- FALSE
    checkedGammaDistribution <- FALSE
    checkedLognormalDistribution <- FALSE

    for(max_stat in org_dist) {
      #max_stat = org_dist[1]

      iterations <- iterations + 1

      if(max_stat == normal_dist && isFALSE(checkedNormalDistribution) && testForNormal){

        df$best_dist <- "Normal"

        if(nondetect_count > 0){

          distData <- EnvStats::enormCensored(obs, cen, ci = TRUE, ci.type = "upper", ci.method = "normal.approx")
          ci_90 <- EnvStats::enormCensored(obs, cen, ci = TRUE, ci.type = "two-sided", ci.method = "normal.approx", conf.level = conf.level)

        } else {

          distData <- EnvStats::enorm(obs, ci = TRUE, ci.type = "upper")
          ci_90 <- EnvStats::enorm(obs, ci = TRUE, ci.type = "two-sided", conf.level = conf.level)

        }

        df$function_used <- "normal_95ucl"
        df$epc <- distData[["interval"]][["limits"]][["UCL"]]
        df$mean <- distData[["parameters"]][["mean"]]

        df$mean_lci <- signif(ci_90$interval$limits[[1]], sigfig)
        df$mean_uci<- signif(ci_90$interval$limits[[2]], sigfig)

        df$sd <- distData[["parameters"]][["sd"]]
        df$median <- stats::qnorm(0.5, df$mean, df$sd)

        firstQuartile <- signif(stats::qnorm(0.25, df$mean, df$sd), sigfig)
        thirdQuartile <- signif(stats::qnorm(0.75, df$mean, df$sd), sigfig)
        df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","),"–", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum( signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")

        checkedNormalDistribution <- TRUE

      } else if(max_stat == gamma_dist && isFALSE(checkedGammaDistribution)){

        df$best_dist <- "Gamma"

        if(nondetect_count > 0){

          distData <- EnvStats::egammaAltCensored(obs, cen, ci = TRUE, ci.type = "upper", ci.method = "normal.approx")
          ci_90 <- EnvStats::egammaAltCensored(obs, cen, ci = TRUE, ci.type = "two-sided", ci.method = "normal.approx", conf.level = conf.level)

        } else {

          distData <- EnvStats::egammaAlt(obs, ci = TRUE, ci.type = "upper", ci.method = "normal.approx")
          ci_90 <- EnvStats::egammaAlt(obs, ci = TRUE, ci.type = "two-sided", ci.method = "normal.approx", conf.level = conf.level)

        }

        df$function_used = "gamma_95ucl"

        df$epc <- distData[["interval"]][["limits"]][["UCL"]]
        df$mean <- distData[["parameters"]][["mean"]]

        df$mean_lci <- signif(ci_90$interval$limits[[1]], sigfig)
        df$mean_uci<- signif(ci_90$interval$limits[[2]], sigfig)

        cv <- distData$parameters[[2]]
        df$sd <- df$mean * cv
        df$median <- EnvStats::qgammaAlt(0.5, df$mean, cv)

        firstQuartile <- signif(EnvStats::qgammaAlt(0.25, df$mean, cv), sigfig)
        thirdQuartile <- signif(EnvStats::qgammaAlt(0.75, df$mean, cv), sigfig)
        df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","),"—", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum( signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")

        checkedGammaDistribution <- TRUE

      } else if(max_stat == lognorm_dist && isFALSE(checkedLognormalDistribution)){

        df$best_dist <- "Lognormal"
        df$function_used = "lognormal_95ucl"

        if(nondetect_count > 0){

          distData <- EnvStats::elnormAltCensored(obs, cen, ci = TRUE,ci.type = "upper", ci.method = "cox")
          ci_90 <- EnvStats::elnormAltCensored(obs, cen, ci = TRUE, ci.type = "two-sided", ci.method = "cox", conf.level = conf.level)

        } else {

          distData <- EnvStats::elnormAlt(obs, ci = TRUE, ci.type = "upper", ci.method = "cox")
          ci_90 <- EnvStats::elnormAlt(obs, ci = TRUE, ci.type = "two-sided", ci.method = "cox", conf.level = conf.level)

        }

        df$epc <- distData[["interval"]][["limits"]][["UCL"]]
        df$mean <- distData[["parameters"]][["mean"]]

        df$mean_lci <- signif(ci_90$interval$limits[[1]], sigfig)
        df$mean_uci<- signif(ci_90$interval$limits[[2]], sigfig)

        cv <- distData$parameters[[2]]
        df$sd <- df$mean * cv # Standard deviation, since it isn't directly reported
        df$median <- EnvStats::qlnormAlt(0.5, df$mean, cv) # Median, since it isn't directly reported

        firstQuartile <- signif(EnvStats::qlnormAlt(0.25, df$mean, cv), sigfig)
        thirdQuartile <- signif(EnvStats::qlnormAlt(0.75, df$mean, cv), sigfig)
        df$dist_iqr <- paste0(prettyNum(firstQuartile, big.mark = ","), "–", prettyNum(thirdQuartile, big.mark = ","), " (", prettyNum(signif(thirdQuartile-firstQuartile, sigfig), big.mark = ","), ")")

        checkedLognormalDistribution <- TRUE

      } else {

        df$function_used <- "there_was_an_error"
        break

      }

      if (df$mean < max_detected_value) {

        success <- TRUE
        break

      } else {

        df$qcontrol <- message_breaks(df$qcontrol, "The model estimated mean for the best fitting distribution was greater than the maximum detected value, so the EPC was evaluated using the next best fitting distribution instead. See section 3.7 of ATSDR's EPC Guidance for Discrete Sampling for further information.")

      }

      if (isFALSE(success)){

        if (testForNormal == FALSE){

          checkedNormalDistribution <- TRUE

        }

        if (isFALSE(checkedNormalDistribution &&
                    checkedGammaDistribution &&
                    checkedLognormalDistribution)){

          if(is.na(max_stat)){

            df$epc <- max_detected_value
            df$function_used <- "max_data_did_not_fit_any_distribution"

            default_error_message <- paste(df$notes,"error or did not fit any model")

            if(nondetect_count > 0){

              if(max(unique_detected_values) < max(obs)){

                df$notes <- message_breaks(df$notes, "The data did not fit any distribution and all detected values are less than the maximum censoring limit, so the maximum detected value was used for the EPC.")

              }

              else {

                df$notes <- default_error_message

              }

            }

            else {

              df$notes <- default_error_message

            }

          }

        } else {

          df$epc <- max_detected_value
          df$function_used <- "max_mean_greater_than_max_detect"
          df$qcontrol <- message_breaks(df$qcontrol, "The model estimated mean was greater than the maximum detected value. Tried using other distributions but all distributions failed to have the model estimated mean be less than the max detected value. Reverting to maximum.")

          #added to break for loop
          break

        }

      }

    } #end of for loop

    if(df$function_used == "normal_95ucl"){

      testZ <- stats::pnorm(-3, mean = 0, 1)
      testValues <- stats::pnorm(0, mean = distData$parameters[[1]], sd = distData$parameters[[2]])
      if(testZ <= testValues){ #not a good fit to normal distribution, falling back to next distribution

        #make recursive function call
        return(calculate_epc(obs, cen, testForNormal = FALSE, useDefaultSeed = useDefaultSeed))

      }

    }

  } #end between 8 and 19

  ros_df <- as.data.frame(NADA::ros(obs, as.logical(cen)))
  average_value <- mean(ros_df$modeled, na.rm = TRUE)

  if(!is.na(df$epc) & substr(df$function_used, 1, 3) != "max"){

    if(df$epc > max_detected_value){

      if (obs_count >= 20) {

        df$qcontrol <- message_breaks(df$qcontrol,"The estimated 95th percentile upper confidence limit of the mean (95UCL) for this dataset was larger than the maximum detected value. Because the dataset contains 20 or more records, the maximum value was used as the EPC instead of the 95UCL. See section 3.7 of ATSDR's EPC Guidance for Discrete Sampling for further information.")
        df$epc <- max_detected_value
        df$function_used <- "max_95ucl_larger_than_max_value"

      } else if (obs_count < 20 & obs_count >= 8) {

        df$qcontrol <- message_breaks(df$qcontrol,"The estimated 95th percentile upper confidence limit of the mean (95UCL) for this dataset was larger than the maximum detected value. Because the dataset contains between 8 and 19 records, the 95UCL was used as the EPC. See section 3.7 of ATSDR's EPC Guidance for Discrete Sampling for further information.")

      }

    } else if(df$epc < average_value){

      df$qcontrol <- message_breaks(df$qcontrol,"The estimated 95th percentile upper confidence limit of the mean (95UCL) for this dataset was less than the average value of this dataset. As a result, the maximum was used as the EPC. See section 3.7 of ATSDR's EPC Guidance for Discrete Sampling for further information.")
      df$epc <- max_detected_value
      df$function_used <- "max_95ucl_less_than_average_value"

    } else if(df$epc > (3*average_value)){

      df$qcontrol <- message_breaks(df$qcontrol,"The estimated 95th percentile upper confidence limit of the mean (95UCL) for this dataset was more than three times the average value of this dataset. The 95UCL was used as the EPC for this dataset but should be verified. See section 3.7 of ATSDR's EPC Guidance for Discrete Sampling for further information.")

    }

  }

  #Check for case where called functions returned NaN or Inf
  if(is.na(df$epc) || is.nan(df$epc) || is.infinite(df$epc)){

    df$function_used <- "EPC_not_calculated_due_to_NaN_or_Inf"
  }

  #Finalize notes
  if(df$function_used == "bootstrap_95ucl"){

    df$notes <- message_breaks(df$notes, "This dataset contained 20 or more records, so the EPC was calculated using bootstrap sampling.")

  } else if(df$function_used == "lognormalBootstrap_95ucl"){

    df$notes <- message_breaks(df$notes, "This dataset contained 20 or more records. The data are either singly censored or have multiple censoring limits with no interspersed levels, so the EPC was calculated using bootstrap sampling of a lognormal distribution with regression on order statistics.")

  } else if(df$function_used == "normal_95ucl"){

    df$notes <- message_breaks(df$notes, "This dataset contained between 8 and 19 records. A normal distribution best fit the imported data, so the EPC was calculated assuming a normal distribution.")

  } else if(df$function_used == "lognormal_95ucl"){

    if(testForNormal){

      df$notes <- message_breaks(df$notes,"This dataset contained between 8 and 19 records. A lognormal distribution best fit the imported data, so the EPC was calculated assuming a lognormal distribution.")

    } else {

      df$qcontrol <- message_breaks(df$qcontrol, "The normal distribution best fit the imported data, but it included an unacceptable amount of negative values so the next best-fitting distribution was used instead. See Section 3.5 of ATSDR's EPC Guidance for Discrete Sampling for further information.")
      df$notes <- message_breaks(df$notes,"This dataset contained between 8 and 19 records. A normal distribution best fit the imported data, but the assumed distribution contained an unrealistic amount of negative values. The lognormal distribution was the next best-fitting distribution, so the EPC was calculated assuming a lognormal distribution.")

    }

  } else if (df$function_used == "gamma_95ucl"){

    if(testForNormal){

      df$notes <- message_breaks(df$notes,"This dataset contained between 8 and 19 records. A gamma distribution best fit the imported data, so the EPC was calculated assuming a gamma distribution.")

    } else {

      df$qcontrol <- message_breaks(df$qcontrol, "The normal distribution best fit the imported data, but it included an unacceptable amount of negative values so the next best-fitting distribution was used instead. See section 3.5 of ATSDR's EPC Guidance for Discrete Sampling for further information.")
      df$notes <- message_breaks(df$notes,"This dataset contained between 8 and 19 records. A normal distribution best fit the imported data, but the assumed distribution contained an unrealistic amount of negative values. The gamma distribution was the next best-fitting distribution, so the EPC was calculated assuming a gamma distribution.")

    }
  }

  if(df$qcontrol != ""){

    df$notes = message_breaks(df$notes, "See this dataset's quality control flag for further information about the calculated EPC.")

  }

  df$mean_ci <- paste0(" (", prettyNum(df$mean_lci, big.mark = ","), "—", prettyNum(df$mean_uci, big.mark = ","), ")")

  return(df)

}

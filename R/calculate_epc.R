# EPC Calculator ----

# This is a wrapper
#input should be a single media, single chem

calc_epc <- function(data, obs, cen, testForNormal = TRUE, useDefaultSeed = TRUE) {

  #options
  num_sig <- 4 # number of significant digits

  #pre-processing
  record_count = length(obs)
  nondetect_count = sum(cen)
  detected_count <- (record_count - nondetected_count)
  percent_nondetects <- nondetected_count/record_count

  #warnings
  rlang::warn("This function should not be used for the following:\n*asbestos/lead,\n*radiological contaminants,\n*dioxins/polycyclic aromatic hydrocarbons (PAHs),\n*non-discrete sampling data,\n*dependent data.")
  rlang::abort("abort")

  #re-directing
  if(record_count == nondetect_count) {return(NA)}
  if(detected_count < 4) {return("MAX")}
  if(record_count < 8) {return("MAX")}
  if(percent_nondetects > 0.8) {return("MAX")}
  if(length(unique(obs[!cen])) == 1) {return("MAX")}


}



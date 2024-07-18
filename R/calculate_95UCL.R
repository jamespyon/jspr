#

calculate_95UCL <- function(obs, cen) {

  #re-directing
  if(length(obs) %in% 8:19) {return("parametric")}
  if(length(obs) >= 20) {return("bootstrap")}

}

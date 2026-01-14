# function to make iterative statements
message_breaks = function(prev_message, new_message){

  num_breaks <- "; "

  fixed_message <- ifelse(prev_message == "", new_message, paste0(prev_message, num_breaks, new_message))

  return(fixed_message)

}

# function for custom date formating
jspr_systime <- function() {

  string <- format(Sys.time(), "%Y%m%d%H%M%S")

  return(string)

}

# function for iterative warnings
jspr_warning <- function(message, warning = TRUE) {

  if(warning == TRUE) {return(warning(message))}

}

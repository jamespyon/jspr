# function to make iterative statements
message_breaks = function(prev_message, new_message){

  num_breaks <- "; "

  fixed_message <- ifelse(prev_message == "", new_message, paste0(prev_message, num_breaks, new_message))

  return(fixed_message)

}

# function for custom date formating
jspr_systime <- function() {

  string <- Sys.time()

  year <- substr(string, 1, 4)
  sec <- substr(string, 18, 19)

  paste0(year, month, day, " ", hour, ":", min, ":", sec)

}

# function for iterative warnings
jspr_warning <- function(message, warning = TRUE) {

  if(warning == TRUE) {return(warning(message))}

}

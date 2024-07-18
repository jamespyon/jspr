message_breaks = function(prev_message, new_message){

  num_breaks <- "; "

  fixed_message <- ifelse(prev_message == "", new_message, paste0(prev_message, num_breaks, new_message))

  return(fixed_message)

}

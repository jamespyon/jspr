#' message_breaks
#'
#' @param prev_message The message that already exists
#' @param new_message The new message to be added
#'
#' @description Add breaks between multiple quality control or notes messages
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

message_breaks = function(prev_message, new_message){

  num_breaks <- "; "

  fixed_message <- ifelse(prev_message == "", new_message, paste0(prev_message, num_breaks, new_message))

  return(fixed_message)

}

#' Fix Chemical Abstracts Service (CAS) Registry Number (RN)
#'
#' @description Formats CAS-RN. Able to check for whitespaces. Checks for CAS-RN validity.
#'
#' @param casrn A character vector representing CAS-RN.
#' @param format A character vector representing format for CAS-RN.
#'
#' @returns A character string.
#'
#' @export
#'
#' @examples
#' fix_casrn(c("39001-02-0", "74472-37-0"))

fix_casrn <- function(casrn, format = c("xx-xx-x", "xxxxx")){

  # initialize
  cas_string <- as.character(casrn) |> gsub(" ", "", x = _)
  if(identical(format, c("xx-xx-x", "xxxxx"))) {format <- "xx-xx-x"}

  # check format of input
  if(any(grepl("^[Nn][Uu][Ll][Ll]", cas_string))) {
    cas_string <- stringr::str_replace(cas_string, "^[Nn][Uu][Ll][Ll]-[Cc][Aa][Ss]-", "NULL-CAS-")
  }
  if(any(grepl("^0*([0-9]{1,6})-?([0-9]{2})-?([0-9])$", cas_string))) {
    cas_string <- stringr::str_replace(cas_string, "^0*(\\d{1,6})-?(\\d{2})-?(\\d)$", "\\1-\\2-\\3")
  }

  if(any(!grepl("^0*(\\d{1,6})-?(\\d{2})-?(\\d)$", cas_string) & !grepl("^[Nn][Uu][Ll][Ll]", cas_string))) {
    stop("Unknown CAS-RN format detected.")
  }

  # check cas check digit
  numeric_cas <- cas_string[!grepl("NULL", cas_string)]

  for(x in unique(numeric_cas)) {

    simple_cas <- stringr::str_replace_all(x, "-", "")
    fun_seq <- as.integer(stringr::str_split_1(simple_cas, ""))
    check_digit <- fun_seq[length(fun_seq)]
    fun_seq <- fun_seq[-length(fun_seq)]
    fun_i <- length(fun_seq):1
    fun_seq_sum <- sum(fun_seq *fun_i)

    if(fun_seq_sum %% 10 != check_digit) {
      stop(paste("False CAS-RN detected.", x, "is not a valid CAS-RN."))
    }

  }

  # format as 10 digits
  cas_string <- ifelse(grepl("NULL", cas_string),
                       cas_string,
                       paste0(strrep("0", 11 - nchar(cas_string)), cas_string))

  # format cas-rn
  if(format == "xx-xx-x") {

    output <- cas_string

  } else if(format == "xxxxx") {

    output <- ifelse(grepl("NULL", cas_string), cas_string, gsub("-", "", cas_string))

  }

  return(output)

}

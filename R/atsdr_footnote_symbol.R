#' Footnote Symbols based on ATSDR Style Manual (2019)
#'
#'@description Produces the proper footnote symbol based on a numeric index or numeric vector.
#'
#' @param i An index according to the symbol order of a footnote. Value of 0 gives the five unique footnote symbols in order of usage in the manual (useful if you just need the symbols themselves). If the index is a vector, a list of selected footnote symbols will be outputted. Default is 0.
#'
#' @return A character object.
#'
#' @export
#'
#' @examples
#' atsdr_footnote_symbol(1)
#' atsdr_footnote_symbol(1:10)
#'
atsdr_footnote_symbol <- function(i = 0) {

  #return output
  output <- c()

  #footnote key
  key_vctr <- c("*", "†", "‡", "§", "¶")

  n <- length(i)

  if(n > 1) { #character vector

    #set list of symbols
    symbol_list <- lapply(1:max(i), function(x) {

      index <- as.numeric(x)-1
      integer <- index%/%5
      remainder <- index%%5
      symbol <- key_vctr[remainder+1]
      vector <- rep(symbol, each = integer+1)
      paste(vector, collapse = "")

    })

    output <- unlist(symbol_list[i])


  } else if(i == 0 & n == 1) { #default

    output <- key_vctr

  } else if(i > 0 & n == 1) { #single character

    index <- as.numeric(i)-1
    integer <- index%/%5
    remainder <- index%%5
    symbol <- key_vctr[remainder+1]
    vector <- rep(symbol, each = integer+1)

    output <- paste(vector, collapse = "")

  } else {

    output <- "Error"

  }

  return(output)

}

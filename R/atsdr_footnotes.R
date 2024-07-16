#Footnotes based on ATSDR Guidance

atsdr_footnotes <- function(i = 0) {

  key_vctr <- c("*", "†", "‡", "§", "¶")

  if(i == 0) {
    print(key_vctr)
  } else {
    index <- as.numeric(i)-1
    integer <- index%/%5
    remainder <- index%%5
    symbol <- key_vctr[remainder+1]
    vector <- rep(symbol, each = integer+1)
    print(paste(vector, collapse = ""))
  }

}

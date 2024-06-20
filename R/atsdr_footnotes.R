

atsdr_footnotes <- function(i = 0) {

  key_vctr <- c("*", "†", "‡", "§", "¶", "**", "††", "‡‡", "§§", "¶¶")

  if(sum(i == 0:10) == 0) {stop("i must be within [0:10]")}

  if(i == 0) {

    print(key_vctr)

  } else {

    print(key_vctr[as.numeric(i)])

  }

}

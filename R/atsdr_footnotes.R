

atsdr_footnotes <- function(i = 0) {

  key_table <- data.frame(
    index = 1:10,
    symbol = c("*", "†", "‡", "§", "¶", "**", "††", "‡‡", "§§", "¶¶")
  )

  if(symbol == 0) {

    (key_table$symbol)

  } else {

    (key_table$symbol[index==i,])

  }

}

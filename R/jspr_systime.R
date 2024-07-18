jspr_systime <- function() {
  string <- Sys.time()

  year <- substr(string, 1, 4)
  month <- substr(string, 6, 7)
  day <- substr(string, 9, 10)

  hour <- substr(string, 12, 13)
  min <- substr(string, 15, 16)
  sec <- substr(string, 18, 19)

  paste0(year, month, day, " ", hour, ":", min, ":", sec)
}

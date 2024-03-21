#' Convert temperature between Celsius, Fahrenheit, and Kelvin
#' @param x numeric vector
#' @param from character vector, one of "C", "F", or "K"
#' @param to character vector, one of "C", "F", or "K"
#' @return numeric vector
#' @export
#' @author Eva Marques 
convert_temp <- function(x, from, to) {
  if (from == "C") {
    ifelse(to == "F",
           return((x * 9 / 5) + 32),
           return(x + 273.15))
  } else if (from == "F") {
    ifelse(to == "C",
           return((x - 32) * 5 / 9),
           return((x - 32) * 5 / 9 + 273.15))
  } else if (from == "K") {
    ifelse(to == "C",
           return(x - 273.15),
           return((x - 273.15) * 9 / 5 + 32))
  }
}
#'Miles to Meters
#'
#'@description
#'This function takes a value in miles and converts it to meters; needed for some mapping functions, which require values in meters
#'@Usage
#'miles2meters(50)
#'
#'@param miles the number of miles to convert to meters

#'@return returns a value in meters

#'@examples
#'miles2meters(50)
#'
#'# Miles to meters conversion
miles2meters <- function(miles) {
  miles * 1609.344
}

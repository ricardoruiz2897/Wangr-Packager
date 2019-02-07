#'Map Circle
#'
#'@description
#'This function takes longitude and latitude coordinates (center) and draws a circle around it
#'@Usage
#'map.circle(center, radius, nPoints= 100)
#'
#'@param center the data frame including longitude and latitude of a reference point
#'@param radius the size of the circle in km but can be converted to miles (50miles = 80.4672 km)
#'@param nPoints= 100 the number of longitude and latitude points used to create the circle

#'@return return a data frame including longtude and latitude points used to create the circle

#'@examples
#'map.circle(center, 31.0686)
#'
#' modified from https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2
map.circle <- function(center, radius, nPoints = 100){
  # center: the data frame of center with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(center$latitude)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3)
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(center$operation.id, each = nPoints))
  angle <- seq(0, 2*pi, length.out = nPoints)

  circleDF$lon <- unlist(lapply(center$longitude, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(center$latitude, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# a different example:  https://gis.stackexchange.com/questions/119736/ggmap-create-circle-symbol-where-radius-represents-distance-miles-or-km

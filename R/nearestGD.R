#' nearest accessible position
#'
#'nearest() calculates the nearest position to the given coordinates which can be accessed by car.
#' The coordinate-standard is WGS84. Attention: The OSRM API v4 is only working locally, but
#' not with the 'OSRM' webserver.
#'
#' @param lat A numeric (-90 < lat < 90)
#' @param lng A numeric (-180 < lng < 180)
#' @param localhost A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#' @param solutions A numeric (number of different solutions)
#'
#' @return A data.frame with lat, lng and distance to the original point
#' @export
#'
#' @examples
#'
#' lat <- 9.382589
#' lng <- 47.428731
#'
#' nearest(lat,lng,F,3)
#'
nearest <- function(lat, lng, localhost, solutions)
{
address <- server_address(localhost)

goal <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/", lng, ",", lat, "?number=",solutions, sep = "", NULL))

nearest <- data.frame(name="a", distance=NA, Lat=NA, Lng=NA,stringsAsFactors = F)

for (i in 1:solutions){
  nearest[i,1] <- data.frame(goal$waypoints[[i]]$name,stringsAsFactors = F)
  nearest[i,2] <- data.frame(goal$waypoints[[i]]$distance)
  nearest[i,3] <- data.frame(goal$waypoints[[i]]$location[2])
  nearest[i,4] <- data.frame(goal$waypoints[[i]]$location[1])
}
return(nearest)
}

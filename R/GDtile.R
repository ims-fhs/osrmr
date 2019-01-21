#' route on a map
#'
#' @param coordinates A character which contains the coordinates "lat1,lng1;lat2,lng2..."
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param n A numeric (the number of coordinates)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return route in a map
#' @export
#'
#' @examples
#'
#' coordinates <- "21.034426,43.3878;9.368092,47.422701"
#' n <- 2
#'
#' tile(coordinates,n,F)
#'
tile <- function(coordinates,n,localhost)
{

  address <- server_address(localhost)

  R.utils::withTimeout({
    repeat {
      route <- try(res <- rjson::fromJSON(file = paste(address, "/route/v1/driving/", coordinates, sep = "", NULL)))
      if (class(route) != "try-error") {
        if (!is.null(route)) {
          break # waytime found
        } else {
          stop("in sim911::osrm_viaroute: calculate nearest necessary?")
        }
      }
    }
  }, timeout = 1, onTimeout = "warning")



#make a string to nicely label the route
s <- route$routes[[1]]$duration
kms <- round(route$routes[[1]]$distance/1000, 1)
routelabel <- paste0(s%/%60, "m ", s%%60, "s , ", kms, "kms")

#create a basic map


library(bitops)
library(sp)

decode <- function(str, multiplier=1e5){

  if (!require(bitops)) stop("Package: bitops required.")
  if (!require(sp)) stop("Package: sp required.")

  truck <- 0
  trucks <- c()
  carriage_q <- 0

  for (i in 0:(nchar(str)-1)){
    ch <- substr(str, (i+1), (i+1))
    x <- as.numeric(charToRaw(ch)) - 63
    x5 <- bitShiftR(bitShiftL(x, 32-5), 32-5)
    truck <- bitOr(truck, bitShiftL(x5, carriage_q))
    carriage_q <- carriage_q + 5
    islast <- bitAnd(x, 32) == 0
    if (islast){
      negative <- bitAnd(truck, 1) == 1
      if (negative) truck <- -bitShiftR(-bitFlip(truck), 1)/multiplier
      else truck <- bitShiftR(truck, 1)/multiplier
      trucks <- c(trucks, truck)
      carriage_q <- 0
      truck <- 0
    }
  }
  lat <- trucks[c(T,F)][-1]
  lng <- trucks[c(F,T)][-1]
  res <- data.frame(lat=c(trucks[1],cumsum(lat)+trucks[1]),
                    lng=c(trucks[2],cumsum(lng)+trucks[2]))

  coordinates(res) <- ~lng+lat
  proj4string(res) <- CRS("+init=epsg:4326")
  return(SpatialLines(list(Lines(Line(res), 1)), CRS("+init=epsg:4326")))
}
path <- decode(route$routes[[1]]$geometry, multiplier=1e5)

library(leaflet)
m <- leaflet(width="100%") %>%
  addTiles()  %>%
  addPolylines(data=path, popup=routelabel, color = "#000000", opacity=1, weight = 3)

  for (i in 1:n){
  m <- addMarkers(m,lng=route$waypoints[[i]]$location[1], lat=route$waypoints[[i]]$location[2], popup=route$waypoints[[i]]$name)
  }

return(m)
}



address <- "http://router.project-osrm.org"
lat1 <- 47.49642
lng1 <- 9.240543
lat2 <- 47.423255
lng2 <- 9.367246
lat3 <- 47.483255
lng3 <- 9.357246

route <- rjson::fromJSON(file = paste(address, "/route/v1/driving/", lng1, ",", lat1,";", lng2, ",", lat2,";", lng3, ",", lat3,"?overview=full", sep = "", NULL))

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
  addPolylines(data=path, popup=routelabel, color = "#000000", opacity=1, weight = 3) %>%
  addMarkers(lng=lng1, lat=lat1, popup=route$waypoints[[1]]$name) %>%
  addMarkers(lng=lng2, lat=lat2, popup=route$waypoints[[2]]$name) %>%
  addMarkers(lng=lng3, lat=lat3, popup=route$waypoints[[3]]$name)
m


#function tile
#
#
#coordinates <- "13.38,52.41;12.4,48.3;13.2,51.3"
#
#n <- 3
#
trip <- function(coordinates, n, localhost){

address <- server_address(localhost)


R.utils::withTimeout({
  repeat {
    goal <- try(trip <- rjson::fromJSON(file = paste(address, "/trip/v1/driving/", coordinates, sep = "", NULL)))
    if (class(goal) != "try-error") {
      if (!is.null(goal)) {
        break # waytime found
      } else {
        stop("in sim911::osrm_viaroute: calculate nearest necessary?")
      }
    }
  }
}, timeout = 1, onTimeout = "warning")


trip <- data.frame(sequence= NA,Lat=NA, Lng=NA, Duration=NA, Disatnce=NA)
for (i in 1:n){
  trip[i,1] <- data.frame(goal$waypoints[[i]]$waypoint_index)
  trip[i,2] <- data.frame(goal$waypoints[[i]]$location[2])
  trip[i,3] <- data.frame(goal$waypoints[[i]]$location[1])
  trip[i,4] <- data.frame(goal$trips[[1]]$legs[[i]]$duration)
  trip[i,5] <- data.frame(goal$trips[[1]]$legs[[i]]$distance)

}

return(trip)
}


#' soves the Traveling Salesman Problem
#'
#'
#'The trip function solves the Traveling Salesman Problem. The returned path does not have to be the fastest path.
#'As TSP is NP-hard it only returns an approximation.
#'
#' @param coordinates A character which contains the coordinates "lat1,lng1;lat2,lng2..."
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param n A numeric (the number of coordinates)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame included the waypoint order, the coordinates, the duration and the distance.
#' @export
#'
#' @examples
#'
#' points <- "9.382589,47.428731;9.240543,47.49642;9.252642,47.412704"
#' number <- 3
#'
#' trip(points,number,F)
#'
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
  rownames(trip)[i] <- goal$waypoints[[i]]$name
}

return(trip)
}


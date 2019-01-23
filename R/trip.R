#' solves the Traveling Salesman Problem
#'
#'
#'The trip function solves the Traveling Salesman Problem. The returned path does not have to be the fastest path.
#'As TSP is NP-hard it only returns an approximation.
#'
#' @param coordinates A date.frame which contains 3 column: id, lat, lng
#' lat and lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A numeric giving the trip-index for every input-coordinates
#' @export
#'
#' @examples
#' coordinates <- data.frame(id=1:3,
#' lat=c(47.4623349064579,47.46229863897051,47.462226103920706),
#' lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#' trip(coordinates,F)
#' trip(coordinates,T)
trip <- function(coordinates, localhost){

  address <- osrmr:::server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/trip/v1/driving/", coordinates_char, sep = "", NULL)
  res <- osrmr:::make_request(request)

  trip_index <- rep(NA,(nrow(coordinates)))
  for (i in 1:nrow(coordinates)){
    trip_index[i] <- res$waypoints[[i]]$waypoint_index
  }
  assertthat::assert_that(!any(is.na(trip_index)))

return(trip_index)
}


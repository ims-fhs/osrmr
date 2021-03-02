#' Find a solution for the TSP (Traveling Salesman Problem)
#'
#' The trip function solves the Traveling Salesman Problem using a greedy heuristic
#' (farthest-insertion algorithm). The returned path does not have to be the fastest
#' path as TSP is NP-hard it is only an approximation. Note that if the input coordinates
#' can not be joined by a single trip (e.g. the coordinates are on several disconnected
#' islands) multiple trips for each connected component are returned.
#' For further details see:
#' http://project-osrm.org/docs/v5.5.1/api/#trip-service
#'
#' @param coordinates A data.frame with 2 columns: lat, lng.
#' lat and lng are coordinates in WGS84-format - numeric.
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#' @param t_max An integer, maximal time in seconds to wait for answer from server -
#' see function osrmr:::make_request()
#'
#' @return A numeric vector giving the trip-index for every input-coordinate
#' @export
#'
#' @examples
#' coordinates <- data.frame(lat=c(47.4623,47.4622,47.4622),lng=c(9.0422,9.0425,9.0429))
#'
#' \dontrun{
#' # example with onlinehost
#' osrmr:::trip(coordinates,F)
#'
#' # example with localhost
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#' osrmr::trip(coordinates,T)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH")}
trip <- function(coordinates, localhost, t_max = 10){

  address <- server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/trip/v1/driving/", coordinates_char, sep = "", NULL)
  res <- make_request(request, t_max = t_max)

  trip_index <- rep(NA,(nrow(coordinates)))
  for (i in 1:nrow(coordinates)){
    trip_index[i] <- res$waypoints[[i]]$waypoint_index
  }
  assertthat::assert_that(!any(is.na(trip_index)))

return(trip_index)
}


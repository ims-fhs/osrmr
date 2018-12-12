#' solves the Traveling Salesman Problem
#'
#'
#'The trip function solves the Traveling Salesman Problem. The returned path does not have to be the fastest path.
#'As TSP is NP-hard it only returns an approximation.
#'
#' @param coordinates A date.frame which contains 3 column: ID, Lat, Lng
#' Lat and Lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame included the waypoint order
#' @export
#'
#' @examples
#'
#' coordinates <- data.frame(ID=1:3,lat=c(47.4623349064579,47.46229863897051,47.462226103920706),lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#' trip(coordinates,F)
#'
trip <- function(coordinates, localhost){

  n <- nrow(coordinates)
  address <- osrmr:::server_address(localhost)

  coordinates_char <- input_address(coordinates)

  code <- paste(address, "/trip/v1/driving/", coordinates_char, sep = "", NULL)

  goal <-  timeout(code)


trip <- data.frame(sequence=numeric())
for (i in 1:n){
  trip[i,1] <- data.frame(goal$waypoints[[i]]$waypoint_index)
}

return(trip)
}


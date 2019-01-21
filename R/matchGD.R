#' GPS points are matched to the road network
#'
#' Map matching matches given GPS points to the road network in the most plausible way.
#' Large jumps in the timestamps (> 60s) or improbable transitions lead to trace splits if a complete matching could not be found.
#' The algorithm might not be able to match all points. Outliers are removed if they can not be matched successfully.
#'
#' @param coordinates A date.frame which contains 3 column: ID, Lat, Lng
#' Lat and Lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame with the new coordinates
#' @export
#'
#' @examples
#'
#' coordinates <- data.frame(ID=1:3,lat=c(47.4623349064579,47.46229863897051,47.462226103920706),lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#'
#' match_service(coordinates,F)
#'
match_service <- function(coordinates, localhost){

  n <- nrow(coordinates)
  address <- osrmr:::server_address(localhost)

  coordinates_char <- input_address(coordinates)

  code <- paste(address, "/match/v1/driving/", coordinates_char, sep = "", NULL)

  goal <-  timeout(code)

solution <- data.frame(Lat=numeric(), Lng=numeric())
for (i in 1:n){
  solution[i,1] <- data.frame(goal$tracepoints[[i]]$location[2])
  solution[i,2] <- data.frame(goal$tracepoints[[i]]$location[1])


}

return(solution)
}




#'duration of the fastest route between all pairs of supplied coordinates
#'
#'For given coordinates, table() calculates route informations using OSRM.
#'OSRM choooses the nearest point which can be accessed by car for the coordinates.
#'The coordinate-standard is WGS84.
#'
#' @param coordinates A date.frame which contains 3 column: ID, Lat, Lng
#' Lat and Lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame with the duration of all pairs of supplied coordinates
#' (ID of the columns is always the start point)
#' @export
#'
#' @examples
#'
#'coordinates <- data.frame(ID=c(12,8,1),lat=c(47.4623349064579,47.46229863897051,47.462226103920706),lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#'table(coordinates,F)
#'
table <- function(coordinates, localhost){

  n <- nrow(coordinates)
  address <- osrmr:::server_address(localhost)

  coordinates_char <- input_address(coordinates)

  code <- paste(address, "/table/v1/driving/", coordinates_char, sep = "", NULL)

  goal <-  timeout(code)


  solution <- data.frame(goal$durations[[1]])
  for (i in 2:n){
    solution[,i] <- data.frame(goal$durations[[i]])
  }
  for (i in 1:n){
  colnames(solution)[i] <- coordinates$ID[i]
  rownames(solution)[i] <- coordinates$ID[i]
  }
  return(solution)

}




#'uration of the fastest route between all pairs of supplied coordinates
#'
#'For given coordinates, table() calculates route informations using OSRM.
#'OSRM choooses the nearest point which can be accessed by car for the coordinates.
#'The coordinate-standard is WGS84.
#'
#'
#' @param coordinates A date.frame which contains 3 column: id, lat, lng
#' Lat and Lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A matrix with the duration of all pairs of supplied coordinates
#' (Entry (i,j) gives the duration from point i to point j)
#' @export
#'
#' @examples
#'
#' coordinates <- data.frame(id=c(12,8,1),
#' lat=c(47.4623349064579,47.46229863897051,47.462226103920706),
#' lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#' coordinates2 <- data.frame(id=c(12,8,1),
#' lat=c(47.4,47.5,47.6),
#' lng=c(9.0,9.1,9.2))
#'
#' table(coordinates,F)
#' table(coordinates,T)
#' table(coordinates2,F)
#' table(coordinates2,T)
table <- function(coordinates, localhost){
  address <- osrmr:::server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/table/v1/driving/", coordinates_char, sep = "", NULL)
  res <- osrmr:::make_request(request)
  result <- matrix(unlist(res$durations), nrow = nrow(coordinates), byrow = T)
  assertthat::assert_that(nrow(result) == ncol(result))
  assertthat::assert_that(nrow(result) == nrow(coordinates))
  rownames(result) <- coordinates$id; colnames(result) <- coordinates$id

  return(result)
}


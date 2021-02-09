#' Calculate a distance matrix for given coordinates
#'
#' Given a set of coordinates, this function uses every coordinate once as start
#' and destination. For further details see:
#' http://project-osrm.org/docs/v5.5.1/api/#table-service
#'
#' @param coordinates A data.frame with 3 columns: id, lat, lng.
#' id can be numeric or a character
#' Lat and lng are coordinates in WGS84-format - numeric.
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
#' coordinates <- data.frame(id=c("z", "b", "c"),
#' lat=c(47.4,47.5,47.6),
#' lng=c(9.04,9.04,9.04))
#' \dontrun{
#' # example with onlinehost
#' osrmr::table(coordinates,T)
#'
#' # example with localhost
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#' osrmr::table(coordinates,T)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH")}
table <- function(coordinates, localhost){
  address <- server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/table/v1/driving/", coordinates_char, sep = "", NULL)
  res <- make_request(request)
  result <- matrix(unlist(res$durations), nrow = nrow(coordinates), byrow = T)
  assertthat::assert_that(nrow(result) == ncol(result))
  assertthat::assert_that(nrow(result) == nrow(coordinates))
  rownames(result) <- coordinates$id; colnames(result) <- coordinates$id

  return(result)
}


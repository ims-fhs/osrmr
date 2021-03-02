#' Match GPS points to the road network in the most plaubislbe way
#'
#' Details see: http://project-osrm.org/docs/v5.5.1/api/#match-service
#' Large jumps in the timestamps (> 60s) or improbable transitions lead to trace
#' splits if a complete matching could not be found. The algorithm might not be
#' able to match all points. Outliers are removed if they can not be matched successfully.
#'
#' @param coordinates A data.frame with 2 columns "lat", "lng", giving a set of WGS84 coordinates.
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#' @param t_max An integer, maximal time in seconds to wait for answer from server -
#' see function osrmr:::make_request()
#'
#' @return A data.frame with the new coordinates
#' @export
#'
#' @examples
#' coordinates <- data.frame(lat=c(47.4623,47.4622,47.4622),
#' lng=c(9.0422,9.0425,9.0429))
#' \dontrun{
#' # example with onlinehost
#' osrmr:::match(coordinates,F)
#'
#' # example with localhost
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#' osrmr::match(coordinates,T)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH")}
match <- function(coordinates, localhost, t_max = 10){

  address <- server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/match/v1/driving/", coordinates_char, sep = "", NULL)
  res <- make_request(request, t_max = t_max)

  result <- data.frame(lat=numeric(), lng=numeric(), stringsAsFactors = FALSE)
  for (i in 1:nrow(coordinates)){
    result[i,"lat"] <- data.frame(res$tracepoints[[i]]$location[2])
    result[i,"lng"] <- data.frame(res$tracepoints[[i]]$location[1])
  }
  assertthat::assert_that(nrow(result) == nrow(coordinates))

return(result)
}




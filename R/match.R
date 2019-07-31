#' GPS points are matched to the road network
#'
#' Map matching matches given GPS points to the road network in the most plausible way.
#' Large jumps in the timestamps (> 60s) or improbable transitions lead to trace splits if a complete matching could not be found.
#' The algorithm might not be able to match all points. Outliers are removed if they can not be matched successfully.
#'
#' @param coordinates A date.frame which contains 3 column: ID, lat, lng
#' lat and lng are coordinates with the following notation:
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame with the new coordinates
#' @export
#'
#' @examples
#' coordinates <- data.frame(id=c(12,8,1),
#' lat=c(47.4623349064579,47.46229863897051,47.462226103920706),
#' lng=c(9.042273759841919,9.042563438415527,9.042906761169434))
#'
#' match(coordinates,F)
#' match(coordinates,T)
match <- function(coordinates, localhost){

  address <- osrmr:::server_address(localhost)
  coordinates_char <- paste(coordinates$lng, coordinates$lat, sep = ",", collapse = ";")
  request <- paste(address, "/match/v1/driving/", coordinates_char, sep = "", NULL)
  res <- osrmr:::make_request(request)

  result <- data.frame(lat=numeric(), lng=numeric(), stringsAsFactors = FALSE)
  for (i in 1:nrow(coordinates)){
    result[i,"lat"] <- data.frame(res$tracepoints[[i]]$location[2])
    result[i,"lng"] <- data.frame(res$tracepoints[[i]]$location[1])
  }
  assertthat::assert_that(nrow(result) == nrow(coordinates))

return(result)
}




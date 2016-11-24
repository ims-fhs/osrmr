#' cacluate the nearest position which can be accessed via car. Be careful: Not
#' all combinations of api_version and localhost are possible
#'
#' @param lat, A number
#' @param lng, A number
#' @param api_version, A character
#' @param localhost, A boolean. T = local server used
#'
#' @return nearest
#' @export
nearest <- function(lat, lng, api_version = 5, localhost = F) {
  address <- server_address(localhost)

  assertthat::assert_that(api_version %in% c(4,5))
  if (api_version == 4) {
    route <- rjson::fromJSON(file = paste(address, "/nearest?loc=",
                                          lat, ",", lng, sep = "", NULL))
  } else if (api_version == 5) {
    route <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                          lng, ",", lat, "?number=1", sep = "", NULL)) # &bearings=0,20
  } else {
    stop("api_version needs to be 4 or 5")
  }
  return(route)
}


#' nearest_api_v4
#'
#' @param lat
#' @param lng
#' @param address
#'
#' @return nearest A data.frame with lat and lng

#' @examples
#' osrmr:::nearest_api_v4(47,9, osrmr:::server_address(T))
#' #        lat      lng
#' # 1 47.02872 9.006633
nearest_api_v4 <- function(lat, lng, address) {
  nearest <- rjson::fromJSON(file = paste(address, "/nearest?loc=",
                                        lat, ",", lng, sep = "", NULL))$mapped_coordinate
  nearest <- data.frame(
    lat = nearest[1],
    lng = nearest[2]
  )
  return(nearest)
}


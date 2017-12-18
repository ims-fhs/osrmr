#' nearest accessible position
#'
#' nearest() cacluates the nearest position to the given coordinates which can be accessed by car.
#' The coordinate-standard is WGS84. Attention: The OSRM API-4 is only working locally, but
#' not with the onlinehost.
#'
#' @param lat, A numeric (-90 < lat < 90)
#' @param lng, A numeric (-180 < lng < 180)
#' @param api_version, A numeric (either 4 or 5)
#' @param localhost, A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#'
#' @return A data.frame with lat and lng
#' @export
#'
#' @examples
#' # osrmr::nearest(47,9, 5, F)
#' #        lat      lng
#' # 1 47.02872 9.006633
#'
#' # osrmr::run_server("C:/OSRM_API5/", "switzerland-latest.osrm")
#' # osrmr:::nearest(47,9, 5, T)
#' #        lat      lng
#' # 1 47.02872 9.006633
#' # osrmr::quit_server()
#'
#' # osrmr::run_server("C:/OSRM_API4/", "switzerland-latest.osrm")
#' # osrmr:::nearest(47,9, 4, T)
#' #        lat      lng
#' # 1 47.02891 9.006577
#' # osrmr::quit_server()
nearest <- function(lat, lng, api_version = 5, localhost = F) {
  assertthat::assert_that(api_version %in% c(4,5))

  address <- server_address(localhost)

  if (api_version == 4) {
    nearest <- nearest_api_v4(lat, lng, address)
  } else {
    nearest <- nearest_api_v5(lat, lng, address)
  }
  return(nearest)
}


#' nearest accessible position for OSRM API 4
#'
#' nearest_api_v4() cacluates the nearest position to the given coordinates which can be accessed
#' by car with the OSRM API 4. The coordinate-standard is WGS84. Attention: The OSRM API-4
#' is only working locally, but not with the onlinehost.
#'
#' @param lat, A numeric (-90 < lat < 90)
#' @param lng, A numeric (-180 < lng < 180)
#' @param address, A character specifying the serveraddress (local or online)
#'
#' @return A data.frame with lat and lng
#'
#' @examples
#' # osrmr::run_server("C:/OSRM_API4/", "switzerland-latest.osrm")
#' # osrmr:::nearest_api_v4(47,9, osrmr:::server_address(T))
#' #        lat      lng
#' # 1 47.02891 9.006577
#' # osrmr::quit_server()
nearest_api_v4 <- function(lat, lng, address) {
  nearest <- rjson::fromJSON(file = paste(address, "/nearest?loc=",
                                        lat, ",", lng, sep = "", NULL))$mapped_coordinate
  nearest <- data.frame(
    lat = nearest[1],
    lng = nearest[2]
  )
  return(nearest)
}

#' nearest accessible position for OSRM API 5
#'
#' nearest_api_v5() cacluates the nearest position to the given coordinates which can be accessed
#' by car with the OSRM API 5. The coordinate-standard is WGS84.
#'
#' @param lat, A numeric (-90 < lat < 90)
#' @param lng, A numeric (-180 < lng < 180)
#' @param address, A character specifying the serveraddress (local or online)
#'
#' @return A data.frame with lat and lng

#' @examples
#' # osrmr::run_server("C:/OSRM_API5/", "switzerland-latest.osrm")
#' # osrmr:::nearest_api_v5(47,9, osrmr:::server_address(T))
#' #        lat      lng
#' # 1 47.02872 9.006633
#' # osrmr:::nearest_api_v5(47,9, osrmr:::server_address(F))
#' # lat      lng
#' # 1 47.02872 9.006633
#' # osrmr::quit_server()
nearest_api_v5 <- function(lat, lng, address) {
  nearest <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                          lng, ",", lat, "?number=1", sep = "", NULL))$waypoints[[1]]$location
  nearest <- data.frame(
    lat = nearest[2],
    lng = nearest[1]
  )
  return(nearest)
}



#' nearest accessible position
#'
#' nearest2() calculates the nearest position to the given coordinates which can be accessed by car.
#' The coordinate-standard is WGS84.
#'
#' @param lat, A numeric (-90 < lat < 90)
#' @param lng, A numeric (-180 < lng < 180)
#' @param localhost, A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#'
#' @return A data.frame with lat and lng
#' @export
#'
#' @examples
#' \dontrun{
#' osrmr::nearest(47,9, FALSE)
#'
#' Sys.setenv("OSRM_PATH_API_5"="C:/OSRM_API5")
#' osrmr::run_server(Sys.getenv("OSRM_PATH_API_5"), "switzerland-latest.osrm")
#' osrmr::nearest(47,9, TRUE)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH_API_5")
#'}
nearest2 <- function(lat, lng, localhost = F, timeout = 0.001) {

  address <- server_address(localhost)
  Sys.sleep(timeout)

  nearest <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                          lng, ",", lat, "?number=1", sep = "", NULL))$waypoints[[1]]$location
  nearest <- data.frame(
    lat = nearest[2],
    lng = nearest[1]
  )

  return(nearest)
}

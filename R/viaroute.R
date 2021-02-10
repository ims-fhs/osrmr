#' Calculate travel time or full information of a route
#'
#' For a given start- and end-destination, route() calculates route informations
#' using OSRM. OSRM chooses the nearest point which can be accessed by car for
#' the start- and end-destination. The coordinate-standard is WGS84.
#'
#' @param lat1 A numeric (-90 < lat1 < 90) -> start-destination
#' @param lng1 A numeric (-180 < lng1 < 180) -> start-destination
#' @param lat2 A numeric (-90 < lat2 < 90) -> end-destination
#' @param lng2 A numeric (-180 < lng2 < 180) -> end-destination
#' @param instructions A logical. If FALSE, only the traveltime (in seconds,
#' as numeric) will be returned.
#'  If TRUE, the whole route is returned as list
#' @param localhost A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#' @param timeout A numeric indicating the timeout between server requests (in order to prevent queue overflows). Default is 0.001s.
#' @param t_max An integer, maximal time in seconds to wait for answer from server -
#' see function osrmr:::make_request()
#'
#' @return a numeric or a list (depending on instructions)
#' @export
#'
#' @examples
#' # direct examples of the online API
#' \dontrun{
#' link <- "http://router.project-osrm.org/route/v1/driving/8.1,47.1;8.3,46.9?steps=false"
#' a <- rjson::fromJSON(file = link)
#'
#' # example with onlinehost
#' osrmr::viaroute(47.1, 8.1, 46.9, 8.3, FALSE, FALSE)
#' osrmr::viaroute(47.1, 8.1, 46.9, 8.3, FALSE, TRUE)
#'
#' # example with localhost
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#' osrmr::viaroute(47.1, 8.1, 46.9, 8.3, TRUE, FALSE)
#' osrmr::viaroute(47.1, 8.1, 46.9, 8.3, TRUE, TRUE)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH")}
viaroute <- function(lat1, lng1, lat2, lng2, localhost, instructions = FALSE, timeout = 0.001,
                     t_max = 1) {
  address <- server_address(localhost)
  Sys.sleep(timeout)

  if (!instructions) {
    request <- paste(address, "/route/v1/driving/", lng1, ",", lat1, ";", lng2, ",", lat2,
                     "?overview=false", sep = "", NULL)
  } else {
    request <- paste(address, "/route/v1/driving/", lng1, ",", lat1, ";", lng2, ",", lat2,
                     "?overview=full", sep = "", NULL)
  }

  res <- make_request(request, t_max = t_max)

  if (!instructions) {
    if (res$code == "Ok") {
      assertthat::assert_that(assertthat::is.number(res$routes[[1]]$duration))
      return(res$routes[[1]]$duration)
    } else {
      t_guess <- 16*60  #.............. This is really bad. Calculate time at speed 20 km/h
      warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "),
              ". Travel time set to ", t_guess/60 , " min.")
    }
  } else {
    return(res)
  }
}

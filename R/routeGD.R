#' travel time or full information of a route
#'
#' For a given start- and end-destination, route() calculates route informations using OSRM.
#' OSRM chooses the nearest point which can be accessed by car for the start- and end-destination.
#' The coordinate-standard is WGS84.
#'
#' @param lat1 A numeric (-90 < lat1 < 90) -> start-destination
#' @param lng1 A numeric (-180 < lng1 < 180) -> start-destination
#' @param lat2 A numeric (-90 < lat2 < 90) -> end-destination
#' @param lng2 A numeric (-180 < lng2 < 180) -> end-destination
#' @param return.as A character. If "time", only the traveltime (in seconds, as numeric) will be returned.
#'  If "geometry", the geometry of the route is returned (as character).
#' @param localhost A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#'
#' @return a numeric or a character (depending on return.as)
#' @export
#'
#' @examples
#' # direct examples of the online API
#' \dontrun{
#' #' link <- "http://router.project-osrm.org/route/v1/driving/8.1,47.1;8.3,46.9?steps=false"
#' a <- rjson::fromJSON(file = link)
#'
#' # example with onlinehost
#' osrmr:::viaroute(47.1, 8.1, 46.9, 8.3,"time", FALSE)
#'
#' # examples with localhost
#'
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#' osrmr::viaroute(47.1, 8.1, 46.9, 8.3,"time" , TRUE)
#' osrmr::quit_server()
#' Sys.unsetenv("OSRM_PATH")}
route <- function(lat1, lng1, lat2, lng2, return.as = c("time", "geometry"), localhost, timeout = 0.001) {
  assertthat::assert_that(return.as %in% c("time","geometry"))
  address <- osrmr:::server_address(localhost)

  Sys.sleep(timeout)

  if (return.as=="time") {
    request <- paste(address, "/route/v1/driving/",
                     lng1, ",", lat1, ";", lng2, ",", lat2,
                     "?overview=false", sep = "", NULL)
  } else {
    request <- paste(address, "/route/v1/driving/",
                     lng1, ",", lat1, ";", lng2, ",", lat2,
                     "?overview=full", sep = "", NULL)
  }
  R.utils::withTimeout({
    repeat {
      res <- try(
        route <- rjson::fromJSON(
          file = request))
      if (class(res) != "try-error") {
        if (!is.null(res)) {
          break # waytime found
        } else {
          stop("in sim911::osrm_viaroute: calculate nearest necessary?")
        }
      }
    }
  }, timeout = 1, onTimeout = "warning")

  assertthat::assert_that(assertthat::is.number(res$routes[[1]]$duration))

  if (return.as=="time") {
    if (res$code == "Ok") {
      return(res$routes[[1]]$duration)
    } else {
      t_guess <- 16*60
      warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "),
              ". Travel time set to ", t_guess/60 , " min.")
    }
  } else {
    return(res$routes[[1]]$geometry)
  }
}

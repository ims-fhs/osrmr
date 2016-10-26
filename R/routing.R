#' Calculate route from source to destination (both given in WGS84). osrm chooses
#' the nearest point which can be accessed with a car. Be careful: Not all
#' combinations of api_version and localhost are possible
#'
#' @param lat1, A number
#' @param lng1, A number
#' @param lat2, A number
#' @param lng2, A number
#' @param instructions, A boolean for API V5: T = details are returned
#' @param api_version, A character
#' @param localhost, A boolean. T = local server used
#'
#' @return route
#' @export
viaroute <- function(lat1, lng1, lat2, lng2, instructions, api_version, localhost) {
  message(paste0("Use OSRM for ", paste(lat1, lng1, lat2, lng2, collapse = ", ")))
  if (!localhost) {
    address <- "http://router.project-osrm.org"
  } else {
    address <- "http://localhost:5000"
  }

  if (api_version == 4) {
    R.utils::evalWithTimeout({
      repeat{
        res <- try(
          route <- rjson::fromJSON(
            file = paste(address, "/viaroute?loc=",
                         lat1, ',', lng1, '&loc=', lat2, ',', lng2, sep = "", collapse = NULL)))
        # Handle error from try:
        if (class(res) != "try-error") {
          if (!is.null(res)) {
            break # waytime found
          } else {
            stop("in osrm::viaroute: calculate nearest necessary?")
          }
        }
      }
    }, timeout = 1, onTimeout = "warning")

    if (!instructions) {
      if (!res$status == 207) {
        return(res$route_summary$total_time)
      } else {
        warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "))
        return(3*60) # Guess a short walk of 2 minutes.
      }
    } else {
      return(res)
    }
    # return(res$route_summary$total_time)
  } else if (api_version == 5) {
    R.utils::evalWithTimeout({
      repeat {
        res <- try(
          # if (instructions) { # if not necessary => always the same. Only return different?
          #   route <- rjson::fromJSON(
          #     file = paste(address, "/route/v1/driving/",
          #                  lng1, ",", lat1, ";", lng2, ",", lat2,
          #                  "?overview=full", sep = "", NULL))
          # } else {
          route <- rjson::fromJSON(
            file = paste(address, "/route/v1/driving/",
                         lng1, ",", lat1, ";", lng2, ",", lat2,
                         "?overview=false", sep = "", NULL)))
          # })
        # Handle error from try:
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
    if (!instructions) {
      if (!res$status == "207") {
        return(res$routes[[1]]$duration)
      } else {
        warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "))
        return(3*60) # Guess a short walk of 2 minutes.
      }
    } else {
      return(res)
    }
  } else {
    stop("api_version needs to be 4 or 5")
  }
}


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
nearest <- function(lat, lng, api_version, localhost) {
  if (!localhost) {
    address <- "http://router.project-osrm.org"
  } else {
    address <- "http://localhost:5000"
  }

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


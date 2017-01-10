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
#'
#' @examples
#' "http://router.project-osrm.org/route/v1/driving/8.1,47.1;8.3,46.9?steps=true"
#' osrmr:::viaroute(47.1, 8.1, 46.9, 8.3, F, 5, F)
#' osrmr::run_server("C:/OSRM/", "windows")
#' osrmr:::viaroute(47.1, 8.1, 46.9, 8.3, F, 4, T) # [1] 3341
#' osrmr::quit_server()
viaroute <- function(lat1, lng1, lat2, lng2, instructions, api_version, localhost) {
  assertthat::assert_that(api_version %in% c(4,5))

  message(paste0("Use OSRM for ", paste(lat1, lng1, lat2, lng2, collapse = ", "))) # SCN/SQC: move api v4 and api v5 versions into different subroutines ..............
  address <- server_address(localhost)

  if (api_version == 4) {
    viaroute_api_v4(lat1, lng1, lat2, lng2, instructions, address)
  } else {
    viaroute_api_v5(lat1, lng1, lat2, lng2, instructions, address)
  }
}


#' viaroute_api_v4
#'
#' @param lat1
#' @param lng1
#' @param lat2
#' @param lng2
#' @param instructions
#' @param address
#'
#' @return
#'
#' @examples
#' osrmr:::viaroute_api_v4(47,9,48,10,F, osrmr:::server_address(T))
#' # [1] 8250
viaroute_api_v4 <- function(lat1, lng1, lat2, lng2, instructions, address) {
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
}



#' viaroute_api_v5
#'
#' @param lat1
#' @param lng1
#' @param lat2
#' @param lng2
#' @param instructions
#' @param address
#'
#' @return
#'
#' @examples
#' osrmr:::viaroute_api_v5(47,9,48,10,F, osrmr:::server_address(F))
#' # [1] 8828.1
viaroute_api_v5 <- function(lat1, lng1, lat2, lng2, instructions, address) {
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
    if (res$code == "Ok") {
      return(res$routes[[1]]$duration)
    } else {
      warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "))
      return(3*60) # Guess a short walk of 2 minutes.
    }
  } else {
    return(res)
  }
}

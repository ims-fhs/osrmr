#route function
#
#
#
#
#
#
route<- function(lat1, lng1, lat2, lng2, localhost, instructions) {

   address <- server_address(localhost)

  if (!instructions) {
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

  if (!instructions) {
    if (res$code == "Ok") {
      return(res$routes[[1]]$duration)
    } else {
      t_guess <- 16*60
      warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "),
              ". Travel time set to ", t_guess/60 , " min.")
    }
  } else {

    return(res)
  }
}

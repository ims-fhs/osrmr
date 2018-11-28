#' travel time or full information of a route
#'
#'For a given start- and end-destination, viaroute() calculates route informations using OSRM.
#' OSRM chooses the nearest point which can be accessed by car for the start- and end-destination.
#' The coordinate-standard is WGS84.
#'
#'
#' @param coordinates A character which contains the coordinates "lat1,lng1;lat2,lng2..."
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param localhost A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#' @param instructions A logical. If FALSE, only the traveltime (in seconds, as data.frame) will be returned.
#'  If TRUE, more details of the route are returned (as list).
#'
#' @return a data.frame or a list (depending on instructions)
#' @export
#'
#' @examples
#'
#' coordinates <- "47.4623349064579,9.042273759841919;47.46229863897051,9.042563438415527;47.462226103920706,9.042906761169434;47.46213751232282,9.043614864349365"
#' n <- 4
#'
#' route(coordinates,n,F)
#'
route<- function(coordinates, localhost, instructions) {

   address <- server_address(localhost)

  if (!instructions) {
    request <- paste(address, "/route/v1/driving/",
                     "coordinates",
                     "?overview=false", sep = "", NULL)
  } else {
    request <- paste(address, "/route/v1/driving/",
                     "coordinates",
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


      route_service <- data.frame(Lat=NA, Lng=NA, Location=NA, Duration=NA, Disatnce=NA,stringsAsFactors = F)
      route_service[1,] <- c(res$waypoints[[1]]$location[2],res$waypoints[[1]]$location[1],res$waypoints[[1]]$name,res$routes[[1]]$duration,res$routes[[1]]$distance)
      route_service[2,] <- c(res$waypoints[[n]]$location[2],res$waypoints[[n]]$location[1],res$waypoints[[n]]$name,NA,NA)

      return(route_service)


    } else {
      t_guess <- 16*60
      warning("Route not found: ", paste(lat1, lng1, lat2, lng2, collapse = ", "),
              ". Travel time set to ", t_guess/60 , " min.")
    }
  } else {

    a <- n-1
    solution <- data.frame(Lat=NA, Lng=NA, Duration=NA, Disatnce=NA)
    for (i in 1:a){
      solution[i,1] <- data.frame(res$waypoints[[i]]$location[2])
      solution[i,2] <- data.frame(res$waypoints[[i]]$location[1])
      solution[i,3] <- data.frame(res$routes[[1]]$legs[[i]]$duration)
      solution[i,4] <- data.frame(res$routes[[1]]$legs[[i]]$distance)

    }
    solution[n,] <- (c(res$waypoints[[n]]$location[2],res$waypoints[[n]]$location[1],NA,NA))

    route_service <- data.frame(Lat=NA, Lng=NA, Location=NA, Duration=NA, Disatnce=NA,stringsAsFactors = F)
    route_service[1,] <- c(res$waypoints[[1]]$location[2],res$waypoints[[1]]$location[1],res$waypoints[[1]]$name,res$routes[[1]]$duration,res$routes[[1]]$distance)
    route_service[2,] <- c(res$waypoints[[n]]$location[2],res$waypoints[[n]]$location[1],res$waypoints[[n]]$name,NA,NA)

    geometry <- data.frame(geometry=res$routes[[1]]$geometry)

    return(list(solution, route_service,geometry))

  }
}

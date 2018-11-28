#' nearest accessible position
#'
#' nearest() calculates the nearest position to the given coordinates which can be accessed by car.
#' The coordinate-standard is WGS84. Attention: The OSRM API v4 is only working locally, but
#' not with the 'OSRM' webserver.
#'
#' @param lng,      A numeric (-90 < lat < 90)
#' @param lat,      A numeric (-180 < lng < 180)
#' @param address,  A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#' @param n,        Number of coordination points
#'
#' @return          A data.frame with lng/lat
#' @export
#'
#' @examples
#' osrmr:::server_address(TRUE)
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#'
#' address <- "http://localhost:5000"
#' lng     <- 9.382589
#' lat     <- 47.428731
#' n       <- 2
#'
#' nearest_local(lng,lat, address, n)
#'

  nearest_local <- function(lng,lat,address,n){
    nearest <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                            lng, ",", lat, "?number=",
                                            number, sep = "", NULL))

    df <-   data.frame(Street=NA, lng=NA, lat=NA, stringsAsFactors = FALSE)

    for(i in 1:n){
      df[i,1] <- nearest$waypoints[[i]]$name
      df[i,2] <- nearest$waypoints[[i]]$location[[1]]
      df[i,3] <- nearest$waypoints[[i]]$location[[2]]

    }
    return(df)
  }


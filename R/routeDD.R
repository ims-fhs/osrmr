#' travel time or full information of a route
#'
#' For a given start- and end-destination, viaroute() calculates route informations using OSRM.
#' OSRM chooses the nearest point which can be accessed by car for the start- and end-destination.
#' The coordinate-standard is WGS84.
#' Attention: The OSRM API-4 is only working locally, but not with the onlinehost.
#'
#' @param lng1      A numeric (-180 < lng < 180)
#' @param lat1      A numeric (-90 < lat < 90)
#' @param lng2      A numeric (-180 < lng < 180)
#' @param lat2      A numeric (-90 < lat < 90)
#' @param address   A character specifying the serveraddress (local or online)
#' @param n         Number of coordination points
#'
#' @return          A data.frame with the name of the streets and the total of the duration
#' @export
#'
#' @examples
#'
#' osrmr:::server_address(TRUE)
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#'
#' address <- "http://localhost:5000"
#' lng1 <- 9.382589
#' lat1 <- 47.428731
#' lng2 <- 9.240543
#' lat2 <- 47.49642
#' n <- 2
#'
#' request_local(lng1, lat1, lng2, lat2, address, n)
#'
request_local <- function(lng1, lat1, lng2, lat2, address, n){

request <- rjson::fromJSON(file = paste(address, "/route/v1/driving/", lng1, ",", lat1, ";", lng2, ",", lat2, "?overview=false", sep = "", NULL))

df1 <- data.frame(Streets=NA, Duration_total=NA, stringsAsFactors = FALSE )

for(i in 1:n) {

  df1[i,1] <- request$waypoints[[i]]$name

}

df1[1,2] <- request$routes[[1]]$duration

df1

return(df1)
}


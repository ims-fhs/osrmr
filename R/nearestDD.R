#' nearest accessible position
#'
#' nearest() calculates the nearest position to the given coordinates which can be accessed by car.
#' The coordinate-standard is WGS84. Attention: The OSRM API v4 is only working locally, but
#' not with the 'OSRM' webserver.
#'
#' @param lng,      A numeric (-90 < lat < 90)
#' @param lat,      A numeric (-180 < lng < 180)
#' @param address,  A logical (TRUE = localhost is used, FALSE = onlinehost is used)
#' @param number,   Number of coordination points
#'
#' @return          A data.frame with lng/lat
#' @export
#'
#' @examples
#'
#'   lng     <- 9.382589
#'   lat     <- 47.428731
#'   address <- "http://localhost:5000"
#'   number  <- 2
#'
#' nearest_local(lng,lat, address, number)
#'
nearest_local <- function(lng, lat,address,number) {

  osrmr:::server_address(TRUE)

  Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
  osrmr::run_server("switzerland-latest.osrm")


  nearest <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                          lng, ",", lat, "?number=", number,
                                          sep = "", NULL))



  for(i in 1:number){

    df <-   data.frame(nearest$waypoints[[i]]$name, nearest$waypoints[[i]]$location)
    colnames(df) <- c("Street","lng/lat")

    print(df)
  }
  return(print(df))
}










osrmr:::server_address(TRUE)

Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
osrmr::run_server("switzerland-latest.osrm")
lng     <- 9.382589
lat     <- 47.428731
address <- "http://localhost:5000"
n       <- 2

nearest_local <- function(lng,lat,address,n)
nearest <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                        lng, ",", lat, "?number=",
                                        number, sep = "", NULL))

df <-   data.frame(Street=NA, lng=NA, lat=NA, stringsAsFactors = FALSE)

for(i in 1:n){
df[i,1] <- nearest$waypoints[[i]]$name
df[i,2] <- nearest$waypoints[[i]]$location[[1]]
df[i,3] <- nearest$waypoints[[i]]$location[[2]]

}
df

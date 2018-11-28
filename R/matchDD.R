#' Map matching matches/snaps given GPS points to the road network in the most plausible way.
#' Please note the request might result multiple sub-traces.
#' Large jumps in the timestamps (> 60s) or improbable transitions lead to trace splits if a complete matching
#' could not be found. The algorithm might not be able to match all points.
#' Outliers are removed if they can not be matched successfully.
#'
#' @param coordinates A nummeric with lng and lat, separated with 'lng1,lat1;lng2,lat2'
#' @param address     A character specifying the serveraddress (local or online)
#' @param n           Number of coordination points
#'
#' @return            A data.frame with the name of the Street and waypoint. Fields with NA represents the streets,
#'                    that the system didn't found
#' @export
#'
#' @examples
#'
#' osrmr:::server_address(TRUE)
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#'
#' address <- "http://localhost:5000"
#' coordinates <- "9.240543,47.49642;9.36725,47.423248;9.367511,47.423327;9.367394,47.423563"
#' n <- 4
#'
#' match_local(coordinates, address, n)
#'
match_local <- function(coordinates, address, n){
match <- rjson::fromJSON(file = paste(address, "/match/v1/driving/", coordinates, sep = "", NULL))

df.1 <- data.frame(Street=NA, Waypoint=NA, stringsAsFactors = FALSE)

for(i in 1:n){
if(length(match$tracepoints[[i]]$name) == 0){

  print("Not Found")

} else {


  df.1[i,1] <- match$tracepoints[[i]]$name
  df.1[i,2] <- match$tracepoints[[i]]$waypoint_index

}
}

df.1

return(df.1)
}


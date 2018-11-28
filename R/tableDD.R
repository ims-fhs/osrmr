#' Computes the duration of the fastest route between all pairs of supplied coordinates
#'
#' @param coordinates A nummeric with lng and lat, separated with 'lng1,lat1;lng2,lat2'
#' @param address     A character specifying the serveraddress (local or online)
#' @param n           Number of coordination points
#'
#' @return            A list with data.frames
#' @export
#'
#' @examples
#' osrmr:::server_address(TRUE)
#' Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
#' osrmr::run_server("switzerland-latest.osrm")
#'
#' coordinates  <- "9.382589,47.428731;9.240543,47.49642;9.252642,47.412704"
#' n            <- 3
#' address      <- "http://localhost:5000"
#'
#' fastest_local(coordinates, address, n)
#'
fastest_local <- function(coordinates, address, n){

fastest <- rjson::fromJSON(file = paste(address, "/table/v1/driving/",
                                        coordinates, sep = "", NULL))


df <- list()
for (i in 1:n){
  for (j in 1:n){

df[[i]] <- data.frame(Startpunkt=NA, Endpunkt=NA,Duration=NA, stringsAsFactors = FALSE)
df[[i]][j,] <- NA

}}
for (i in 1:n){

  df[[i]][1] <- fastest$sources[[i]]$name
}

for (i in 1:n){
  for (j in 1:n){

  df[[i]][j,2] <- fastest$destinations[[j]]$name
  }
}

for (i in 1:n){

  df[[i]][,3] <- c(fastest$durations[[i]])
}
df

return(df)
}

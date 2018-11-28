#' GPS points are matched to the road network
#'
#' Map matching matches given GPS points to the road network in the most plausible way.
#' Large jumps in the timestamps (> 60s) or improbable transitions lead to trace splits if a complete matching could not be found.
#' The algorithm might not be able to match all points. Outliers are removed if they can not be matched successfully.
#'
#' @param coordinates A character which contains the coordinates "lat1,lng1;lat2,lng2..."
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param n A numeric (the number of coordinates)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A list with two data.frames incl. all the information
#' @export
#'
#' @examples
#'
#' coordinates <- "47.4623349064579,9.042273759841919;47.46229863897051,9.042563438415527;47.462226103920706,9.042906761169434;47.46213751232282,9.043614864349365"
#' n <- 4
#'
#' match_service(coordinates,n,F)
#'
match_service <- function(coordinates, n, localhost){

  address <- server_address(localhost)

R.utils::withTimeout({
  repeat {
    goal <- try(
      match_service <- rjson::fromJSON(file = paste(address, "/match/v1/driving/", "coordinates", sep = "", NULL)))

    if (class(goal) != "try-error") {
      if (!is.null(goal)) {
        break # waytime found
      } else {
        stop("in sim911::osrm_viaroute: calculate nearest necessary?")
      }
    }
  }
}, timeout = 1, onTimeout = "warning")

a <- n-1
solution <- data.frame(Lat=NA, Lng=NA, Duration=NA, Disatnce=NA)
for (i in 1:a){
  solution[i,1] <- data.frame(goal$tracepoints[[i]]$location[2])
  solution[i,2] <- data.frame(goal$tracepoints[[i]]$location[1])
  solution[i,3] <- data.frame(goal$matchings[[1]]$legs[[i]]$duration)
  solution[i,4] <- data.frame(goal$matchings[[1]]$legs[[i]]$distance)

}
solution[n,] <- (c(goal$tracepoints[[n]]$location[2],goal$tracepoints[[n]]$location[1],NA,NA))

match_service <- data.frame(Lat=NA, Lng=NA, Location=NA, Duration=NA, Disatnce=NA,stringsAsFactors = F)
match_service[1,] <- c(goal$tracepoints[[1]]$location[2],goal$tracepoints[[1]]$location[1],goal$tracepoints[[1]]$name,goal$matchings[[1]]$duration,goal$matchings[[1]]$distance)
match_service[2,] <- c(goal$tracepoints[[n]]$location[2],goal$tracepoints[[n]]$location[1],goal$tracepoints[[n]]$name,NA,NA)

return(list(solution, match_service))

}

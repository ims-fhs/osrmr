#function match
#
#
#PROBLEM MIT KOORDINATEN, WELCHE NICHT UNTER 60 SEKUNDEN SIND!!!
#
#
#
#lat1 <- 47.4623349064579
#lng1 <- 9.042273759841919
#lat2 <- 47.46229863897051
#lng2 <- 9.042563438415527
#lat3 <- 47.462226103920706
#lng3 <- 9.042906761169434
#lat4 <- 47.46213751232282
#lng4 <- 9.043614864349365
#
#n <- 4
#
#coordinates <- "23.432,432;43,34;......"
# lng1 etc. müssten in rjson ersetzt werden, damit mehr Koordinaten eingeführt werden können

match_service <- function(lat1, lng1, lat2, lng2, lat3, lng3, lat4, lng4, n, localhost){

  address <- server_address(localhost)

R.utils::withTimeout({
  repeat {
    goal <- try(
      match_service <- rjson::fromJSON(file = paste(address, "/match/v1/driving/", lng1, ",", lat1,";", lng2, ",", lat2,";",lng3, ",", lat3,";",lng4, ",", lat4, sep = "", NULL)))

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

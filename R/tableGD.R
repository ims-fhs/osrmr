#tabel function
#
#coordinates <- "13.38,52.41;12.4,48.3;13.2,51.3"
#n <- 3
#
#
#
table <- function(coordinates, n, localhost){


  address <- server_address(localhost)

  R.utils::withTimeout({
    repeat {
      goal <- try(table <- rjson::fromJSON(file = paste(address, "/table/v1/driving/", coordinates, sep = "", NULL)))
      if (class(goal) != "try-error") {
        if (!is.null(goal)) {
          break # waytime found
        } else {
          stop("in sim911::osrm_viaroute: calculate nearest necessary?")
        }
      }
    }
  }, timeout = 1, onTimeout = "warning")



  solution <- data.frame(t(goal$durations[[1]]))
  for (i in 2:n){
    solution[i,] <- data.frame(t(goal$durations[[i]]))
  }
  return(solution)

}




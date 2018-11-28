#'duration of the fastest route between all pairs of supplied coordinates
#'
#'For given coordinates, table() calculates route informations using OSRM.
#'OSRM choooses the nearest point which can be accessed by car for the coordinates.
#'The coordinate-standard is WGS84.
#'
#' @param coordinates A character which contains the coordinates "lat1,lng1;lat2,lng2..."
#' (-90 < lat < 90)
#' (-180 < lng <180)
#' @param n A numeric (the number of coordinates)
#' @param localhost A logical(TRUE-localhost is used, FALSE=onlinehost is used)
#'
#' @return A data.frame with the duration of all pairs of supplied coordinates
#' @export
#'
#' @examples
#'
#' points <- "9.382589,47.428731;9.240543,47.49642;9.252642,47.412704"
#' number <- 3
#'
#' table(points,number,F)
#'
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



  solution <- data.frame(goal$durations[[1]])
  for (i in 2:n){
    solution[,i] <- data.frame(goal$durations[[i]])
  }
  for (i in 1:n){
  colnames(solution)[i] <- paste("START",goal$sources[[i]]$name, sep=" ",NULL)
  rownames(solution)[i] <- paste("END",goal$sources[[i]]$name, sep=" ",NULL)
  }
  return(solution)

}




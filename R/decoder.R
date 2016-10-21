#' Transform encoded polylines to lat-lng list. Code from
#' http://stackoverflow.com/questions/32476218/how-to-decode-encoded-polylines-
#' from-osrm-and-plotting-route-geometry
#'
#' @param encoded
#'
#' @return data.frame with lat and lng
#' @export
decode_geom <- function(encoded) {
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0

  while (index <= len) {
    shift <- result <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;

    shift <- result <- b <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * 1e-6, lng = lng * 1e-6)
    df.index <- df.index + 1
  }

  geometry <- data.frame(array[1:df.index - 1,])
  names(geometry) <- c("lat", "lng")
  geometry$lat <- 10 * geometry$lat # .......................................... ???
  geometry$lng <- 10 * geometry$lng
  return(geometry)
}


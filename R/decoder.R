#' Transform encoded polylines to lat-lng data.frame.
#'
#' decode_geom() uses a decoding algorithm to decode polylines. (http://stackoverflow.com/questions/32476218/how-
#' to-decode-encoded-polylines-from-osrm-and-plotting-route-geometry)
#'
#' @param encoded A character containing encoded polylines
#' @param api_version A numeric (either 4 or 5) to specify the OSRM API version
#'
#' @return data.frame with lat and lng
#' @export
#'
#' @examples
#' encoded_string_api_4 <- osrmr::encoded_string_api_4
#' encoded_string_api_5 <- osrmr::encoded_string_api_5
#' decoded_api_4 <- decode_geom(encoded_string_api_4, api_version = 4)
#' decoded_api_5 <- decode_geom(encoded_string_api_5, api_version = 5)
#' decoded_api_4[1:3,]
#' #        lat     lng
#' # 1 47.10020 8.09970
#' # 2 47.09850 8.09207
#' # 3 47.09617 8.09118
#' decoded_api_5[1:3,]
#' #        lat      lng
#' # 1 47.10020 8.099703
#' # 2 47.09850 8.092074
#' # 3 47.09617 8.091181
#' assertthat::assert_that(all.equal(decoded_api_4, decoded_api_5, tolerance = 1e-6))
decode_geom <- function(encoded, api_version) {
  if (api_version == 4) {
    scale <- 1e-5
  } else if (api_version == 5) {
    scale <- 1e-6
  }
  len = stringr::str_length(encoded)
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
      result = bitops::bitOr(result, bitops::bitShiftL(bitops::bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlat = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lat = lat + dlat;

    shift <- result <- b <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitops::bitOr(result, bitops::bitShiftL(bitops::bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlng = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * scale, lng = lng * scale)
    df.index <- df.index + 1
  }

  geometry <- data.frame(array[1:df.index - 1,])
  names(geometry) <- c("lat", "lng")
  return(geometry)
}


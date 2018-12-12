#' Data.frame in character
#'
#' @param coordinates
#'
#' @return chracter
#' @export
#'
#' @examples
#'
input_address <- function(coordinates){


end <- character()
first <- paste(coordinates$lng[1],coordinates$lat[1],sep=",",collapse = NULL)
end <- paste0(end,first,collapse = NULL)
for (i in 2:nrow(coordinates)){

  first <- paste(coordinates$lng[i],coordinates$lat[i],sep=",",collapse = NULL)
  end <- paste(end,first,sep=";",collapse = NULL)
}
return(end)
}

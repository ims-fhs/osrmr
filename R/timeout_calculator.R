#' timeout calculator
#'
#' @param code
#'
#' @return goal
#' @export
#'
#' @examples
#'
timeout <- function(code){

R.utils::withTimeout({
  repeat {
    goal <- try(
      abc <- rjson::fromJSON(file = code))

    if (class(goal) != "try-error") {
      if (!is.null(goal)) {
        break # waytime found
      } else {
        stop("in sim911::osrm_...: calculate necessary?")
      }
    }
  }
}, timeout = 1, onTimeout = "warning")

return(goal)
}

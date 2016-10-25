#' In case of a new map: OSRM prepare (Creating the Hierarchy, creating the graph, nodeset etc..)
#'
#' @param osrm_path, A character
#'
#' @return NULL
#' @export
prepare_map <- function(osrm_path, plattform = "windows"){
  wd <- getwd()
  setwd(osrm_path)
  ifelse(plattform == "windows", # ............................................. if else better readable
         shell('osrm-prepare switzerland-exact.osrm >nul 2>nul', wait=F),
         system('osrm-prepare switzerland-exact.osrm', wait=F)
  )
  setwd(wd)
  return(NULL)
}

#' Start OSRM with map "switzerland-exact.osrm"
#'
#' @param osrm_path, A character
#'
#' @return error_code, A character
#' @export
run_server <- function(osrm_path, plattform){
  wd <- getwd()
  setwd(osrm_path)
  if(plattform == "windows") {
    error_code <- shell('osrm-routed switzerland-exact.osrm >nul 2>nul', wait=F)
  } else if(plattform == "mac") {
    error_code <- system(paste0(
      osrm_path, 'osrm-routed switzerland-latest.osrm'), wait=F)
  } else {
    stop("Unknown Plattform")
  }

  Sys.sleep(3) # OSRM needs time
  setwd(wd)
  return(error_code)
}

#' Quit osrm server
#'
#' @return NULL
#' @export
quit_server <- function() {
  shell("TaskKill /F /IM osrm-routed.exe >nul 2>nul")
  return(NULL)
}

#' Calculate route from source to destination (both given in WGS84). osrm chooses
#' the nearest point which can be accessed with a car. Be careful: Not all
#' combinations of api_version and localhost are possible
#'
#' @param lat1, A number
#' @param lng1, A number
#' @param lat2, A number
#' @param lng2, A number
#' @param instructions, A boolean for API V5: T = details are returned
#' @param api_version, A character
#' @param localhost, A boolean. T = local server used
#'
#' @return route
#' @export
viaroute <- function(lat1, lng1, lat2, lng2, instructions, api_version, localhost) {
  message(paste0("Use OSRM for ", paste(lat1, lng1, lat2, lng2, sep = ", ")))
  if (!localhost) {
    address <- "http://router.project-osrm.org"
  } else {
    address <- "http://localhost:5000"
  }

  if (api_version == 4) {
    R.utils::evalWithTimeout({
      repeat{
        res <- try(
          route <- rjson::fromJSON(
            file = paste(address, "/viaroute?loc=",
                         lat1, ',', lng1, '&loc=', lat2, ',', lng2, sep = "", collapse = NULL)))
        # Handle error from try:
        if (class(res) != "try-error") {
          if (!is.null(res)) {
            break # waytime found
          } else {
            stop("in osrm::viaroute: calculate nearest necessary?")
          }
        }
      }
    }, timeout = 1, onTimeout = "warning")
    return(res$route_summary$total_time)
  } else if (api_version == 5) {
    R.utils::evalWithTimeout({
      repeat {
        res <- try(
          if (instructions) {
            route <- rjson::fromJSON(
              file = paste(address, "/route/v1/driving/",
                           lng1, ",", lat1, ";", lng2, ",", lat2,
                           "?overview=full", sep = "", NULL))
          } else {
            route <- rjson::fromJSON(
              file = paste(address, "/route/v1/driving/",
                           lng1, ",", lat1, ";", lng2, ",", lat2,
                           "?overview=false", sep = "", NULL))
          })
        # Handle error from try:
        if (class(res) != "try-error") {
          if (!is.null(res)) {
            break # waytime found
          } else {
            stop("in sim911::osrm_viaroute: calculate nearest necessary?")
          }
        }
      }
    }, timeout = 1, onTimeout = "warning")
    assertthat::assert_that(assertthat::is.number(res$routes[[1]]$duration))
    if (!instructions) {
      return(res$routes[[1]]$duration)
    } else {
      return(res)
    }
  } else {
    stop("api_version needs to be 4 or 5")
  }
}


#' cacluate the nearest position which can be accessed via car. Be careful: Not
#' all combinations of api_version and localhost are possible
#'
#' @param lat, A number
#' @param lng, A number
#' @param api_version, A character
#' @param localhost, A boolean. T = local server used
#'
#' @return nearest
#' @export
nearest <- function(lat, lng, api_version, localhost) {
  if (!localhost) {
    address <- "http://router.project-osrm.org"
  } else {
    address <- "http://localhost:5000"
  }

  if (api_version == 4) {
    route <- rjson::fromJSON(file = paste(address, "/nearest?loc=",
                                          lat, ",", lng, sep = "", NULL))
  } else if (api_version == 5) {
    route <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/",
                                          lng, ",", lat, "?number=1", sep = "", NULL)) # &bearings=0,20
  } else {
    stop("api_version needs to be 4 or 5")
  }
  return(route)
}


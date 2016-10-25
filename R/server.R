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


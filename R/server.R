#' In case of a new map: OSRM prepare (Creating the Hierarchy, creating the graph, nodeset etc..)
#'
#' @param osrm_path, A character
#'
#' @return NULL
#' @export
prepare_map <- function(osrm_path){
  wd <- getwd()
  setwd(osrm_path)

  if (.Platform$OS.type == "windows") {
    shell('osrm-prepare switzerland-exact.osrm >nul 2>nul', wait = F)
  } else {
    system('osrm-prepare switzerland-exact.osrm', wait = F)
  }
  setwd(wd)
  return(NULL)
}

#' Start OSRM with map "switzerland-exact.osrm"
#'
#' @param osrm_path, A character
#'
#' @return error_code, A character
#' @export
run_server <- function(osrm_path){
  wd <- getwd()
  setwd(osrm_path)

  if (.Platform$OS.type == "windows") {
    error_code <- shell('osrm-routed switzerland-exact.osrm >nul 2>nul', wait = F)
  } else {
    error_code <- system(paste0(
      osrm_path, 'osrm-routed switzerland-latest.osrm'), wait = F)
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
  if (.Platform$OS.type == "windows") {
    shell("TaskKill /F /IM osrm-routed.exe >nul 2>nul")
  } else {
    system("pkill osrm-routed")
  }
  return(NULL)
}


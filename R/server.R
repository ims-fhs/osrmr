#' Start OSRM with map "switzerland-latest.osrm"
#'
#' @param osrm_path A string containing the path where OSRM is installed
#'
#' @return error_code A character
#' @export
#' @examples
#' # Start Server on SCNs Mac
#' osrmr::run_server("/Users/adrianschmid/Documents/OSRM/osrm-backend/build/")
run_server <- function(osrm_path){
  wd <- getwd()
  setwd(osrm_path)

  if (.Platform$OS.type == "windows") {
    error_code <- shell('osrm-routed switzerland-latest.osrm >nul 2>nul', wait = F)
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


#' server_address
#'
#' @param use_localhost A binary
#'
#' @return address The address of a OSRM server
#'
#' @examples
#' osrmr:::server_address(T)
#' # "http://localhost:5000"
#' osrmr:::server_address(F)
#' # "http://router.project-osrm.org"
server_address <- function(use_localhost) {
  if (use_localhost == T) {
    address <- "http://localhost:5000"
  } else {
    address <- "http://router.project-osrm.org"
  }
  return(address)
}


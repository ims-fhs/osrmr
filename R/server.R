#' Start local OSRM server
#'
#' run_server() starts your local OSRM server by using a shell command (depending on
#' your OS). A local (pre-built) version of the OSRM-engine must be on your device.
#' (https://github.com/Project-OSRM/osrm-backend/wiki/Building-OSRM).
#'
#' @param osrm_path A character containing the directory of the local OSRM-engine
#' @param map_name A character (name of your pre-built map - e.g. "switzerland-latest.osrm")
#'
#' @return error_code A character
#' @export
#' @examples
#' # osrmr::run_server("C:/path/to/local/osrm_engine/", "switzerland-latest.osrm")
#' # osrmr::run_server("/Users/adrianschmid/Documents/OSRM/osrm-backend/build/",
#' # "switzerland-latest.osrm")
#' # osrmr::run_server("C:/OSRM_API5/", "switzerland-latest.osrm")
#' # osrmr::run_server("C:/OSRM_API4/", "switzerland-latest.osrm")
run_server <- function(osrm_path, map_name){
  wd <- getwd()
  setwd(osrm_path)

  if (.Platform$OS.type == "windows") {
    error_code <- shell(paste0("osrm-routed ", map_name, " >nul 2>nul"), wait = F)
  } else {
    error_code <- system(paste0(
      osrm_path, "osrm-routed ", map_name), wait = F)
  }

  Sys.sleep(3) # OSRM needs time
  setwd(wd)
  return(error_code)
}

#' Quit local OSRM server
#'
#' quit_server() quits your local OSRM server by using a taskkill shell command (depending on
#' your OS).
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


#' Set server address
#'
#' server_address() returns an address depending on the choice of a localhost or webserver.
#' This address is used as a part of a OSRM server-request.
#'
#' @param use_localhost A logical, indicating whether to use the localhost or not.
#'
#' @return character, the address of an OSRM server
#'
#' @examples
#' osrmr:::server_address(TRUE)
#' # "http://localhost:5000"
#' osrmr:::server_address(FALSE)
#' # "http://router.project-osrm.org"
server_address <- function(use_localhost) {
  if (use_localhost == T) {
    address <- "http://localhost:5000"
  } else {
    address <- "http://router.project-osrm.org"
  }
  return(address)
}


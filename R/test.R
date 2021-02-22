
osrmr::quit_server()
Sys.setenv("OSRM_PATH"="C:/OSRM_API5")

# From osrmr::run_server("switzerland-lates.osrm")
map_name <- "switzerland-latest.osrm"
osrm_path <- Sys.getenv("OSRM_PATH")
wd <- getwd()
setwd(osrm_path)

error_code <- shell("osrm-routed switzerland-latest.osrm", intern = TRUE, wait = FALSE)

# system(command = "osrm-routed switzerland-latest.osrm", intern = FALSE, # funktioniert, C:\\OSRM_API5\\ nicht nötig
#        ignore.stdout = FALSE, ignore.stderr = FALSE,
#        wait = FALSE, input = NULL, show.output.on.console = TRUE,
#        minimized = FALSE, invisible = FALSE, timeout = 0)

system2('osrm-routed switzerland-latest.osrm',                            # funktioniert nicht, C:\\OSRM_API5\\ ändert nichts...
        args = character(),
        stdout = "", stderr = "", stdin = "", input = NULL,
        env = character(), wait = TRUE,
        minimized = FALSE, invisible = TRUE, timeout = 0)

setwd(wd)

osrmr::viaroute(47.1, 8.1, 46.9, 8.3, localhost = TRUE, instructions = FALSE)
osrmr::quit_server()



#' call_jar <- function(command) {
#'   # on a Unix-alike ‘system’ launches a shell which then runs ‘command’.  On
#'   # Windows the command is run directly - use ‘shell’ for an interface which
#'   # runs ‘command’ _via_ a shell (by default the Windows shell ‘cmd.exe’, which
#'   # has many differences from the POSIX shell).
#'   shell(command, intern = TRUE, wait = FALSE)
#'   Sys.sleep(1)
#' }
#'
#'
#' #' Process one testcase based on a json file
#' #'
#' #' @param path_tlopt4j A character
#' #' @param filename A character
#' #' @param path_json A character
#' #' @param path_csv A character
#' #' @param keep_files A boolean
#' #'
#' #' @return vrp, an ims vrp-object
#' #' @export
#' #'
#' #' @examples
#' #' path_tlopt4j <- "C:/13 Jprogress/tlopt4j"
#' #' filename <- "TC2018.1.3.json"
#' #' path_json <- "C:\\07 Rprogress\\01 Packages\\plotcargo\\tests\\testthat\\cc_files"
#' #' path_csv <- "C:\\07 Rprogress\\01 Packages\\plotcargo\\tests\\testthat\\cc_files"
#' #' process_json_from_java(path_tlopt4j, filename, path_json, path_csv)
#' process_json_from_java <- function(path_tlopt4j = "C:/Users/sqc/OneDrive - OST/13 Jprogress/tlopt4j/target",
#'                                    filename, path_json,
#'                                    path_csv, keep_files = TRUE,
#'                                    call_java = TRUE) {
#'   wd <- getwd()
#'   setwd(path_tlopt4j)
#'   # cmd <- "java -jar tlopt4j-0.0.1-SNAPSHOT.jar -reporting \"" # old
#'   cmd <- "java -jar tlopt4jTestClient-0.0.1-SNAPSHOT.jar -reporting \"" # new service
#'
#'   # java -jar pathandname.jar -reporting "input_json_file" "input_json_path" "output_csv_base_path"
#'   command <- paste0(cmd, filename, "\" \"", path_json, "\" \"",
#'                     path_csv, "\"")
#'
#'   if (call_java) {
#'     call_jar(command)


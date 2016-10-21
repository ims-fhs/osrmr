# .......................................... integrate in functions for OS-independecy? What is state of the art?
if (Sys.info()[[1]] == "Windows") {
  plattform <- "windows" # or "mac"
} # ............................................................................ Add result MAC from SCN

if (plattform == "windows") {
  osrm_path <- "C:/OSRM/" # or "/Users/adrianschmid/Documents/OSRM/osrm-backend/build/" => Needs to be an argument
} # ............................................................................ Add result MAC from SCN


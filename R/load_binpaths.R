#' @title Load the paths of external executables
#' @description Internal function to load the paths of executables 
#'  from the JSON where they are saved when installed.
#' @param bins Character vector with one of more of the following values:
#'  "gdal", sen2cor", "wget", "aria2", "python".
#'  If an executable corresponding to the passed `bins` value is not found
#'  in the JSON, it is installed (Windows) or checked (Linux).
#' @return The list of the paths
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#'
#' # Load only existing paths
#' binpaths <- load_binpaths()
#' 
#' # Load paths, forcing to check GDAL and sen2cor
#' binpaths <- load_binpaths(c("gdal", "sen2cor"))
#' }


load_binpaths <- function(bins = NULL) {
  
  # Define where the JSON with the paths is
  binpaths_file <- file.path(system.file("extdata",package="sen2r"),"paths.json")
  
  # If it exists, load it; otherwise, create empty
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list()
  }
  
  ## Check that the required binaries are present
  
  # Check GDAL
  if ("gdal" %in% bins & is.null(binpaths$gdalinfo)) {
    check_gdal()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Check sen2cor
  if ("sen2cor" %in% bins & is.null(binpaths$sen2cor)) {
    if (interactive()) {
      print_message(
        type="waiting",
        "sen2cor was not found in your system; press ENTER to install, ESC to escape."
      )
    } else {
      print_message(
        type="message",
        "sen2cor was not found in your system and will be installed."
      )
    }
    install_sen2cor() %>% suppressMessages()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Check wget
  if ("wget" %in% bins & is.null(binpaths$wget)) {
    if (Sys.info()["sysname"] == "Windows") {
      suppressMessages(install_wget())
    } else if (Sys.which("wget") != "") {
      binpaths$wget <- normalizePath(Sys.which("wget"))
      writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
      binpaths <- jsonlite::fromJSON(binpaths_file)
    }
  }
  
  # Check aria2
  if ("aria2" %in% bins & is.null(binpaths$aria2c)) {
    if (Sys.info()["sysname"] == "Windows") {
      suppressMessages(install_aria2())
    } else if (Sys.which("aria2c") != "") {
      binpaths$aria2c <- normalizePath(Sys.which("aria2c"))
      writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
      binpaths <- jsonlite::fromJSON(binpaths_file)
    }
  }
  
  # Check python
  if ("python" %in% bins & is.null(binpaths$python)) {
    init_python()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Return the list
  attr(binpaths, "path") <- binpaths_file
  binpaths
  
}

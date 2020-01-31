#' @title Load the paths of external executables
#' @description Internal function to load the paths of executables 
#'  from the JSON where they are saved when installed.
#' @param bins Character vector with one of more of the following values:
#'  "gdal", sen2cor", "aria2", "python".
#'  If an executable corresponding to the passed `bins` value is not found
#'  in the JSON, it is checked (when possible).
#' @return The list of the paths
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \donttest{
#' # Load only existing paths
#' binpaths <- load_binpaths()
#' binpaths
#' }
#' 
#' \dontrun{
#' # Load paths, forcing to check GDAL and sen2cor
#' binpaths <- load_binpaths(c("gdal", "sen2cor"))
#' binpaths
#' }


load_binpaths <- function(bins = NULL) {
  
  # Define where the JSON with the paths is
  binpaths_file <- file.path(
    if (dir.exists("~/.sen2r")) {"~/.sen2r"} else {tempdir()},
    "paths.json"
  )
  
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
    print_message(
      type="warning",
      "Sen2Cor was not found in your system; you can install it ",
      "using the function install_sen2cor()."
    )
  }
  
  # Check aria2
  if (any(c("aria2","aria2c") %in% bins) & is.null(binpaths$aria2c)) {
    if (Sys.which("aria2c") != "") {
      binpaths$aria2c <- normalize_path(Sys.which("aria2c"))
      writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
      binpaths <- jsonlite::fromJSON(binpaths_file)
    } else {
      print_message(
        type="warning",
        "aria2 was not found in your system; press install ",
        if (Sys.info()["sysname"] == "Windows") {
          "it using the function install_aria2()."
        } else {
          "the system package \"aria2\"."
        }
      )
    }
  }
  
  # # Check python
  # if ("python" %in% bins & is.null(binpaths$python)) {
  #   init_python()
  #   binpaths <- jsonlite::fromJSON(binpaths_file)
  # }
  
  # Return the list
  attr(binpaths, "path") <- binpaths_file
  binpaths
  
}

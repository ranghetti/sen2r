#' @title Check GDAL installation
#' @description The function check that GDAL is installed and updated to
#'  the minimum required version (2.1.3, since previous versions do not
#'  manage SAFE format).
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case no GDAL installation is found; if FALSE, a warning is shown
#'  and FALSE is returned.
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @return Logical (invisible): TRUE in case the installation is ok, FALSE 
#'  if GDAL is missing and abort=FALSE (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gdalUtils gdal_setInstallation gdal_chooseInstallation
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#'
#' # Remove previous GDAL information
#' options("gdalUtils_gdalPath" = NULL)
#' getOption("gdalUtils_gdalPath")
#'
#' # Use function
#' check_gdal()
#' getOption("gdalUtils_gdalPath")
#' }

# TODO check also python and GDAL outside install_s2download
# (one could be interested only in preprocessing and not in downloading)

check_gdal <- function(abort = TRUE, force = FALSE) {

  # set minimum GDAL version
  gdal_minversion <- package_version("2.1.3")
  
  # load the saved GDAL path, if exists
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (force != TRUE & !is.null(binpaths$gdalinfo)) {
    # print_message(
    #   type = "message",
    #   "GDAL is already set; to reconfigure it, set force = TRUE."
    # )
    return(invisible(TRUE))
  }
  
  # If GDAL is not found, search for it
  print_message(
    type="message",
    "Searching for a valid GDAL installation...")
  gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  
  # Check if this found version supports OpenJPEG
  gdal_check_jp2 <- tryCatch(gdal_chooseInstallation(hasDrivers=c("JP2OpenJPEG")), error = print)

  # If GDAL is not found, or if found version does not support JP2, perform a full search
  if (is.null(getOption("gdalUtils_gdalPath")) | is(gdal_check_jp2, "error")) {
    print_message(
      type="message",
      "GDAL was not found in the system PATH, search in the full ",
      "system (this could take some time, please wait...")
    gdal_setInstallation(ignore.full_scan = FALSE, verbose = TRUE)
  }
  
  # Check again if this found version supports OpenJPEG
  gdal_check_jp2 <- tryCatch(gdal_chooseInstallation(hasDrivers=c("JP2OpenJPEG")), error = print)


  # If GDAL is not found, return FALSE
  # (this should not happen, since GDAL is required by rgdal)
  if (is.null(getOption("gdalUtils_gdalPath")) | is(gdal_check_jp2, "error")) {
    print_message(
      type=message_type,
      "GDAL was not found, please install it."
    )
    return(invisible(FALSE))
  }
    
  # set message method
  message_type <- ifelse(abort==TRUE, "error", "warning")

  # check requisites (minimum version)
  gdal_version <- package_version(gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*", "\\1",
                                       rgdal::getGDALVersionInfo(str = "--version")))

  if (gdal_version < gdal_minversion) {
    print_message(
      type=message_type,
      "GDAL version must be at least ", gdal_minversion, ". Please update it.")
    return(invisible(FALSE))
  }

  # check requisites (JP2 support)
  gdal_JP2support <- length(grep("JP2OpenJPEG", gdalUtils::gdalinfo(formats = TRUE))) > 0
  if (!gdal_JP2support) {
    print_message(
      type=message_type,
      "Your local GDAL installation does not support JPEG2000 (JP2OpenJPEG) format. ",
      "Please install JP2OpenJPEG support and recompile GDAL.")
    return(invisible(FALSE))
  }
  
  # set this version to be used with gdalUtils
  gdal_chooseInstallation(hasDrivers=c("JP2OpenJPEG"))
  
  # save the path for use with external calls
  binpaths$gdalbuildvrt <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("gdalbuildvrt")))
  binpaths$gdal_translate <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("gdal_translate")))
  binpaths$gdalwarp <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("gdalwarp")))
  binpaths$gdal_calc <- if (Sys.info()["sysname"] == "Windows") {
    binpaths$python <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("python")))
    paste0(binpaths$python," ",getOption("gdalUtils_gdalPath")[[1]]$path,"gdal_calc.py")
  } else { 
    file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("gdal_calc.py")))
  }
  binpaths$gdalinfo <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("gdalinfo")))
  binpaths$ogrinfo <- file.path(getOption("gdalUtils_gdalPath")[[1]]$path,basename(Sys.which("ogrinfo")))
  
  lapply(
    c("gdalbuildvrt","gdal_translate","gdalwarp","gdalinfo","ogrinfo"), 
    function(x){
      binpaths[[x]] <- normalizePath(binpaths[[x]])
    }
  )
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
  
  print_message(
    type="message",
    "GDAL version in use: ", as.character(gdal_version))
  return(invisible(TRUE))

}



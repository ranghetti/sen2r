#' @title Create the database of GDAL supported formats
#' @description The internal function checks if gdal_formats.json (the
#'  database of spectral indices) already exists; if not, it
#'  downloads source files and creates it.
#'  Since on Windows there is an issue in reading the extension of 
#'  a GDAL driver with `reticulate` functions when the package `fidolasen`
#'  was already charged, this function can be used only from from
#'  Linux. It is not necessary, since a gdal_formats.json file is
#'  present in the package, but can be used to update the list in accordance
#'  with the formats actually managed by your GDAl installation.
#' @param json_path (optional) The path of the output JSON file.
#'  *Warning*: to create a file which wil be usable by the package,
#'  this option must be left to NA (default location is within the
#'  package installation). Edit this only to create the file in another
#'  place for external use.
#' @param force (optional) Logical: if FALSE (default), the db is created only
#'  if missing or not updated; if TRUE, it is created in any case..
#' @return NULL
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate py_to_r
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom rgdal gdalDrivers
#' @importFrom utils packageVersion

gdal_formats_db <- function(json_path = NA,
                            force = FALSE) {
  
  # check if gdal_formats.json already exists, and if the version is updated
  # we assume that a new version of gdal_formats.json is created at every new package update
  if (is.na(json_path)) {
    json_path <- file.path(system.file("extdata",package="fidolasen"),"gdal_formats.json")
  }
  if (system.file("extdata","gdal_formats.json", package="fidolasen") == json_path) {
    json_version <- jsonlite::fromJSON(json_path)$fidolasen_version %>%
      package_version()
    if (force == FALSE & json_version >= packageVersion("fidolasen")) {
      return(invisible(NULL))
    }
  }
  
  # load python modules
  py <- init_python()
  
  # read GDAL drivers
  gdal_driver_list <- list()
  for (i in 1:(py_to_r(gdal$GetDriverCount())-1)) {
    gdal_driver_list[[i]] <- py$osgeo$gdal$GetDriver(py$py$int(i))
  }
  
  # export vectors of names and extensions
  gdal_driver_ext <- sapply(
    gdal_driver_list,
    function(x) {
      unlist(strsplit(paste0(py_to_r(x$GetMetadataItem(py$osgeo$gdal$DMD_EXTENSIONS))," ")," "))[1]
    }
  )
  gdal_driver_names <- sapply(
    gdal_driver_list,
    function(x) {
      py_to_r(x$ShortName)
    }
  )
  gdal_driver_namext <- data.frame(
    "name" = gdal_driver_names,
    "ext" = gdal_driver_ext,
    stringsAsFactors = FALSE
  )
  
  # read other information using rgdal
  gdal_drivers <- gdalDrivers()
  
  gdal_drivers <- merge(gdal_drivers, gdal_driver_namext, by="name", all.x=TRUE)
  
  # fix specific formats
  gdal_drivers[gdal_drivers$name=="VRT","ext"] <- "vrt"
  gdal_drivers[gdal_drivers$name=="ENVI","ext"] <- "dat"
  
  ## Convert in JSON
  writeLines(jsonlite::toJSON(gdal_drivers, pretty=TRUE), json_path)
  return(invisible(NULL))
  
}

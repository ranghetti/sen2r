#' @title Check GDAL installation
#' @description The function check that GDAL is installed and updated to
#'  the minimum required version (2.1.2).
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case no GDAL installation is found; if FALSE, a warning is shown
#'  and FALSE is returned.
#' @param gdal_path (optional) Character: the path in which GDAL must be
#'  searched in. If NULL (default), search is performed in the whole file system.
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @param ignore.full_scan (optional) Logical: argument passed 
#'  to [gdal_setInstallation] (default is TRUE; set to FALSE in case
#'  of problems with the retrieved GDAL installation, e.g. when an undesired
#'  GDAL version is retrieved and the user wants to associate another one).
#' @return Logical (invisible): TRUE in case the installation is ok, FALSE 
#'  if GDAL is missing and abort=FALSE (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gdalUtils gdal_setInstallation gdal_chooseInstallation
#' @importFrom jsonlite fromJSON
#' @export
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

check_gdal <- function(abort = TRUE, gdal_path = NULL, force = FALSE, ignore.full_scan = TRUE) {
  
  # set minimum GDAL version
  gdal_minversion <- package_version("2.1.2")
  
  # # normalize gdal_path
  # normalize_path(gdal_path)
  
  # load the saved GDAL path, if exists
  binpaths <- load_binpaths()
  if (force != TRUE & !is.null(binpaths$gdalinfo)) {
    # print_message(
    #   type = "message",
    #   "GDAL is already set; to reconfigure it, set force = TRUE."
    # )
    gdal_setInstallation(search_path=dirname(binpaths$gdalinfo), rescan=TRUE)
    return(invisible(TRUE))
  }
  
  # If GDAL is not found, search for it
  print_message(
    type="message",
    "Searching for a valid GDAL installation...")
  gdal_check_fastpath <- tryCatch(
    gdal_setInstallation(ignore.full_scan = ignore.full_scan, verbose = FALSE), 
    error = print
  )
  
  # Check if this found version supports OpenJPEG
  gdal_check_jp2 <- tryCatch(
    gdal_chooseInstallation(hasDrivers=c("JP2OpenJPEG")), 
    error = print
  )
  
  # If GDAL is not found, or if found version does not support JP2, perform a full search
  if (is.null(getOption("gdalUtils_gdalPath")) |
      is(gdal_check_jp2, "error") |
      is(gdal_check_fastpath, "error")) {
    print_message(
      type="message",
      "GDAL was not found in the system PATH, search in the full ",
      "system (this could take some time, please wait)...")
    gdal_setInstallation(ignore.full_scan = FALSE, verbose = FALSE)
    # Check again if this found version supports OpenJPEG
    gdal_check_jp2 <- tryCatch(
      gdal_chooseInstallation(hasDrivers=c("JP2OpenJPEG")), 
      error = print
    )
  }
  
  
  # set message method
  message_type <- ifelse(abort==TRUE, "error", "warning")
  
  # If GDAL is not found, return FALSE
  if (is.null(getOption("gdalUtils_gdalPath")) | is(gdal_check_jp2, "error")) {
    print_message(
      type=message_type,
      "GDAL was not found, please install it."
    )
    return(invisible(FALSE))
  }
  
  
  # Retrieve found GDAL installations
  bin_ext <- ifelse(Sys.info()["sysname"] == "Windows", ".exe", "")
  gdal_dirs <- normalize_path(sapply(getOption("gdalUtils_gdalPath"), function(x){x$path}))
  gdalinfo_paths <- normalize_path(file.path(gdal_dirs,paste0("gdalinfo",bin_ext)))
  gdal_versions <- package_version(gsub(
    "^.*GDAL ([0-9\\.]+)[^0-9].*$", "\\1", 
    sapply(paste(gdalinfo_paths, "--version"), system, intern = TRUE)
  ))
  
  # check requisite 1: minimum version
  if (all(gdal_versions < gdal_minversion)) {
    print_message(
      type=message_type,
      "GDAL version must be at least ", as.character(gdal_minversion), 
      ". Please update it."
    )
    return(invisible(FALSE))
  }
  # filter GDAL installations respecting the requisite
  gdal_dirs <- gdal_dirs[!gdal_versions < gdal_minversion]
  gdalinfo_paths <- gdalinfo_paths[!gdal_versions < gdal_minversion]
  gdal_versions <- gdal_versions[!gdal_versions < gdal_minversion]
  # order by version
  gdal_dirs <- gdal_dirs[order(gdal_versions, decreasing = TRUE)]
  gdalinfo_paths <- gdalinfo_paths[order(gdal_versions, decreasing = TRUE)]
  gdal_versions <- sort(gdal_versions, decreasing = TRUE)
  
  # in Windows, prefer (filter, from v. 1.1.0) default OSGeo version
  if (Sys.info()["sysname"] == "Windows") {
    gdal_order <- c(
      grep("^C:\\\\OSGEO4~1", gdal_dirs), # 1: C:\OSGeo4W64
      grep("^(?!C:\\\\OSGEO4~1).*OSGEO4", gdal_dirs, perl=TRUE)#, # 2: other paths containing OSGEO4~1
      # grep("^((?!OSGEO4).)*$", gdal_dirs, perl=TRUE) # 3: all other paths
      # other paths were disabled to avoid selecting other installations
    )
    gdal_dirs <- gdal_dirs[gdal_order]
    gdalinfo_paths <- gdalinfo_paths[gdal_order]
    gdal_versions <- gdal_versions[gdal_order]
  }
  
  # check requisite 2: JP2 support
  gdal_formats <- if (length(gdalinfo_paths) > 0) {
    sapply(paste(gdalinfo_paths, "--formats"), system, intern = TRUE, simplify = FALSE)
  } else {
    character(0)
  }
  gdal_JP2support <- sapply(gdal_formats, function(x) {length(grepl("JP2OpenJPEG", x)) > 0})
  if (all(!gdal_JP2support)) {
    print_message(
      type=message_type,
      "Your local GDAL installation does not support JPEG2000 (JP2OpenJPEG) format. ",
      "Please install JP2OpenJPEG support and recompile GDAL.",
      if (Sys.info()["sysname"] == "Windows" & message_type=="error") {
        paste0(
          "\nUsing the OSGeo4W installer ",
          "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
          if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
          "choose the \"Advanced install\" and ",
          "check the packages \"gdal-python\" and \"openjpeg\"."
        )
      }
    )
    return(invisible(FALSE))
  } 
  # filter GDAL installations respecting the requisite
  gdal_dirs <- gdal_dirs[gdal_JP2support]
  gdalinfo_paths <- gdalinfo_paths[gdal_JP2support]
  gdal_versions <- gdal_versions[gdal_JP2support]
  
  
  # filter basing on the GDAL installation path defined with gdal_path
  if (!is.null(gdal_path)) {
    gdal_path <- normalize_path(gdal_path)
    # if no GDAL installations match the gdal_path, use another one
    if (all(!grepl(gdal_path, gdal_dirs, fixed = TRUE))) {
      print_message(
        type="warning",
        "No GDAL installations matching the requisites ",
        "(version >= 2.1.2 and supporting JPEG2000 format) ",
        "were found in \"",gdal_path,
        "\", so another local GDAL installation is used."
      )
    } else {
      gdal_dirs <- gdal_dirs[grepl(gdal_path, gdal_dirs, fixed = TRUE)]
      gdalinfo_paths <- gdalinfo_paths[grepl(gdal_path, gdal_dirs, fixed = TRUE)]
      gdal_versions <- gdal_versions[grepl(gdal_path, gdal_dirs, fixed = TRUE)]
    }
  }
  
  # set the version to be used with gdalUtils
  gdal_setInstallation(search_path = gdal_dirs[1], rescan = TRUE)
  
  # save the path for use with external calls
  gdal_dir <- gdal_dirs[1]
  binpaths$gdalbuildvrt <- normalize_path(file.path(gdal_dir,paste0("gdalbuildvrt",bin_ext)))
  binpaths$gdal_translate <- normalize_path(file.path(gdal_dir,paste0("gdal_translate",bin_ext)))
  binpaths$gdalwarp <- normalize_path(file.path(gdal_dir,paste0("gdalwarp",bin_ext)))
  binpaths$gdal_calc <- if (Sys.info()["sysname"] == "Windows") {
    binpaths$python <- normalize_path(file.path(gdal_dir,paste0("python",bin_ext)))
    paste0(binpaths$python," ",normalize_path(file.path(gdal_dir,"gdal_calc.py")))
  } else { 
    normalize_path(file.path(gdal_dir,"gdal_calc.py"))
  }
  binpaths$gdal_polygonize <- if (Sys.info()["sysname"] == "Windows") {
    binpaths$python <- normalize_path(file.path(gdal_dir,paste0("python",bin_ext)))
    paste0(binpaths$python," ",normalize_path(file.path(gdal_dir,"gdal_polygonize.py")))
  } else { 
    normalize_path(file.path(gdal_dir,"gdal_polygonize.py"))
  }
  binpaths$gdal_fillnodata <- if (Sys.info()["sysname"] == "Windows") {
    binpaths$python <- normalize_path(file.path(gdal_dir,paste0("python",bin_ext)))
    paste0(binpaths$python," ",normalize_path(file.path(gdal_dir,"gdal_fillnodata.py")))
  } else { 
    normalize_path(file.path(gdal_dir,"gdal_fillnodata.py"))
  }
  binpaths$gdaldem <- normalize_path(file.path(gdal_dir,paste0("gdaldem",bin_ext)))
  binpaths$gdalinfo <- normalize_path(file.path(gdal_dir,paste0("gdalinfo",bin_ext)))
  binpaths$ogrinfo <- normalize_path(file.path(gdal_dir,paste0("ogrinfo",bin_ext)))
  
  lapply(
    c("gdalbuildvrt","gdal_translate","gdalwarp","gdaldem","gdalinfo","ogrinfo"), 
    function(x){
      binpaths[[x]] <- normalize_path(binpaths[[x]])
    }
  )
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  
  print_message(
    type="message",
    "GDAL version in use: ", as.character(gdal_versions[1]))
  return(invisible(TRUE))
  
}

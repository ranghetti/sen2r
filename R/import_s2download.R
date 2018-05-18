#' @title Import s2download python module
#' @description [s2download](https://github.com/ranghetti/s2download) is
#'  a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by this package.
#'  This internal function check if their dependences (python, wget and aria2) 
#'  are installed, and imports the module.
#' @param with_aria2 (optional) Logical: if TRUE (default), the presence of 
#'  aria2 is checked; if FALSE, only Wget and python are checked.
#' @param ... Optional parameters of \code{\link[reticulate]{import}}
#' @return `s2download` python module
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import_from_path import_builtins py_to_r use_python

import_s2download <- function(with_aria2 = TRUE, ...) {
  
  # define the required binary dependencies
  mandeps <- c("python","wget")
  optdeps <- if (with_aria2 == TRUE) {c("aria2c")} else {character()}
  dependencies <- c(mandeps, optdeps)
  
  # define s2download path
  s2download_path <- system.file("s2download", package="sen2r")
  
  # check that git, python2 and wget are installed
  binpaths <- load_binpaths(dependencies)
  
  missing_dep <- dependencies[binpaths[dependencies]==""]
  if (length(missing_dep)>0) {
    
    if (Sys.info()["sysname"] != "Windows") {
      
      # On Linux, send an error / a warning if something is missing
      print_message(
        type = if (length(mandeps[binpaths[mandeps]==""])>0) {"error"} else {"warning"},
        "Some dependencies (",paste(missing_dep,collapse=", "),") were not found in your system; ",
        "please install them or update your system PATH. "
      )
      
    } else {
      
      # On Windows, download and install (them) or inform how to install them)
      
      # Download wget and aria2
      suppressMessages(install_wget())
      if (with_aria2 == TRUE) {suppressMessages(install_aria2())}
      
      # If other ones are missing:
      if (length(missing_dep[!missing_dep %in% c("wget","aria2c")])>0) {
        print_message(
          type = if (length(mandeps[binpaths[mandeps]==""])>0) {"error"} else {"warning"},
          "Some dependencies (",paste(missing_dep,collapse=", "),") were not found in your system; ",
          "please install them or update your system PATH.",
          if ("python" %in% missing_dep) {paste0(
            "\nTo install python, we suggest to use the OSGeo4W installer ",
            "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
            if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
            "to choose the \"Advanced install\" and to check ",
            "the package \"gdal-python\"."
          )}
        )
      }
      
    }
  } #TODO pip2 not working to install gitPython
  
  # checks the python version and import modules
  py <- init_python()
  
  # load s2download
  s2download <- tryCatch(
    import_from_path("s2download", s2download_path, ...), 
    error = print
  )
  if (is(s2download, "error")) {
    s2download <- import_from_path(
      "s2download", 
      paste0(normalize_path(s2download_path),"/"), 
      ...
    )
  }
  
  s2download$inst_path <- s2download_path
  
  return(s2download)
  
}

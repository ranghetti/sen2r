#' @title Initialise python
#' @description Internal function to check that the python dependencies
#'  required in the package are present; if so, load the base modules.
#' @return A list with the required mosules.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins use_python py_module_available py_discover_config

init_python <- function() {
  
  # define the required python module
  py_modules <- c("os","sys","subprocess","re","numpy","urllib","zipfile")
  # osgeo was removed, since the version of python to use was forced to have osgeo
  # py_modules <- c("os","sys","re","numpy","zipfile","osgeo")
  
  # On Windows, start searching in the GDAL directory
  if (Sys.info()["sysname"] == "Windows") {
    check_gdal()
  }
  
  # checks the python version
  # (if possible, use python2 for compatibility with s2download.py)
  binpaths_file <- file.path(system.file("extdata",package="salto"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("python"=NULL)
  }
  
  # Search or install Python
  if (is.null(binpaths$python)) {
    
    # Try to search Python
    binpaths$python <- tryCatch(
      normalizePath(py_discover_config(required_module = "osgeo")$python),
      error = function(e) {NULL}
    )
    
    # If not found, ask for installing Osgeo
    if (is.null(binpaths$python)) {
      open_gui <- print_message(
        type="error",
        "Python was not found on your system: please install it.",
        if (Sys.info()["sysname"] == "Windows") {
          paste0(
            "\nIt is recommended to install it together with GDAL using the OSGeo4W ",
            "installer (GDAL is required for processing operations).\n",
            "Download it from  http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
            if (Sys.info()["machine"]=="x86-64") {"_64"},".exe, ",
            "then choose the \"Advanced install\" and ",
            "check the packages \"python-core\", \"gdal\" and \"openjpeg\"."
          )
        }
      )
    }
    
    writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
    
  }
  
  use_python(binpaths$python) # FIXME force using osgeo python, OR load osgeo with import_from_path() instead of import("osgeo")
  py_missing <- py_modules[!sapply(py_modules,py_module_available)]
  if (length(py_missing)>0) {
    print_message(
      type="error",
      "Some modules (",paste(py_missing,collapse=", "),") are missing in your python distribution. ",
      "Please install them before continuing (depending on your distribution, you can find ",
      "packaged version of them, otherwise you can install them manually with ",
      "'sudo pip2 install ",paste(py_missing,collapse=" "),"' - pip2 is required).")
  } #TODO pip2 not working to install gitPython
  
  # import python modules
  py <- list()
  py$py <- import_builtins(convert=FALSE)
  for (mod in py_modules) {
    py[[mod]] <- import(mod, convert=FALSE)
  }
  
  # return the modules
  py
  
}

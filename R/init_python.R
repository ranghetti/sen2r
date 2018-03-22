#' @title Initialise python
#' @description Internal function to check that the python dependencies
#'  required in the package are present; if so, load the base modules.
#' @return A list with the required modules.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins use_python py_module_available py_discover_config

init_python <- function() {
  
  # define the required python module
  py_modules <- c("os","sys","subprocess","re","numpy","urllib","zipfile")
  py_modules_frompath <- c("numpy","osgeo")
  # py_modules are imported with import() function, so from any of the
  # discovered python versions; 
  # py_modules_frompath are imported with import_from_path(), which grants to
  # use the chosen (osgeo for windows) python version.
  # This was necessary, since in some windows installations use_python()
  # was unable to set the python version correctly (python was set, but not
  # pythonhome, so import() continued to try to import from pythonhome, i.e. 
  # osgeo from Anaconda python, which was missing).
  
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
  
  use_python(binpaths$python)
  py_missing <- py_modules[!sapply(
    ifelse(Sys.info()["sysname"] == "Windows", py_modules, c(py_modules,py_modules_frompath)),
    py_module_available
  )]
  if (length(py_missing)>0) {
    print_message(
      type="error",
      "Some modules (",paste(py_missing,collapse=", "),") are missing in your python distribution. ",
      "Please install them before continuing (depending on your distribution, you can find ",
      "packaged version of them, otherwise you can install them manually with ",
      "'sudo pip2 install ",paste(py_missing,collapse=" "),"' - pip2 is required).")
  }
  
  # import python modules
  py <- list()
  py$py <- import_builtins(convert=FALSE)
  for (mod in py_modules) {
    py[[mod]] <- import(mod, convert=FALSE)
  }
  for (mod in py_modules_frompath) {
    py[[mod]] <- if (Sys.info()["sysname"] != "Windows") {
      import(mod, convert=FALSE)
    } else {
      import_from_path(
        mod, 
        file.path(dirname(dirname(binpaths$python)), "apps/Python27", mod)
      )
    }
  }
  
  # return the modules
  py
  
}

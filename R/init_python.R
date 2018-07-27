#' @title Initialise python
#' @description Internal function to check that the python dependencies
#'  required in the package are present; if so, load the base modules.
#' @return A list with the required modules.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins use_python py_module_available py_discover_config
#' @export

init_python <- function() {
  
  # define the required python module
  py_modules <- c("os","sys","subprocess","re","numpy","urllib","zipfile","numpy","gdal","osr")
  
  # On Windows, start searching in the GDAL directory
  if (Sys.info()["sysname"] == "Windows") {
    check_gdal()
  }
  
  # checks the python version
  # (if possible, use python2 for compatibility with s2download.py)
  binpaths <- load_binpaths()
  
  # Search or install Python
  if (is.null(binpaths$python)) {
    
    # Try to search Python
    binpaths$python <- tryCatch(
      normalize_path(py_discover_config(required_module = "gdal")$python),
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
            "check the package \"gdal-python\"."
          )
        }
      )
    }
    
    # On Windows, set the environment variable
    if (Sys.info()["sysname"] == "Windows") {
      pythonhome_new <- normalize_path(list.files(
        file.path(dirname(dirname(paths$gdalinfo)),"apps"), 
        pattern="^Python", 
        full.names=TRUE
      ))
      pythonhome_exi <- normalize_path(Sys.getenv("PYTHONHOME"))
      if (pythonhome_new != pythonhome_exi) {
        Sys.setenv(PYTHONHOME = pythonhome_new)
      }
    }

    writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
    
  }
  
  # set the proper Python installation
  use_python(binpaths$python, required = TRUE)
  
  # import python modules
  # in Windows they are imported with import_from_path(), which grants to
  # use the chosen (osgeo for windows) python version.
  # This was necessary, since in some windows installations use_python()
  # was unable to set the python version correctly (python was set, but not
  # pythonhome, so import() continued to try to import from pythonhome, i.e. 
  # osgeo from Anaconda python, which was missing).
  py <- list()
  py$py <- import_builtins(convert=FALSE)
  for (mod in py_modules) {
    py[[mod]] <- if (Sys.info()["sysname"] != "Windows") {
      import(mod, convert=FALSE)
    } else {
      import_from_path(
        mod, 
        file.path(dirname(dirname(binpaths$python)), "apps/Python27/Lib/site-packages")
      )
    }
  }
  # if (!dirname(binpaths$python) %in% py_to_r(py$sys$path)) {
  #   py$sys$path$append(dirname(binpaths$python))
  # }
  
  # check for missing modules
  py_missing <- py_modules[!sapply(py_modules,py_module_available)]
  if (length(py_missing)>0) {
    print_message(
      type="error",
      "Some modules (",paste(py_missing,collapse=", "),") are missing in your python distribution. ",
      "Please install them before continuing (depending on your distribution, you can find ",
      "packaged version of them, otherwise you can install them manually with ",
      "'sudo pip2 install ",paste(py_missing,collapse=" "),"' - pip2 is required).")
  }
  
  # return the modules
  py
  
}

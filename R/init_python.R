#' @title Initialise python
#' @description Internal function to check that the python dependencies
#'  required in the package are present; if so, load the base modules.
#' @return NULL (the function is called for its side effects)
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0

init_python <- function() {
  
  # # define the required python module
  # py_modules <- c("os","sys","subprocess","re","numpy","urllib","zipfile","numpy","gdal","osr")
  
  # On Windows, start searching in the GDAL directory
  if (Sys.info()["sysname"] == "Windows") {
    check_gdal()
  }
  
  # checks the python version
  binpaths <- load_binpaths()
  

  # On Windows, set the environment variable
  if (Sys.info()["sysname"] == "Windows") {
    pythonhome_new <- list.files(
      file.path(dirname(dirname(binpaths$gdalinfo)),"apps"), 
      pattern="^Python", 
      full.names=TRUE
    )[1]
    pythonhome_exi <- Sys.getenv("PYTHONHOME")
    if (!normalize_path(pythonhome_exi) %in% normalize_path(pythonhome_new)) {
      Sys.setenv(PYTHONHOME = pythonhome_new)
      on.exit(Sys.setenv(PYTHONHOME = pythonhome_exi))
    }
    pythonpath_new <- list.files(pythonhome_new,"^[Ll]ib",full.names=TRUE)[1]
    pythonpath_exi <- Sys.getenv("PYTHONPATH")
    if (!normalize_path(pythonpath_exi) %in% normalize_path(pythonpath_new)) {
      Sys.setenv(PYTHONPATH = pythonpath_new)
      on.exit(Sys.setenv(PYTHONPATH = pythonpath_exi))
    }
    path_exi <- Sys.getenv("PATH")
    if (!any(grepl(
      normalize_path(pythonhome_new), 
      normalize_path(unlist(strsplit(path_exi, ";")), mustWork = FALSE), 
      fixed=TRUE
    ))) {
      Sys.setenv(PATH = paste0(pythonhome_new,";",Sys.getenv("PATH")))
      on.exit(Sys.setenv(PATH = path_exi))
    }
  }
  

  # # import python modules
  # # in Windows they are imported with import_from_path(), which grants to
  # # use the chosen (osgeo for windows) python version.
  # # This was necessary, since in some windows installations use_python()
  # # was unable to set the python version correctly (python was set, but not
  # # pythonhome, so import() continued to try to import from pythonhome, i.e. 
  # # osgeo from Anaconda python, which was missing).
  # py <- list()
  # py$py <- import_builtins(convert=FALSE)
  # for (mod in py_modules) {
  #   py[[mod]] <- if (Sys.info()["sysname"] != "Windows") {
  #     import(mod, convert=FALSE)
  #   } else {
  #     import_from_path(
  #       mod, 
  #       file.path(dirname(dirname(binpaths$python)), "apps/Python27/Lib/site-packages")
  #     )
  #   }
  # }
  # # if (!dirname(binpaths$python) %in% py_to_r(py$sys$path)) {
  # #   py$sys$path$append(dirname(binpaths$python))
  # # }
  # 
  # # check for missing modules
  # py_missing <- py_modules[!sapply(py_modules,py_module_available)]
  # if (length(py_missing)>0) {
  #   print_message(
  #     type="error",
  #     "Some modules (",paste(py_missing,collapse=", "),") are missing in your python distribution. ",
  #     "Please install them before continuing (depending on your distribution, you can find ",
  #     "packaged version of them, otherwise you can install them manually with ",
  #     "'sudo pip2 install ",paste(py_missing,collapse=" "),"' - pip2 is required).")
  # }
  # 
  # # return the modules
  # py
  
}

#' @title Initialise python
#' @description Internal function to check that the python dependencies
#'  required in the package are present; if so, load the base modules.
#' @return A list with the required mosules.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins use_python py_module_available py_discover_config

init_python <- function(inst_path=NA) {
  
  # define the required python module
  # py_modules <- c("os","sys","git","subprocess","re","numpy","zipfile","osgeo")
  py_modules <- c("os","sys","re","numpy","zipfile","osgeo")

  # checks the python version
  # (if possible, use python2 for compatibility with s2download.py)
  py_discover_config()
  if (Sys.which("python2")[1]!=""){
    use_python(Sys.which("python2")[1])
  }
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

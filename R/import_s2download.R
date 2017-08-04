#' @title Import s2download python module
#' @description s2download is a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by RSPrePro.
#'  This internal function check if they are installed and imports them.
#' @param ... optional parameters of import() function
#' @return s2download python module
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins py_to_r use_python

import_s2download <- function(...) {

  # check that s2download and dependencies were cloned
  # this ensures also that python2 and other dependencies are present)
  if (!file.exists(file.path(system.file(package="RSPrePro"),"s2download_path.txt"))) {
    print_message(
      type="waiting",
      "s2download was not found in your system; press ENTER to install, ESC to escape."
    )
    install_s2download()
  }

  # import python modules
  use_python(Sys.which("python2")[1])
  py <- import_builtins(convert=FALSE)
  sys <- import("sys",convert=FALSE)

  # load s2download
  s2download_path <- readLines(file.path(system.file(package="RSPrePro"),"s2download_path.txt"))[1]
  if (!s2download_path %in% py_to_r(sys$path)) {
    sys$path$insert(py$int(0),s2download_path)
  }

  s2download <- import("s2download", ...)
  s2download$inst_path <- s2download_path

  return(s2download)

}

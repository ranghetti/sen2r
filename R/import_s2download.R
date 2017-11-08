#' @title Import s2download python module
#' @description [s2download](https://github.com/ranghetti/s2download) is
#'  a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by this package.
#'  This internal function check if they are installed and imports them.
#' @param ... Optional parameters of \code{\link[reticulate]{import}}
#' @return `s2download` python module
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins py_to_r use_python

import_s2download <- function(...) {
  
  # check that python and the required modules are installed
  py <- init_python()

  # check that s2download and dependencies were cloned
  # this ensures also that python2 and other dependencies are present)
  s2download_metapath <- file.path(system.file("extdata",package="fidolasen"),"s2download_path.txt")

  if (!file.exists(s2download_metapath)) {
    print_message(
      type="waiting",
      "s2download was not found in your system; press ENTER to install, ESC to escape."
    )
    install_s2download()
  }

  # load s2download
  s2download_path <- readLines(s2download_metapath)[1]
  if (!s2download_path %in% py_to_r(py$sys$path)) {
    py$sys$path$insert(py$py$int(0),s2download_path)
  }

  s2download <- import("s2download", ...)
  s2download$inst_path <- s2download_path

  return(s2download)

}

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
#' @importFrom reticulate import_from_path import_builtins py_to_r use_python

import_s2download <- function(...) {
  
  # check that python and the required modules are installed
  py <- init_python()
  
  # check that s2download and dependencies were cloned
  # this ensures also that python2 and other dependencies are present)
  # check if it is already installed
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("s2download" = NULL)
  }
  if (length(binpaths$s2download)==0) {
    if (interactive()) {
      print_message(
        type="waiting",
        "s2download was not found in your system; press ENTER to install, ESC to escape."
      )
    }
    install_s2download()
  }
  
  # load s2download
  binpaths <- jsonlite::fromJSON(binpaths_file)
  s2download <- tryCatch(
    import_from_path("s2download", binpaths$s2download, ...), 
    error = print
  )
  if (is(s2download, "error")) {
    s2download <- import_from_path(
      "s2download", 
      paste0(normalizePath(binpaths$s2download),"/"), 
      ...
    )
  }
  
  s2download$inst_path <- binpaths$s2download
  
  return(s2download)
  
}

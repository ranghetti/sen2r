#' @title Expand a path with a parent directory
#' @description Accessory function which checks if a path is absolute or relative;
#'  if relative, use a specified parent directory instead than the working
#'  dorectory to expand it.
#'  Useful for functions which accept more than one path as arguments,
#'  in which one of them contains the absolute position, and the others
#'  do not.
#' @param ... `R` objects which are concatenated.
#' @param path `character` the path name to check ad eventually expand.
#' @param parent `character` the parent directory to use if 'path' is
#'  relative (default value: the working directory).
#' @param silent `logical` if TRUE (default), no message are shown;
#'  otherwise, a message inform if 'parent' were applied or not.
#' @return `character` the path eventually expanded.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


expand_path <- function(path, parent=NA, silent=TRUE) {

  if (is.na(parent)) {
    parent <- getwd()
  }

  # check if path is relative
  path_isabsolute <- if (Sys.info()["sysname"] == "Windows") {
    # for windows, the second character is used to discriminate absolute from relative
    substr(path.expand(path),2,2) == ":"
  } else {
    # for unix, the first character is used
    substr(path.expand(path),1,1) == "/"
  }

  # return
  if (path_isabsolute) {
    if (!silent) {
      print_message(type="message", "Path '",path,"' is already absolute.")
    }
    return(path.expand(path))
  } else {
    if (!silent) {
      print_message(type="message", "Path '",path,"' is not absolute; '",
                  gsub("/$","",parent),"' is used as prefix.")
    }
    return(path.expand(file.path(gsub("/$","",parent),path)))

  }

}

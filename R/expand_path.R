#' @title Expand a path with a parent directory
#' @description Accessory function which checks if a path is absolute or relative;
#'  if relative, use a specified parent directory instead than the working
#'  dorectory to expand it.
#'  Useful for functions which accept more than one path as arguments,
#'  in which one of them contains the absolute position, and the others
#'  do not.
#' @param ... `R` objects which are concatenated.
#' @param path The path name (`character`) to check ad eventually expand.
#' @param parent The parent directory (`character`) to use if `path` is
#'  relative (default value: the working directory).
#' @param silent Logical value: if TRUE (default), no message are shown;
#'  if FALSE, a message inform if `parent` were applied or not;
#'  if NA, a warning is returned if `path` is expanded, nothing if it
#'  is already an absolute path.
#' @param normalize Logical value: if TRUE (default), the path is normalised
#'  (\code{\link[base]{normalizePath}} is applied); if FALSE it is simply
#'  appended.
#' @return The path eventually expanded.
#' @export
#' @importFrom magrittr "%>%"
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


expand_path <- function(path, parent=getwd(), silent=TRUE, normalize=TRUE) {
  
  # to avoid NOTE on check
  . <- NULL
  
  # chose the function to apply
  expand_fun <- ifelse(normalize==TRUE, "normalizePath", "path.expand")
  
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
    
    if (!is.na(silent) & silent==FALSE) {
      print_message(type="message", "Path '",path,"' is already absolute.")
    }
    do.call(expand_fun, list(path)) %>%
      return()
    
  } else {
    
    if (is.na(silent) | silent==FALSE) {
      print_message(
        type=ifelse(is.na(silent),"warning","message"),
        "Path '",path,"' is not absolute; '",
        gsub("/$","",parent),"' is used as prefix.")
    }
    
    gsub("/$","",parent) %>%
      file.path(path) %>%
      list() %>%
      do.call(expand_fun, .) %>%
      return()
    
  }
  
}




#' @title Express file paths in canonical Form basing on the operating system
#' @description Accessory function wrapper for [normalizePath()] in Linux
#'  and [shortPathName(normalizePath())] in Windows.
#' @param path character vector of file paths
#' @param ... additional parameters passed to [normalizePath] (i.e. mustWork).
#' @return The paths normalized.
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


normalize_path <- function(path, ...) {
  
  if (Sys.info()["sysname"] == "Windows") {
    utils::shortPathName(normalizePath(path, ...))
  } else {
    normalizePath(path, ...)
  }
  
}

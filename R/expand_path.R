#' @title Expand a path with a parent directory
#' @description Accessory function which checks if a path is absolute or relative;
#'  if relative, use a specified parent directory instead than the working
#'  directory to expand it.
#'  Useful for functions which accept more than one path as arguments,
#'  in which one of them contains the absolute position, and the others
#'  do not.
#' @param path The path name (`character`) to check ad eventually expand.
#' @param parent The parent directory (`character`) to use if `path` is
#'  relative (default value: the working directory).
#' @param silent Logical value: if TRUE (default), no message are shown;
#'  if FALSE, a message inform if `parent` were applied or not;
#'  if NA, a warning is returned if `path` is expanded, nothing if it
#'  is already an absolute path.
#' @param normalize Logical value: if TRUE (default), the path is normalised
#'  (`normalizePath()` is applied); if FALSE it is simply
#'  appended.
#' @return The path eventually expanded.
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
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
    return(do.call(expand_fun, list(path)))
    
  } else {
    
    if (is.na(silent) | silent==FALSE) {
      print_message(
        type=ifelse(is.na(silent),"warning","message"),
        "Path '",path,"' is not absolute; '",
        gsub("/$","",parent),"' is used as prefix.")
    }
    
    return(
      do.call(expand_fun, list(file.path(gsub("/$","",parent), path)))
    )
    
  }
  
}




#' @title Express file paths in canonical Form depending on the operating system
#' @description Accessory function wrapper for `normalizePath()` in Linux
#'  and `shortPathName(normalizePath())` in Windows.
#' @param path character vector of file paths
#' @param ... additional parameters passed to [normalizePath] (i.e. `mustWork`).
#' @return The paths normalized.
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0


normalize_path <- function(path, ...) {
  
  if (Sys.info()["sysname"] == "Windows") {
    utils::shortPathName(normalizePath(gsub("\\\\$", "", path), ...))
    
  } else {
    normalizePath(path, ...)
  }
  
}

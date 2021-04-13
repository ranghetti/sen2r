#' @title Convert a path to a relative path
#' @description The function convert an absolute path to a relative path
#'  in respect to a reference. The longest common parent directory is
#'  taken as reference. Symbolic links are converted to original paths
#'  before performing the operation.
#' @param path The path to be converted (if it is not absolute,
#'  the current working directory is considered as its parent, and a
#'  warning is shown).
#' @param ref_path The reference path to be compared to
#'  `path` to obtain the relative directory.
#'  *Important*: the path is considered as a directory
#'  also if it is the path of a file!
#' @param mustWork (optional) logical: if TRUE an error is given
#'  if `path` or `ref_path` do not exists; if NA (default) then a
#'  warning; if FALSE nothing is shown.
#' @return The relative path
#'
#' @author Luigi Ranghetti, phD (2017) \email{luigi.ranghetti@@gmail.com}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @export
#'
#' @examples
#' # the reference path
#' (ref_path <- system.file(package="sen2r"))
#' # a path with a common parent with ref_path
#' (in_path_1 <- system.file(package="gdalUtils"))
#' # a path included in ref_path
#' (in_path_2 <- system.file("R/abs2rel.R", package="sen2r"))
#' # a path external to ref_path (in Linux)
#' (in_path_3 <- system.file(package="base"))
#' # an unexisting path
#' (in_path_4 <- gsub("sen2r","r2sen",ref_path))
#'
#' abs2rel(in_path_1, ref_path)
#'
#' abs2rel(in_path_2, ref_path)
#'
#' suppressWarnings(abs2rel(in_path_3, ref_path))
#'
#' suppressWarnings(abs2rel(in_path_4, ref_path, mustWork=FALSE))
#'
#' suppressWarnings(abs2rel(ref_path, ref_path))
abs2rel <- function(path, ref_path, mustWork=NA) {
  
  # to avoid NOTE on check
  . <- NULL
  
  # check if path is absolute
  path <- expand_path(path, silent=NA)
  
  # normalise paths
  path <- normalizePath(path, winslash="/", mustWork=mustWork)
  ref_path <- normalizePath(ref_path, winslash="/", mustWork=mustWork) # this also removes ending "/"
  
  # if path = ref_path, return with a warning
  if (path == ref_path) {
    print_message(
      type="warning", 
      "Input (",path,") and reference (",ref_path,") point to the same path."
    )
    return(".")
  }
  
  # search common parent directory
  com_path <- gsub(
    "/$", "", # remove the ending "/" if present
    comsub(c(path,ref_path), sep="/")
  )
  
  # if com_path is not null, return path with a warning
  if (com_path=="") {
    print_message(
      type="warning",
      "Input (",path,") and reference (",ref_path,
      ") paths do not have a common parent directory; ",
      "an absolute path is returned.")
    return(path)
  }
  
  # different part of directories
  diff_ref <- gsub("^/", "", gsub(paste0("^",com_path),"",ref_path))
  diff_in <- gsub("^/", "", gsub(paste0("^",com_path),"",path))
  
  # number of directory to "scale"
  n_ref <- length(strsplit(diff_ref,"/")[[1]])
  
  # relative path
  out_prefix <- if (n_ref==0) {
    "."
  } else {
    paste(rep("..",n_ref), collapse="/")
  }
  rel_path <- file.path(out_prefix,diff_in)
  
  return(rel_path)
  
}



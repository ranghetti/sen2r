#' @title Initialise python
#' @description Internal function to set the environmental variables
#'  required to run Python-based GDAL utilities on Windows.
#' @return NULL (the function is called for its side effects)
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0

init_python <- function() {
  
  # Setting environmental variables is required only on Windows
  if (Sys.info()["sysname"] != "Windows") {
    return(invisible(NULL))
  }
  
  binpaths <- load_binpaths("gdal")
  pythonhome_new <- list.files(
    file.path(dirname(dirname(binpaths$gdalinfo)),"apps"), 
    pattern="^Python", 
    full.names=TRUE
  )[1]
  pythonhome_exi <- Sys.getenv("PYTHONHOME")
  if (!normalize_path(pythonhome_exi, mustWork = FALSE) %in% normalize_path(pythonhome_new, mustWork = FALSE)) {
    Sys.setenv(PYTHONHOME = pythonhome_new)
    on.exit(Sys.setenv(PYTHONHOME = pythonhome_exi))
  }
  pythonpath_new <- list.files(pythonhome_new,"^[Ll]ib",full.names=TRUE)[1]
  pythonpath_exi <- Sys.getenv("PYTHONPATH")
  if (!normalize_path(pythonpath_exi, mustWork = FALSE) %in% normalize_path(pythonpath_new, mustWork = FALSE)) {
    Sys.setenv(PYTHONPATH = pythonpath_new)
    on.exit(Sys.setenv(PYTHONPATH = pythonpath_exi))
  }
  path_exi <- Sys.getenv("PATH")
  if (!any(grepl(
    normalize_path(pythonhome_new, mustWork = FALSE), 
    normalize_path(unlist(strsplit(path_exi, ";")), mustWork = FALSE), 
    fixed=TRUE
  ))) {
    Sys.setenv(PATH = paste0(pythonhome_new,";",Sys.getenv("PATH")))
    on.exit(Sys.setenv(PATH = path_exi))
  }

}

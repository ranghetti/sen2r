#' @title Return a parameter used in a WRT projection
#' @description Return the value of a parameter (or the name) present in
#'  the WKT of the given proj4string.
#' @param proj4string The proj4string to be named.
#' @param par Character corresponding to the parameter name.
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  proj4string is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the content of the parameter (NULL if the
#'  parameter is not recognised) or the name of the projection, and an
#'   attribute `proj4string` with the input projection checked using
#'  [sf::st_crs()].
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note Python is needed.
#' @export
#' @importFrom reticulate r_to_py py_to_r
#' @importFrom sf st_as_text st_crs
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \donttest{
#' projpar("+init=epsg:4326", "Unit")
#' }

projpar <- function(proj4string, par, abort = FALSE) {
  
  # import python modules
  py <- init_python()
  
  crs_check <- tryCatch(
    st_crs2(proj4string), 
    error = function(e) {st_crs(NA)}
  )
  
  if (is.na(crs_check$proj4string)) {
    return(NA)
  }
  
  proj4_wkt <- st_as_text(crs_check) %>%
    r_to_py() %>%
    py$osr$SpatialReference()
  proj4_par <- proj4_wkt$GetAttrValue(par)
  if (!is(proj4_par, "character")) {
    proj4_par <- py_to_r(proj4_par)
  }
    
  
  attr(proj4_par, "proj4string") <- crs_check$proj4string
  
  return(proj4_par)
  
}


#' @name projname
#' @rdname projpar
#' @export
#' @importFrom sf st_is_longlat st_crs
#' @examples
#' \donttest{
#' projname("+init=epsg:4326")
#' }

projname <- function(proj4string, abort = FALSE) { # nocov start
  
  proj4_name <- projpar(proj4string, "geogcs")
  if (!st_is_longlat(st_crs(attr(proj4_name, "proj4string")))) {
    proj4_name <- projpar(proj4string, "projcs")
  }
  proj4_name <- gsub("\\_"," ",proj4_name)
  
  return(proj4_name)
  
}  # nocov end

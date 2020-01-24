#' @title Return a parameter used in a WKT projection
#' @description Return the value of a parameter (or the name) present in
#'  the WKT of the given CRS
#' @param x The CRS to be named (any [st_crs2] input is accepted).
#' @param par Character corresponding to the parameter name.
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  CRS is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the content of the parameter (NULL if the
#'  parameter is not recognised) or the name of the projection, and an
#'   attribute `crs` with the input projection checked using
#'  [sf::st_crs()].
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note Python is needed.
#' @export
#' @importFrom reticulate r_to_py py_to_r
#' @importFrom sf st_as_text st_crs
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \donttest{
#' projpar(4326, "Unit")
#' }

projpar <- function(x, par, abort = FALSE) {
  
  # import python modules
  py <- init_python()
  
  crs_check <- try(st_crs2(x), silent = TRUE)
  if (inherits(crs_check, "try-error")) {return(NA)}
  
  proj4_wkt <- st_as_text_2(crs_check) %>%
    r_to_py() %>%
    py$osr$SpatialReference()
  proj4_par <- proj4_wkt$GetAttrValue(par)
  if (!is(proj4_par, "character")) {
    proj4_par <- py_to_r(proj4_par)
  }
  attr(proj4_par, "crs") <- crs_check
  
  return(proj4_par)
  
}


#' @name projname
#' @rdname projpar
#' @export
#' @importFrom sf st_is_longlat st_crs
#' @examples
#' \donttest{
#' projname(4326)
#' }

projname <- function(x, abort = FALSE) {
  
  crs_check <- try(st_crs2(x), silent = TRUE)
  if (inherits(crs_check, "try-error")) {return(NA)}
  
  proj4_wkt <- st_as_text_2(crs_check, pretty = TRUE)
  proj4_name <- gsub(
    "^((PROJCR?S)|(GEOGCR?S))\\[\\\"(.*)\\\",$", "\\4", 
    strsplit(proj4_wkt, "\n")[[1]][1]
  )
  attr(proj4_name, "crs") <- crs_check
  
  return(proj4_name)
  
}

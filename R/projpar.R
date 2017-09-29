#' @title Return a parameter used in a WRT projection
#' @description Return the value of a parameter (or the name) present in
#'  the WKT of the given proj4string.
#' @param proj4string The proj4string to be named (a character or a [CRS]
#'  object).
#' @param par Character corresponding to the parameter name.
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  proj4string is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the content of the parameter (NULL if the
#'  parameter is not recognised) or the name of the projection, and an
#'   attribute `proj4string` with the input projection checked using
#'  [sprawl::check_proj4string].
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note Python is needed.
#' @export
#' @importFrom reticulate import r_to_py py_to_r
#' @importFrom sprawl check_proj4string
#' @importFrom sp CRS
#' @importFrom rgdal showWKT
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#' projpar("+init=epsg:4326", "Unit")
#' }

projpar <- function(proj4string, par, abort = FALSE) {

  # import python modules
  osr <- import("osgeo",convert=FALSE)$osr

  proj4string_check <- check_proj4string(proj4string, abort=abort)

  if (proj4string_check == "invalid") {
    return(NA)
  }

  proj4_wkt <- proj4string_check %>%
    showWKT() %>%
    r_to_py() %>%
    osr$SpatialReference()
  proj4_par <- proj4_wkt$GetAttrValue(par) %>%
    py_to_r()

  attr(proj4_par, "proj4string") <- proj4string_check

  return(proj4_par)

}


#' @name projname
#' @rdname projpar
#' @export
#' @importFrom sp is.projected CRS
#' @examples
#'
#' projname("+init=epsg:4326")

projname <- function(proj4string, abort = FALSE) {

  proj4_name <- projpar(proj4string, "geogcs")

  if (is.projected(CRS(attr(proj4_name, "proj4string")))) {
    proj4_name <- projpar(proj4string, "projcs")
  }
  proj4_name <- gsub("\\_"," ",proj4_name)

  return(proj4_name)

}

#' @title Return the name of a projection
#' @description Return the projection name of a recognised proj4string.
#' @param proj4string The proj4string to be named (a character or a [CRS]
#'  object).
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  proj4string is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the name of the projection (and an attribute
#'  `proj4string` with the input projection checked using
#'  [sprawl::check_proj4string]).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note Python is needed.
#' @export
#' @importFrom reticulate import r_to_py
#' @importFrom sprawl check_proj4string
#' @importFrom sp is.projected CRS
#' @importFrom rgdal showWKT
#' @importFrom magrittr "%>%"
#'
#' @examples
#' projname("+init=epsg:4326")


projname <- function(proj4string, abort = FALSE) {

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
  proj4_name <- if (is.projected(CRS(proj4string))) {
    proj4_wkt$GetAttrValue('projcs')
  } else {
    proj4_wkt$GetAttrValue('geogcs')
  } %>%
    gsub("\\_"," ",.)

  attr(proj4_name, "proj4string") <- proj4string_check

  return(proj4_name)

}

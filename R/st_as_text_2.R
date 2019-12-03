#' @title Return WKT or WKT2 basing on the installed rgdal version
#' @description This is a convenience temporary function which returns the WKT
#'  representation of a CRS, using [sf::st_as_text] in case PROJ < 3,
#'  [rgdal::CRS] otherwise.
#'  This has the advantage to perform precise transformations with PROJ >=3,
#'  and to avoid conversion errors
#'  (see https://rsbivand.github.io/ECS530_h19/ECS530_III.html).
#'  This function will be deleted whenever `sf` will manage WKT2.
#' @param x object of class sfg, sfc or crs
#' @return Well-known Text representation of simple feature geometry or 
#'  coordinate reference system
#' @importFrom sf sf_extSoftVersion
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#'
#' @examples
#' sen2r:::st_as_text_2(st_crs(32632))

st_as_text_2 <- function(x) {
  if (all(
    package_version(sf_extSoftVersion()["proj.4"]) >= 6,
    packageVersion("rgdal") >= 1.5
  )) {
    x_crs <- st_crs(x)
    if (is.na(x_crs$epsg)) {
      rgdal::checkCRSArgs_ng(uprojargs = x_crs$proj4string)
    } else {
      rgdal::checkCRSArgs_ng(SRS_string = paste0("EPSG:",x_crs$epsg))
    }
  } else {
    st_as_text(x)
  }
}

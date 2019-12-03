#' @title Return WKT or WKT2 basing on the installed rgdal version
#' @description This is a convenience temporary function which returns the WKT
#'  representation of a CRS, using [sf::st_as_text] in case PROJ < 3,
#'  [rgdal::CRS] otherwise.
#'  This has the advantage to perform precise transformations with PROJ >=3,
#'  and to avoid conversion errors
#'  (see https://rsbivand.github.io/ECS530_h19/ECS530_III.html).
#'  This function will be deleted whenever `sf` will manage WKT2.
#' @param x object of class sfg, sfc or crs
#' @param pretty logical; if TRUE, print human-readable well-known-text
#'  representation of a coordinate reference system
#' @return Well-known Text representation of simple feature geometry or 
#'  coordinate reference system
#' @importFrom sf sf_extSoftVersion st_crs st_as_text
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#'
#' @examples
#' sen2r:::st_as_text_2(st_crs(32632))

st_as_text_2 <- function(x, pretty = FALSE) {
  if (all(
    package_version(sf_extSoftVersion()["proj.4"]) >= 6,
    packageVersion("rgdal") >= 1.5
  )) {
    x_crs <- st_crs(x)
    rgdal::showSRID(
      if (is.na(x_crs$epsg)) {
        x_crs$proj4string
      } else {
        paste0("EPSG:",x_crs$epsg)
      },
      format = "WKT2", 
      multiline = if (pretty == TRUE) {"YES"} else {"NO"}
    )
  } else {
    st_as_text(x, ...)
  }
}

#' @title Compare two non-null CRS
#' @description Internal function: convenience function to compare two non-null
#'  CRS in tests without using EPSG (so usable with rgdal >= 1.5).
#' @param crs1 CRS 1 to compare
#' @param crs2 CRS 2 to compare
#' @return `testthat::expect_equal()` output.
#' @importFrom sf st_geometry st_read st_coordinates st_transform
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal

expect_equal_crs <- function(crs1, crs2) {
  ref_vec <- st_geometry(st_read(
    system.file("extdata/vector/barbellino.geojson", package = "sen2r"), 
    quiet = TRUE
  ))
  testthat::expect_equal(
    as.integer(st_coordinates(st_transform(ref_vec, crs1))[,c("X","Y")]),
    as.integer(st_coordinates(st_transform(ref_vec, crs2))[,c("X","Y")])
  )
}

#' @title Don't use
#' @description Don't use this function.
#' @details This function simply call an `rgdal` function, so to be able to
#'  include package `rgdal` among imported dependencies.
#'  This is necessary since using `raster` without having `rgdal` installed
#'  causes troublings in R 4.
#' @importFrom rgdal GDALinfo
#' @return NULL
#' @keywords internal
dontuse <- function() {
  suppressWarnings(GDALinfo(
    system.file("extdata/out/S2A2A_20190723_022_Barbellino_SCL_10.tif", package = "sen2r")
  ))
  message("Disobedient.")
  invisible(NULL)
}

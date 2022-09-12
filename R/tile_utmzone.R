#' @title Obtain the UTM zone associated to Sentinel-2 tiles
#' @description Internal function: convenience function to obtain the UTM zone
#'  from tile IDs.
#' @param tile_id Character: tile ID (5 chars), e.g. `"32TNR"` (multiple values
#'  can be provided).
#' @return A vector of the same length of `tile_id` with UTM zones (e.g. `"32N`").
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal

tile_utmzone <- function(tile_id) {
    paste0(
      substr(tile_id,1,2),
      ifelse(substr(tile_id,3,3) %in% LETTERS[1:13], "S", "N")
    )
}


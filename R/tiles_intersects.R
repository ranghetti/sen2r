#' @title Select the tiles intersecting the extent
#' @description Function which returns the tile IDs of the Sentinel-2 tiles
#'  which overlap a provided extent.
#' @param extent `sf` object with the spatial extent.
#' @param all logical: if TRUE, all the tiles overlapping the extent are
#'  provided;
#'  if FALSE (default), unnecessary tiles are skipped.
#'  Unnecessary tiles are tiles which overlaps the extent for an area already
#'  coveder by another tile.
#'  In case the extent is all included in an overlapping area, only one of the
#'  two candidate tiles is returned (the first in alphabetical order).
#' @param out_format character: if "sf", the spatial object of the overlapping tiles
#'  is returned; if "id" (default), a character vector with the tile IDs.
#' @param .s2tiles output of [s2_tiles()] function (it is possible to pass it
#'  in order to speed up the execution;
#'  otherwise leave to NULL and it will be generated within the function).
#' @return the tiles intersecting the extent (see argument `out_format`).
#' @export
#' @importFrom sf st_area st_crs st_difference st_geometry 
#'  st_intersects st_transform st_union
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples
#' ex_extent <- sf::st_read(
#'   system.file("extdata/example_files/scalve.kml", package="sen2r"),
#'   quiet = TRUE
#' )
#' ex_extent <- ex_extent[ex_extent$description == "Schilpario",]
#' 
#' # Tile ID of the required S2 tile
#' tiles_intersects(ex_extent)
#' 
#' # Tile ID of all the overlapping S2 tiles
#' tiles_intersects(ex_extent, all = TRUE)
#' 
#' # Spatial object with the required tile
#' sel_tiles <- tiles_intersects(ex_extent, out_format = "sf")
#' plot(sf::st_geometry(sel_tiles)); plot(sf::st_geometry(ex_extent), add=TRUE, col="yellow")
#' 
#' # Spatial object with the overlapping S2 tiles
#' sel_tiles <- tiles_intersects(ex_extent, all = TRUE, out_format = "sf")
#' plot(sf::st_geometry(sel_tiles)); plot(sf::st_geometry(ex_extent), add=TRUE, col="yellow")

tiles_intersects <- function(extent, all = FALSE, out_format = "id", .s2tiles=NULL) {
  
  # Load S2 tiles
  s2tiles <- if (is.null(.s2tiles)) {
    s2_tiles()
  } else if (st_crs(.s2tiles) == st_crs(4326)) {
    .s2tiles
  } else {
    st_transform(.s2tiles, 4326)
  }
  
  # Extent to longlat
  extent <- st_transform(extent,4326)
  if (is(extent,"sf")) {
    extent <- st_geometry(extent)
  }
  
  # Select all the tiles which intersects the extent
  tiles_intersecting_all <- s2tiles[unique(unlist(suppressMessages(
    st_intersects(extent, s2tiles)
  ))),]
  
  tiles_intersects <- rep(TRUE, nrow(tiles_intersecting_all))
  
  # Cycle on tiles
  if (length(tiles_intersects) > 1 & all == FALSE) {
    for (i in rev(seq_len(nrow(tiles_intersecting_all)))) {
      sel_tile <- tiles_intersecting_all[i,]
      # intersection between extent and portion of tiles not overlapping other tiles
      # (with the exception of already discharged ones)
      sel_tile_notoverlap <- if (
        any(tiles_intersects & tiles_intersecting_all$tile_id != sel_tile$tile_id)
      ) {
        suppressMessages(st_difference(
          st_geometry(tiles_intersecting_all), 
          st_union(
            tiles_intersecting_all[tiles_intersects & tiles_intersecting_all$tile_id != sel_tile$tile_id,]
          )
        ))
      } else {
        st_geometry(tiles_intersecting_all)
      }
      # Check if any portion ot the extent is exclusive of sel_tile
      tiles_intersects[i] <- any(suppressMessages(
        st_intersects(extent, sel_tile_notoverlap, sparse = FALSE)
      ))
    }
  }
  
  # Return the required output
  if (out_format == "id") {
    tiles_intersecting_all[tiles_intersects,]$tile_id
  } else {
    tiles_intersecting_all[tiles_intersects,]
  }
  
}

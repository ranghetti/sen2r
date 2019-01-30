#' @title Load Sentinel-2 tiles
#' @description Load the vector object of the Sentinel-2 tiles.
#' @return An sf spatial object containing the extent of the tiles.
#' @export
#' @importFrom sf st_read
#' @importFrom utils unzip
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples 
#' # Retrieve all the tiles
#' s2tiles <- s2_tiles()
#' 
#' # Show a subset of all the tiles
#' s2tiles_it <- s2tiles[grep("^3[23]T", s2tiles$tile_id),]
#' s2_coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(s2tiles_it)))
#' plot(s2tiles_it$geometry)
#' text(s2_coords[,1], s2_coords[,2], s2tiles_it$tile_id, cex=0.5)

s2_tiles <- function() {
  
  # extract and import tiles kml
  s2tiles_kmz <- system.file("extdata","vector","s2_tiles.kmz",package="sen2r")
  s2tiles_kml <- gsub("\\.kmz$",".kml",s2tiles_kmz)
  if (!file.exists(s2tiles_kml)) {
    unzip(zipfile = s2tiles_kmz,
          files   = basename(s2tiles_kml),
          exdir   = dirname(s2tiles_kml),
          unzip   = "internal")
  }
  s2tiles <- st_read(s2tiles_kml, stringsAsFactors=FALSE, quiet=TRUE)
  s2tiles[,!names(s2tiles)%in%c("Name","geometry")] <- NULL
  names(s2tiles) <- gsub("^Name$","tile_id",names(s2tiles))
  s2tiles
  
}
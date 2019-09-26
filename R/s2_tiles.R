#' @title Load Sentinel-2 tiles
#' @description Load the vector object of the Sentinel-2 tiles.
#'  When the function is run for the first time, it downloads the vector file
#'  from the sen2r GitHub repository and it saves it on disk.
#' @return An sf spatial object containing the extent of the tiles.
#' @export
#' @importFrom sf st_read st_zm st_collection_extract st_write st_cast
#' @importFrom httr GET write_disk
#' @importFrom stats aggregate
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples
#' # Retrieve all the tiles
#' s2tiles <- s2_tiles()
#' 
#' # Extract a subset of all the tiles
#' httr::GET(
#'   "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_ITA_0_sf.rds", 
#'   httr::write_disk(it_path <- tempfile())
#' )
#' it <- readRDS(it_path)
#' s2tiles_it <- s2tiles[suppressMessages(sf::st_intersects(it, s2tiles))[[1]],]
#' s2_coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(s2tiles_it)))
#' 
#' # Show the tiles
#' plot(s2tiles_it$geometry, border = "blue")
#' plot(it$geometry, border = "red", add = TRUE)
#' text(s2_coords[,1], s2_coords[,2], s2tiles_it$tile_id, col = "blue", cex = 0.5)

s2_tiles <- function() {
  
  . <- NULL # to avoid NOTE on check
  
  # extract and import tiles kml
  s2tiles_rds <- file.path(dirname(attr(load_binpaths(), "path")), "s2_tiles.rds")
  if (!file.exists(s2tiles_rds)) {
    # nocov start
    print_message(
      date = TRUE,
      type = "message",
      "Downloading and creating the vector of Sentinel-2 tiles ",
      "(this happens once)..."
    )
    
    # # instructions to create the file from ESA original file
    # s2tiles_url <- "https://sentinel.esa.int/documents/247904/1955685/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml"
    # GET(s2tiles_url, write_disk(s2tiles_raw <- tempfile(fileext = ".kml")), overwrite=TRUE)
    # s2tiles <- st_read(s2tiles_raw, stringsAsFactors=FALSE, quiet=TRUE)
    # s2tiles <- s2tiles[,"Name"] %>%
    #   st_zm() %>% # remove Z column
    #   st_collection_extract("POLYGON") %>% # from GEOMETRY to POLYGON
    #   aggregate(list(.$Name), function(x) x[1]) %>% # group polygons by id
    #   st_cast("MULTIPOLYGON")
    # names(s2tiles) <- gsub("^Name$","tile_id",names(s2tiles))
    # saveRDS(
    #   s2tiles[,"tile_id"], 
    #   file.path(dirname(attr(load_binpaths(), "path")), "s2_tiles.rds")
    # )
    
    GET(
      "https://raw.githubusercontent.com/ranghetti/sen2r/devel/utils/vector/s2_tiles.rds", # FIXME use master
      write_disk(file.path(dirname(attr(load_binpaths(), "path")), "s2_tiles.rds")),
      overwrite=TRUE
    )
    # nocov end
  }
  readRDS(s2tiles_rds)
  
}

#' @title Retrieve coordinate reference system from sf or sfc object
#' @name st_crs2
#' @description This function is a wrapper for [sf::st_crs], unless
#'  threating numeric `character` strings as integers, and
#'  accepting also UTM timezones, paths of spatial files and paths of 
#'  text files containing WKT like .prj (see details) .
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}, being:
#'     - EPSG code: numeric (e.g. `32632`) or character (in the form
#'         `"32632"` or `"EPSG:32632"`);
#'     - UTM zone: numeric (e.g. `32`, interpreted as 32 North) or character
#'         (e.g. `"32"` or `"32N"` for zone 32 North, `"32S"` for 32 South);
#'     - WKT test: passed as character string or as path of a text file 
#'         containing it (e.g. the path of a .prj file);
#'     - PROJ.4 string, passed as character (e.g. 
#'         `"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"` 
#'         (**NOTE**: this representation is deprecated with PROJ >= 6
#'         -- see http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html --
#'         so a warning is returned using it, unless the string contains only
#'         the epsg code -- e.g. `"+init=epsg:32632"`, in which case the EPSG
#'         code is taken);
#'     - path of a spatial file (managed by [sf::st_read] or [stars::read_stars]),
#'         passed as character string of length 1;
#'     - spatial file of class \link{sf} or \link{sfc}.
#' @param ... other parameters passed to [sf::st_crs].
#' @return An object of class \link{crs} of length 2.
#' @details See [sf::st_crs] for details.
#' @importFrom sf st_crs
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples 
#' ## CRS from EPSG
#' st_crs2(32609)
#' st_crs2("EPSG:32609")
#' 
#' ## CRS from UTM zone
#' st_crs2(9)
#' st_crs2("09")
#' st_crs2("9N")
#' st_crs2("09S")
#' 
#' ## CRS from WKT (string or path)
#' (wkt_32n <- st_as_text(st_crs(32609)))
#' st_crs2(wkt_32n)
#' writeLines(wkt_32n, wkt_32n_path <- tempfile())
#' st_crs2(wkt_32n_path)
#' 
#' ## CRS from spatial file path
#' raster_path <- system.file(
#'   "extdata/out/S2A2A_20170703_022_Barbellino_BOA_10.tif", 
#'   package="sen2r"
#' )
#' vector_path <- system.file(
#'   "extdata/vector/barbellino.geojson", 
#'   package="sen2r"
#' )
#' st_crs2(raster_path)
#' st_crs2(vector_path)
#' 
#' ## CRS from spatial files
#' st_crs2(stars::read_stars(raster_path))
#' st_crs2(raster::raster(raster_path))
#' st_crs2(sf::read_sf(vector_path))
#' 
#' \donttest{
#' CRS from PROJ.4 string
#' # (avoid using this with PROJ >= 6!)
#' st_crs2("+init=epsg:32609") # this makes use of the EPSG code
#' st_crs2("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
#' }


st_crs2 <- function(x, ...) {
  
  x2 <- if (inherits(x, c("character", "integer", "numeric"))) {
    if (grepl("^(([0-5]?[0-9])|60)[Nn]?$", x)) {
      # x: UTM zone North -> x2: integer EPSG
      as.integer(paste0(
        "326", 
        str_pad2(
          gsub("^(([0-5]?[0-9])|60)[Nn]?$", "\\1", x),
          2, "left", "0"
        )
      ))
    } else if (grepl("^(([0-5]?[0-9])|60)[Ss]$", x)) {
      # x: UTM zone South -> x2: integer EPSG
      as.integer(paste0(
        "327", 
        str_pad2(
          gsub("^(([0-5]?[0-9])|60)[Ss]$", "\\1", x),
          2, "left", "0"
        )
      ))
    } else if (grepl("^[0-9]+$", x)) {
      # x: EPSG (integer, numeric or character) -> x2: integer EPSG
      as.integer(x)
    } else if (grepl("^[Ee][Pp][Ss][Gg]\\:[0-9]+$", x)) {
      # x: EPSG (in the form "EPSG:xxx") -> x2: integer EPSG
      as.integer(gsub("^[Ee][Pp][Ss][Gg]\\:([0-9]+)$", "\\1", x))
    } else if (grepl("^\\+init\\=epsg:[0-9]+$", tolower(x))) {
      # x: PROJ.4 with only EPSG -> x2: integer EPSG
      as.integer(gsub("^\\+init\\=epsg:([0-9]+)$", "\\1", tolower(x)))
    } else if (grepl("^\\+[a-z]+\\=", x)) {
      # x: PROJ.4 -> x2: character PROJ.4 with warning
      proj_version <- package_version(
        .Call('_sf_CPL_proj_version', PACKAGE = 'sf', FALSE)
      )
      if (proj_version >= 6) {
        print_message(
          type = "warning",
          "Using PROJ.4 strings is deprecated with PROJ >= 6 ",
          "(see http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html)."
        )
      }
      x
    } else if (file.exists(as.character(x))) {
      # x: file path -> x2_spatial file or WKT
      tryCatch(
        x2 <- st_read(x, quiet = TRUE),
        warning = function(w) {
          if (grepl("no simple feature geometries present\\: returning a data\\.frame", w)) {
            x
          } else {
            st_read(x, quiet = TRUE)
          }
        },
        error = function(e) {tryCatch(
          x2 <- read_stars(x, quiet = TRUE, proxy = TRUE),
          error = function(e) {tryCatch(
            x2 <- st_crs(readLines(x, warn = FALSE)),
            error = function(e) {x}
          )}
        )}
      )
    } else {
      # x: any other string -> x2: x (leave st_crs() managing it)
      x
    } 
  } else {
    # x: spatial classes, or any other object -> x2: x (leave st_crs() managing it)
    x
  }
  
  st_crs(x = x2, ...)
  
}

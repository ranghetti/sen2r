#' @title Retrieve coordinate reference system from sf or sfc object
#' @name st_crs2
#' @description This function is a wrapper for [sf::st_crs], unless
#'  threating numeric `character` strings as integers, and
#'  accepting also UTM timezones, paths of spatial files and paths of 
#'  text files containing WKT like .prj (see details) .
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}, being:
#'  - EPSG code: numeric (e.g. `32632`) or character (in the form
#'      `"32632"` or `"EPSG:32632"`);
#'  - UTM zone: numeric (e.g. `32`, interpreted as 32 North) or character
#'      (e.g. `"32"` or `"32N"` for zone 32 North, `"32S"` for 32 South);
#'  - WKT test: passed as character string or as path of a text file 
#'      containing it (e.g. the path of a .prj file);
#'  - PROJ.4 string, passed as character (e.g. 
#'      `"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"` 
#'      (**NOTE**: this representation is deprecated with PROJ >= 6
#'      -- see http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html --
#'      so a warning is returned using it, unless the string contains only
#'      the epsg code -- e.g. `"+init=epsg:32632"`, in which case the EPSG
#'      code is taken);
#'  - path of a spatial file (managed by [sf::st_read] or [stars::read_stars]),
#'      passed as character string of length 1;
#'  - spatial file of class \link{sf} or \link{sfc}.
#' @param ... other parameters passed to [sf::st_crs].
#' @return An object of class \link{crs} of length 2.
#' @details See [sf::st_crs] for details.
#' @importFrom sf gdal_crs st_crs
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
#' (wkt_32n <- sf::st_as_text(sf::st_crs(32609)))
#' st_crs2(wkt_32n)
#' writeLines(wkt_32n, wkt_32n_path <- tempfile())
#' st_crs2(wkt_32n_path)
#' 
#' ## CRS from spatial file path
#' raster_path <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif", 
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
#' st_crs2(sf::read_sf(vector_path))
#' 
#' \donttest{
#' ## CRS from PROJ.4 string
#' # (avoid using this with PROJ >= 6!)
#' st_crs2("+init=epsg:32609") # this makes use of the EPSG code
#' st_crs2("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
#' st_crs2(raster::raster(raster_path)) # st_crs(raster) uses the PROJ.4 as input
#' }


st_crs2 <- function(x, ...) UseMethod("st_crs2")

## character: several cases (see)
#' @export
st_crs2.character <- function(x, ...) {
  
  ## case 1: EPSG code / UTM zone
  x_epsg <- if (grepl("^(([0-5]?[0-9])|60)[Nn]?$", x)) {
    # x: UTM zone North -> integer EPSG
    as.integer(paste0(
      "326", 
      str_pad2(
        gsub("^(([0-5]?[0-9])|60)[Nn]?$", "\\1", x),
        2, "left", "0"
      )
    ))
  } else if (grepl("^(([0-5]?[0-9])|60)[Ss]$", x)) {
    # x: UTM zone South -> integer EPSG
    as.integer(paste0(
      "327", 
      str_pad2(
        gsub("^(([0-5]?[0-9])|60)[Ss]$", "\\1", x),
        2, "left", "0"
      )
    ))
  } else if (grepl("^[0-9]+$", x)) {
    # x: EPSG (integer, numeric or character) -> integer EPSG
    as.integer(x)
  } else if (grepl("^[Ee][Pp][Ss][Gg]\\:[0-9]+$", x)) {
    # x: EPSG (in the form "EPSG:xxx") -> integer EPSG
    as.integer(gsub("^[Ee][Pp][Ss][Gg]\\:([0-9]+)$", "\\1", x))
  } else if (grepl("^\\+init\\=epsg:[0-9]+$", tolower(x))) {
    # x: PROJ.4 with only EPSG -> integer EPSG
    as.integer(gsub("^\\+init\\=epsg:([0-9]+)$", "\\1", tolower(x)))
  } else {
    NULL
  }
  if (!is.null(x_epsg)) {
    return(sf::st_crs(x_epsg, ...))
  }
  
  ## case 2: PROJ.4
  if (grepl("^\\+[a-z]+\\=", x)) {
    # x: PROJ.4 -> character PROJ.4 with warning
    gdal_version <- tryCatch(
      package_version(gsub(
        "^.*GDAL ([0-9\\.]+)[^0-9].*$", "\\1",
        system(paste0(load_binpaths("gdal")$gdalinfo," --version"), intern = TRUE)
      )), # checking GDAL >=3 instead than PROJ >= 6 for simplicity
      error = function(e) {3} # in case of errors, return the warning
    )
    if (gdal_version >= 3) {
      print_message(
        type = "warning",
        "Using PROJ.4 strings is deprecated with PROJ >= 6 ",
        "(see http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html)."
      )
    } else if (packageVersion("sf") >= 0.9) {
      print_message(
        type = "warning",
        "Using PROJ.4 strings is deprecated with sf >= 0.9.0 ",
        "(see https://www.r-spatial.org/r/2020/03/17/wkt.html)."
      )
    }
    return(sf::st_crs(x, ...))
  }
  
  ## case 3: file path
  if (file.exists(as.character(x))) {
    # x: file path -> spatial file or WKT
    x2 <- tryCatch(
      # x: path of a vector file -> sf
      st_read(x, quiet = TRUE),
      warning = function(w) {
        if (grepl("no simple feature geometries present\\: returning a data\\.frame", w)) {
          # x: path of a tabular file -> x (st_crs will return the proper error)
          x
        } else {st_read(x, quiet = TRUE)}
      },
      error = function(e) {tryCatch(
        # x: path of a text file with WKT -> crs
        if (packageVersion("sf") >= 0.9) {
          suppressWarnings(sf::st_crs(readLines(x)))
        } else {
          suppressWarnings(sf::st_crs(wkt = readLines(x)))
        },
        error = function(e) {tryCatch(
          # x: path of a raster file -> stars proxy
          if (packageVersion("sf") >= 0.9) {
            gdal_crs(x)
          } else {
            gdal_crs(x)$crs
          },
          error = function(e) {
            # x: path of a non supported file -> x (st_crs will return the proper error)
            x
          }
        )}
      )}
    )
    return(sf::st_crs(x2, ...))
  }
  
  ## case 4: WKT and other characters
  if (grepl("^((PROJCR?S)|(GEOGCR?S))\\[.+\\]$", x)) {
    # x: WKT string -> crs
    if (packageVersion("sf") >= 0.9) {
      return(sf::st_crs(x, ...))
    } else {
      return(sf::st_crs(wkt = x, ...))
    }
  }
  
  ## any other case: pass to st_crs as is
  sf::st_crs(x, ...)
  
}

## integer or numeric (EPGS / UTM zone): threat as character
#' @export
st_crs2.integer <- function(x, ...) {st_crs2.character(as.character(x), ...)}
#' @export
st_crs2.numeric <- function(x, ...) {st_crs2.character(as.character(x), ...)}

## classes already managed by st_crs()
#' @export
st_crs2.default <- function(x, ...) {
  if (missing(x)) {sf::st_crs(NA)} else {sf::st_crs(x, ...)}
}

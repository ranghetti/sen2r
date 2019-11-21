#' @title Retrieve coordinate reference system from sf or sfc object
#' @name st_crs2
#' @description This function is a wrapper for [sf::st_crs], unless
#'  threating numeric `character` strings as integers, and
#'  accepting also UTM timezones, paths of spatial files and paths of 
#'  text files containing WKT like .prj (see details) .
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}
#' @param ... other parameters passed to [sf::st_crs].
#' @return If \code{x} is 0 < numeric <= 60, return \code{crs} object for UTM timezone \code{x} N;
#'  if \code{x} is numeric > 60, return \code{crs} object for SRID \code{x};
#'  if \code{x} is of class \code{sf} or \code{sfc}, return its \code{crs} object;
#'  if \code{x} is character in the form `nn`, `nnN` or `nnS`, being `nn` a number 0 < nn <= 60,
#'  return \code{crs} object for UTM timezone \code{x} N or \code{x} S (N if not specified);
#'  if \code{wkt} is given, return \code{crs} object for well-known-text representation \code{wkt}; 
#'  if \code{x} is a path of a spatial file or of a text file containing a WKT
#'  (like .prj), return the corresponding \code{crs} object; 
#'  if \code{x} is a different character, return \code{crs} object for proj4string \code{x},
#'  with a warning in case PROJ >= 3 is being used. 
#' @details See [sf::st_crs] for details.
#' @importFrom sf st_crs
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples 
#' st_crs2("+init=epsg:32609")
#' st_crs2(32609)
#' st_crs2(9)
#' st_crs2("09")
#' st_crs2("9N")
#' st_crs2("09S")

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

#' @title Retrieve coordinate reference system from sf or sfc object
#' @name st_crs2
#' @description This function is a wrapper for [sf::st_crs], unless accepting
#'  also UTM timezones (see details).
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}
#' @param ... other parameters passed to [sf::st_crs].
#' @return If \code{x} is 0 < numeric <= 60, return \code{crs} object for UTM timezone \code{x} N;
#'  if \code{x} is numeric > 60, return \code{crs} object for SRID \code{x};  
#'  if \code{x} is character in the form `nn`, `nnN` or `nnS`, being `nn` a number 0 < nn <= 60,
#'  return  \code{crs} object for UTM timezone \code{x} N or \code{x} S (N if not specified);
#'  if \code{x} is a different character, return \code{crs} object for proj4string \code{x}; 
#'  if \code{wkt} is given, return \code{crs} object for well-known-text representation \code{wkt}; 
#'  if \code{x} is of class \code{sf} or \code{sfc}, return its \code{crs} object.
#' @details See [sf::st_crs] for details.
#' @importFrom stringr str_pad
#' @importFrom sf st_crs
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples 
#' st_crs2("+init=epsg:32609")
#' st_crs2(32609)
#' st_crs2(9)
#' st_crs2("09")
#' st_crs2("9N")
#' st_crs2("09S")

st_crs2 <- function(x, ...) {
  
  sel_crs <- if (grepl("^(([0-5]?[0-9])|60)[Nn]?$", x)) {
    paste0(
      "+init=epsg:326", 
      str_pad(
        gsub("^(([0-5]?[0-9])|60)[Nn]?$", "\\1", x),
        2, "left", "0"
      )
    )
  } else if (grepl("^(([0-5]?[0-9])|60)[Ss]$", x)) {
    paste0(
      "+init=epsg:327", 
      str_pad(
        gsub("^(([0-5]?[0-9])|60)[Ss]$", "\\1", x),
        2, "left", "0"
      )
    )
  } else {
    x
  }
  
  st_crs(x = sel_crs, ...)
  
}

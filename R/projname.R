#' @title Return the CRS name
#' @description Return the name present in the WKT of the given CRS.
#' @param x The CRS to be named (any [st_crs2] input is accepted).
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  CRS is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the CRS name, and an
#'   attribute `crs` with the input projection checked using
#'  [sf::st_crs()].
#' @details [projpar()] is deprecated.
#'
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @importFrom sf st_is_longlat st_crs
#' @export
#' @examples
#' \donttest{
#' projname(4326)
#' }

projname <- function(x, abort = FALSE) {
  
  crs_check <- try(st_crs2(x), silent = TRUE)
  if (inherits(crs_check, "try-error")) {return(NA)}
  
  proj4_wkt <- st_as_text_2(crs_check, pretty = TRUE)
  proj4_name <- gsub(
    "^((PROJCR?S)|(GEOGCR?S))\\[\\\"(.*)\\\",$", "\\4", 
    strsplit(proj4_wkt, "\n")[[1]][1]
  )
  attr(proj4_name, "crs") <- crs_check
  
  return(proj4_name)
  
}


#' @name projpar
#' @rdname projname
projpar <- function(x, par, abort = FALSE) {
  stop("This function is deprecated.")
}

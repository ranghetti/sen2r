#' @title Return a parameter used in a WKT projection
#' @description Return the value of a parameter (the name or the unit)
#'  present in the WKT of the given CRS.
#' @param x The CRS to be named (any [st_crs2] input is accepted).
#' @param par Character corresponding to the parameter name
#'  (it can be one among "name" and "unit" - case insensitive).
#' @param abort logical: if TRUE, the function aborts in case an invalid
#'  CRS is passed; if FALSE (default), the function returns NA,
#'  and a warning is shown.
#' @return A character with the content of the parameter, and an
#'   attribute `crs` with the input projection checked using
#'  [sf::st_crs()].
#' @note
#'  The old function, which was searching for a generic parameter
#'  parsing the WKT, was deprecated: now [projpar()] only accepts `par = "name"`
#'  and `par = "unit"`, and `projname()` is an alias for `projpar(..., par = "name")`.
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @export
#' @importFrom sf st_as_text st_crs
#'
#' @examples
#' \donttest{
#' projpar(4326, "name")
#' projpar(4326, "unit")
#' }

projpar <- function(x, par, abort = FALSE) {
  
  crs_check <- try(st_crs2(x), silent = TRUE)
  if (inherits(crs_check, "try-error")) {return(NA)}
  
  proj4_wkt <- st_as_text_2(crs_check, pretty = TRUE)
  
  proj4_par <- if (tolower(par) %in% c("name", "geogcs", "projgcs")) {
    if (tolower(par) != "name") {
      print_message(
        type = "warning",
        "par = \"",par,"\" is now an alias of par = \"name\", ",
        "and will be deprecated in future."
      )
    }
    gsub(
      "^((PROJCR?S)|(GEOGCR?S))\\[\\\"(.*)\\\",$", "\\4", 
      strsplit(proj4_wkt, "\n")[[1]][1]
    )
  } else if (tolower(par) == "unit") {
    gsub(
      "^.+UNIT *\\[\\\"([^\"]*)\\\".+$", "\\1", 
      proj4_wkt
    )
  } else {
    print_message(
      type = "error",
      "par = \"",par,"\" is no longer accepted ",
      "(only \"name\" and \"unit\" can be used)."
    )
  }
  
  attr(proj4_par, "crs") <- crs_check
  
  return(proj4_par)
  
}


#' @name projname
#' @rdname projpar
#' @export
#' @importFrom sf st_is_longlat st_crs
#' @examples
#' \donttest{
#' projname(4326)
#' }

projname <- function(x, abort = FALSE) {
  projpar(x, "name", abort = abort)
}

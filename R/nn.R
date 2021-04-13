#' @title Replace NULL with character()
#' @description Internal function: return character(0) instead of NULL.
#'  This is sometimes needed not to return error when applying some functions.
#' @param x Input variable
#' @return `character(0)` if `x==NULL`, `x` elsewhere
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#'
#' @examples
#' tryCatch(basename(NULL), error = print) # error
#' basename(character()) # ok
#' basename(sen2r:::nn(NULL)) # ok

nn <- function(x) {if (is.null(x)) character(0) else x}

# # Version which replaces also non-existing objects
# nn <- function(x) {
#   if (!exists(deparse(substitute(x)))) {
#     character(0)
#   } else if (is.null(x)) {
#     character(0)
#   } else {
#     x
#   }
# }

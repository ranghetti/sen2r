#' @title Replace NULL with character()
#' @description Internal function: return character(0) instead of NULL.
#'  This is sometimes needed not to return error when applying some functions.
#' @param x Input variable
#' @return `character(0)` if `x==NULL`, `x` elsewhere
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
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

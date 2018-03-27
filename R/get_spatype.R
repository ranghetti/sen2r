#' @title check the "spatial type" of an object or file
#' @description Check if a `R` object or a filename correspond to a valid vector
#'   object, to a vector file or none of the above. Useful to detect which kind
#'   of input is passed to a function and abort / do something in the case of
#'   "wrong" input.
#' @param in_vect name of an `R` object, or `character` giving the full path
#'  to a vector file
#' @param abort If TRUE, and `in_vect` is neither a spatial object or
#'  filename, send an error message and abort, Default: TRUE
#' @return `character` equal to "vectfile" (if `in_vect` is a raster file),
#'   `spobject` (if `in_vect` is of class `spatial`), `sfobject` (if `in_vect`
#'   is of class `sf`), or `NA` if it is neither (unless `abort` == TRUE)
#' @rdname get_vectype
#' @note Functions [get_spatype], [get_rastype] and [get_vectype] come from
#'  package [sprawl](http://lbusett.github.io/sprawl).
#' @importFrom gdalUtils ogrinfo

get_vectype  <- function(in_vect, abort = TRUE) {
  UseMethod("get_vectype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_vectype default
get_vectype.default <- function(in_vect, abort = TRUE) {
  stop_message <- paste0("\"", deparse(substitute(in_vect)),
                         "\" is not a recognised vector object or filename.")
  if (abort) {
    stop(stop_message)
  } else {
    warning(stop_message)
    return(NA)
  }
}

#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_vectype character
#' @importFrom gdalUtils ogrinfo
#' @export
get_vectype.character <- function(in_vect, abort = TRUE) {
  vecttry <- suppressWarnings(
    try(gdalUtils::ogrinfo(in_vect, al = TRUE,
                           so = TRUE,
                           verbose = FALSE,
                           q = TRUE),
        silent = TRUE)
  )
  if (is.null(attr(vecttry, "status"))) {
    return("vectfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_vect)),
                           "\" is not a recognised vector filename.")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
}

#   ____________________________________________________________________________
#   Method for "sf"                                                         ####

#' @method get_vectype sf
#' @export
get_vectype.sf <- function(in_vect, abort = TRUE) {
  "sfobject"
}

#   ____________________________________________________________________________
#   Method for "sfc"                                                       ####

#' @method get_vectype sfc
#' @export
get_vectype.sfc  <- function(in_vect, abort = TRUE) {
  "sfobject"
}


#   ____________________________________________________________________________
#   Method for "Spatial"                                                    ####

#' @method get_vectype Spatial
#' @export
get_vectype.Spatial <- function(in_vect, abort = TRUE) {
  "spobject"
}



#' @title Check in the input is a `Raster` object or raster file
#' @description Check if a `R` object or a filename correspond to a valid `Raster`
#'   object, to a raster file or none of the above. Useful to detect which kind
#'   of input is passed to a function and abort / do something in the case of
#'   "wrong" input.
#' @param in_rast name of an `R` object, or `character` giving the full path
#'  to a spatial file
#' @param abort If TRUE, and `in_rast` is neither a raster object or
#'  filename, send an error message and abort, Default: TRUE
#' @return `character` equal to "rastfile" (if `in_rast` is a raster file),
#'   `rastobject` (if `in_rast` is a `R` raster object) or `NA`` if it is
#'   neither (unless `abort` == TRUE)
#' @note Functions [get_spatype], [get_rastype] and [get_vectype] come from
#'  package [sprawl](http://lbusett.github.io/sprawl).
#' @rdname get_rastype
#' @export
#' @importFrom rgdal GDALinfo
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

get_rastype  <- function(in_rast, abort = TRUE) {
  UseMethod("get_rastype")
}

#   ____________________________________________________________________________
#   Fallback method: class of object is none of the specified ones: issue   ####
#   an error

#' @method get_rastype default
#' @export
get_rastype.default <- function(in_rast, abort = TRUE) {
  
  stop_message <- paste0("\"", deparse(substitute(in_rast)),
                         "\" is not a recognised raster object or filename.")
  if (abort) {
    stop(stop_message)
  } else {
    warning(stop_message)
    return(NA)
  }
}
#   ____________________________________________________________________________
#   Method for "character" - find if file exists and is "spatial"           ####

#' @method get_rastype character
#' @export
get_rastype.character <- function(in_rast, abort = TRUE) {
  
  rastry  <- suppressWarnings(try(rgdal::GDALinfo(in_rast),
                                  silent = TRUE))
  if (!is(rastry, "try-error")) {
    return("rastfile")
  } else {
    stop_message <- paste0("\"", deparse(substitute(in_rast)),
                           "\" is not a recognised raster filename.")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
}
#   __________________________________________________________________________
#   Method for "Raster"                                                  ####

#' @method get_rastype Raster
#' @export
get_rastype.Raster <- function(in_rast, abort = TRUE) {
  "rastobject"
}


#' @title check the "spatial type" of an object or file
#' @description accessory function to check if an object passed to the function
#'  corresponds to a `*Spatial` Object, a `sf` object, a R `raster` object, a
#'  file corresponding to a vector, or a file corresponding to a raster.
#'  NOTE: to check only for vector or raster types, the associated functions
#'  `get_vectype` and `get_rastype` can be used, with the same syntax.
#' @param in_object either a `R` object or a `character` string pointing to a
#'  vector or raster layer
#' @param abort `logical` if TRUE the function aborts if `object` is not
#' recognized as an R spatial file or valid vector or raster file; if FALSE,
#' a warning is shown and `NA`` is returned.
#' @return character (\"*spobject*\" | \"*sfobject*\" | \"*rastobject* | \"
#' *vectfile*\" | *rastfile*), or `NA` if the input does not
#' belong to any spatial category and abort == FALSE
#' @name get_spatype
#' @rdname get_spatype
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @note Functions [get_spatype], [get_rastype] and [get_vectype] come from
#'  package [sprawl](http://lbusett.github.io/sprawl).
get_spatype <- function(in_object,
                        abort = TRUE) {
  obj_type <- suppressWarnings(
    get_rastype(in_object, abort = FALSE)
  )
  
  if (is.na(obj_type)) {
    obj_type <-  suppressWarnings(
      get_vectype(in_object, abort = FALSE)
    )
  }
  if (is.na(obj_type)) {
    stop_message <- paste0(
      "\"", deparse(substitute(in_object)),
      "\" is not a recognised vector/raster object or filename")
    if (abort) {
      stop(stop_message)
    } else {
      warning(stop_message)
      return(NA)
    }
  }
  
  return(obj_type)
}
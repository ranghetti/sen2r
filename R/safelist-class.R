#' @title Format for SAFE archive lists
#' @description `safelist` is a format thought to manage lists of SAFE
#'  Sentinel.2 archives.
#'  It is a named character in which names are SAFE codes 
#'  (e.g. `S2A_MSIL2A_20170507T102031_N0205_R065_T32TNR_20170507T102319.SAFE`),
#'  and values are URLs used to retrieve them from ESA API Hub (e.g. 
#'  `https://scihub.copernicus.eu/apihub/odata/v1/Products('a4a026c0-db7b-4ba8-9b09-53027ab0d7ab')/$value`).
#'  Some attributes may be included, basically information retrieved by
#'  function [s2_list] containing product metadata.
#'  Moreover, the attribute `online` (retrieved by function [safe_is_online]
#'  may contain logical values (TRUE for products available for download,
#'  FALSE for products stored in the Long Term Archive). 
#'  
#'  The class can be generated as an output of function [s2_list], or converting
#'  named characters (with the same structures), data.frames or data.tables
#'  (including the columns `name` and `url`) using [as] (see examples).
#'  Objects of class `safelist` can be converted to named character, data.frames
#'  or data.tables (see examples). The conversion to data.frame / data.table is
#'  useful for reading hidden attributes.
#'  
#' @name safelist-class
#' @aliases safelist
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @examples 
#' \donttest{
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2017-05-01", "2017-05-31"))
#' 
#' ## Create an object of class safelist
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' list_safe
#' class(list_safe)
#' attr(list_safe, "sensing_datetime") # extract an hidden attribute from a safelist
#' 
#' ## Convert to other classes
#' (s2_char <- as.character(list_safe)) # convert to a simple named character
#' (s2_df <- as.data.frame(list_safe)) # convert to a data.frame
#' library(data.table)
#' (s2_dt <- as.data.table(list_safe)) # convert to a data.table
#' library(sf)
#' (s2_sf <- st_as_sf(list_safe)) # convert to sf
#' 
#' ## Convert from other classes
#' as(s2_char, "safelist") # this causes the loss of hidden attributes
#' as(s2_df, "safelist") # this (and followings) maintain attributes as columns
#' as(s2_dt, "safelist")
#' as(s2_sf, "safelist")
#' }

setClass("safelist", contains = "character")


## Methods TO safelist

setAs("character", "safelist", function(from) {
  # import x if it is the path of a JSON filelist
  if (all(length(from) == 1, file.exists(from))) {
    from <- jsonlite::fromJSON(from)
  }
  if (length(nn(from)) == 0) {} else if (
    length(names(from)) == 3 && 
    all(names(from) == c("ordered", "available", "notordered"))
  ) {
    # check if input can be converted - case of list saved by s2_order
    order_status <- c(
      rep("available", length(from$available)),
      rep("ordered", length(from$ordered)),
      rep("notordered", length(from$notordered))
    )
    from <- c(from$available, from$ordered, from$notordered)
    from <- setNames(as.character(from), names(from))
    # add an "order_status" attribute, used in s2_order to eventually re-check
    # order status, or just order datasets with attribute "notordered"
    attr(from, "order_status") <- order_status
    if (any(c(
      is.null(names(from)),
      !grepl("^http.+Products\\(.+\\)/\\$value$", as.vector(from)),
      !grepl("^S2[AB]\\_MSIL[12][AC]\\_[0-9]{8}T[0-9]{6}\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_[0-9]{8}T[0-9]{6}\\.SAFE$", names(from))
    ))) {
      stop("cannot convert to safelist (input format not recognised)")
    }
  } else {
    # check if input can be converted - case of list saved by s2_list, or "bare"
    if (is(from, "list")) {
      from <- setNames(as.character(from), names(from))
    }
    # list
    if (any(c(
      is.null(names(from)),
      !grepl("^http.+Products\\(.+\\)/\\$value$", as.vector(from)),
      !grepl("^S2[AB]\\_MSIL[12][AC]\\_[0-9]{8}T[0-9]{6}\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_[0-9]{8}T[0-9]{6}\\.SAFE$", names(from))
    ))) {
      stop("cannot convert to safelist (input format not recognised)")
    }
  }
  class(from) <- unique(c("safelist", class(from)))
  from
})

setAs("data.frame", "safelist", function(from) {
  # check if input can be converted
  if (nrow(from) == 0) {} else if (any(c(
    is.null(from$name), is.null(from$url), 
    !grepl("^http.+Products\\(.+\\)/\\$value$", from$url),
    !grepl("^S2[AB]\\_MSIL[12][AC]\\_[0-9]{8}T[0-9]{6}\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_[0-9]{8}T[0-9]{6}\\.SAFE$", from$name)
  ))) {
    stop("cannot convert to safelist (input format not recognised)")
  }
  to <- as.character(from$url)
  names(to) <- from$name
  attrs <- names(from)[!names(from) %in% c("name", "url")]
  if (inherits(from, "data.table")) {
    for (a in attrs) {
      attr(to, a) <- from[,eval(parse(text=a))]
    }
  } else {
    for (a in attrs) {
      attr(to, a) <- from[,a]
    }
  }
  as(to, "safelist")
})

setAs("sf", "safelist", function(from) {
  as(as.data.frame(from), "safelist")
})

## Methods FROM safelist

#' @export
as.data.frame.safelist <- function(x, row.names = NULL, optional = FALSE, ...) {
  to <- data.frame(name = names(x), url = as.vector(x), stringsAsFactors = FALSE)
  autoRN <- (is.null(row.names) || length(row.names) != nrow(to))
  attr(to, "row.names") <- if (autoRN) {seq_len(nrow(to))} else {row.names}
  
  attrs <- names(attributes(x))[!names(attributes(x)) %in% c("names", "class")]
  for (a in attrs) {
    to[,a] <- attr(x, a)
  }
  to
}
setAs("safelist", "data.frame", function(from) {
  as.data.frame(from)
})

#' @export
as.data.table.safelist <- function(x, keep.rownames = FALSE, ...) {
  rownames <- if (keep.rownames) {
    names(x)
  }
  data.table(as.data.frame(x, row.names = rownames), keep.rownames = keep.rownames)
}
setAs("safelist", "data.table", function(from) {
  as.data.table(from)
})

#' @export
as.character.safelist <- function(x, ...) {
  x[seq_len(length(x))]
}
setAs("safelist", "character", function(from) {
  as.character(from)
})

#' @export
st_as_sf.safelist <- function(x, ...) {
  if (!is.null(attr(x, "footprint"))) {
    sf::st_as_sf(as.data.frame(x), wkt = "footprint", crs = 4326)
  } else {
    stop("cannot convert to safelist (missing footprint)")
  }
}
setAs("safelist", "sf", function(from) {
  st_as_sf(from)
})


## Print method
#' @export
print.safelist = function(x, ...) {
  x_print <- as.character(x)[seq_len(min(length(x),5))]
  names(x_print) <- names(x)[seq_len(min(length(x),5))]
  # x_url <- paste0(substr(x,1,60),"...")
  cat("A named vector with", length(x), "SAFE archives.\n")
  print(x_print)
  if (length(x) > 5) {
    cat("...with", length(x)-5, "more elements.\n")
  }
  attrs <- names(attributes(x))[!names(attributes(x)) %in% c("names", "class")]
  if (length(attrs) > 0) {
    cat("The following attributes are included:", paste(attrs, collapse=", "))
    cat(".\n")
  }
  invisible(x)
}

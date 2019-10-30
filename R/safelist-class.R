#' @title Format for SAFE archive lists
#' @description Create or convert lists of SAFE archives from named vectors
#'  or data frames.
#' @name safelist-class
#' @aliases safelist
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples 
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2017-05-01", "2017-05-31"))
#' 
#' ## Create an object of class safelist
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' list_safe
#' class(list_safe)
#' as.character(list_safe) # convert to a simple named character
#' as.data.frame(list_safe) # convert to a data.frame (or to a data.table using as.data.table)
#' attr(list_safe, "date") # extract an hidden attribute from a safelist

setClass("safelist", contains = "character")


## Methods TO safelist

setAs("character", "safelist", function(from) {
  # import x if it is the path of a JSON filelist
  if (all(length(from) == 1, file.exists(from))) {
    from <- unlist(fromJSON(from))
  }
  # check if input can be converted
  if (any(c(
    length(nn(names(from))) == 0,
    !grepl("^http.+Products\\(.+\\)/\\$value$", as.vector(from)),
    !grepl("^S2[AB]\\_MSIL[12][AC]\\_[0-9]{8}T[0-9]{6}\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_[0-9]{8}T[0-9]{6}\\.SAFE$", names(from))
  ))) {
    stop("cannot convert to safelist (input format not recognised)")
  }
  class(from) <- unique(c("safelist", class(from)))
  from
})

setAs("data.frame", "safelist", function(from) {
  # check if input can be converted
  if (any(c(
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


## Methods FROM safelist

#' @export
as.data.frame.safelist <- function(from) {
  to <- data.frame(name = names(from), url = as.vector(from))
  attrs <- names(attributes(from))[!names(attributes(from)) %in% c("names", "class")]
  for (a in attrs) {
    to[,a] <- attr(from, a)
  }
  to
}
setAs("safelist", "data.frame", function(from) {
  as.data.frame(from)
})

#' @export
as.data.table.safelist <- function(from) {
  data.table(as.data.frame(from))
}
setAs("safelist", "data.table", function(from) {
  as.data.table(from)
})

#' @export
as.character.safelist <- function(from) {
  from[seq_len(length(from))]
}
setAs("safelist", "character", function(from) {
  as.character(from)
})


## Print methods

# #' @export
# print.s2dt = function(x) {
#   if (!all(c("name","url") %in% names(x))) {
#     print(data.table(x))
#   } else {
#     printed_cols <- c("name", "url")
#     x_print <- as.data.frame(x)[seq_len(min(nrow(x),5)), printed_cols]
#     x_print$url <- paste0(substr(x_print$url,1,20),"...")
#     cat("A data.table with", nrow(x), "SAFE archives.\n")
#     print.data.frame(x_print)
#     if (nrow(x) > 5 | ncol(x) > ncol(x_print)) {
#       cat("...with")
#       if (nrow(x) > 5) {
#         cat("", nrow(x)-5, "more rows")
#       }
#       if (nrow(x) > 5 & ncol(x) > ncol(x_print)) {
#         cat(" and")
#       }
#       if (ncol(x) > ncol(x_print)) {
#         cat("", ncol(x)-ncol(x_print), "more columns: ")
#         cat(paste(names(x)[!names(x) %in% printed_cols], collapse = ", "))
#       }
#       cat(".\n")
#     }
#   }
#   invisible(x)
# }

#' @export
print.safelist = function(x) {
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


## Extract metadata

#' @export
safe_date <- function(x) {
  x <- as(x, "safelist")
  sapply(names(x), function(y) {
    as.POSIXct(safe_getMetadata(y, info = "nameinfo")$sensing_datetime)
  })
}
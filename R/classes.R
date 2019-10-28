## Functions with class definitions

#' @name s2-classes
#' @title Formats for SAFE archive lists
#' @description Create or convert lists of SAFE archives from named vectors
#'  or data frames.
#' @param x Input object, which could be:
#'  - a named vector, in which elements are SciHub URLs and names are SAFE names;
#'  - a data frame or data.table, in which columns `name` and `url` must be present;
#'  - an object of class `s2list` or `s2dt`.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0

setClass("s2list", contains = "character")
setClass("s2dt", contains = "data.table")

#' @rdname s2-classes
#' @return `as.s2dt` returns an object of class `s2dt`: a data.table
#'  in which column `name` contains SAFE names, 
#'  column `url` contains SciHub URLs
#'  and additional columns include other product metadata.
#' @export
#' @examples 
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2017-05-01", "2017-05-31"))
#' 
#' ## Convert from an object s2list
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' list_safe
#' class(list_safe) # default output class of s2_list()
#' as.character(list_safe) # convert to a simple named character
#' as.data.frame(list_safe) # convert to a data.frame (or to a data.table using as.data.table)
#' attr(list_safe, "date") # extract an hidden attribute from an s2list
#' 
#' ## Convert from an object s2dt
#' out_dt <- as.s2dt(list_safe)
#' out_dt
#' class(out_dt)
#' as.character(out_dt) # convert to a simple named character
#' as.data.frame(out_dt) # convert to a data.frame (or to a data.table using as.data.table)
#' as.s2list(out_dt) # convert to an object s2list


as.s2dt <- function(x) {
  if (inherits(x, "character")) {
    # import x if it is the path of a JSON filelist
    if (all(length(x) == 1, file.exists(x))) {
      x <- unlist(fromJSON(x))
    }
    xdt <- data.table(name = names(x), url = as.character(x))
    x_attrs <- names(attributes(x))[!names(attributes(x)) %in% c("names", "class")]
    for (a in x_attrs) {
      xdt[,a] <- attr(x, a)
    }
    class(xdt) <- unique(c("s2dt", class(xdt)))
  } else if (inherits(x, "data.frame")) {
    xdt <- if (!inherits(x, "data.table")) {data.table(x)} else {x}
    class(xdt) <- unique(c("s2dt", class(xdt)))
  } else {
    stop("format not recognised")
  }
  xdt
}

#' @rdname s2-classes
#' @return `as.s2list` returns an object of class `s2list`: a named
#'  vector in which elements are SciHub URLs and names are SAFE names, 
#'  eventually including some product metadata (stored as attributes).
#' @export
as.s2list <- function(x) {
  if (inherits(x, "data.frame")) {
    if (!inherits(x, "data.table")) {
      x <- data.table(x)
    }
    xlist <- x$url
    names(xlist) <- x$name
    x_attrs <- names(x)[!names(x) %in% c("name", "url")]
    for (a in x_attrs) {
      attr(xlist, a) <- x[,eval(parse(text=a))]
    }
    class(xlist) <- unique(c("s2list", class(xlist)))
  } else if (inherits(x, "character")) {
    # import x if it is the path of a JSON filelist
    xlist <- if (all(length(x) == 1, file.exists(x))) {
      x <- unlist(fromJSON(x))
    } else {
      x
    }
    class(xlist) <- unique(c("s2list", class(xlist)))
  } else {
    stop("format not recognised")
  }
  xlist
}


## Methods TO s2list / s2dt

setAs("s2list", "s2dt", function(from) {
  as.s2dt(from)
})

setAs("s2dt", "s2list", function(from) {
  as.s2list(from)
})

setAs("character", "s2list", function(from) {
  as.s2list(from)
})

setAs("character", "s2dt", function(from) {
  as.s2dt(from)
})

setAs("data.table", "s2list", function(from) {
  as.s2list(from)
})

setAs("data.table", "s2dt", function(from) {
  as.s2dt(from)
})


## Methods FROM s2list / s2dt

#' @export
as.data.table.s2list <- function(from) {
  as.data.table(as.s2dt(from))
}
setAs("s2list", "data.table", function(from) {
  as.data.table(from)
})

setAs("s2dt", "data.table", function(from) {
  as.data.table(from)
})

#' @export
as.data.frame.s2list <- function(from) {
  as.data.frame(as.s2dt(from))
}
setAs("s2list", "data.frame", function(from) {
  as.data.frame(from)
})

setAs("s2dt", "data.frame", function(from) {
  as.data.frame(from)
})

#' @export
as.character.s2dt <- function(from) {
  as.s2list(from)[seq_len(nrow(from))]
}
setAs("s2dt", "character", function(from) {
  as.character(from)
})

#' @export
as.character.s2list <- function(from) {
  from[seq_len(length(from))]
}
setAs("s2list", "character", function(from) {
  as.character(from)
})


## Print methods

#' @export
print.s2dt = function(x) {
  if (!all(c("name","url") %in% names(x))) {
    print(data.table(x))
  } else {
    printed_cols <- c("name", "url")
    x_print <- as.data.frame(x)[seq_len(min(nrow(x),5)), printed_cols]
    x_print$url <- paste0(substr(x_print$url,1,20),"...")
    cat("A data.table with", nrow(x), "SAFE archives.\n")
    print.data.frame(x_print)
    if (nrow(x) > 5 | ncol(x) > ncol(x_print)) {
      cat("...with")
      if (nrow(x) > 5) {
        cat("", nrow(x)-5, "more rows")
      }
      if (nrow(x) > 5 & ncol(x) > ncol(x_print)) {
        cat(" and")
      }
      if (ncol(x) > ncol(x_print)) {
        cat("", ncol(x)-ncol(x_print), "more columns: ")
        cat(paste(names(x)[!names(x) %in% printed_cols], collapse = ", "))
      }
      cat(".\n")
    }
  }
  invisible(x)
}

#' @export
print.s2list = function(x) {
  x_print <- as.character(x)[seq_len(min(length(x),5))]
  names(x_print) <- names(x)[seq_len(min(length(x),5))]
  # x_url <- paste0(substr(x,1,60),"...")
  cat("A named vector with", length(x), "SAFE archives.\n")
  print(x_print)
  if (length(x) > 5) {
    cat("...with", length(x)-5, "more elements.\n")
  }
  x_attrs <- names(attributes(x))[!names(attributes(x)) %in% c("names", "class")]
  if (length(x_attrs) > 0) {
    cat("The following attributes are included:", paste(x_attrs, collapse=", "))
    cat(".\n")
  }
  invisible(x)
}

#' @title Get information from S2 short name
#' @description This accessory function extracts metadata included in
#'  the name of a Sentinel-2 product which follows the sen2r
#'  naming convention (see [safe_shortname]).
#' @param s2_names A vector of Sentinel-2 product names in the
#'  sen2r naming convention.
#' @param format One between `data.table` (default), `data.frame` and `list`.
#' @param abort Logical parameter: if TRUE (default), the function aborts 
#'  in case any of `s2_names` is not recognised; if FALSE, a warning is shown,
#'  and a list with only the element "type"='unrecognised' is returned.
#' @return A data.table, data.frame or list of the output metadata.
#'
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @import data.table
#' @examples
#' # Define product name
#' fs2nc_examplename <-
#'   "/path/of/the/product/S2A1C_20170603_022_32TQQ_TOA_20.tif"
#'
#' # Return metadata
#' sen2r_getElements(fs2nc_examplename)

sen2r_getElements <- function(s2_names, format="data.table", abort=TRUE) {
  
  # check format
  if (!format %in% c("list", "data.frame", "data.table")) {
    print_message(
      type="warning",
      "Argument must be one between 'data.frame' and 'list'.",
      "Returnig a list.")
    format <- "list"
  }
  
  # if input is NULL, return NULL
  if (is.null(s2_names)) {
    return(invisible(NULL))
  }
  
  # define regular expressions to identify products
  fs2nc_regex <- list(
    "regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_([^\\_\\.]+)\\_([126]0)\\.?([^\\_]*)$",
    "elements" = c("mission","level","sensing_date","id_orbit","extent_name","prod_type","res","file_ext")
  )
  
  metadata <- data.frame(
    "type" = rep(NA, length(s2_names))
  ) # output object, with requested metadata
  
  s2_names <- basename(s2_names)
  
  # retrieve info
  for (sel_el in fs2nc_regex$elements) {
    # generic formattation
    metadata[,sel_el] <- gsub(
      fs2nc_regex$regex,
      paste0("\\",which(fs2nc_regex$elements==sel_el)),
      s2_names
    )
  }
  # specific formattations
  metadata[,"sensing_date"] <- as.Date(metadata[,"sensing_date"], format="%Y%m%d")
  if (nrow(metadata)>0) {
    metadata[,"res"] <- paste0(metadata[,"res"],"m")
    # retrieve type
    metadata$type <- ifelse(
      !grepl(fs2nc_regex$regex,s2_names), "unrecognised",
      ifelse(
        grepl("[0-9]{2}[A-Z]{3}",metadata$extent_name), "tile",
        ifelse(
          metadata$extent_name=="", "merged", "clipped"
        )
      )
    )
  } else {
    metadata$type <- as.character(metadata$type)
  }
  
  # manage unrecognised files 
  if (sum(metadata$type=="unrecognised") > 0) {
    print_message(
      type = if(abort==TRUE){"error"}else{"warning"},
      "\"",paste(s2_names[metadata$type=="unrecognised"], collapse="\", \""),
      "\" were not recognised."
    )
    metadata[metadata$type=="unrecognised",2:(length(fs2nc_regex$elements)+1)] <- NA
  }
  
  # return output
  if (format == "data.table") {
    return(data.table(metadata))
  } else if (format == "data.frame") {
    return(metadata)
  } else if (format == "list") {
    meta_list <- lapply(seq_along(s2_names), function(i) {
      l <- as.list(metadata[i,])
      l$sensing_date <- as.character(l$sensing_date)
      l[l==""|is.na(l)] <- NULL
      l
    })
    names(meta_list) <- s2_names
    if (length(meta_list)==1) {
      return(meta_list[[1]])
    } else {
      return(meta_list)
    }
    
  }
  
}

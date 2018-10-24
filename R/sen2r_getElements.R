#' @title Get information from S2 short name
#' @description This accessory function extracts metadata included in
#'  the name of a Sentinel-2 product which follows the sen2r
#'  naming convention (see [safe_shortname]).
#' @param s2_names A vector of Sentinel-2 product names in the
#'  sen2r naming convention.
#' @param format One between `list` of `data.frame`.
#' @param abort Logical parameter: if TRUE (default), the function aborts 
#'  in case any of `s2_names` is not recognised; if FALSE, a warning is shown,
#'  and a list with only the element "type"='unrecognised' is returned.
#' @return A list or a data.frame of the output metadata.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom data.table as.data.table
#' @examples
#' # Define product name
#' fs2nc_examplename <-
#'   "/path/of/the/product/S2A1C_20170603_022_32TQQ_TOA_20.tif"
#'
#' # Return metadata
#' sen2r_getElements(fs2nc_examplename)

sen2r_getElements <- function(s2_names, format="list", abort=TRUE) {
  
  # if input is NULL, return NULL
  if (is.null(s2_names)) {
    return(invisible(NULL))
  }
  
  # define regular expressions to identify products
  fs2nc_regex <- list(
    "tile" = list("regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([0-9A-Z]{5})\\_([A-Za-z0-9\\-\\:]+)\\_([126]0)\\.?([^\\_]*)$",
                  "elements" = c("mission","level","sensing_date","id_orbit","id_tile","prod_type","res","file_ext")),
    "merged" = list("regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_\\_([A-Za-z0-9\\-\\:]+)\\_([126]0)\\.?([^\\_]*)$",
                    "elements" = c("mission","level","sensing_date","id_orbit","prod_type","res","file_ext")),
    "clipped" = list("regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]+)\\_([A-Za-z0-9\\-\\:]+)\\_([126]0)\\.?([^\\_]*)$",
                     "elements" = c("mission","level","sensing_date","id_orbit","extent_name","prod_type","res","file_ext")))
  
  metadata <- list() # output object, with requested metadata
  
  s2_names <- basename(s2_names)
  
  for (i in seq_along(s2_names)) {
    
    s2_name <- s2_names[i]
    metadata[[i]] <- list()
    names(metadata)[i] <- s2_name
    
    # retrieve type
    if(length(grep(fs2nc_regex$tile$regex, s2_name))==1) {
      metadata[[i]]$type <- "tile"
    } else if(length(grep(fs2nc_regex$merged$regex, s2_name))==1) {
      metadata[[i]]$type <- "merged"
    } else if(length(grep(fs2nc_regex$clipped$regex, s2_name))==1) {
      metadata[[i]]$type <- "clipped"
    } else {
      print_message(
        type=if(abort==TRUE){"error"}else{"warning"},
        "\"",s2_name,"\" was not recognised.")
      metadata[[i]]$type <- "unrecognised" # so not to enter in the next cycle
    }
    
    # retrieve info
    for (sel_el in fs2nc_regex[[metadata[[i]]$type]]$elements) {
      # generic formattation
      metadata[[i]][[sel_el]] <- gsub(
        fs2nc_regex[[metadata[[i]]$type]]$regex,
        paste0("\\",which(fs2nc_regex[[metadata[[i]]$type]]$elements==sel_el)),
        s2_name)
      # specific formattations
      if (sel_el=="sensing_date") {
        metadata[[i]][[sel_el]] <- as.Date(metadata[[i]][[sel_el]], format="%Y%m%d")
      }
      if (sel_el=="res") {
        metadata[[i]][[sel_el]] <- paste0(metadata[[i]][[sel_el]],"m")
      }
    }
    
  } # end of prod cycle
  
  # return output
  if (format=="data.frame") {
    return(as.data.frame(
      do.call(
        "rbind", 
        c(lapply(metadata, as.data.table, stringsAsFactors=FALSE), 
          fill=TRUE)
      )
    ))
  }
  
  if (format!="list") {
    print_message(
      type="warning",
      "Argument must be one between 'data.frame' and 'list'.",
      "Returnig a list.")
  }
  if (length(metadata)==1) {
    return(metadata[[1]])
  } else {
    return(metadata)
  }
  
}

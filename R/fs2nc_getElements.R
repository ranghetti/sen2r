#' @title Get information from S2 short name
#' @description This accessory function extracts metadata included in
#'  the name of a Sentinel-2 product which follows the fidolasen-S2
#'  naming convention (see [s2_shortname]).
#' @param s2_name A Sentinel-2 product name in the fidolasen-S2
#'  naming convention.
#' @return A list of the output metadata.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#'
#' @examples
#' # Define product name
#' fs2nc_examplename <-
#'   "/path/of/the/product/S2A1C_20170603_022_32TQQ.tif"
#'
#' # Return metadata
#' fs2nc_getElements(fs2nc_examplename)

fs2nc_getElements <- function(s2_name) {

  # define regular expressions to identify products
  fs2nc_regex <- list(
    "tile" = list("regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([A-Z0-9]{5})\\.?(.*)$",
                  "elements" = c("mission","level","sensing_date","id_orbit","id_tile","file_format")),
    "merged" = list("regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\.?(.*)$",
                    "elements" = c("mission","level","sensing_date","id_orbit","file_format")))

  metadata <- list() # output object, with requested metadata

  s2_name <- basename(s2_name)

  # retrieve type
  if(length(grep(fs2nc_regex$tile$regex, s2_name))==1) {
    metadata$type <- "tile"
  } else if(length(grep(fs2nc_regex$merged$regex, s2_name))==1) {
    metadata$type <- "merged"
  } else {
    print_message(
      type="error",
      "\"",s2_name,"\" was not recognised.")
  }

  # retrieve info
  for (sel_el in fs2nc_regex[[metadata$type]]$elements) {
    metadata[[sel_el]] <- gsub(
      fs2nc_regex[[metadata$type]]$regex,
      paste0("\\",which(fs2nc_regex[[metadata$type]]$elements==sel_el)),
      s2_name)
    # format if it is a date
    if (sel_el=="sensing_date") {
      metadata[[sel_el]] <- as.Date(metadata[[sel_el]], format="%Y%m%d")
    }
  }

  return(metadata)

}

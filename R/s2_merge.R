#' @title Merge S2 tiles with the same date and orbit
#' @description The function merge the input Sentinel-2 products with
#'  the same date and orbit number. Outputs are a set of products in
#'  the specified format.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a format managed by
#'  GDAL (use [s2_translate] to do it); their names must be in the
#'  fidolasen-S2 naming convention ([s2_shortname]).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default value is "VRT" (Virtual Raster).
#' @param compress (optional) In the case a GTiff format is
#'  chosen, the compression indicated with this parameter is used.
#' @return A vector with the names of the merged products.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0



s2_merge <- function(infiles,
                     outdir=".",
                     format="VRT",
                     compress="DEFLATE") {

  # Check that files exist
  if (!any(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      "The input files do not exists locally; please check file names and paths.")
  } else if (!all(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      "Some of the input files (\"",
      paste(infiles[!sapply(infiles, file.exists)], collapse="\", \""),
      "\") do not exists locally; please check file names and paths.")
  }

  # Get files metadata
  infiles_meta <- lapply(infiles, fs2nc_getElements)

  # Group by orbit id
  # ids_orbit <-

  # WIP





  # TODO
  return(NULL)

}

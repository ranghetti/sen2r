#' @title Warp basing on the grid of another file
#' @description The function apply `gdalwarp`` to build rasters with the
#'  same projection, resolution and grid alignment of another raster.
#'  If not specified, the output format of each file is the same of the
#'  corresponding source file.
#' @param srcfiles A vector of input file paths (managed by GDAL).
#' @param dstfiles A vector of input file paths.
#' @param ref Path of the raster taken as reference.
#' @param of The output format (use the short format name). Default is
#'  the format of every input filename.
#' @param r Resampling_method ("near"|"bilinear"|"cubic"|"cubicspline"|
#' "lanczos"|"average"|"mode"|"max"|"min"|"med"|"q1"|"q3").
#' @return NULL (the function is called for its side effects)
#' @importFrom methods as
#' @importFrom reticulate py_to_r
#' @importFrom sf st_as_sfc
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples
#' # Define file names
#' ex_sel <- system.file(
#'   "extdata/example_files/out_ref/S2A2A_20170703_022_Barbellino_BOA_10.tif",
#'   package = "sen2r"
#' )
#' ex_ref <- system.file(
#'   "extdata/example_files/out_ref/S2A2A_20170703_022_Barbellino_SCL_10.tif",
#'   package = "sen2r"
#' )
#' ex_out <- tempfile(fileext = "_BOA_out.tif")
#'
#' # Run function
#' sen2r:::gdalwarp_grid(ex_sel, ex_out, ref = ex_ref)
#' 
#' \donttest{
#' # Show output
#' par(mfrow = c(1,3))
#' par(mar = rep(0,4)); image(stars::read_stars(ex_sel), rgb = 4:2, maxColorValue = 3500)
#' par(mar = rep(2/3,4)); image(stars::read_stars(ex_ref))
#' par(mar = rep(0,4)); image(stars::read_stars(ex_out), rgb = 4:2, maxColorValue = 3500)
#' }

gdalwarp_grid <- function(srcfiles,
                          dstfiles,
                          ref,
                          of = NULL,
                          r = NULL) {
  
  # import python modules
  py <- init_python()
  
  # read ref parameters
  ref_metadata <- raster_metadata(ref, c("res", "bbox", "proj"), format = "list")[[1]]
  ref_res <- ref_metadata$res
  ref_min <- ref_metadata$bbox[c("xmin","ymin")]
  ref_proj <- ref_metadata$proj
  
  # check consistency between inputs and outputs
  if (length(srcfiles) != length(dstfiles)) {
    print_message(
      type="error", 
      "\"srcfiles\" (\"",
      paste(srcfiles, collapse="\", \""),
      "\") and \"dstfiles\" (\"",
      paste(dstfiles, collapse="\", \""),
      "\") must be of the same length."
    )
  }
  
  # check output format
  if (!is.null(of)) {
    sel_driver <- py$gdal$GetDriverByName(of)
    if (is.null(py_to_r(sel_driver))) {
      print_message(
        type="error",
        "Format \"",of,"\" is not recognised; ",
        "please use one of the formats supported by your GDAL installation.\n\n",
        "To list them, use the following command:\n",
        "gdalUtils::gdalinfo(formats=TRUE)\n\n",
        "To search for a specific format, use:\n",
        "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
    }
  }
  
  # cycle on each infiles
  for (i in seq_along(srcfiles)) {
    srcfile <- srcfiles[i]
    dstfile <- dstfiles[i]
    
    # read infile parameters
    sel_metadata <- raster_metadata(srcfile, c("proj", "bbox", "outformat"), format = "list")[[1]]
    sel_proj <- sel_metadata$proj
    sel_bbox <- sel_metadata$bbox
    of <- ifelse (is.null(of), sel_metadata$outformat, of)
    
    # get reprojected extent
    out_bbox <- matrix(
      st_bbox(st_transform(st_as_sfc(sel_bbox), ref_proj)), 
      nrow=2, ncol=2, 
      dimnames=list(c("x","y"),c("min","max"))
    )
    
    # allineate out_extent to ref grid
    out_bbox_mod <- ceiling((out_bbox - ref_min) / ref_res) * ref_res + ref_min
    
    
    # warp
    system(
      paste0(
        load_binpaths()$gdalwarp," ",
        "-s_srs \"",sel_proj$proj4string,"\" ",
        "-t_srs \"",ref_proj$proj4string,"\" ",
        "-te ",paste(out_bbox_mod, collapse = " ")," ",
        "-tr ",paste(ref_res, collapse = " ")," ",
        if (!is.null(r)) {paste0("-r ",r," ")},
        if (!is.null(of)) {paste0("-of ",of," ")},
        "\"",srcfile,"\" ",
        "\"",dstfile,"\""),
      intern = Sys.info()["sysname"] == "Windows"
    )

  }
  
}

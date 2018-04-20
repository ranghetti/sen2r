#' @title Warp basing on the grid of another file
#' @description The function apply [gdalwarp] to build rasters with the
#'  same projection, resolution and grid alignment of another raster.
#'  If not specified, the output format of each file is the same of the
#'  corresponding source file.
#' @param srcfiles A vector of input file paths (managed by GDAL).
#' @param dstfiles A vector of input file paths.
#' @param ref Path of the raster taken as reference.
#' @param of The output format (use the short format name). Default is
#'  the format of every input filename.
#' @param ... Additional parameters of [gdalwarp] (different from `s_srs`,
#'  `t_srs`, `te`, `tr`, `ts` and `of`).
#' @return NULL
#' @importFrom rgdal GDALinfo
#' @importFrom gdalUtils gdalwarp
#' @importFrom methods as
#' @importFrom reticulate py_to_r
#' @importFrom sf st_as_sfc
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples
#' \dontrun{
#' ex_sel <- c("/path/of/existing/input/file.tif",
#'             "/path/of/existing/input/anotherfile.jp2")
#' ex_ref <- "/path/of/the/reference/file.jp2"
#' ex_out <- c("/path/of/the/output/file.tif",
#'             "/path/of/the/output/anotherfile.jp2")
#'
#' gdalwarp_grid(ex_sel, ex_out, ex_ref, dstnodata=0, overwrite=TRUE)
#' }

gdalwarp_grid <- function(srcfiles,
                          dstfiles,
                          ref,
                          of = NULL,
                          ...) {
  
  # import python modules
  py <- init_python()
  
  # read ref parameters
  ref_metadata <- suppressWarnings(GDALinfo(ref))
  ref_res <- ref_metadata[c("res.x","res.y")]
  ref_min <- ref_metadata[c("ll.x","ll.y")]
  ref_proj <- attr(ref_metadata, "projection")
  
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
    sel_metadata <- suppressWarnings(GDALinfo(srcfile))
    sel_res <- sel_metadata[c("res.x","res.y")]
    sel_proj <- attr(sel_metadata, "projection")
    sel_bbox <- c(
      sel_metadata[c("ll.x","ll.y")],
      sel_metadata[c("ll.x","ll.y")] + sel_metadata[c("rows","columns")] * sel_res)
    names(sel_bbox) <- c("xmin", "ymin", "xmax", "ymax")
    sel_bbox <- st_bbox(sel_bbox, crs = sel_proj)
    of <- ifelse (is.null(of), attr(sel_metadata, "driver"), of)
    
    # get reprojected extent
    out_bbox <- matrix(
      st_bbox(st_transform(st_as_sfc(sel_bbox), ref_proj)), 
      nrow=2, ncol=2, 
      dimnames=list(c("x","y"),c("min","max"))
    )
    
    # allineate out_extent to ref grid
    out_bbox_mod <- round((out_bbox - ref_min) / ref_res) * ref_res + ref_min

    
    
    
    
    # warp
    # (using gdalwarp() instead of calling gdalwarp from system() is a bit slower,
    # but it easily allows to pass additional parameters)
    gdalwarp(srcfile = srcfile, dstfile = dstfile,
             s_srs = sel_proj, t_srs = ref_proj,
             te = c(out_bbox_mod),
             tr = ref_res,
             of = of,
             ...)
    
  }
  
}

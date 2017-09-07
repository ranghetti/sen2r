#' @title Clip, reproject and warp raster files
#' @description The function applies [gdal_translate] or [gdalwarp]
#'  to clip, reproject and/or warp raster files. The choice of the
#'  algorythm is based on the comparison between input and output
#'  projections ([gdal_translate] if they are equal, [gdalwarp] elsewhere).
#'  If not specified, the output format of each file is the same of the
#'  corresponding source file.
#' @param srcfiles A vector of input file paths (managed by GDAL).
#' @param dstfiles A vector of corresponding output file paths.
#' @param of The output format (use the short format name). Default is
#'  the format of every input filename.
#' @param ref Path of the raster taken as reference: if provided, other
#'  parameters regarding the output grid (`te`, `tr`, `t_srs`) are ignored,
#'  and the output files are built with the same grid (alignment and
#'  resolution) of `ref` raster.
#' @param tr Numeric. (`c(xres,yres)``). set output file resolution
#'  (in target georeferenced units).
#' @param te Georeferenced extents of output file to be created
#'  (xmin,ymin,xmax,ymax)
#' @param t_srs A vector of corresponding output file paths.
#' @param r A vector of corresponding output file paths.
#' @param dstnodata A vector of corresponding output file paths.
#' @param ... Additional parameters of [gdalwarp] or [gdal_translate]
#'  (different from `s_srs`, `t_srs`, `te`, `tr`, `ts` and `of`).
#' @return NULL
#' @export
#' @importFrom rgdal GDALinfo CRSargs
#' @importFrom gdalUtils gdalwarp gdal_translate
#' @importFrom sp CRS
#' @importFrom methods as
#' @importFrom reticulate import py_to_r
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
#' gdal_warp(ex_sel, ex_out, ex_ref, dstnodata=0, overwrite=TRUE)
#' }

gdal_warp <- function(srcfiles,
                      dstfiles,
                      of = NULL,
                      ref = NULL,
                      tr = NULL,
                      te = NULL,
                      t_srs = NULL,
                      r = NULL,
                      dstnodata = NULL,
                      ...) {

  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal

  # check consistency between inputs and outputs
  if (length(srcfiles) != length(dstfiles)) {
    print_message(type="error", "\"srcfiles\" and \"dstfiles\" must be of the same length.")
  }

  # check output format
  if (!is.null(of)) {
    sel_driver <- gdal$GetDriverByName(of)
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

  # if "ref" is specified, read ref parameters
  if (!is.null(ref)) {
    ref_metadata <- suppressWarnings(GDALinfo(ref))
    ref_res <- ref_metadata[c("res.x","res.y")]
    ref_ll <- ref_metadata[c("ll.x","ll.y")]
    ref_size <- ref_metadata[c("columns","rows")]
    ref_bbox <- matrix(
      c(ref_ll, ref_ll + ref_size * ref_res),
      ncol=2)
    dimnames(ref_bbox) <- list(c("x","y"),c("min","max"))
    t_srs <- attr(ref_metadata, "projection") %>%
      CRS() %>% CRSargs()
    # round "tr" to ref grid
    if (is.null(tr)) {
      tr <- ref_res
    } else {
      tr <- ref_size*ref_res/round((ref_size*ref_res)/tr)
    }
  }

  # cycle on each infiles
  for (i in seq_along(srcfiles)) {
    srcfile <- srcfiles[i]
    dstfile <- dstfiles[i]

    # read infile parameters
    sel_metadata <- suppressWarnings(GDALinfo(srcfile))
    sel_res <- sel_metadata[c("res.x","res.y")]
    sel_s_srs <- attr(sel_metadata, "projection") %>%
      CRS() %>% CRSargs()
    sel_bbox <- matrix(
      c(sel_metadata[c("ll.x","ll.y")],
        sel_metadata[c("ll.x","ll.y")] + sel_metadata[c("columns","rows")] * sel_res),
      ncol=2)
    dimnames(sel_bbox) <- list(c("x","y"),c("min","max"))
    sel_extent <- get_extent(sel_bbox, sel_s_srs)
    sel_of <- ifelse (is.null(of), attr(sel_metadata, "driver"), of)

    # set default parameter values (if not specified)
    sel_t_srs <- ifelse(is.null(t_srs), sel_s_srs, t_srs)
    sel_tr <- if (is.null(tr)) {sel_res} else {tr}
    # default method: near if the target resolution is lower than an half of the source,
    # mode elsewhere
    sel_r <- if (is.null(r)) {
      ifelse(all(2*tr < sel_res), "near", "mode")
    } else {
      r
    }

    # get reprojected extent
    suppressWarnings(out_extent <- reproj_extent(sel_extent, sel_t_srs))

    # allineate out_extent to ref grid
    if (is.null(ref)) {
      if (is.null(te)) {
        sel_te <- as(out_extent, "matrix")
      } else if (any(is.na(te))) {
        sel_te <- as(out_extent, "matrix")
      } else {
        sel_te <- te
      }
    } else {
      if (is.null(te)) {
        sel_te <- ref_bbox
      } else if (any(is.na(te))) {
        sel_te <- as(out_extent, "matrix")
      } else {
        sel_te <- (as(out_extent, "matrix") - ref_ll) / tr
        sel_te <- cbind(floor(sel_te[,1]), ceiling(sel_te[,2]))
        dimnames(sel_te) <- list(c("x","y"),c("min","max"))
        sel_te <- sel_te * tr + ref_ll
      }
    }

    # use gdal_translate if:
    # - t_srs = s_srs, and
    # - align = TRUE
    # gdalwarp elsewhere

    if (sel_t_srs == sel_s_srs) {
      gdal_translate(src_dataset = srcfile, dst_dataset = dstfile,
               projwin = sel_te[c(1,4,3,2)],
               tr = sel_tr,
               of = sel_of,
               r = sel_r,
               a_nodata = dstnodata,
               ...)
    } else {
      gdalwarp(srcfile = srcfile, dstfile = dstfile,
               s_srs = sel_s_srs, t_srs = sel_t_srs,
               te = c(sel_te),
               tr = sel_tr,
               of = sel_of,
               r = sel_r,
               dstnodata = dstnodata,
               ...)
    }

  }

}

#' @title Warp a raster file aligning it on the grid of another file
#' @description The function applies `gdalwarp` to build rasters with the
#'  same projection, resolution and grid alignment of another raster.
#'  If not specified, the output format of each file is the same of the
#'  corresponding source file.
#' @param srcfiles A vector of input file paths (managed by GDAL).
#' @param dstfiles A vector of input file paths.
#' @param ref Path of the raster taken as reference.
#' @param of The output format (use the short format name). Default is
#'  the format of every input filename.
#' @param r Resampling_method (`"near"`|`"bilinear"`|`"cubic"`|`"cubicspline"`|
#' `"lanczos"`|`"average"`|`"mode"`|`"max"`|`"min"`|`"med"`|`"q1"`|`"q3"`).
#' @param tmpdir (optional) Path where intermediate files (.prj) will be created.
#'  Default is a temporary directory.
#' @return NULL (the function is called for its side effects)
#' @importFrom sf st_as_sfc st_bbox st_transform st_drivers
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @examples
#' \donttest{
#' # Define file names
#' ex_sel <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif",
#'   package = "sen2r"
#' )
#' ex_ref <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_SCL_10.tif",
#'   package = "sen2r"
#' )
#' ex_out <- tempfile(fileext = "_BOA_out.tif")
#'
#' # Run function
#' sen2r:::gdalwarp_grid(ex_sel, ex_out, ref = ex_ref)
#' 
#' # Show output
#' oldpar <- par(mfrow = c(1,3), mar = rep(0,4))
#' image(stars::read_stars(ex_sel), rgb = 4:2, maxColorValue = 3500)
#' par(mar = rep(2/3,4)); image(stars::read_stars(ex_ref))
#' par(mar = rep(0,4)); image(stars::read_stars(ex_out), rgb = 4:2, maxColorValue = 3500)
#' par(oldpar)
#' }

gdalwarp_grid <- function(
  srcfiles,
  dstfiles,
  ref,
  of = NULL,
  r = NULL,
  tmpdir = tempdir()
) {
  
  # read ref parameters
  ref_metadata <- raster_metadata(ref, c("res", "bbox", "proj"), format = "list")[[1]]
  ref_res <- ref_metadata$res
  ref_min <- ref_metadata$bbox[c("xmin","ymin")]
  ref_proj <- ref_metadata$proj
  
  # check tmpdir
  dir.create(tmpdir, showWarnings = FALSE, recursive = FALSE)
  
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
    local_ofs <- as.character(st_drivers("raster")$name)
    if (!of %in% local_ofs) {
      print_message(
        type="error",
        "Format \"",of,"\" is not recognised; ",
        "please use one of the formats supported by package \"sf\".\n\n",
        "To list them, use the following command:\n",
        "\u00A0\u00A0sf::st_drivers(\"raster\")")
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
      st_bbox(st_transform(st_as_sfc(sel_bbox), st_crs2(ref_proj))), 
      nrow=2, ncol=2, 
      dimnames=list(c("x","y"),c("min","max"))
    )
    
    # allineate out_extent to ref grid
    out_bbox_mod <- ceiling((out_bbox - ref_min) / ref_res) * ref_res + ref_min
    
    # extract out CRS string
    sel_proj_string <- if (!is.na(sel_proj$epsg)) {
      paste0("EPSG:",sel_proj$epsg)
    } else {
      writeLines(
        st_as_text_2(sel_proj),
        sel_proj_path <- tempfile(pattern = "sel_proj_", tmpdir = tmpdir, fileext = ".prj")
      )
      sel_proj_path
    }
    ref_proj_string <- if (!is.na(ref_proj$epsg)) {
      paste0("EPSG:",ref_proj$epsg)
    } else {
      writeLines(
        st_as_text_2(ref_proj),
        ref_proj_path <- tempfile(pattern = "ref_proj_", tmpdir = tmpdir, fileext = ".prj")
      )
      ref_proj_path
    }
    
    # warp
    gdalUtil(
      "warp",
      source = srcfile,
      destination = dstfile,
      options = c(
        "-s_srs", sel_proj_string,
        "-t_srs", ref_proj_string,
        "-te", c(out_bbox_mod),
        "-tr", as.vector(ref_res),
        if (!is.null(r)) {c("-r", r)},
        if (!is.null(of)) {c("-of", of)}
      ),
      quiet = TRUE
    )
    
  }
  
}

#' @title Clip, reproject and warp raster files
#' @description The function applies `gdalwarp`
#'  to clip, reproject and/or warp raster files.
#'  If not specified, the output format of each file is the same of the
#'  corresponding source file.
#' @param srcfiles A vector of input file paths (managed by GDAL).
#' @param dstfiles A vector of corresponding output file paths.
#' @param of The output format (use the short format name). Default is
#'  the format of every input filename.
#' @param co Character. Passes a creation option to the output format driver.
#'  Multiple -co options may be listed. See format specific documentation
#'  for legal creation options for each format.
#' @param ref Path of the raster taken as reference: if provided,
#'  parameters regarding the output grid (alignment, resolution and
#'  extent) are taken from this raster. To set differently some of
#'  these values, specify also other values of `mask` and/or `tr`.
#'  `t_srs` parameter value is always ignored when `ref` is provided.
#' @param mask Spatial path or object from which to take the extent
#'  of output files. If it is a polygon, this is used as masking layer;
#'  otherwise, only the bounding box is considered. If both `ref`
#'  and `mask` are provided, this parameter will overlay the extent of the
#'  reference raster. In order to take only the grid from `res` and not
#'  to clip on its extent, set `mask=NA`. Notice that the output
#'  projection is never taken from `mask`.
#' @param tr Numeric. (`c(xres,yres)`). set output file resolution
#'  (in target georeferenced units). If bot `ref` and `tr` are provided,
#'  `tr` is rounded in order to match the exact extent.
#' @param t_srs Target spatial reference set (character). The coordinate
#'  systems that can be passed are anything supported by [st_crs2].
#' @param r Resampling_method (`"near"`|`"bilinear"`|`"cubic"`|`"cubicspline"`|
#' `"lanczos"`|`"average"`|`"mode"`|`"max"`|`"min"`|`"med"`|`"q1"`|`"q3"`).
#' @param dstnodata Set nodata values for output bands (different values
#'  can be supplied for each band). If more than one value is supplied
#'  all values should be quoted to keep them together as a single
#'  operating system argument. New files will be initialized to this
#'  value and if possible the nodata value will be recorded in the output
#'  file. Use a value of NA to ensure that nodata is not defined.
#'  A vector with the same length of `srcfiles` can be supplied, in order to
#'  specify different nodata values for each input file.
#'  If this argument is not used then nodata values will be copied from
#'  the source datasets. At the moment it is not possible to set different
#'  values for different `srcfiles` (use multiple calls of the functions).
#' @param tap Logical (target aligned pixels) align the coordinates of the
#'  extent of the output file to the values of the `tr`, such that the aligned
#'  extent includes the minimum extent. Default is FALSE.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param tmpdir (optional) Path where intermediate files (maskfile)
#'  will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @return NULL (the function is called for its side effects)
#' @export
#' @importFrom sf st_transform st_geometry st_geometry_type st_write st_cast st_zm
#'  st_area st_bbox st_sfc st_sf st_polygon st_as_sf st_as_sfc st_as_sf st_crs
#'  st_as_text gdal_utils
#' @importFrom methods is
#' @importFrom stars read_stars
#' @author Luigi Ranghetti, phD (2019)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @examples
#' \donttest{
#' #' # Define file names
#' ex_sel <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_RGB432B_10.tif",
#'   package = "sen2r"
#' )
#' ex_ref <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_SCL_10.tif",
#'   package = "sen2r"
#' )
#' crop_poly <- system.file("extdata/vector/dam.geojson", package = "sen2r")
#' crop_line <- sf::st_cast(sf::read_sf(crop_poly), "LINESTRING")
#'
#' # Simple clip
#' test1 <- tempfile(fileext = "_test1.tif")
#' gdal_warp(ex_sel, test1, mask = crop_line)
#'
#' # Clip and mask
#' test2 <- tempfile(fileext = "_test2.tif")
#' gdal_warp(ex_sel, test2, mask = crop_poly)
#'
#' # Show output
#' crop_bbox <- sf::st_as_sfc(sf::st_bbox(crop_line))
#' oldpar <- par(mfrow = c(1,3), mar = rep(0,4))
#' image(stars::read_stars(ex_sel), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line, add = TRUE, col = "blue", lwd = 2)
#' plot(crop_bbox, add = TRUE, border = "red", lwd = 2)
#' image(stars::read_stars(test1), rgb = 1:3, useRaster = TRUE)
#' plot(crop_bbox, add = TRUE, border = "red", lwd = 2)
#' image(stars::read_stars(test2), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line, add = TRUE, col = "blue", lwd = 2)
#'
#' # Warp on a reference raster
#' test3 <- tempfile(fileext = "_test3.tif")
#' gdal_warp(ex_sel, test3, ref = ex_ref)
#'
#' # Show output
#' par(mfrow = c(1,3))
#' par(mar = rep(0,4)); image(stars::read_stars(ex_sel), rgb = 1:3, useRaster = TRUE)
#' par(mar = rep(2/3,4)); image(stars::read_stars(ex_ref), useRaster = TRUE)
#' par(mar = rep(0,4)); image(stars::read_stars(test3), rgb = 1:3, useRaster = TRUE)
#'
#' # Reproject all the input file
#' test4 <- tempfile(fileext = "_test4.tif")
#' gdal_warp(ex_sel, test4, t_srs = 32631)
#'
#' # Reproject and clip on a bounding box
#' test5 <- tempfile(fileext = "_test5.tif")
#' gdal_warp(ex_sel, test5, t_srs = "EPSG:32631", mask = stars::read_stars(test1))
#'
#' # Reproject and clip on polygon (masking outside)
#' test6 <- tempfile(fileext = "_test6.tif")
#' gdal_warp(ex_sel, test6, t_srs = "31N", mask = crop_poly)
#'
#' # Show output
#' crop_line_31N <- sf::st_transform(crop_line, 32631)
#' test1_bbox <- sf::st_as_sfc(sf::st_bbox(stars::read_stars(test1)))
#' test1_bbox_31N <- sf::st_transform(test1_bbox, 32631)
#' par(mfrow = c(1,4), mar = rep(0,4))
#' image(stars::read_stars(ex_sel), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line, add = TRUE, col = "blue", lwd = 2)
#' plot(test1_bbox, add = TRUE, border = "red", lwd = 2)
#' image(stars::read_stars(test4), rgb = 1:3, useRaster = TRUE)
#' image(stars::read_stars(test5), rgb = 1:3, useRaster = TRUE)
#' plot(test1_bbox_31N, add = TRUE, border = "red", lwd = 2)
#' image(stars::read_stars(test6), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line_31N, add = TRUE, col = "blue", lwd = 2)
#'
#' # Use a reference raster with a different projection
#' test7 <- tempfile(fileext = "_test7.tif")
#' gdal_warp(ex_sel, test7, ref = test6)
#'
#' # Use a reference raster with a different projection
#' # and specify a different bounding box
#' test8 <- tempfile(fileext = "_test8.tif")
#' gdal_warp(ex_sel, test8, mask = stars::read_stars(test1), ref = test6)
#'
#' # Use a reference raster with a different projection and a mask
#' test9 <- tempfile(fileext = "_test9.tif")
#' gdal_warp(ex_sel, test9, mask = crop_poly, ref = test6)
#'
#' # Show output
#' par(mfrow = c(1,4), mar = rep(0,4))
#' image(stars::read_stars(ex_sel), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line, add = TRUE, col = "blue", lwd = 2)
#' image(stars::read_stars(test7), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line_31N, add = TRUE, col = "blue", lwd = 2)
#' image(stars::read_stars(test8), rgb = 1:3, useRaster = TRUE)
#' plot(test1_bbox_31N, add = TRUE, border = "red", lwd = 2)
#' image(stars::read_stars(test9), rgb = 1:3, useRaster = TRUE)
#' plot(crop_line_31N, add = TRUE, col = "blue", lwd = 2)
#' 
#' par(oldpar)
#' }

gdal_warp <- function(srcfiles,
                      dstfiles,
                      of = NULL,
                      co = NULL,
                      ref = NULL,
                      mask = NULL,
                      tr = NULL,
                      t_srs = NULL,
                      r = NULL,
                      dstnodata = NULL,
                      tap = FALSE,
                      overwrite = FALSE,
                      tmpdir = NA,
                      rmtmp = TRUE) {
  
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
  
  # check the length of dstnodata
  if (!is.null(dstnodata)) {
    if (length(dstnodata)==1) {
      dstnodata <- rep(dstnodata, length(srcfiles))
    } else if (length(dstnodata)!=length(srcfiles)) {
      print_message(
        type="error",
        "\"dstnodata\" must be of length 1",
        if (length(srcfiles) > 1) {
          paste0(" or ",length(srcfiles))
        },
        " (the length of \"srcfiles\")."
      )
    }
  }
  
  # check t_srs
  if (all(!is.null(t_srs), !is(t_srs, "crs"))) {
    tryCatch(
      t_srs <- st_crs2(t_srs),
      error = function (e) {
        print_message(
          type = "error",
          "The input CRS (",t_srs,") was not recognised."
        )
      }
    )
  }
  
  # check output format
  if (!is.null(of)) {
    gdal_formats <- fromJSON(
      system.file("extdata/settings/gdal_formats.json",package="sen2r")
    )$drivers
    sel_driver <- gdal_formats[gdal_formats$name==of,]
    if (nrow(sel_driver)==0) {
      print_message(
        type="error",
        "Format \"",of,"\" is not recognised; ",
        "please use one of the formats supported by your GDAL installation."#\n\n",
        # "To list them, use the following command:\n",
        # "gdalUtils::gdalinfo(formats=TRUE)\n\n",
        # "To search for a specific format, use:\n",
        # "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
      )
    }
  }
  
  # define tmpdir 
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="gdalwarp_")
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="gdalwarp_")))
  }
  
  # actions to perform if "mask" is specified
  if (
    !is.null(mask) && 
    (!inherits(mask, "logical") || inherits(mask, "logical") && !anyNA(mask))
  ) {
    
    # cast "mask" to sf
    mask <- st_zm(
      if (is(mask, "sf") | is(mask, "sfc")) {
        st_sf(mask)
      } else if (is(mask, "Spatial")) {
        st_as_sf(mask)
      } else if (is(mask, "Raster") | is(mask, "stars")) {
        st_as_sfc(st_bbox(mask))
      } else if (is(mask, "character")) {
        mask0 <- try(st_read(mask, quiet=TRUE), silent = TRUE)
        if (is(mask0, "sf")) {
          mask0
        } else {
          mask1 <- try(read_stars(mask, proxy=TRUE), silent = TRUE)
          if (is(mask0, "stars")) {
            st_as_sfc(st_bbox(mask1))
          } else {
            stop("'mask' is not a recognised spatial file.")
          }
        }
      } 
    )
    
    # Check that the polygon is not empty
    if (requireNamespace("units", quietly = TRUE)) {
      if (length(grep("POLYGON",st_geometry_type(mask)))>=1 &
          sum(st_area(st_geometry(mask))) <= 0*units::as_units("m^2")) {
        print_message(
          type = "error",
          "The polygon provided as mask cannot be empty."
        )
      }
    } else {
      print_message(
        type = "warning",
        "Pakcage \"units\" is required to properly check polygon masks not ",
        "to be empty; install it in order to perform also this check."
      )
      if (length(grep("POLYGON",st_geometry_type(mask)))>=1) {
        print_message(
          type = "error",
          "The polygon provided as mask cannot be empty."
        )
      }
    }
    
    # save as cropping cutline file (if cutline must be applied)
    dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
    if (length(grep("LINESTRING",st_geometry_type(mask)))==0) {
      st_write(
        st_cast(mask, "MULTIPOLYGON"),
        mask_file <- file.path(
          tmpdir, basename(tempfile(pattern = "mask_", fileext = ".shp"))
        ),
        quiet = TRUE
      )
    }
    
  }
  
  # if "ref" is specified, read ref parameters
  if (length(ref) > 0) {
    
    ref_metadata <- raster_metadata(ref, format = "list")[[1]]
    ref_res <- ref_metadata$res
    ref_size <- ref_metadata$size
    t_srs <- ref_metadata$proj
    ref_bbox <- ref_metadata$bbox
    ref_offset <- ref_bbox %% ref_res
    sel_of <- ifelse(is.null(of), ref_metadata$outformat, of)
    
    # round "tr" to ref grid
    if (is.null(tr)) {
      tr <- ref_res
    } else {
      tr <- ref_size*ref_res/round((ref_size*ref_res)/tr)
    }
    
    # compute "te"
    te <- if (is.null(mask)) {
      ref_bbox
    } else if (
      !inherits(mask, "logical") || inherits(mask, "logical") && !anyNA(mask)
    ) {
      te_1 <- st_bbox(st_transform(mask, t_srs))
      te_1 + c(0,0,ref_res) - (te_1-ref_offset) %% ref_res
    } else {
      NULL # define "sel_te" in the srcfile cycle
    }
    
  }
  
  
  # cycle on each srcfile
  for (i in seq_along(srcfiles)) {
    srcfile <- srcfiles[i]
    dstfile <- dstfiles[i]
    sel_nodata <- if (is.null(dstnodata)) {NULL} else {dstnodata[i]}
    
    # if dstfile already exists and overwrite==FALSE, do not proceed
    if (!file.exists(dstfile) | overwrite==TRUE) {
      
      # read infile parameters
      sel_metadata <- raster_metadata(srcfile, format = "list")[[1]]
      sel_res <- sel_metadata$res
      sel_size <- sel_metadata$size
      sel_s_srs <- sel_metadata$proj
      sel_bbox <- sel_metadata$bbox
      sel_ll <- sel_bbox[c("xmin","ymin")]
      sel_of <- ifelse(is.null(of), sel_metadata$outformat, of)
      
      # set default parameter values (if not specified)
      sel_t_srs <- if (is.null(t_srs)) {sel_s_srs} else {t_srs}
      sel_tr <- if (is.null(tr)) {sel_res} else {tr}
      # default method: near if the target resolution is lower than an half of the source,
      # mode elsewhere
      sel_r <- if (is.null(r)) {
        if (all(2*tr < sel_res)) {"near"} else {"mode"}
      } else {
        r
      }
      
      if (length(ref) > 0) {
        sel_te <- if (!is.null(te)) {
          te
        } else {
          sel_te_1 <- st_bbox(st_transform(st_as_sfc(sel_bbox), sel_t_srs))
          sel_te_1 + c(0,0,ref_res) - (sel_te_1-ref_offset) %% ref_res
        }
      }
      
      # define CRS strings
      sel_s_srs_string <- if (!is.na(sel_s_srs$epsg)) {
        paste0("EPSG:",sel_s_srs$epsg)
      } else {
        dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
        writeLines(
          st_as_text_2(sel_s_srs),
          sel_s_srs_path <- tempfile(pattern = "s_srs_", tmpdir = tmpdir, fileext = ".prj")
        )
        sel_s_srs_path
      }
      sel_t_srs_string <- if (!is.na(sel_t_srs$epsg)) {
        paste0("EPSG:",sel_t_srs$epsg)
      } else {
        dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
        writeLines(
          st_as_text_2(sel_t_srs),
          sel_t_srs_path <- tempfile(pattern = "t_srs_", tmpdir = tmpdir, fileext = ".prj")
        )
        sel_t_srs_path
      }
      
      # Is cropping needed?
      # if "ref" is defined, crop in the first step
      # (this is possible because the output grid is known)
      crop_in_step1 <- length(ref) > 0
      # if "res" not defined and "mask" is provided, crop in a separate step
      # (this is required because the grid applied by gdalwarp is unknown)
      crop_in_step2 <- !is.null(mask) &&
        (!inherits(mask, "logical") || inherits(mask, "logical") && !anyNA(mask)) &&
        length(ref) == 0

      # first gdal_warp application (all except cropping)
      if (crop_in_step2) {
        step1_dstfile <- file.path(
          tmpdir, 
          basename(tempfile(pattern = "warp_", fileext = ".vrt"))
        )
        step1_of <- "VRT"
        step1_co <- NULL
      } else {
        step1_dstfile <- dstfile
        step1_of <- sel_of
        step1_co <- co
      }
      gdalUtil(
        "warp",
        source = srcfile,
        destination = step1_dstfile,
        options = c(
          "-s_srs", sel_s_srs_string,
          "-t_srs", sel_t_srs_string,
          if (crop_in_step1) {c("-te", c(sel_te))},
          if (exists("mask_file")) {c("-cutline", mask_file)},
          if (any(!is.null(tr), tap == TRUE)) {c("-tr", as.vector(sel_tr))},
          if (!is.null(step1_of)) {c("-of",as.vector(step1_of))},
          if (!is.null(step1_co)) {unlist(lapply(step1_co, function(x){c("-co", x)}))},
          "-r", sel_r,
          if (!is.null(sel_nodata)) {
            if (is.na(sel_nodata)) {
              c("-dstnodata", "None")
            } else {
              c("-dstnodata", sel_nodata)
            }
          },
          if (tap == TRUE) {"-tap"},
          if (overwrite) {"-overwrite"}
        ),
        quiet = TRUE
      )
      
      if (crop_in_step2) {
        
        # retrieve gdalwarp_path1 grid info 
        step1_metadata <- raster_metadata(step1_dstfile, format = "list")[[1]]
        step1_res <- step1_metadata$res
        step1_offset <- step1_metadata$bbox %% step1_res
        
        # get reprojected extent
        sel_te_1 <- st_bbox(st_transform(mask, sel_t_srs))
        sel_te <- sel_te_1 + c(0,0,step1_res) - (sel_te_1-step1_offset) %% step1_res
        
        # final gdal_warp application (crop matching the out grid)
        gdalUtil(
          "warp",
          source = step1_dstfile,
          destination = dstfile,
          options = c(
            "-te", c(sel_te),
            if (tap == TRUE) {c("-tr", as.vector(sel_tr))},
            if (!is.null(of)) {c("-of",as.vector(sel_of))},
            if (!is.null(co)) {unlist(lapply(co, function(x){c("-co", x)}))},
            if (!is.null(sel_nodata)) {
              if (is.na(sel_nodata)) {
                c("-dstnodata", "None")
              } else {
                c("-dstnodata", sel_nodata)
              }
            },
            if (tap == TRUE) {"-tap"},
            if (overwrite) {"-overwrite"}
          ),
          quiet = TRUE
        )
        
      }
      
    } # end of overwrite IF cycle
    
  }
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
}

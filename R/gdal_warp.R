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
#'  otherwise, only the bounding box  is considered;. If both `ref`
#'  and `mask` are provided, this parameter will overlay the extent of the
#'  reference raster. In order to take only the grid from `res` and not
#'  to clip on its extent, set `mask=NA`. Notice that the output
#'  projection is never taken from `mask`.
#' @param tr Numeric. (`c(xres,yres)`). set output file resolution
#'  (in target georeferenced units). If bot `ref` and `tr` are provided,
#'  `tr` is rounded in order to match the exact extent.
#' @param t_srs Target spatial reference set (character). The coordinate
#'  systems that can be passed are anything supported by the
#'  OGRSpatialReference.SetFromUserInput() call, which includes EPSG
#'  PCS and GCSes (i.e. EPSG:4296), PROJ.4 declarations (as above),
#'  or the name of a .prf file containing well known text.
#' @param r Resampling_method ("near"|"bilinear"|"cubic"|"cubicspline"|
#' "lanczos"|"average"|"mode"|"max"|"min"|"med"|"q1"|"q3").
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
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param tmpdir (optional) Path where intermediate files (maskfile) 
#'  will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @return NULL (the function is called for its side effects)
#' @export
#' @importFrom sf st_transform st_geometry st_geometry_type st_write st_cast st_zm
#'  st_area st_bbox st_sfc st_sf st_polygon st_as_sf st_as_sfc st_as_sf st_crs
#' @importFrom methods as
#' @importFrom stars read_stars
#' @importFrom magrittr "%>%"
#' @importFrom units ud_units
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples
#' \dontrun{
#' srcfiles <- c("/path/of/a/s2/file.tif",
#'               "/path/of/another/s2/file.tif")
#' crop_poly <- c("/path/of/a/polygon/vector.shp")
#'
#' # Simple clip
#' gdal_warp(srcfiles[1],
#'           test0_clip <- file.path(tempdir(),"test0_clip.tif"),
#'           mask = get_extent(crop_poly))
#'
#' # Clip and mask
#' gdal_warp(srcfiles,
#'           test0_mask <- c(file.path(tempdir(),"test0_mask.tif"),
#'                           tempfile()),
#'           mask = crop_poly)
#'
#' # Warp on a reference raster
#' gdal_warp(srcfiles[1],
#'           test1 <- file.path(tempdir(),"test1.tif"),
#'           ref = test0_mask[1])
#'
#' # Reproject all the input file
#' gdal_warp(srcfiles[1],
#'           test2 <- file.path(tempdir(),"test2.tif"),
#'           t_srs = "+init=epsg:32631")
#'
#' # Reproject and clip on a bounding box
#' gdal_warp(srcfiles[1],
#'           test3a <- file.path(tempdir(),"test3a.tif"),
#'           mask = get_extent(crop_poly),
#'           t_srs = "+init=epsg:32631")
#' # Reproject and clip on polygon (masking outside)
#' gdal_warp(srcfiles[1],
#'           test3b <- file.path(tempdir(),"test3b.tif"),
#'           mask = crop_poly,
#'           t_srs = "+init=epsg:32631")
#' # Workaround to clip on a bounding box without
#' # enlarging it too much (cause of the reprojection)
#' gdal_warp(srcfiles[1],
#'           test3c <- file.path(tempdir(),"test3c.tif"),
#'           mask = st_cast(crop_poly,"LINESTRING"),
#'           t_srs = "+init=epsg:32631")
#'
#' # Use a reference raster with a different projection
#' gdal_warp(srcfiles[1],
#'           test4a <- file.path(tempdir(),"test4a.tif"),
#'           ref = test3b)
#' # Use a reference raster with a different projection
#' # and specify a different bounding box
#' gdal_warp(srcfiles[1],
#'           test4b <- file.path(tempdir(),"test4b.tif"),
#'           mask = test0_clip,
#'           ref = test3b)
#' # Use a reference raster with a different projection and a mask
#' gdal_warp(srcfiles[1],
#'           test4c <- file.path(tempdir(),"test4c.tif"),
#'           mask = crop_poly,
#'           ref = test3b)
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
  if (!is.null(t_srs)) {
    if (is(t_srs, "crs")) {
      t_srs <- t_srs$proj4string
    } else if (!is.na(st_crs(t_srs)$proj4string)) {
      t_srs <- st_crs(t_srs)$proj4string
    } else {
      print_message(
        type = "error",
        "The input CRS (",t_srs,") was not recognised."
      )
    }
  }
  
  # check output format
  if (!is.null(of)) {
    gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))$drivers
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
  
  # if "ref" is specified, read ref parameters
  if (!is.null(ref)) {
    ref_metadata <- raster_metadata(ref, format = "list")[[1]]
    ref_res <- ref_metadata$res
    ref_size <- ref_metadata$size
    t_srs <- ref_metadata$proj$proj4string
    ref_bbox <- ref_metadata$bbox
    ref_ll <- ref_bbox[c("xmin","ymin")]
    sel_of <- ifelse(is.null(of), ref_metadata$outformat, of)
    
    # round "tr" to ref grid
    if (is.null(tr)) {
      tr <- ref_res
    } else {
      tr <- ref_size*ref_res/round((ref_size*ref_res)/tr)
    }
  }
  
  # if "mask" is specified, take "mask" and "te" from it
  if (!is.null(mask)) {
    mask <- if (is(mask, "sf") | is(mask, "sfc")) {
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
    } %>% st_zm()
    
    # Check that the polygon is not empty
    if (length(grep("POLYGON",st_geometry_type(mask)))>=1 &
        sum(st_area(st_geometry(mask))) <= 0*units::ud_units$m^2) {
      print_message(
        type = "error",
        "The polygon provided as mask cannot be empty."
      )
    }
    # cast to multipolygon
    if (length(grep("POLYGON",st_geometry_type(mask)))>=1) {
      if (is.na(tmpdir)) {
        tmpdir <- tempfile(pattern="gdalwarp_")
      }
      dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
      st_write(
        st_cast(mask, "MULTIPOLYGON"),
        mask_file <- file.path(
          tmpdir, basename(tempfile(pattern = "mask_", fileext = ".shp"))
        ),
        quiet = TRUE
      )
    } # if not, mask_polygon is not created
    
    # create mask_bbox if t_srs is specified;
    # otherwise, create each time within srcfile cycle
    if (!is.null(t_srs)) {
      mask_bbox <- st_transform(mask, t_srs) %>%
        st_bbox() %>% 
        matrix(nrow=2, ncol=2, dimnames=list(c("x","y"),c("min","max")))
      # extent() %>% bbox()
      # get_extent() %>% as("matrix")
    }
  }
  
  # load binpaths
  binpaths <- load_binpaths()
  
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
      sel_s_srs <- sel_metadata$proj$proj4string
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
      
      # get reprojected extent
      # (if already set it was referring to mask; in this case, to srcfile)
      sel_src_bbox <- suppressMessages(
        matrix(
          st_bbox(st_transform(st_as_sfc(sel_bbox), sel_t_srs)), 
          nrow=2, ncol=2, 
          dimnames=list(c("x","y"),c("min","max"))
        )
      )
      
      # dimnames(sel_src_bbox) <- list(c("x","y"), c("min","max"))
      
      # set the correct bounding box for srcfile
      if (is.null(ref)) {
        if (is.null(mask)) {
          # ref NULL & mask NULL: use bbox of srcfile, reprojected
          sel_te <- sel_src_bbox
        } else if (class(mask)=="logical" && is.na(mask)) { # check if mask==NA
          # ref NULL & mask NA: the same (use bbox of srcfile, reprojected)
          sel_te <- sel_src_bbox
        } else {
          # ref NULL & mask provided: use bbox of mask, reprojected and aligned to src grid
          sel_mask_bbox <- if (exists("mask_bbox")) {
            mask_bbox
          } else {
            st_transform(mask, sel_t_srs) %>%
              st_bbox() %>% 
              matrix(nrow=2, ncol=2, dimnames=list(c("x","y"),c("min","max")))
            # get_extent() %>% as("matrix")
          }
          if (sel_t_srs == sel_s_srs) {
            sel_te <- (sel_mask_bbox - sel_ll) / sel_tr
            sel_te <- cbind(floor(sel_te[,1]), ceiling(sel_te[,2]))
            dimnames(sel_te) <- list(c("x","y"),c("min","max"))
            sel_te <- sel_te * sel_tr + sel_ll
          } else {
            sel_te <- sel_mask_bbox
          }
        }
      } else {
        if (is.null(mask)) {
          # ref provided & mask NULL: use bbox of ref
          sel_te <- ref_bbox
        } else if (class(mask)=="logical" && is.na(mask)) {
          # ref provided & mask NA: use bbox of srcfile (reprojected and aligned to ref grid)
          if (sel_t_srs == sel_s_srs) {
            sel_te <- (sel_src_bbox - ref_ll) / sel_tr
            sel_te <- cbind(floor(sel_te[,1]), ceiling(sel_te[,2]))
            dimnames(sel_te) <- list(c("x","y"),c("min","max"))
            sel_te <- sel_te * sel_tr + ref_ll
          } else {
            sel_te <- sel_mask_bbox
          }
        } else {
          # ref provided & mask provided: use bbox of mask (reprojected and aligned to ref grid)
          sel_mask_bbox <- if (exists("mask_bbox")) {
            mask_bbox
          } else {
            st_transform(mask, sel_t_srs) %>%
              st_bbox() %>% 
              matrix(nrow=2, ncol=2, dimnames=list(c("x","y"),c("min","max")))
            # get_extent() %>% as("matrix")
          }
          if (sel_t_srs == sel_s_srs) {
            sel_te <- (sel_mask_bbox - ref_ll) / sel_tr
            sel_te <- cbind(floor(sel_te[,1]), ceiling(sel_te[,2]))
            dimnames(sel_te) <- list(c("x","y"),c("min","max"))
            sel_te <- sel_te * sel_tr + ref_ll
          } else {
            sel_te <- sel_mask_bbox
          }
        }
      }
      
      # finally, apply gdal_warp or gdal_translate
      # temporary leave only gdal_warp to avoid some problems
      # (e.g., translating a 1001x1001 20m to 10m results in 2002x2002 instead of 200[12]x200[12])
      system(
        paste0(
          binpaths$gdalwarp," ",
          "-s_srs \"",sel_s_srs,"\" ",
          "-t_srs \"",sel_t_srs,"\" ",
          "-te ",paste(sel_te, collapse = " ")," ",
          if (exists("mask_file")) {paste0("-cutline \"",mask_file,"\" ")},
          if (!is.null(tr)) {paste0("-tr ",paste(sel_tr, collapse = " ")," ")},
          if (!is.null(of)) {paste0("-of ",sel_of," ")},
          if (!is.null(co)) {paste0("-co \"",co, "\" ")},
          "-r ",sel_r," ",
          if (!is.null(sel_nodata)) {
            if (is.na(sel_nodata)) {
              "-dstnodata None "
            } else {
              paste0("-dstnodata ",sel_nodata," ")
            }
          },
          if (overwrite) {"-overwrite "},
          "\"",srcfile,"\" ",
          "\"",dstfile,"\""
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )
      
    } # end of overwrite IF cycle
    
  }
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
}

#' @title Convert from SAFE format
#' @description The function build a virtual raster from a Sentinel2 SAFE product,
#'  eventually translating it in another spatial format.
#'  Output vrt is at 10m resolution.
#' @param infile Full path of the input SAFE folder (alternatively,
#'  full path of the xml file of the product with metadata).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the directory of `infile`.
#' @param subdirs (optional) Logical: if TRUE, different output products are
#'  placed in separated `outdir` subdirectories; if FALSE, they are placed in
#'  `outdir` directory; if NA (default), subdirectories are created only if
#'  `prod_type` has length > 1.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE).
#'  This parameter takes effect only if the output files are not VRT
#'  (in this case temporary files cannot be deleted, because rasters of source
#'  bands are included within them).
#' @param prod_type (optional) Vector of types to be produced as outputs
#'  (see [safe_shortname] for the list of accepted values). Default is
#'  reflectance (`"TOA"` for level 1C, `"BOA"` for level 2A).
#' @param tiles (optional) Character vector with the desired output tile IDs
#'  (id specified IDs are not present in the input SAFE product, they are not
#'  produced). Default (NA) is to process all the found tiles.
#' @param res (optional) Spatial resolution (one between `'10m'`, `'20m'` or 
#'  `'60m'`); default is `'10m'`.
#'  Notice that, choosing `'10m'` or `'20m'`, bands with lower
#'  resolution will be rescaled to `res`. Band 08 is used with `res = '10m'`,
#'  band 08A with `res = '20m'` and `res = '60m'`.
#' @param method (optional) A resampling method used to generate products 
#'  `"SZA"` (Sun Zenith Angles), `"OZA"` (Sun Azimuth Angles),
#'  `"SAA"` (averaged Viewing Incidence Zenith Angles) 
#'  and `"OAA"` (averaged Viewing Incidence Azimuth Angles) from their
#'  original 5 km resolution.
#'  Accepted values are valid values accepted by `-r` option of 
#'  [`gdalwarp`](https://gdal.org/programs/gdalwarp.html). 
#'  Default is `"bilinear"` (linear interpolation).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default value is `"VRT"` (Virtual Raster).
#' @param compress (optional) In the case a GeoTIFF format is
#'  chosen, the compression indicated with this parameter is used.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GeoTIFF format was chosen. 
#' @param vrt_rel_paths (optional) Logical: if TRUE (default on Linux),
#'  the paths present in the VRT output file are relative to the VRT position;
#'  if FALSE (default on Windows), they are absolute.
#'  This takes effect only with `format = "VRT"`.
#' @param utmzone (optional) UTM zone of output products (default:
#'  the first one retrieved from input granules),
#'  being a 3-length character (e.g. `"32N"`).
#'  Note that this function
#'  does not perform reprojections: if no granules refer to the specified
#'  UTM zone, no output is created.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @return A vector with the names of the created output files
#'   (just created or already existing).
#' @author Luigi Ranghetti, phD (2019)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' s2_l1c_example <- file.path(
#'   "/existing/path",
#'   "S2A_MSIL1C_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE"
#' )
#' s2_l2a_example <- file.path(
#'   "/existing/path",
#'   "S2A_MSIL2A_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE"
#' )
#'
#' # Create a single TOA GeoTIFF in the same directory
#' s2_translate(s2_l1c_example, format="GTiff")
#'
#' # Create a single BOA VRT with a custom name
#' s2_translate(
#'   s2_l2a_example,
#'   "/new/path/example_sentinel2_sr.vrt",
#'   vrt_rel_paths = TRUE
#' )
#'
#' # Create four products (ENVI) in the same directory at 60m resolution,
#' # using a cubic interpolation for "OAA"
#' s2_translate(
#'   s2_l2a_example, 
#'   format = "ENVI", 
#'   prod_type = c("BOA","TCI","SCL","OAA"),
#'   res = "60m", 
#'   method = "cubic",
#'   subdirs = TRUE
#' )
#' 
#' # Create all the four angle products from TOA in GeoTIFF format 
#' # in a temporary directory
#' s2_translate(
#'   s2_l1c_example, 
#'   format = "GTiff", 
#'   prod_type = c("SZA", "OZA", "SAA", "OAA"), 
#'   outdir = tempdir()
#' )
#'}

s2_translate <- function(
    infile,
    outdir = ".",
    subdirs = NA,
    tmpdir = NA,
    rmtmp = TRUE,
    prod_type = NULL,
    tiles = NA,
    res = "10m",
    method = "bilinear",
    format = "VRT",
    compress = "DEFLATE",
    bigtiff = FALSE,
    vrt_rel_paths = NA,
    utmzone = "",
    overwrite = FALSE
) {
  
  # Define vrt_rel_paths
  if (is.na(vrt_rel_paths)) {
    vrt_rel_paths <- Sys.info()["sysname"] != "Windows"
  }
  
  # check res (and use the resolutions >= specified res)
  if (!res %in% c("10m","20m","60m")) {
    print_message(
      type="error",
      "\"res\" value is not recognised (accepted values are '10m', '20m' ",
      "and '60m').")
  }
  if (res == "10m") {
    res <- c("10m","20m","60m")
  } else if (res == "20m") {
    res <- c("20m","60m")
  }
  
  # Check "tiles" argument
  if (is.null(tiles)) {tiles <- NA}
  if (anyNA(tiles)) {tiles <- NA} else if (all(tiles=="")) {tiles <- NA}
  
  # check output format
  gdal_formats <- fromJSON(
    system.file("extdata/settings/gdal_formats.json",package="sen2r")
  )$drivers
  sel_driver <- gdal_formats[gdal_formats$name==format,]
  if (nrow(sel_driver)==0) {
    print_message(
      type="error",
      "Format \"",format,"\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "\u00A0\u00A0gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "\u00A0\u00A0gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
  }
  
  # check method
  if (!method %in% c(
    "near", "bilinear", "cubic", "cubicspline", "lanczos", "average",
    "rms", "mode", "max", "min", "med", "q1", "q3", "sum"
  )) {
    print_message(
      type = "warning",
      "Resampling method \"",method,"\" is not recognised; using \"bilinear\"."
    )
    method <- "bilinear"
  }
  
  # Retrieve xml required metadata
  infile_meta <- safe_getMetadata(
    infile, 
    info = c("xml_main","xml_granules","utm","level","tiles", "jp2list", "footprint", "offset"),
    format = "list", simplify = TRUE
  )
  infile_dir = dirname(infile_meta$xml_main)
  
  # create outdir if not existing (and dirname(outdir) exists)
  suppressWarnings(outdir <- expand_path(outdir, parent=dirname(infile_dir), silent=TRUE))
  if (!dir.exists(dirname(outdir))) {
    print_message(
      type = "error",
      "The parent folder of 'outdir' (",outdir,") does not exist; ",
      "please create it."
    )
  }
  dir.create(outdir, recursive=FALSE, showWarnings=FALSE)
  
  # create subdirs
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(prod_type)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,prod_type), dir.create, showWarnings=FALSE)
  }
  
  # check compression value
  if (format=="GTiff") {
    if (!compress %in% c("JPEG","LZW","PACKBITS","DEFLATE","CCITTRLE",
                         "CCITTFAX3","CCITTFAX4","LZMA","NONE")) {
      print_message(
        type="warning",
        "'",toupper(compress),"' is not a valid compression value; ",
        "the default 'DEFLATE' value will be used."
      )
      compress <- "DEFLATE"
    }
  }
  
  # retrieve UTM zone
  if (utmzone=="") {
    print_message(
      type="message",
      "Using UTM zone ",sel_utmzone <- infile_meta$utm[1],".")
  } else {
    sel_utmzone <- which(infile_meta$utm == utmzone)
    if (length(sel_utmzone)==0) {
      print_message(
        type="warning",
        "Tiles with UTM zone ",utmzone," are not present: zone ",
        sel_utmzone <- infile_meta$utm[1]," will be used.")
    }
  }
  
  # select default product type if missing
  if (is.null(prod_type)) {
    if (infile_meta$level=="1C") {
      prod_type <- "TOA"
    } else if (infile_meta$level=="2A") {
      prod_type <- "BOA"
    }
  }
  
  # define output extension
  out_ext <- sel_driver[1,"ext"]
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- if (all(!is.na(format), format == "VRT")) {
      rmtmp <- FALSE # force not to remove intermediate files
      if (!missing(outdir)) {
        file.path(outdir, ".vrt")
      } else {
        tempfile(pattern="s2translate_")
      }
    } else {
      tempfile(pattern="s2translate_")
    }
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="s2translate_")))
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # create angles if required
  out_names <- s2_angles(
    infiles = infile, 
    outdir = outdir,
    subdirs = subdirs,
    tmpdir = tmpdir,
    rmtmp = rmtmp,
    prod_type = prod_type[prod_type %in% c("SZA", "OZA", "SAA", "OAA")], 
    res = res[1],
    method = method,
    format = format,
    compress = compress,
    bigtiff = bigtiff,
    overwrite = overwrite
  )
  
  # create a file / set of files for each prod_type
  for (sel_prod in prod_type[!prod_type %in% c("SZA", "OZA", "SAA", "OAA")]) {try({
    
    if (sel_prod %in% c("BOA","TOA")) {
      sel_type <- "MSI"
    } else {
      sel_type <- sel_prod
    }
    
    # define NA flag
    sel_na <- s2_defNA(sel_prod)
    # define output subdir
    out_subdir <- ifelse(subdirs, file.path(outdir,sel_prod), outdir)
    
    # TODO check that required bands are present
    
    # cycle on granules (with compact names, this runs only once; with old name, one or more)
    for (sel_granule in infile_meta$xml_granules) {try({
      
      sel_tile <- safe_getMetadata(
        dirname(sel_granule), info = "id_tile", 
        format = "vector", simplify = TRUE
      )
      
      # continue only if sel_tile is within desired tiles
      if (anyNA(tiles) | sel_tile %in% tiles) {
        
        # define output basename
        out_prefix <- safe_shortname(sel_granule, prod_type=sel_prod, res=res[1], full.name=FALSE, abort=TRUE)
        # complete output filename
        out_name <- file.path(out_subdir,paste0(out_prefix,".",out_ext))
        
        # if out_name already exists and overwrite==FALSE, do not proceed
        if (!file.exists(out_name) | overwrite==TRUE) {
          
          # select required bands from the list and order them by resolution
          jp2df_selbands <- infile_meta$jp2list[infile_meta$jp2list$type==sel_type &
                                                  infile_meta$jp2list$tile==sel_tile,]
          jp2df_selbands <- jp2df_selbands[with(jp2df_selbands,order(band,res)),]
          # associate offsets
          names(infile_meta$offset) <- gsub("^B([0-9])$", "B0\\1", names(infile_meta$offset))
          jp2df_selbands$offset <- infile_meta$offset[jp2df_selbands$band]
          # remove lower resolutions and keep only the best resolution for each band
          if (!any(jp2df_selbands$res=="")) {
            jp2df_selbands <- jp2df_selbands[as.integer(substr(jp2df_selbands$res,1,2))>=as.integer(substr(res[1],1,2)),]
          } else {
            # for oldname L1C (which do not have "res" in granule name) remove B08 or B8A basing on the requested "res"
            if (as.integer(substr(res[1],1,2)) < 20) {
              jp2df_selbands <- jp2df_selbands[-grep("B8A.jp2$",jp2df_selbands$layer),]
            } else {
              jp2df_selbands <- jp2df_selbands[-grep("B08.jp2$",jp2df_selbands$layer),]
            }
          }
          jp2df_selbands <- jp2df_selbands[!duplicated(with(jp2df_selbands,paste(band,tile))),]
          # (check that only BOA/TOA prods have multiple bands)
          if (nrow(jp2df_selbands)>1 & !sel_prod %in% c("TOA","BOA")) {
            stop("Internal error (exception not managed).")
          }
          
          
          # define the steps to perform:
          # 1. create a multiband stack and apply band offset
          do_step1 <- nrow(jp2df_selbands)>1
          # 2: reshape L2A in case of wrong native resolution
          # (i.e. SAFE not containing 10m bands with res = "10m")
          res_out <- if (all(
            sel_prod %in% c("SCL","CLD","SNW"),
            res[1] == "10m"
          )) {res[2]} else {res[1]}
          res_ratio <- min(as.integer(substr(jp2df_selbands$res,1,2))) / 
            as.integer(substr(res_out,1,2))
          do_step2 <- infile_meta$level == "2A" && res_ratio > 1
          # 3. mask nodata values if the tile does not completely cover the orbit
          infile_footprint <- st_transform(
            st_as_sfc(infile_meta$footprint, crs = 4326), 
            st_crs2(sel_utmzone)
          )
          infile_area <- sum(st_area(infile_footprint))
          do_step3 <- infile_area < 109e3^2*units::as_units("m^2")
          # 4. convert the final VRT to a physical format
          do_step4 <- format != "VRT"
          
          vrt1_path <- if (!do_step1) { # skip step 1
            file.path(infile_dir,jp2df_selbands[,"relpath"])
          } else if (all(!do_step2, !do_step3, !do_step4)) { # step 1 is the last
            out_name
          } else { # perform step 1
            paste0(tmpdir,"/",out_prefix,"_step1.vrt")
          }
          vrt2_path <- if (!do_step2) { # skip step 2
            vrt1_path 
          } else if (all(!do_step3, !do_step4)) { # step 2 is the last
            out_name
          } else { # perform step 1
            paste0(tmpdir,"/",out_prefix,"_step2.vrt")
          }
          vrt3_path <- if (!do_step3) { # skip step 3
            vrt2_path
          } else if (!do_step4) { # step 1 is the last
            out_name
          } else { # perform step 3
            paste0(tmpdir,"/",out_prefix,"_step3.vrt")
          }
          
          # step 1
          if (do_step1) {
            vrt1a_path <- paste0(tmpdir,"/",out_prefix,"_step1a.vrt")
            gdalUtil(
              "buildvrt",
              source = file.path(infile_dir,jp2df_selbands[,"relpath"]),
              destination = vrt1a_path,
              options = c(
                "-separate",
                "-resolution", "highest",
                "-a_srs", paste0("EPSG:",st_crs2(sel_utmzone)$epsg)
              ),
              quiet = TRUE
            )
            if (vrt_rel_paths==TRUE) {
              gdal_abs2rel(vrt1a_path)
            }
            gdalUtil(
              "translate",
              source = vrt1a_path,
              destination = vrt1_path,
              options = c(
                unlist(lapply(seq_len(nrow(jp2df_selbands)), function(bn) {
                  c(paste0("-scale_",bn), c(0,1e4), c(0,1e4)+jp2df_selbands[bn,"offset"])
                })),
                "-of", "VRT"
              ),
              quiet = TRUE
            )
            if (vrt_rel_paths==TRUE) {
              gdal_abs2rel(vrt1_path)
            }
          }
          
          # step 2
          if (do_step2) {
            gdalUtil(
              "translate",
              source = vrt1_path,
              destination = vrt2_path,
              options = c(
                "-outsize", rep(paste0(res_ratio*100,"%"), 2),
                "-of", "VRT"
              ),
              quiet = TRUE
            )
            if (vrt_rel_paths==TRUE) {
              gdal_abs2rel(vrt2_path)
            }
          }
          
          # step 3
          if (do_step3) {
            cutline_path <- paste0(tmpdir,"/",out_prefix,"_cutline.gpkg")
            st_write(infile_footprint, cutline_path, append = FALSE, quiet = TRUE)
            gdalUtil(
              "warp",
              source = vrt1_path,
              destination = vrt3_path,
              options = c(
                "-cutline", cutline_path,
                "-of", "VRT",
                if (!is.na(sel_na)) {c("-dstnodata", sel_na)}
              ),
              quiet = TRUE
            )
            if (vrt_rel_paths==TRUE) {
              gdal_abs2rel(vrt3_path)
            }
          }
          
          
          # step 4
          if (do_step4 | all(!do_step1, !do_step2, !do_step3)) {
            gdalUtil(
              "translate",
              source = vrt3_path,
              destination = out_name,
              options = c(
                "-of", format,
                if (format == "GTiff") {c(
                  "-co", paste0("COMPRESS=",toupper(compress)),
                  "-co", "TILED=YES"
                )},
                if (format=="GTiff" & bigtiff==TRUE) {c("-co", "BIGTIFF=YES")},
                if (!is.na(sel_na)) {c("-a_nodata", sel_na)}
              ),
              quiet = TRUE
            )
            if (!do_step4 & vrt_rel_paths==TRUE) {
              gdal_abs2rel(out_name)
            }
          }
          
          # fix for envi extension (writeRaster uses .envi)
          if (format=="ENVI") {fix_envi_format(out_name)}
          
        } # end of "overwite" IF cycle
        
        out_names <- c(out_names, out_name)
        
      } # end of "sel_tile %in% tiles" IF cycle
      
    })} # end of sel_granule cycle
    
  })} # end of prod_type cycle
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  print_message(
    type="message",
    length(out_names)," output files were correctly created."
  )
  return(out_names)
  
}

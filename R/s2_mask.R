#' @title Apply cloud masks
#' @description [s2_mask] Applies a cloud mask to a Sentinel-2 product. Since
#'  [raster] functions are used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([safe_shortname]).
#' @param maskfiles A vector of filenames from which to take the
#'  information about cloud coverage (for now, only SCL products
#'  have been implemented). It is not necessary that `maskfiles`
#'  elements strictly match `infiles` ones. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([safe_shortname]).
#' @param mask_type (optional) Character vector which determines the type of
#'  mask to be applied. Accepted values are:
#'  - "nomask": do not mask any pixel;
#'  - "nodata": mask pixels checked as "No data" or "Saturated or defective" 
#'      in the SCL product (all pixels with values are maintained);
#'  - "cloud_high_proba": mask pixels checked as "No data", "Saturated or 
#'      defective" or "Cloud (high probability)" in the SCL product;
#'  - "cloud_medium_proba": mask pixels checked as "No data", "Saturated or 
#'      defective" or "Cloud (high or medium probability)" in the SCL product;
#'  - "cloud_low_proba": mask pixels checked as "No data", "Saturated or 
#'      defective" or "Cloud (any probability)" in the SCL product;
#'  - "cloud_and_shadow": mask pixels checked as "No data", "Saturated or 
#'      defective", "Cloud (any probability)", "Cloud shadow" or "Dark area"
#'      in the SCL product;
#'  - "clear_sky": mask pixels checked as "No data", "Saturated or 
#'      defective", "Cloud (any probability)", "Cloud shadow", "Dark area"
#'      or "Thin cirrus" in the SCL product
#'      (only pixels classified as clear-sky surface - so "Vegetation", 
#'      "Bare soil", "Water" or "Snow" - are maintained);
#'  - "land": mask pixels checked as "No data", "Saturated or 
#'      defective", "Cloud (any probability)", "Cloud shadow", "Dark area",
#'      "Thin cirrus", "Water" or "Snow" in the SCL product
#'      (only pixels classified as land surface - so "Vegetation" or 
#'      "Bare soil" - are maintained);
#'  - a string in the following form: "scl_n_m_n", where n, m and n are one or
#'      more SCL class numbers (e.g. "scl_0_8_9_11"): mask pixels corresponding
#'      to the classes specified in the string. E.g. string "scl_0_8_9_11" can
#'      be used to mask classes 0 ("No data"), 8-9 ("Cloud (high or medium 
#'      probability)") and 11 ("Snow");
#'  - "opaque_clouds" (still to be implemented).
#' @param smooth (optional) Numerical (positive): should the mask be smoothed=the size (in the unit of
#'  `inmask`, typically metres) to be used as radius for the smoothing
#'  (the higher it is, the more smooth the output mask will result). 
#'  Defaul is 20.
#' @param buffer (optional) Numerical (positive or negative): the size of the 
#'  buffer (in the unit of `inmask`, typically metres) to be applied to the 
#'  masked area after smoothing it (positive to enlarge, negative to reduce).
#'  Defaul is 10.
#' @param max_mask (optional) Numeric value (range 0 to 100), which represents
#'  the maximum percentage of allowed masked surface (by clouds or any other 
#'  type of mask chosen with argument `mask_type`) for producing outputs. 
#'  Images with a percentage of masked surface greater than `max_mask`%
#'  are not processed (the list of expected output files which have not been 
#'  generated is returned as an attribute, named "skipped"). 
#'  Default value is 80.
#'  Notice that the percentage is computed on non-NA values (if input images 
#'  had previously been clipped and masked using a polygon, the percentage is
#'  computed on the surface included in the masking polygons).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: "current directory"masked"
#'  subdir of current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE).
#'  This parameter takes effect only if the output files are not VRT
#'  (in this case temporary files cannot be deleted, because rasters of source
#'  bands are included within them).
#' @param save_binary_mask (optional) Logical: should binary masks be exported?
#'  Binary mask are intermediate rasters used to apply the cloud mask: 
#'  pixel values can be 1 (no cloud mask), 0 (cloud mask) or NA (original NA
#'  value, i.e. because input rasters had been clipped on the extent polygons).
#'  If FALSE (default) they are not exported; if TRUE, they are exported
#'  as MSK prod type (so saved within `outdir`, in a subdirectory called "MSK"
#'  if `subdirs = TRUE`).
#'  Notice that the presence of "MSK" products is not checked before running 
#'  `sen2r()`, as done for the other products; this means that missing products
#'  which are not required to apply cloud masks will not be produced.
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or "GTiff" in case of VRT input images).
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param parallel (optional) Logical: if TRUE, masking is conducted using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#'  Multiprocess masking computation is always performed in singlecore mode
#'  if `format != "VRT"` (because in this case there is no gain in using
#'  multicore processing).
#' @param overwrite (optional) Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @return [s2_mask] returns a vector with the names of the created products.
#'  An attribute "toomasked" contains the paths of the outputs which were not
#'  created cause to the high percentage of cloud coverage.
#' @export
#' @importFrom rgdal GDALinfo
#' @importFrom raster brick calc dataType mask overlay stack values
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_mask <- function(infiles,
                    maskfiles,
                    mask_type = "cloud_medium_proba",
                    smooth = 20,
                    buffer = 10,
                    max_mask = 80,
                    outdir = "./masked",
                    tmpdir = NA,
                    rmtmp = TRUE,
                    save_binary_mask = FALSE,
                    format = NA,
                    subdirs = NA,
                    compress = "DEFLATE",
                    parallel = FALSE,
                    overwrite = FALSE,
                    .log_message = NA,
                    .log_output = NA) {
  .s2_mask(infiles = infiles,
           maskfiles = maskfiles,
           mask_type = mask_type,
           smooth = smooth,
           buffer = buffer,
           max_mask = max_mask,
           outdir = outdir,
           tmpdir = tmpdir,
           rmtmp = rmtmp,
           save_binary_mask = save_binary_mask,
           format = format,
           subdirs = subdirs,
           compress = compress,
           parallel = parallel,
           overwrite = overwrite,
           output_type = "s2_mask",
           .log_message = .log_message,
           .log_output = .log_output)
}

.s2_mask <- function(infiles,
                     maskfiles,
                     mask_type = "cloud_medium_proba",
                     smooth = 250,
                     buffer = 250,
                     max_mask = 80,
                     outdir = "./masked",
                     tmpdir = NA,
                     rmtmp = TRUE,
                     save_binary_mask = FALSE,
                     format = NA,
                     subdirs = NA,
                     compress = "DEFLATE",
                     parallel = FALSE,
                     overwrite = FALSE,
                     output_type = "s2_mask", # determines if using s2_mask() or s2_perc_masked()
                     .log_message = NA,
                     .log_output = NA) {
  
  . <- NULL
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Check that files exist
  if (!any(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      if (!all(sapply(infiles, file.exists))) {"The "} else {"Some of the "},
      "input files (\"",
      paste(infiles[!sapply(infiles, file.exists)], collapse="\", \""),
      "\") do not exists locally; please check file names and paths.")
  }
  
  # check output format
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))
  if (!is.na(format)) {
    sel_driver <- gdal_formats[gdal_formats$name==format,]
    if (nrow(sel_driver)==0) {
      print_message(
        type="error",
        "Format \"",format,"\" is not recognised; ",
        "please use one of the formats supported by your GDAL installation.\n\n",
        "To list them, use the following command:\n",
        "gdalUtils::gdalinfo(formats=TRUE)\n\n",
        "To search for a specific format, use:\n",
        "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
    }
  }
  
  # Check tmpdir
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- if (format == "VRT") {
      if (!missing(outdir)) {
        autotmpdir <- FALSE # logical: TRUE if tmpdir should be specified 
        # for each out file (when tmpdir was not specified and output files are vrt),
        # FALSE if a single tmpdir should be used (otherwise)
        file.path(outdir, ".vrt")
      } else {
        autotmpdir <- TRUE
        tempfile(pattern="s2mask_")
      }
    } else {
      autotmpdir <- FALSE
      tempfile(pattern="s2mask_")
    }
  } else {
    autotmpdir <- FALSE
  }
  if (format == "VRT") {
    rmtmp <- FALSE # force not to remove intermediate files
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # Get files metadata
  infiles_meta <- data.table(sen2r_getElements(infiles, format="data.frame"))
  maskfiles_meta <- data.table(sen2r_getElements(maskfiles, format="data.frame"))
  # suppressWarnings(
  #   infiles_meta_gdal <- sapply(infiles, function(x) {attributes(GDALinfo(x))[c("df")]})
  # )
  
  # create outdir if not existing
  dir.create(outdir, recursive=FALSE, showWarnings=FALSE)
  # create subdirs (if requested)
  prod_types <- unique(infiles_meta$prod_type)
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(prod_types)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,prod_types), dir.create, showWarnings=FALSE)
  }
  
  # check smooth and buffer
  if (anyNA(smooth)) {smooth <- 0}
  if (anyNA(buffer)) {buffer <- 0}
  
  # define required bands and formula to compute masks
  # accepted mask_type values: nodata, cloud_high_proba, cloud_medium_proba, cloud_low_proba, cloud_and_shadow, cloud_shadow_cirrus, opaque_clouds
  # structure of req_masks: list, names are prod_types, content are values of the files to set as 0, otherwise 1
  if (mask_type == "nomask") {
    req_masks <- list()
  } else if (mask_type == "nodata") {
    req_masks <- list("SCL"=c(0:1))
  } else if (mask_type == "cloud_medium_proba") {
    req_masks <- list("SCL"=c(0:1,8:9))
  } else if (mask_type == "cloud_low_proba") {
    req_masks <- list("SCL"=c(0:1,7:9))
  } else if (mask_type == "cloud_and_shadow") {
    req_masks <- list("SCL"=c(0:3,7:9))
  } else if (mask_type == "cloud_shadow_cirrus") {
    req_masks <- list("SCL"=c(0:3,7:10))
  } else if (mask_type == "land") {
    req_masks <- list("SCL"=c(0:3,6:11))
  } else if (grepl("^scl\\_", mask_type)) {
    req_masks <- list("SCL"=strsplit(mask_type,"_")[[1]][-1])
  } else if (mask_type == "opaque_clouds") {
    print_message(type="error", "Mask type 'opaque_clouds' has not been yet implemented.")
  }
  
  ## Cycle on each file
  if (output_type == "s2_mask") {
    outfiles <- character(0) # vector with paths of created files
    outfiles_toomasked <- character(0) # vector with the path of outputs which 
    # were not created cause to the higher masked surface
  } else if (output_type == "perc") {
    outpercs <- numeric(0)
  }
  for (i in seq_along(infiles)) {try({
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])
    sel_format <- suppressWarnings(ifelse(
      !is.na(format), format, attr(GDALinfo(sel_infile), "driver")
    ))
    sel_rmtmp <- ifelse(sel_format=="VRT", FALSE, rmtmp)
    sel_out_ext <- gdal_formats[gdal_formats$name==sel_format,"ext"][1]
    sel_naflag <- s2_defNA(sel_infile_meta$prod_type)
    
    # check that infile has the correct maskfile
    sel_maskfiles <- sapply(names(req_masks), function(m) {
      maskfiles[which(maskfiles_meta$prod_type==m &
                        maskfiles_meta$type==sel_infile_meta$type &
                        maskfiles_meta$mission==sel_infile_meta$mission &
                        maskfiles_meta$sensing_date==sel_infile_meta$sensing_date &
                        maskfiles_meta$id_orbit==sel_infile_meta$id_orbit &
                        maskfiles_meta$res==sel_infile_meta$res)][1]
    })
    
    # define subdir
    out_subdir <- ifelse(subdirs, file.path(outdir,infiles_meta[i,"prod_type"]), outdir)
    
    # define out name (a vrt for all except the last mask)
    sel_outfile <- file.path(
      out_subdir,
      gsub(paste0("\\.",infiles_meta[i,"file_ext"],"$"),
           paste0(".",sel_out_ext),
           basename(sel_infile)))
    
    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(sel_outfile) | overwrite==TRUE) {
      
      # if no masking is required, "copy" input files
      if (length(sel_maskfiles)==0) {
        system(
          paste0(
            binpaths$gdal_translate," -of ",sel_format," ",
            if (sel_format=="GTiff") {paste0("-co COMPRESS=",toupper(compress)," ")},
            "\"",sel_infile,"\" ",
            "\"",sel_outfile,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
      } else {

      # load input rasters
      inmask <- raster::stack(sel_maskfiles)
      
      # path for bug #47
      if (Sys.info()["sysname"] == "Windows" & gsub(".*\\.([^\\.]+)$","\\1",sel_infile)=="vrt") {
        # on Windows, use input physical files
        system(
          paste0(
            binpaths$gdal_translate," -of GTiff ",
            paste0("-co COMPRESS=",toupper(compress)," "),
            "\"",sel_infile,"\" ",
            "\"",gsub("\\.vrt$",".tif",sel_infile),"\""
          ), intern = TRUE
        )
        sel_infile <- gsub("\\.vrt$",".tif",sel_infile)
      }
      
      # if tmpdir should vary for each file, define it
      sel_tmpdir <- if (autotmpdir) {
        file.path(out_subdir, ".vrt")
      } else {
        tmpdir
      }
      dir.create(sel_tmpdir, showWarnings=FALSE)
      
      # create global mask
      mask_tmpfiles <- character(0) # files which compose the mask
      naval_tmpfiles <- character(0) # files which determine the amount of NA
      for (i in seq_along(inmask@layers)) {
        mask_tmpfiles <- c(
          mask_tmpfiles,
          file.path(sel_tmpdir, basename(tempfile(pattern = "mask_", fileext = ".tif")))
        )
        raster::calc(inmask[[i]],
                     function(x){as.integer(!is.na(x) & !x %in% req_masks[[i]])},
                     filename = mask_tmpfiles[i],
                     options  = "COMPRESS=LZW",
                     datatype = "INT1U")
        naval_tmpfiles <- c(
          naval_tmpfiles,
          file.path(sel_tmpdir, basename(tempfile(pattern = "naval_", fileext = ".tif")))
        )
        raster::calc(inmask[[i]],
                     function(x){as.integer(!is.na(x))},
                     filename = naval_tmpfiles[i],
                     options  = "COMPRESS=LZW",
                     datatype = "INT1U")
      }
      if(length(mask_tmpfiles)==1) {
        outmask <- mask_tmpfiles
        outnaval <- naval_tmpfiles
      } else {
        outmask <- file.path(sel_tmpdir, basename(tempfile(pattern = "mask_", fileext = ".tif")))
        outnaval <- file.path(sel_tmpdir, basename(tempfile(pattern = "naval_", fileext = ".tif")))
        raster::overlay(stack(mask_tmpfiles),
                        fun = sum,
                        filename = outmask,
                        options  = "COMPRESS=LZW",
                        datatype = "INT1U")
        raster::overlay(stack(naval_tmpfiles),
                        fun = sum,
                        filename = outnaval,
                        options  = "COMPRESS=LZW",
                        datatype = "INT1U")
      }
      
      # compute the percentage of masked surface
      values_naval <- values(raster(outnaval))
      mean_values_naval <- mean(values_naval, na.rm=TRUE)
      mean_values_mask <- mean(values(raster(outmask)), na.rm=TRUE)
      perc_mask <- 100 * (mean_values_naval - mean_values_mask) / mean_values_naval
      if (!is.finite(perc_mask)) {perc_mask <- 100}
      
      # if the user required to save 0-1 masks, save them
      if (save_binary_mask == TRUE) {
        # define out MSK name
        binmask <- file.path(
          ifelse(subdirs, file.path(outdir,"MSK"), outdir),
          gsub(paste0("\\.",infiles_meta[i,"file_ext"],"$"),
               paste0(".",sel_out_ext),
               gsub(paste0("\\_",infiles_meta[i,"prod_type"],"\\_"),
                    "_MSK_",
                    basename(sel_infile)))
        )
        # create subdir if missing
        if (subdirs & !dir.exists(file.path(outdir,"MSK"))) {
          dir.create(file.path(outdir,"MSK"))
        }
        # mask NA values
        raster::mask(
          raster(outmask),
          raster(outnaval),
          filename = binmask,
          maskvalue = 0,
          updatevalue = sel_naflag,
          updateNA = TRUE,
          NAflag = 255,
          datatype = "INT1U",
          format = sel_format,
          options = if(sel_format == "GTiff") {paste0("COMPRESS=",compress)},
          overwrite = overwrite
        )
      }
      
      # if the requested output is this value, return it; else, continue masking
      if (output_type == "perc") {
        names(perc_mask) <- sel_infile
        outpercs <- c(outpercs, perc_mask)
      } else if (output_type == "s2_mask") {
        
        # evaluate if the output have to be produced
        # if the image is sufficiently clean, mask it
        if (is.na(max_mask) | perc_mask <= max_mask) {
          
          # if mask is at different resolution than inraster
          # (e.g. 20m instead of 10m),
          # resample it
          if (any(suppressWarnings(GDALinfo(sel_infile)[c("res.x","res.y")]) !=
                  suppressWarnings(GDALinfo(outmask)[c("res.x","res.y")]))) {
            gdal_warp( # DO NOT use raster::disaggregate (1. not faster, 2. it does not always provide the right resolution)
              outmask,
              outmask_res <- file.path(sel_tmpdir, basename(tempfile(pattern = "mask_", fileext = ".tif"))),
              ref = sel_infile
            )
#             outmask_res0 <- file.path(sel_tmpdir, basename(tempfile(pattern = "mask_", fileext = ".tif")))
#             outmask_res <- file.path(sel_tmpdir, basename(tempfile(pattern = "mask_", fileext = ".tif")))
#             disaggregate(
#               raster(outmask),
#               filename = outmask_res0,
#               fact = round(
#                 suppressWarnings(GDALinfo(outmask)[c("res.x","res.y")]) / 
#                   suppressWarnings(GDALinfo(sel_infile)[c("res.x","res.y")])
#               ),
#               method = "",
#               options  = "COMPRESS=LZW",
#               datatype = "INT1U"
#             )
#             if (any(suppressWarnings(GDALinfo(sel_infile)[c("rows","columns")]) !=
#                     suppressWarnings(GDALinfo(outmask_res0)[c("rows","columns")]))) {
#               crop(
#                 raster(outmask_res0),
#                 raster(sel_infile),
#                 filename = outmask_res,
#                 options  = "COMPRESS=LZW",
#                 datatype = "INT1U"
#               )
#             } else {
#               outmask_res <- outmask_res0
#             }
          } else {
            outmask_res <- outmask
          }

          # the same for outnaval
          if (any(suppressWarnings(GDALinfo(sel_infile)[c("res.x","res.y")]) !=
                  suppressWarnings(GDALinfo(outnaval)[c("res.x","res.y")])) & 
              (smooth > 0 | buffer != 0)) {
            gdal_warp(
              outnaval,
              outnaval_res <- file.path(sel_tmpdir, basename(tempfile(pattern = "naval_", fileext = ".tif"))),
              ref = sel_infile
            )
          } else {
            outnaval_res <- outnaval
          }
          
          # apply the smoothing (if required)
          outmask_smooth <- if (smooth > 0 | buffer != 0) {
            # if the unit is not metres, approximate it
            if (projpar(attr(suppressWarnings(GDALinfo(sel_infile)),"projection"), "Unit") == "degree") {
              buffer <- buffer * 8.15e-6
              smooth <- smooth * 8.15e-6
            }
            # apply the smooth to the mask
            smooth_mask(
              outmask_res, 
              radius = smooth, buffer = buffer, 
              namask = if (any(values_naval==0)) {outnaval_res} else {NULL}, # TODO NULL if no Nodata values are present
              binpaths = binpaths, tmpdir = sel_tmpdir
            )
          } else {
            outmask_res
          }
          
          # load mask
          inraster <- raster::brick(sel_infile)
          
          if (sel_format!="VRT") {
            raster::mask(
              inraster,
              raster(outmask_smooth),
              filename = sel_outfile,
              maskvalue = 0,
              updatevalue = sel_naflag,
              updateNA = TRUE,
              NAflag = sel_naflag,
              datatype = dataType(inraster),
              format = sel_format,
              options = if(sel_format == "GTiff") {paste0("COMPRESS=",compress)},
              overwrite = overwrite
            )
          } else {
            print_message(
              type = "message",
              date = TRUE,
              "Starting parallel application of masks on file ",basename(sel_infile),"..."
            )
            maskapply_parallel(
              inraster, 
              raster(outmask_smooth), 
              outpath = sel_outfile,
              tmpdir = sel_tmpdir,
              binpaths = binpaths,
              NAflag = sel_naflag,
              parallel = parallel,
              datatype = dataType(inraster),
              overwrite = overwrite,
              .log_message=.log_message, 
              .log_output=.log_output
            )
            print_message(
              type = "message",
              date = TRUE,
              "Parallel application of masks on file ",basename(sel_infile)," done."
            )
          }
          
          
          # fix for envi extension (writeRaster use .envi)
          if (sel_format=="ENVI" &
              file.exists(gsub(paste0("\\.",sel_out_ext,"$"),".envi",sel_outfile))) {
            file.rename(gsub(paste0("\\.",sel_out_ext,"$"),".envi",sel_outfile),
                        sel_outfile)
            file.rename(paste0(gsub(paste0("\\.",sel_out_ext,"$"),".envi",sel_outfile),".aux.xml"),
                        paste0(sel_outfile,".aux.xml"))
          }
          
        } else { # end of max_mask IF cycle
          outfiles_toomasked <- c(outfiles_toomasked, sel_outfile)
        }
        
      } # end of output_type IF cycle
      
      if (sel_rmtmp == TRUE) {
        unlink(sel_tmpdir, recursive=TRUE) # FIXME check not to delete files created outside sel_ cycle!
      }
      
      } # end of length(sel_maskfiles)==0 IF cycle
      
    } # end of overwrite IF cycle
    
    if (output_type == "s2_mask" & file.exists(sel_outfile)) {
      outfiles <- c(outfiles, sel_outfile)
    }
    
  })} # end on infiles cycle
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  if (output_type == "s2_mask") {
    attr(outfiles, "toomasked") <- outfiles_toomasked
    return(outfiles)
  } else if (output_type == "perc") {
    return(outpercs)
  }
  
}


#' @title Compute the percentage of cloud-masked surface
#' @description [s2_perc_masked] computes the percentage of cloud-masked surface.
#'  The function is similar to [s2_mask], but it returns percentages instead
#'  of masked rasters.
#' @return [s2_perc_masked] returns a names vector with the percentages 
#'  of masked surtfaces.
#' @rdname s2_mask
#' @export

s2_perc_masked <- function(infiles,
                           maskfiles,
                           mask_type = "cloud_medium_proba",
                           tmpdir = NA,
                           rmtmp = TRUE,
                           parallel = FALSE) {
  .s2_mask(infiles = infiles,
           maskfiles = maskfiles,
           mask_type = mask_type,
           smooth = 0,
           buffer = 0,
           max_mask = 100,
           tmpdir = tmpdir,
           rmtmp = rmtmp,
           parallel = parallel,
           output_type = "perc")
}

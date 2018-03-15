#' @title Apply cloud masks
#' @description Apply a cloud mask to a Sentinel-2 product. Since
#'  [raster] functions are used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the SALTO-S2 naming convention
#'  ([s2_shortname]).
#' @param maskfiles A vector of filenames from which to take the
#'  information about cloud coverage (for now, only SCL products
#'  have been implemented). It is not necessary that `maskfiles`
#'  elements strictly match `infiles` ones. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the salto-S2 naming convention
#'  ([s2_shortname]).
#' @param mask_type (optional) Character vector which determines the type of
#'  mask to be applied. Accepted values are:
#'  - "nodata": mask pixels checked as "No data" in the SCL product;
#'  - "cloud_high_proba": mask pixels checked as "No data" or
#'      "Cloud (high probability)" in the SCL product;
#'  - "cloud_medium_proba": mask pixels checked as "No data" or
#'      "Cloud (high or medium probability)" in the SCL product;
#'  - "cloud_low_proba": mask pixels checked as "No data" or
#'      "Cloud (any probability)" in the SCL product;
#'  - "cloud_and_shadow": mask pixels checked as "No data",
#'      "Cloud (any probability)" or "Cloud shadow" in the SCL product;
#'  - "cloud_shadow_cirrus": mask pixels checked as "No data",
#'      "Cloud (any probability)", "Cloud shadow" or "Thin cirrus"
#'      in the SCL product;
#'  - a string in the following form: "scl_n_m_n", where n, m and n are one or
#'      more SCL class numbers (e.g. "scl_0_8_9_11"): mask pixels corresponding
#'      to the classes specified in the string. E.g. string "scl_0_8_9_11" can
#'      be used to mask classes 0 ("No data"), 8-9 ("Cloud (high or medium 
#'      probability)") and 11 ("Snow");
#'  - "opaque_clouds" (still to be implemented).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: "current directory"masked"
#'  subdir of current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param tmpdir (optional) Path where intermediate VRT will be created.
#'  Default is in a temporary directory.
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
#' @return A vector with the names of the created products.
#' @export
#' @importFrom rgdal GDALinfo
#' @importFrom raster stack brick calc dataType mask overlay
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_mask <- function(infiles,
                    maskfiles,
                    mask_type="cloud_medium_proba",
                    outdir="./masked",
                    tmpdir=NA,
                    format=NA,
                    subdirs=NA,
                    compress="DEFLATE",
                    parallel = FALSE,
                    overwrite = FALSE) {
  
  . <- NULL
  
  # Load GDAL paths
  binpaths_file <- file.path(system.file("extdata",package="salto"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (is.null(binpaths$gdalinfo)) {
    check_gdal()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
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
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="salto"))
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
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="dir")
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # Get files metadata
  infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
  maskfiles_meta <- data.table(fs2nc_getElements(maskfiles, format="data.frame"))
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
  
  # define required bands and formula to compute masks
  # accepted mask_type values: nodata, cloud_high_proba, cloud_medium_proba, cloud_low_proba, cloud_and_shadow, cloud_shadow_cirrus, opaque_clouds
  # structure of req_masks: list, names are prod_types, content are values of the files to set as 0, otherwise 1
  if (mask_type == "nodata") {
    req_masks <- list("SCL"=c(0))
  } else if (mask_type == "cloud_high_proba") {
    req_masks <- list("SCL"=c(0,9))
  } else if (mask_type == "cloud_medium_proba") {
    req_masks <- list("SCL"=c(0,8:9))
  } else if (mask_type == "cloud_low_proba") {
    req_masks <- list("SCL"=c(0,7:9))
  } else if (mask_type == "cloud_and_shadow") {
    req_masks <- list("SCL"=c(0,3,7:9))
  } else if (mask_type == "cloud_shadow_cirrus") {
    req_masks <- list("SCL"=c(0,3,7:10))
  } else if (grepl("^scl\\_", mask_type)) {
    req_masks <- list("SCL"=strsplit(mask_type,"_")[[1]][-1])
  } else if (mask_type == "opaque_clouds") {
    print_message(type="error", "Mask type 'opaque_clouds' has not been yet implemented.")
  }
  
  ## Cycle on each file
  outfiles <- character(0)
  for (i in seq_along(infiles)) {
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])
    sel_format <- suppressWarnings(ifelse(
      !is.na(format), format, attr(GDALinfo(sel_infile), "driver")
    ))
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
      inraster <- raster::brick(sel_infile)
      
      # create global mask
      mask_tmpfiles <- character(0)
      for (i in seq_along(inmask@layers)) {
        mask_tmpfiles <- c(tempfile(fileext=".tif"), mask_tmpfiles)
        raster::calc(inmask[[i]],
                     function(x){as.integer(!is.na(x) & !x %in% req_masks[[i]])},
                     filename = mask_tmpfiles[1],
                     options  = "COMPRESS=LZW",
                     datatype = "INT1U")
      }
      if(length(mask_tmpfiles)==1) {
        outmask <- mask_tmpfiles
      } else {
        outmask <- tempfile(fileext=".tif")
        raster::overlay(stack(mask_tmpfiles),
                        fun = sum,
                        filename = outmask,
                        options  = "COMPRESS=LZW",
                        datatype = "INT1U")
      }
      
      # if mask is at different resolution than inraster
      # (e.g. 20m instead of 10m),
      # resample it
      if (any(suppressWarnings(GDALinfo(sel_infile)[c("res.x","res.y")]) !=
              suppressWarnings(GDALinfo(outmask)[c("res.x","res.y")]))) {
        gdal_warp(outmask,
                  outmask_res <- tempfile(fileext=".tif"),
                  ref = sel_infile)
      } else {
        outmask_res <- outmask
      }
      
      # apply mask
      # using only non-parallel version, since ther is not any speeding up
      # (the bottleneck is writing the final output, which van not be parallelised).
      # TODO (maybe): if the user requires spectral indices but not BOA,
      # parallelise the masking, create a vrt BOA (from singleband TIFFs)
      # so to skip this passage.
      
      if (sel_format!="VRT") {
        raster::mask(
          inraster,
          raster::raster(outmask_res),
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
        maskapply_parallel(
          inraster, 
          raster(outmask_res), 
          outpath = sel_outfile,
          tmpdir = tmpdir,
          binpaths = binpaths,
          NAflag = sel_naflag,
          parallel = parallel,
          datatype = dataType(inraster),
          overwrite = overwrite
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
      
    } # end of overwrite IF cycle
    
    outfiles <- c(outfiles, sel_outfile)
    
  } # end on infiles cycle
  
  # if (parallel==TRUE) {
  #   stopCluster(cl)
  # }
  
  return(outfiles)
  
}

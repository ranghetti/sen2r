#' @title Create RGB images from S2 reflectance products.
#' @description Function to create RGB images from Sentinel-2 reflectances.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([safe_shortname]).
#' @param prod_type (optional) Output product (see [safe_shortname] for the 
#'  list of accepted products). If not provided, it is retrieved from the
#'  file name.
#' @param rgb_bands (optional) 3-length integer vector, which the number of the
#'  bands to be used respectively for red, green and blue. 
#'  Default is 4:2 (true colours).
#'  It is also possible to pass a list of 3-length integer vectors
#'  in order to create multiple RGB types for each input file.
#'  Notice that this is the [actual number name of the bands](
#'  https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial):
#'  so, to use i.e. BOA band 11 (1610nm) use the number 11, even if band 11 is
#'  the 10th band of a BOA product (because band 10 is missing).
#' @param scaleRange (optional) Range of valid values. If can be a 2-length
#'  integer vector (min-max for all the 3 bands) or a 6-length vector or 
#'  3x2 matrix (min red, min green, min blue, max red, max green, max blue).
#'  Default is to use c(0,2500) for bands 1-5; c(0,7500) bands 6-12.
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created. Default is the same directory of 
#'  input reflectance files. # FIXME use a subdir with product name
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or "GTiff" in case of VRT input images).
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#' @param overwrite (optional) Logical value: should existing thumbnails be
#'  overwritten? (default: TRUE)
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @return A vector with the names of the created images.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom rgdal GDALinfo
#' @importFrom jsonlite fromJSON
#' @export

s2_rgb <- function(infiles, 
                   prod_type=NA,
                   rgb_bands=4:2,
                   scaleRange=NA,
                   outdir=NA,
                   subdirs=NA,
                   format=NA,
                   compress="DEFLATE",
                   tmpdir=NA,
                   rmtmp=TRUE,
                   parallel=TRUE,
                   overwrite=FALSE,
                   .log_message=NA,
                   .log_output=NA) {
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="s2rgb_")
  }
  dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Compute n_cores
  n_cores <- if (is.numeric(parallel)) {
    as.integer(parallel)
  } else if (parallel==FALSE) {
    1
  } else {
    min(parallel::detectCores()-1, length(infiles), 11) # use at most 11 cores
  }
  if (n_cores<=1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
  }
  
  # Get files metadata
  if (is.na(prod_type)) {
    infiles_meta <- sen2r_getElements(infiles, format="data.table")
  }
  
  # check output format
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))
  if (!is.na(format)) {
    driver <- gdal_formats[gdal_formats$name==format,]
    if (nrow(driver)==0) {
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
  
  # check rgb_bands and scaleRange
  if (!is.list(rgb_bands)) {rgb_bands <- list(rgb_bands)}
  if (!is.list(scaleRange)) {
    scaleRange <- rep(list(scaleRange), length(rgb_bands))
  }
  if (length(rgb_bands) != length(scaleRange)) {
    print_message(
      type="error",
      "\"rgb_bands\" and \"scaleRange\" must be of the same length."
    )
  }
  
  # create subdirs (if requested)
  # rgb_prodnames <- sapply(rgb_bands, function(x) {
  #   paste0("RGB", paste(as.hexmode(x), collapse=""))
  # })
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(rgb_bands)>1, TRUE, FALSE)
  }
  # if (subdirs) {
  #   sapply(file.path(outdir,rgb_prodnames), dir.create, showWarnings=FALSE)
  # }
  
  if (n_cores > 1) {
    cl <- makeCluster(
      n_cores, 
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )
    registerDoParallel(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Starting parallel production of RGB images..."
    )
  }
  
  out_names <- foreach(
    i = seq_along(infiles), 
    .packages = c("foreach","rgdal","sen2r"), 
    .combine=c, 
    .errorhandling="remove"
  ) %DO% {
    
    # redirect to log files
    if (n_cores > 1) {
      if (!is.na(.log_output)) {
        sink(.log_output, split = TRUE, type = "output", append = TRUE)
      }
      if (!is.na(.log_message)) {
        logfile_message = file(.log_message, open = "a")
        sink(logfile_message, type="message")
      }
    }
    
    sel_infile_path <- infiles[i]
    
    # set outdir
    sel_outdir <- if (is.na(outdir)) {
      dirname(sel_infile_path)
    } else {
      outdir
    }
    
    # Determine prod_type
    sel_prod_type <- if (is.na(prod_type)) {
      infiles_meta[i,prod_type]
    } else {
      prod_type
    }
    sel_format <- if (!is.na(format)) {
      format
    } else {
      suppressWarnings(attr(GDALinfo(sel_infile_path), "driver"))
    }
    sel_out_ext <- gdal_formats[gdal_formats$name==sel_format,][1,"ext"]
    
    
    # exclude non BOA-TOA products
    out_names <- if (!sel_prod_type %in% c("BOA","TOA")) {
      print_message(
        type = "warning",
        "Product ",basename(sel_infile_path)," was not considered, ",
        "since this function is only for reflectance files."
      )
      character(0)
    } else {
      
      # Cycle on each rgb_bands combination
      foreach(sel_rgb_bands = rgb_bands, sel_scaleRange = scaleRange, .combine = c) %do% {
        sel_rgb_prodname <- paste0("RGB", paste(as.hexmode(sel_rgb_bands), collapse=""), substr(sel_prod_type,1,1))
        
        # Set output path
        out_subdir <- ifelse(subdirs, file.path(sel_outdir,sel_rgb_prodname), outdir)
        dir.create(out_subdir, recursive = FALSE, showWarnings = FALSE)
        out_path <- file.path(
          out_subdir, 
          gsub(
            paste0("\\_",sel_prod_type,"\\_"),
            paste0("\\_",sel_rgb_prodname,"\\_"),
            gsub("\\.[^\\.]+$", paste0(".",sel_out_ext), basename(sel_infile_path))
          )
        )
        
        # if output already exists and overwrite==FALSE, do not proceed
        if (!file.exists(out_path) | overwrite==TRUE) {
          
          # From Sentinel-2 band number to actual band numbert in the BOA
          sel_nbands <- if (sel_prod_type=="BOA") {
            ifelse(sel_rgb_bands>10, sel_rgb_bands-1, sel_rgb_bands)
          } else {
            sel_rgb_bands
          }
          
          # Consider only the required bands
          filterbands_path <- file.path(tmpdir, gsub("\\..+$","_filterbands.tif",basename(sel_infile_path)))
          system(
            paste0(
              binpaths$gdal_translate," -of GTiff -co COMPRESS=LZW ",
              "-b ",paste(sel_nbands, collapse=" -b ")," ",
              "\"",sel_infile_path,"\" ",
              "\"",filterbands_path,"\""
            ), intern = Sys.info()["sysname"] == "Windows"
          )
          
          # define scaleRange
          sel_scaleRange <- if (anyNA(sel_scaleRange)) {
            # c(0, switch(
            #   sel_rgb_prodname, 
            #   "RGB432T" = 2500, 
            #   "RGB432B" = 2500, 
            #   7500
            # ))
            c(rep(0,3), ifelse(sel_nbands %in% 1:5, 2500, 7500))
          } else {
            sel_scaleRange
          }
          # convert from vector to matrix
          sel_scaleRange <- matrix(sel_scaleRange,ncol=2)
          
          
          # generate RGB basing on prod_type
          stack2rgb(
            filterbands_path, 
            out_file = out_path,
            minval = sel_scaleRange[,1], 
            maxval = sel_scaleRange[,2],
            format=sel_format,
            compress="90",
            tmpdir = tmpdir
          )
          
        } # end of overwrite IF cycle
        
        out_path
        
      } # end of rgb_bands FOREACH cycle
      
    } # end of !sel_prod_type %in% c("BOA","TOA") IF cycle
    
    # stop sinking
    if (n_cores > 1) {
      if (!is.na(.log_output)) {
        sink(type = "output")
      }
      if (!is.na(.log_message)) {
        sink(type = "message"); close(logfile_message)
      }
    }
    
    out_names
    
  } # end of infiles FOREACH cycle
  if (n_cores > 1) {
    stopCluster(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Parallel production of RGB images done."
    )
  }
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  print_message(
    type="message",
    length(out_names)," output RGB files were correctly created."
  )
  return(out_names)
  
}

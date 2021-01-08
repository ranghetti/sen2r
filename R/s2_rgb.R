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
#'  input reflectance files.
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or "GTiff" in case of VRT input images).
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GTiff format was chosen. 
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param proc_mode (optional) Character: if `"gdal_calc"`,
#'  `gdal_calc` routines are used to compute indices;
#'  if `"raster"` or `"stars"`, R functions are instead used
#'  (using respectively `raster` or `stars` routines).
#'  **Note**: default value (`"raster"`) is the only fully supported mode.
#'  `"gdal_calc"` can be used only if a runtime GDAL environment can be properly
#'  configured (no assistance is provided in case of GDAL-related problems).
#'  `"raster"` mode is experimental.
#'  See `s2_calcindices()` for further details.
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
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \donttest{
#' # Define file names
#' ex_in <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif",
#'   package = "sen2r"
#' )
#'
#' # Run function
#' ex_out <- s2_rgb(
#'   infiles = ex_in,
#'   rgb_bands = list(c(11,8,4),c(9,5,4)),
#'   scaleRange = list(c(0,7500), matrix(c(rep(0,3),8500,6000,4000),ncol=2)),
#'   outdir = tempdir(),
#'   compress = 50
#' )
#' ex_out
#' 
#' # Show output
#' oldpar <- par(mfrow = c(1,3), mar = rep(0,4))
#' image(stars::read_stars(ex_in), rgb = 4:2, maxColorValue = 3500, useRaster = TRUE)
#' image(stars::read_stars(ex_out[1]), rgb = 1:3, useRaster = TRUE)
#' image(stars::read_stars(ex_out[2]), rgb = 1:3, useRaster = TRUE)
#' par(oldpar)
#' }

s2_rgb <- function(infiles, 
                   prod_type=NA,
                   rgb_bands=4:2,
                   scaleRange=NA,
                   outdir=NA,
                   subdirs=NA,
                   format=NA,
                   compress="DEFLATE",
                   bigtiff=FALSE,
                   tmpdir=NA,
                   rmtmp=TRUE,
                   proc_mode="raster",
                   parallel=TRUE,
                   overwrite=FALSE,
                   .log_message=NA,
                   .log_output=NA) {
  
  # to avoid NOTE on check
  i <- sel_rgb_bands <- NULL
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Check proc_mode and GDAL external dependency
  if (!proc_mode %in% c("gdal_calc", "raster", "stars")) {
    print_message(
      type = "warning",
      "proc_mode = \"",proc_mode,"\" is not recognised; ",
      "switching to \"raster\"."
    )
    proc_mode <- "raster"
  }
  if (proc_mode == "gdal_calc" && is.null(load_binpaths()$gdal_calc)) {
    tryCatch(
      check_gdal(abort = TRUE),
      error = function(e) {
        print_message(
          type = "warning",
          "External GDAL binaries are required with 'proc_mode = \"gdal_calc\"'; ",
          "please configure them using function check_gdal() ",
          "or through a GUI with check_sen2r_deps(). ",
          "Now switching to proc_mode = \"raster\"."
        )
        proc_mode <- "raster"
      }
    )
  }
  
  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="s2rgb_")
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="s2rgb_")))
  }
  dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  
  # Compute n_cores
  n_cores <- if (is.numeric(parallel)) {
    min(as.integer(parallel), length(infiles))
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
    infiles_meta <- sen2r_getElements(infiles)
  }
  
  # check output format
  gdal_formats <- fromJSON(
    system.file("extdata/settings/gdal_formats.json",package="sen2r")
  )$drivers
  if (!is.na(format)) {
    driver <- gdal_formats[gdal_formats$name==format,]
    if (nrow(driver)==0) {
      print_message(
        type="error",
        "Format \"",format,"\" is not recognised; ",
        "please use one of the formats supported by your GDAL installation."#\n\n",
        # "To list them, use the following command:\n",
        # "gdalUtils::gdalinfo(formats=TRUE)\n\n",
        # "To search for a specific format, use:\n",
        # "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
      )
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
  
  if (n_cores > 1) { # nocov start
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
  } # nocov end
  
  out_names <- foreach(
    i = seq_along(infiles), 
    .packages = c("foreach","sen2r"), 
    .combine=c, 
    .errorhandling="remove"
  ) %DO% {
    
    # redirect to log files
    if (n_cores > 1) { # nocov start
      if (!is.na(.log_output)) {
        sink(.log_output, split = TRUE, type = "output", append = TRUE)
      }
      if (!is.na(.log_message)) {
        logfile_message = file(.log_message, open = "a")
        sink(logfile_message, type="message")
      }
    } # nocov end
    
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
      raster_metadata(sel_infile_path, "outformat", format = "list")[[1]]$outformat
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
          
          print_message(
            type = "message",
            date = TRUE,
            paste0("Generating image ", basename(out_path),"...")
          )
          
          # From Sentinel-2 band number to actual band numbert in the BOA
          sel_nbands <- if (sel_prod_type=="BOA") {
            ifelse(sel_rgb_bands>10, sel_rgb_bands-1, sel_rgb_bands)
          } else {
            sel_rgb_bands
          }
          
          # Consider only the required bands
          filterbands_path <- file.path(tmpdir, gsub("\\..+$","_filterbands.tif",basename(sel_infile_path)))
          gdalUtil(
            "translate",
            source = sel_infile_path,
            destination = filterbands_path,
            options = c(
              "-of", "GTiff", "-co", "COMPRESS=LZW",
              if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")},
              unlist(lapply(sel_nbands, function(x){c("-b", x)}))
            ),
            quiet = TRUE
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
            format = sel_format,
            compress = compress,
            bigtiff = bigtiff,
            proc_mode = proc_mode,
            tmpdir = tmpdir
          )
          
          # fix for envi extension (writeRaster use .envi)
          if (sel_format=="ENVI") {fix_envi_format(out_path)}
          
        } # end of overwrite IF cycle
        
        out_path
        
      } # end of rgb_bands FOREACH cycle
      
    } # end of !sel_prod_type %in% c("BOA","TOA") IF cycle
    
    # stop sinking
    if (n_cores > 1) { # nocov start
      if (!is.na(.log_output)) {
        sink(type = "output")
      }
      if (!is.na(.log_message)) {
        sink(type = "message"); close(logfile_message)
      }
    } # nocov end
    
    out_names
    
  } # end of infiles FOREACH cycle
  if (n_cores > 1) { # nocov start
    stopCluster(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Parallel production of RGB images done."
    )
  } # nocov end
  
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

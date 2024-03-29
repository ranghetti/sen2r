#' @title Compute maps of spectral indices
#' @description Create maps of a set of spectral indices. Since
#'  `gdal_calc.py` is used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#' @param infiles A vector of input filenames. Input files are paths
#'  of BOA (or TOA) products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([safe_shortname]).
#' @param indices Character vector with the names of the required
#'  indices. Values should be included in names corresponding to the
#'  Abbreviations of the following indices:
#'  [IDB](https://www.indexdatabase.de/db/is.php?sensor_id=96).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param parameters (optional) Values of index parameters. This variable
#'  must be a named list, in which each element is a list of parameters,
#'  i.e.:
#'  `parameters = list("SAVI" = list("a" = 0.5))`
#'  Values can be both numeric values or band names (e.g. "band_1").
#'  If not specified, parameters are set to default values.
#' @param source (optional) Vector with the products from which computing
#'  the indices. It can be `"BOA"`, `"TOA"` or both (default). If both values
#'  are provided, indices are computed from the available products (`"TOA"`
#'  if TOA is available, BOA if BOA is available); in the case both are
#'  available, two files are produced (they can be distinguished from the
#'  level component - S2x1C or S2x2A - in the filename).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or "GTiff" in case of VRT input images).
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param tmpdir (optional) Path where intermediate files (GTiff) will be
#'  created in case `format` is `"VRT"`.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GTiff format was chosen. 
#' @param dataType (optional) Numeric datatype of the output rasters.
#'  if "Float32" or "Float64" is chosen, numeric values are not rescaled;
#'  if `"Int16"` (default) or `"UInt16"`, values are multiplicated by `scaleFactor` argument;
#'  if `"Byte"`, values are shifted by 100, multiplicated by 100 and truncated
#'  at 200 (so that range -1 to 1 is coerced to 0-200), and nodata value
#'  is assigned to 255.
#' @param scaleFactor (optional) Scale factor for output values when an integer
#'  datatype is chosen (default values are 10000 for `"Int16"` and `"UInt16"`,
#'  1E9 for `"Int32"` and `"UInt32"`). Notice that, using `"UInt16"` and 
#'  `"UInt32"` types, negative values will be truncated to 0.
#' @param proc_mode (optional) Character: if `"gdal_calc"`,
#'  `gdal_calc` routines are used to compute indices;
#'  if `"raster"` (default) or `"stars"`, R functions are instead used
#'  (using respectively `raster` or `stars` routines).
#'  **Notes**: 
#'  1. default value (`"raster"`) is the only fully supported mode.
#'  `"gdal_calc"` can be used only if a runtime GDAL environment can be properly
#'  configured (no assistance is provided in case of GDAL-related problems).
#'  `"raster"` mode is experimental.
#'  2. There is a difference in which the two modes manage values out
#'      of ranges (e.g. -32768 to 32767 in Int16 and 0 to 255 in Byte):
#'      `"raster"` and `"stars"` modes set these values to NA, 
#'      `"gdal_calc"` mode clip them to the minimum/maximum values;
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#'  Multiprocess masking computation is always performed in singlecore mode
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @return A vector with the names of the created products.
#' @export
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @importFrom raster brick
#' @author Luigi Ranghetti, phD (2020)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @examples
#' # Define file names
#' ex_in <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif",
#'   package = "sen2r"
#' )
#'
#' # Run function
#' ex_out <- s2_calcindices(
#'   infiles = ex_in,
#'   indices = "EVI",
#'   outdir = tempdir(),
#'   dataType = "Float32"
#' )
#' ex_out
#'
#' # Show output
#' oldpar <- par(mfrow = c(1,2), mar = rep(0,4))
#' image(stars::read_stars(ex_in), rgb = 4:2, maxColorValue = 3500, useRaster = TRUE)
#' par(mar = rep(2/3,4))
#' image(stars::read_stars(ex_out), useRaster = TRUE)
#' par(oldpar)

s2_calcindices <- function(
  infiles,
  indices,
  outdir = ".",
  parameters = NULL,
  source = c("TOA","BOA"),
  format = NA,
  subdirs = NA,
  tmpdir = NA,
  compress = "DEFLATE",
  bigtiff=FALSE,
  dataType = "Int16",
  scaleFactor = NA,
  proc_mode = "raster",
  parallel = FALSE,
  overwrite = FALSE,
  .log_message = NA,
  .log_output = NA
) {
  
  # to avoid NOTE on check
  prod_type <- . <- i <- NULL
  
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
    
  
  # create outdir if not existing (and dirname(outdir) exists)
  suppressWarnings(outdir <- expand_path(outdir, parent=comsub(infiles,"/"), silent=TRUE))
  if (!dir.exists(dirname(outdir))) {
    print_message(
      type = "error",
      "The parent folder of 'outdir' (",outdir,") does not exist; ",
      "please create it."
    )
  }
  dir.create(outdir, recursive=FALSE, showWarnings=FALSE)
  
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
  
  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(
    c("n_index","name","longname","s2_formula","a","b","c","d"),
    all = TRUE
  )
  
  # check that the required indices exists
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type="error",
      if (!any(indices %in% indices_db$name)) {"The "} else {"Some of the "},
      "requested index names (\"",
      paste(indices[!indices %in% indices_db$name], collapse="\", \""),
      "\") are not recognisable; please use accepted ",
      "values. To list accepted index names, type ",
      "'sort(list_indices(\"name\", all=TRUE))'.")
  }
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type="warning",
      "Some of the specified index names (",
      paste(indices[!indices %in% indices_db$name],collapse=", "),
      ") are not recognisable and will be skipped.")
    indices <- indices[indices %in% indices_db$name]
  }
  # extract needed indices_db
  indices_info <- indices_db[match(indices,indices_db$name),]
  
  # check output format
  gdal_formats <- fromJSON(system.file("extdata/settings/gdal_formats.json",package="sen2r"))$drivers
  if (!is.na(format)) {
    sel_driver <- gdal_formats[gdal_formats$name == format,]
    if (nrow(sel_driver)==0) {
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
  
  # assign scaleFactor and nodata values
  if (grepl("Int",dataType) & is.na(scaleFactor)) {
    scaleFactor <- ifelse(grepl("Int32",dataType),1E9,1E4)
  }
  sel_nodata <- switch(
    dataType,
    Int16=-2^15, UInt16=2^16-1, Int32=-2^31, UInt32=2^32-1,
    Float32=-9999, Float64=-9999, Byte=255
  )
  
  # Get files metadata
  infiles_meta <- sen2r_getElements(infiles)
  infiles <- infiles[infiles_meta$prod_type %in% source]
  infiles_meta <- infiles_meta[prod_type %in% source,]
  
  # create subdirs (if requested)
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(indices)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,indices), dir.create, showWarnings=FALSE)
  }
  
  # read TOA/BOA image
  if (n_cores > 1) { # nocov start
    cl <- makeCluster(
      n_cores,
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )
    registerDoParallel(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Starting parallel computation of indices..."
    )
  } # nocov end
  
  outfiles <- foreach(
    i = seq_along(infiles),
    .packages = c("raster","stars","sen2r"),
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
    
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])
    sel_format <- suppressWarnings(
      ifelse(!is.na(format), format, raster_metadata(sel_infile, "outformat", format = "list")[[1]]$outformat)
    )
    sel_out_ext <- gdal_formats[gdal_formats$name==sel_format,"ext"][1]
    
    # check bands to use
    if (sel_infile_meta$prod_type=="TOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:12],"number"=1:12,"band"=paste0("band_",1:12), stringsAsFactors = FALSE)
    } else if (sel_infile_meta$prod_type=="BOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:11],"number"=1:11,"band"=paste0("band_",c(1:9,11:12)), stringsAsFactors = FALSE)
    } else {
      print_message(type="error", "Internal error (this should not happen).")
    }
    
    # compute single indices
    # (this cycle is not parallelised)
    sel_outfiles <- character(0)
    for (j in seq_along(indices)) {
      print_message(
        type = "message",
        date = TRUE,
        paste0(
          "Computing index ", indices[j],
          " on date ",sel_infile_meta$sensing_date,"..."
        )
      )
      # extract parameters
      sel_parameters <- parameters[[indices[j]]]
      
      # define output filename
      sel_outfile <- paste0(
        "S2",sel_infile_meta$mission,sel_infile_meta$level,"_",
        strftime(sel_infile_meta$sensing_date,"%Y%m%d"),"_",
        sel_infile_meta$id_orbit,"_",
        sel_infile_meta$extent_name,"_",
        indices_info[j,"name"],"_",
        gsub("m$","",sel_infile_meta$res),".",
        sel_out_ext)
      
      # define subdir
      out_subdir <- ifelse(subdirs, file.path(outdir,indices[j]), outdir)
      
      # if output already exists and overwrite==FALSE, do not proceed
      if (!file.exists(file.path(out_subdir,sel_outfile)) | overwrite==TRUE) {
        
        # if indices[j] requires different parameters basing on sensor,
        # retrieve the correct information (replace all except index name)
        # (for now, this is done "by hand" since only CR indices requires it)
        if (indices[j] %in% c("CRred","BDred","CRred2")) {
          sel_index_info <- indices_db[indices_db$name==paste0(indices[j],"-2",sel_infile_meta$mission),]
          sel_index_info$name <- indices[j]
        } else {
          sel_index_info <- indices_info[j,]
        }
        
        # change index formula to be used with bands
        for (sel_par in c("a","b","c","d")) {
          assign(sel_par, if (is.null(sel_parameters[[sel_par]])) {sel_index_info[,sel_par]} else {sel_parameters[[sel_par]]})
        }
        sel_formula <- sel_index_info[,"s2_formula"]
        
        for (sel_par in c("a","b","c","d")) {
          sel_formula <- gsub(paste0("([^0-9a-zA-Z])par\\_",sel_par,"([^0-9a-zA-Z])"),
                              paste0("\\1",get(sel_par),"\\2"),
                              sel_formula)
        }
        
        # Edit formula depending on proc_mode
        if (proc_mode == "gdal_calc") {
          for (sel_band in rev(seq_len(nrow(gdal_bands)))) {
            sel_formula <- gsub(
              gdal_bands[sel_band,"band"],
              paste0("(",gdal_bands[sel_band,"letter"],".astype(float)/10000)"),
              sel_formula
            )
          }
        } else if (proc_mode == "raster") {
          for (sel_band in rev(seq_len(nrow(gdal_bands)))) {
            sel_formula <- gsub(
              gdal_bands[sel_band,"band"],
              paste0("(v[,",gdal_bands[sel_band,"number"],"]/10000)"),
              sel_formula
            )
          }
        } else if (proc_mode == "stars") {
          for (sel_band in rev(seq_len(nrow(gdal_bands)))) {
            sel_formula <- gsub(
              gdal_bands[sel_band,"band"],
              paste0("(v[",gdal_bands[sel_band,"number"],"]/10000)"),
              sel_formula
            )
          }
        }
        
        # Clip within the datatype range
        if (grepl("Int", dataType)) {
          sel_formula <- paste0(
            "clip(",
            scaleFactor,"*(",sel_formula,"),",
            switch(dataType, Int16=-2^15+2, UInt16=0, Int32=-2^31+4, UInt32=0),",",
            switch(dataType, Int16=2^15-1, UInt16=2^16-2, Int32=2^31-3, UInt32=2^32-4),")"
          )
        }
        if (dataType == "Byte") {
          sel_formula <- paste0("clip(100+100*(",sel_formula,"),0,200)")
        }
        
        # if sel_format is VRT, create GTiff as intermediate source files
        # (cannot create directly .tif files without breaking _req / _exi names)
        if (sel_format == "VRT") {
          
          # define and create tmpdir
          if (is.na(tmpdir)) {
            tmpdir <- file.path(out_subdir, ".vrt")
          }
          dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
          
          sel_format0 <- "GTiff"
          sel_out_ext0 <- gdal_formats[gdal_formats$name==sel_format0,"ext"][1]
          out_subdir0 <- tmpdir
          sel_outfile0 <- gsub(paste0(sel_out_ext,"$"), sel_out_ext0, sel_outfile)
          
        } else {
          sel_format0 <- sel_format
          out_subdir0 <- out_subdir
          sel_outfile0 <- sel_outfile
        }
        
        # Launch the processing
        if (proc_mode == "gdal_calc") {
          gdalUtil(
            "calc",
            source = rep(sel_infile, nrow(gdal_bands)), 
            destination = file.path(out_subdir0,sel_outfile0),
            formula = sel_formula,
            options = c(
              c(apply(gdal_bands,1,function(l){
                c(paste0("--",l["letter"],"_band"), which(gdal_bands$letter==l["letter"]))
              })),
              "--type", dataType,
              "--NoDataValue", sel_nodata,
              "--format", sel_format0,
              if (overwrite==TRUE) {"--overwrite"},
              if (sel_format0 == "GTiff") {c(
                "--co", paste0("COMPRESS=",toupper(compress)),
                "--co", "TILED=YES"
              )},
              if (sel_format0=="GTiff" & bigtiff==TRUE) {c("--co", "BIGTIFF=YES")}
            ),
            quiet = TRUE
          )
        } else if (proc_mode == "raster") {
          calcindex_raster(
            raster::brick(sel_infile),
            sel_formula,
            out_file = file.path(out_subdir0,sel_outfile0),
            NAflag = sel_nodata,
            sel_format = sel_format0,
            compress = compress,
            datatype = dataType,
            overwrite = overwrite
          )
        } else if (proc_mode == "stars") {
          calcindex_stars(
            sel_infile,
            sel_formula,
            out_file = file.path(out_subdir0,sel_outfile0),
            NAflag = sel_nodata,
            sel_format = sel_format0,
            compress = compress,
            datatype = dataType,
            overwrite = overwrite
          )
        }
        # fix for envi extension (writeRaster use .envi)
        if (sel_format0=="ENVI") {fix_envi_format(file.path(out_subdir0,sel_outfile0))}
        
        if (sel_format == "VRT") {
          gdalUtil(
            "buildvrt",
            source = file.path(out_subdir0,sel_outfile0),
            destination = file.path(out_subdir,sel_outfile),
            quiet = TRUE
          )
        }
        
      } # end of overwrite IF cycle
      
      sel_outfiles <- c(sel_outfiles, file.path(out_subdir,sel_outfile))
      
    } # end of indices FOR cycle
    
    # stop sinking
    if (n_cores > 1) { # nocov start
      if (!is.na(.log_output)) {
        sink(type = "output")
      }
      if (!is.na(.log_message)) {
        sink(type = "message"); close(logfile_message)
      }
    } # nocov end
    
    sel_outfiles
    
  } # end cycle on infiles
  if (n_cores > 1) { # nocov start
    stopCluster(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Parallel computation of indices done."
    )
  } # nocov end
  
  return(outfiles)
  
}

# Accessory functions to interpret NumPy functions power() and clip()
power <- function(x,y) {x^y}
clip <- function(x,min,max) {(x+min+2*max+abs(x-min)-abs(x+min-2*max+abs(x-min)))/4}

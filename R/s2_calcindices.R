#' @title Compute maps of spectral indices
#' @description Create maps of a set of spectral indices. Since
#'  `gdal_calc.py` is used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#' @param infiles A vector of input filenames. Input files are paths
#'  of BOA (or TOA) products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([s2_shortname]).
#' @param indices Character vector with the names of the required
#'  indices. Values should be included in names corresponding to the
#'  Abbreviations of the following indices:
#'  [IDB](http://www.indexdatabase.de/db/is.php?sensor_id=96)
#'  FIXME the list of the accepted values is a subset; this reference
#'  will be replaced with an internal html page integrated in the
#'  shiny interface).
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
#'  the indices. It can be "BOA", "TOA" or both (default). If both values
#'  are provided, indices are computed from the available products ("TOA"
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
#'  created in case `format` is "VRT".
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param dataType (optional) Numeric datatype of the ouptut rasters.
#'  if "Float32" or "Float64" is chosen, numeric values are not rescaled;
#'  if "Int16" (default) or "UInt16", values are multiplicated by `scaleFactor` argument;
#'  if "Byte", values are shifted by 100, multiplicated by 100 and truncated
#'  at 200 (so that range -1 to 1 is coherced to 0-200), and nodata value 
#'  is assigned to 255.
#' @param scaleFactor (optional) Scale factor for output values when an integer
#'  datatype is chosen (default values are 10000 for "Int16" and "UInt16", 
#'  1E9 for "Int32" and "UInt32"). Notice that, using "UInt16" and "UInt32" types,
#'  negative values will be truncated to 0.
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#'  Multiprocess masking computation is always performed in singlecore mode
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .logfile_message (optional) Internal parameter
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
#' @importFrom rgdal GDALinfo
#' @importFrom magrittr "%>%"
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_calcindices <- function(infiles,
                           indices,
                           outdir=".",
                           parameters=NULL,
                           source=c("TOA","BOA"),
                           format=NA,
                           subdirs=NA,
                           tmpdir=NA,
                           compress="DEFLATE",
                           dataType="Int16",
                           scaleFactor=NA,
                           parallel = FALSE,
                           overwrite=FALSE,
                           .logfile_message=NA,
                           .log_output=NA) {
  
  prod_type <- . <- NULL
  
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
  
  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(c("n_index","name","longname","s2_formula","a","b","x"))
  
  # check that the required indices exists
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type="error",
      if (!any(indices %in% indices_db$name)) {"The "} else {"Some of the "},
      "requested index names (\"",
      paste(indices[!indices %in% indices_db$name], collapse="\", \""),
      "\") are not recognisable; please use accepted ",
      "values. To list accepted index names, type ",
      "'sort(list_indices(\"name\"))'.")
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
  
  # assign scaleFactor value
  if (grepl("Int",dataType) & is.na(scaleFactor)) {
    scaleFactor <- ifelse(grepl("Int32",dataType),1E9,1E4)
  }
  
  # Get files metadata
  infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
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
  cl <- makeCluster(
    n_cores, 
    type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
  )
  if (n_cores > 1) {
    registerDoParallel(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Starting parallel computation of indices..."
    )
  }
  
  outfiles <- foreach(
    i = seq_along(infiles), 
    .packages = c("raster","rgdal","sen2r"), 
    .combine=c, 
    .errorhandling="remove"
  )  %DO% {
    
    # redirect to log files
    if (!is.na(.log_output)) {
      sink(.log_output, split = TRUE, type = "output", append = TRUE)
    }
    if (!is.na(.logfile_message)) {
      sink(.logfile_message, type="message")
    }
    
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])
    sel_format <- suppressWarnings(ifelse(
      !is.na(format), format, attr(GDALinfo(sel_infile), "driver")
    ))
    sel_out_ext <- gdal_formats[gdal_formats$name==sel_format,"ext"][1]
    
    # check bands to use
    if (sel_infile_meta$prod_type=="TOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:12],"band"=paste0("band_",1:12))
    } else if (sel_infile_meta$prod_type=="BOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:11],"band"=paste0("band_",c(1:9,11:12)))
    } else {
      print_message(type="error", "Internal error (this should not happen).")
    }
    
    # compute single indices
    # (this cycle is not parallelised)
    sel_outfiles <- character(0)
    for (j in seq_along(indices)) {
      
      # extract parameters
      sel_parameters <- parameters[[indices[j]]]
      
      # define output filename
      sel_outfile <- paste0(
        "S2",sel_infile_meta$mission,sel_infile_meta$level,"_",
        strftime(sel_infile_meta$sensing_date,"%Y%m%d"),"_",
        sel_infile_meta$id_orbit,"_",
        switch(sel_infile_meta$type, 
               tile = sel_infile_meta$id_tile, 
               clipped = sel_infile_meta$extent_name, 
               ""),"_",
        indices_info[j,"name"],"_",
        gsub("m$","",sel_infile_meta$res),".",
        sel_out_ext)
      
      # define subdir
      out_subdir <- ifelse(subdirs, file.path(outdir,indices[j]), outdir)
      
      # if output already exists and overwrite==FALSE, do not proceed
      if (!file.exists(file.path(out_subdir,sel_outfile)) | overwrite==TRUE) {
        
        # change index formula to be used with bands
        for (sel_par in c("a","b","x")) {
          assign(sel_par, if (is.null(sel_parameters[[sel_par]])) {indices_info[j,sel_par]} else {sel_parameters[[sel_par]]})
        }
        sel_formula <- indices_info[j,"s2_formula"]
        for (sel_par in c("a","b","x")) {
          sel_formula <- gsub(paste0("([^0-9a-zA-Z])par\\_",sel_par,"([^0-9a-zA-Z])"),
                              paste0("\\1",get(sel_par),"\\2"),
                              sel_formula)
        }
        for (sel_band in seq_len(nrow(gdal_bands))) {
          sel_formula <- gsub(paste0("([^0-9a-zA-Z])",gdal_bands[sel_band,"band"],"([^0-9a-zA-Z])"),
                              paste0("\\1",gdal_bands[sel_band,"letter"],".astype(float)\\2"),
                              sel_formula)
        }
        if (grepl("Int", dataType)) {
          sel_formula <- paste0(
            "clip(",
            scaleFactor,"*(",sel_formula,"),",
            switch(dataType, Int16=-2^15+2, UInt16=0, Int32=-2^31+4, UInt32=0),",",
            switch(dataType, Int16=2^15-1, UInt16=2^16-2, Int32=2^31-3, UInt32=2^32-4),")"
          )
        }
        sel_nodata <- switch(
          dataType, 
          Int16=-2^15, UInt16=2^16-1, Int32=-2^31, UInt32=2^32-1,
          Float32=-9999, Float64=-9999, Byte=255
        )
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
        
        # apply gdal_calc
        system(
          paste0(
            binpaths$gdal_calc," ",
            paste(apply(gdal_bands,1,function(l){
              paste0("-",l["letter"]," \"",sel_infile,"\" --",l["letter"],"_band=",which(gdal_bands$letter==l["letter"]))
            }), collapse=" ")," ",
            "--outfile=\"",file.path(out_subdir0,sel_outfile0),"\" ",
            "--type=\"",dataType,"\" ",
            "--NoDataValue=",sel_nodata," ",
            "--format=\"",sel_format0,"\" ",
            if (overwrite==TRUE) {"--overwrite "},
            if (sel_format0=="GTiff") {paste0("--co=\"COMPRESS=",toupper(compress),"\" ")},
            "--calc=\"",sel_formula,"\""
          ),
          intern = Sys.info()["sysname"] == "Windows"
        )
        
        if (sel_format == "VRT") {
          system(
            paste0(
              binpaths$gdalbuildvrt," ",
              "\"",file.path(out_subdir,sel_outfile),"\" ",
              file.path(out_subdir0,sel_outfile0)
            ),
            intern = Sys.info()["sysname"] == "Windows"
          )
        }
        
      } # end of overwrite IF cycle
      
      sel_outfiles <- c(sel_outfiles, file.path(out_subdir,sel_outfile))
      
    } # end of indices FOR cycle
    
    # stop sinking
    if (!is.na(.log_message)) {sink(type = "message")}
    if (!is.na(.log_output)) {sink(type = "output")}
    
    file.path(out_subdir,sel_outfiles)
    
  } # end cycle on infiles
  if (n_cores > 1) {
    stopCluster(cl)
    print_message(
      type = "message",
      date = TRUE,
      "Parallel computation of indices done."
    )
  }
  
  return(outfiles)
  
}

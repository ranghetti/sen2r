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
#'  in order to create multiple RGB types for each input file. # TODO
#'  Notice that this is the [actual number name of the bands](
#'  https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial):
#'  so, to use i.e. BOA band 11 (1610nm) use the number 11, even if band 11 is
#'  the 10th band of a BOA product (because band 10 is missing).
#' @param scaleRange (optional) Range of valid values. If can be a 2-length
#'  integer vector (min-max for all the 3 bands) or a 6-length vector or 
#'  3x2 matrix (min red, min green, min blue, max red, max green, max blue).
#'  Default is c(0,2500) for true colours RGB, c(0,7500) for others.
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
#' @param overwrite (optional) Logical value: should existing thumbnails be
#'  overwritten? (default: TRUE)
#' @return A vector with the names of the created images.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
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
                   overwrite=FALSE) {
  
  # Check that GDAL suports JPEG JFIF format
  # TODO

  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="s2rgb_")
  }
  dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Get files metadata
  if (is.na(prod_type)) {
    infiles_meta <- data.table(sen2r_getElements(infiles, format="data.frame"))
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
  
  # create subdirs (if requested)
  if (!is.list(rgb_bands)) {rgb_bands <- list(rgb_bands)}
  rgb_prodnames <- sapply(rgb_bands, function(x) {
    paste0("RGB", paste(as.hexmode(x), collapse=""))
  })
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(rgb_bands)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,rgb_prodnames), dir.create, showWarnings=FALSE)
  }
  
  out_names <- character(0) # names of created files
  for (i in seq_along(infiles)) {
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
    sel_format <- suppressWarnings(ifelse(
      !is.na(format), format, attr(GDALinfo(sel_infile_path), "driver")
    ))
    
    # exclude non BOA-TOA products
    if (!sel_prod_type %in% c("BOA","TOA")) {
      print_message(
        type = "warning",
        "Product ",basename(sel_infile_path)," was not considered, ",
        "since this function is only for reflectance files."
      )
      break
    }
    
    
    # Cycle on each rgb_bands combination
    for (sel_rgb_bands in rgb_bands) {
      sel_rgb_prodname <- paste0("RGB", paste(as.hexmode(sel_rgb_bands), collapse=""))
      
      # Set output path
      out_subdir <- ifelse(subdirs, file.path(sel_outdir,sel_rgb_prodname), outdir)
      dir.create(out_subdir, recursive = FALSE, showWarnings = FALSE)
      out_path <- file.path(
        out_subdir, 
        gsub(
          paste0("\\_",sel_prod_type,"\\_"),
          paste0("\\_",sel_rgb_prodname,"\\_"),
          basename(sel_infile_path)
        )
      )
      
      # if output already exists and overwrite==FALSE, do not proceed
      if (!file.exists(out_path) | overwrite==TRUE) {
        
        # Consider only the required bands
        filterbands_path <- file.path(tmpdir, gsub("\\..+$","_filterbands.tif",basename(sel_infile_path)))
        system(
          paste0(
            binpaths$gdal_translate," -of GTiff -co COMPRESS=LZW ",
            "-b ",paste(sel_rgb_bands, collapse=" -b ")," ",
            "\"",sel_infile_path,"\" ",
            "\"",filterbands_path,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
        
        # define scaleRange
        if (anyNA(scaleRange)) {
          scaleRange <- c(0, switch(
            sel_rgb_prodname, 
            "RGB432" = 2500, 
            7500
          ))
        }
        
        # generate RGB basing on prod_type
        stack2rgb(
          filterbands_path, 
          out_file = out_path,
          minval = scaleRange[1], 
          maxval = scaleRange[2],
          format=sel_format,
          compress="90",
          tmpdir = tmpdir
        )
        
      } # end of overwrite IF cycle
      
      out_names <- c(out_names, out_path)
      
    } # end of rgb_bands FOR cycle

  } # end of infiles cycle
  
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

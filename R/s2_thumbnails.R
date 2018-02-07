#' @title Produce an RGB image from a multiband raster file.
#' @description Internal function to create JPEG images from a multiband raster
#'  file. This function is used by [s2_thumbnails], and it will be exported 
#'  when it would be more generalised.
#' @param in_rast Path of the input multiband raster.
#' @param out_file (optional) Path of the output RGB JPEG image; if NULL
#'  (default), a RasterBrick will be returned.
#' @param bands (optional) 3-length integer argument, with the position of 
#'  the three bands to be used respectively for red, green and blue.
#' @param minval (optional) the value corresponding to black (default: 0).
#' @param maxval (optional) the value corresponding to white (default: 10000).
#' @return The path of the output image; alternatively, the output image
#'  as RasterBrick (if `out_rast = NULL`).
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom raster brick calc
#' @importFrom rgdal writeGDAL
#' @importFrom methods as

stack2rgb <- function(in_rast, 
                      out_file = NULL, 
                      bands = 1:3, 
                      minval = 0, 
                      maxval = 1E4) {
  
  # compute RGB
  out_rast <- calc(brick(in_rast), function(x){
    c(
      as.integer(min(max(x[[bands[1]]],minval),maxval)*255/(maxval-minval)+minval),
      as.integer(min(max(x[[bands[2]]],minval),maxval)*255/(maxval-minval)+minval),
      as.integer(min(max(x[[bands[3]]],minval),maxval)*255/(maxval-minval)+minval)
    )
  })
  
  # Return output raster
  if (is.null(out_file)) {
    return(out_rast)
  } else {
    suppressWarnings(
      writeGDAL(
        as(out_rast, "SpatialGridDataFrame"), 
        out_file,
        drivername="JPEG",
        type="Byte",
        options=c("COMPRESS=JPEG")
      )
    )
    return(invisible(NULL))
  }
  
}


#' @title Produce an RGB image from a singleband raster file.
#' @description Internal function to create JPEG or PNG images from a 
#'  singleband raster file. This function is used by [s2_thumbnails], 
#'  and it will be exported when it would be more generalised.
#' @param in_rast Path of the input multiband raster.
#' @param out_file (optional) Path of the output RGB JPEG image; if NULL
#'  (default), a RasterLayer will be returned.
#' @param palette Path of the palette file to be used (cpt or txt),
#'  or character value of a builtin palette ("SCL", "NDVI", the default 
#'  "generic_ndsi" or "generic_ndsi_2").
#' @param minval (to be implemented) the value corresponding to the minimum 
#'  value of the palette (for now, only -1 is used). A quantile will be also 
#'  accepted.
#' @param maxval (to be implemented) the value corresponding to the maximum 
#'  value of the palette (for now, only 1 is used). A quantile will be also 
#'  accepted.
#' @return The path of the output image; alternatively, the output image
#'  as RasterLayer (if `out_rast = NULL`).
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom raster raster
#' @importFrom jsonlite fromJSON

raster2rgb <- function(in_rast, 
                       out_file = NULL, 
                       palette = "generic_ndsi_2", 
                       minval = -1, 
                       maxval = 1) {
  # TODO minval, maxval: now they do not work,
  # add a code to read cpt file, rescale values and save as temp file
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Define builtin palette paths
  palette_builtin <- c(
    "SCL" = system.file("extdata","palettes","SCL.txt", package="fidolasen"),
    "NDVI" = system.file("extdata","palettes","NDVI.cpt", package="fidolasen"),
    "generic_ndsi" = system.file("extdata","palettes","NDSI.cpt", package="fidolasen"),
    "Zscore" = system.file("extdata","palettes","Zscore.cpt", package="fidolasen")
  )
  
  # Load GDAL paths
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (is.null(binpaths$gdalinfo)) {
    check_gdal()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Load palette
  if (!is.character(palette)) {
    print_message(
      type = "error",
      "Argument \"palette\" must be an allowed character value ",
      "or the path of a cpt file."
    )
  }
  if (palette %in% names(palette_builtin)) {
    palette <- palette_builtin[palette]
  }
  
  # Set output file
  return_raster <- if (is.null(out_file)) {
    out_file <- tempfile()
    TRUE
  } else {
    FALSE
  }
  
  # Compute RGB with gdaldem
  system(
    paste0(
      binpaths$gdaldem," color-relief ",
      if (gsub("^.*\\.(.+)$","\\1",out_file) == "png") {
        "-of PNG -co ZLEVEL=9 -co NBITS=8 " # discrete values
      } else { # if (gsub("^.*\\.(.+)$","\\1",out_file) %in% c("jpg","jpeg")) {
        "-of JPEG -co QUALITY=90 " # continuous values
      },
      "\"",in_rast,"\" ",
      "\"",palette,"\" ",
      "\"",out_file,"\""
    ), intern = Sys.info()["sysname"] == "Windows"
  )
  
  # Return output raster
  if (return_raster) {
    return(raster(out_file))
  } else {
    return(invisible(NULL))
  }
  
}


#' @title Create thumbnails for S2 products.
#' @description Function to create thumbnail images for Sentinel-2
#'  products. BOA and TOA multiband images are rendered as false colour
#'  JPEG images; SCL maps are rendered as 8-bit PNG;
#'  other singleband images (like spectral indices) are rendered as 
#'  JPEG images with a standard color palette.
#'  Some improvements still have to be done:
#'  * allowing the possibility to chose the palette and the limits 
#'      (for now, the range -1 to 1 is always used);
#'  * adding support for RGB products.
#'  
#'  Output images are georeferenced.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the fidolasen-S2 naming convention
#'  ([s2_shortname]).
#' @param prod_type (optional) Output product (see [s2_shortname] for the 
#'  list of accepted products). If not provided, it is retrieved from the
#'  file name.
#' @param rgb_type (optional) For BOA and TOA products, this value determine
#'  the type of false colours to be used for the thumbnails:
#'  * `"SwirNirR"` (default) for SWIR-NIR-Red;
#'  * `"NirRG"` for NIR-Red-Green;
#'  * `"RGB"` for true colours;
#'  
#' @param dim Integer value, with the maximum greater dimension in pixels (width or 
#'  height) of the output images (default: 1024 px). 
#'  If this is lower than the corresponding dimension of the maps, maps are
#'  rescaled before producing the thumbnails; otherwise the original dimensions
#'  are maintained. 
#'  To keep the original size in any case, set `dim = Inf`.
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created.  Default is a subdirectory (named 
#'  "thumbnails") of the parent directory of each input file.
#' @param tmpdir (optional) Path where intermediate VRT will be created.
#'  Default is a temporary directory.
#' @param overwrite (optional) Logical value: should existing thumbnails be
#'  overwritten? (default: TRUE)
#' @return A vector with the names of the created images.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
#' @export

s2_thumbnails <- function(infiles, 
                          prod_type=NA, # TODO implement (for now only NOA / TOA)
                          rgb_type="SwirNirR",
                          dim=1024, 
                          outdir=NA,
                          tmpdir=NA,
                          overwrite=FALSE) {
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="dir")
    dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  }
  
  # Load GDAL paths
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (is.null(binpaths$gdalinfo)) {
    check_gdal()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Get files metadata
  if (is.na(prod_type)) {
    infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
  }
  
  out_names <- character(0) # names of created files
  for (i in seq_along(infiles)) {
    sel_infile_path <- infiles[i]
    
    # set outdir
    if (is.na(outdir)) {
      sel_outdir <- file.path(dirname(sel_infile_path), "thumbnails")
      dir.create(sel_outdir, recursive = FALSE, showWarnings = FALSE)
    }
    
    # Determine prod_type
    sel_prod_type <- if (is.na(prod_type)) {
      infiles_meta[i,prod_type]
    } else {
      prod_type
    }
    
    # Set output path
    out_path <- file.path(
      sel_outdir, 
      gsub(
        "\\..+$",
        if (sel_prod_type %in% c("SCL")) {".png"} else {".jpg"}, # resp. discrete or continuous values
        basename(sel_infile_path)
      )
    )
    
    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(out_path) | overwrite==TRUE) {
      
      # Resize input if necessary
      sel_infile_size <- suppressWarnings(GDALinfo(sel_infile_path)[c("rows","columns")])
      if (dim < max(sel_infile_size)) {
        out_size <- round(sel_infile_size * min(dim,max(sel_infile_size)) / max(sel_infile_size))
        resized_path <- file.path(tmpdir, gsub("\\..+$",".vrt",basename(sel_infile_path)))
        system(
          paste0(
            binpaths$gdal_translate," -of VRT ",
            "-outsize ",out_size[2]," ",out_size[1]," ",
            if (sel_prod_type %in% c("SCL")) {"-r mode "} else {"-r average "}, # resp. discrete or continuous values
            "\"",sel_infile_path,"\" ",
            "\"",resized_path,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
      } else {
        resized_path <- sel_infile_path
      }
      
      # generate RGB basing on prod_type
      if (sel_prod_type %in% c("BOA","TOA")) {
        
        stack2rgb(
          resized_path, 
          out_file = out_path,
          bands = switch(
            rgb_type,
            "SwirNirR" = c(11,8,4),
            "NirRG" = c(8,4,3),
            "RGB" = c(4,3,2)
          ),
          minval = 0, 
          maxval = switch(
            rgb_type,
            "SwirNirR" = 8000,
            "NirRG" = 7500,
            "RGB" = 2500
          )
        )
        
      } else {
        
        raster2rgb(
          resized_path, 
          out_file = out_path, 
          palette = if (sel_prod_type %in% c("SCL")) {
            sel_prod_type
          } else if (grepl("\\-Z$",sel_prod_type)) {
            "Zscore"
          } else {
            "generic_ndsi"
          }#,
          # TODO cycle here above basing on sel_prod_type to use different palettes for different products / indices
          # minval = -1, 
          # maxval = 1
        )
        
      }
      
    } # end of overwrite IF cycle
    
    out_names <- c(out_names, out_path)
    
  } # end of infiles cycle
  
  print_message(
    type="message",
    length(out_names)," output files were correctly created."
  )
  return(out_names)
  
}

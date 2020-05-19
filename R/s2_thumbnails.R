#' @title Produce an RGB image from a multiband raster file.
#' @description Internal function to create JPEG images from a multiband raster
#'  file. This function is used by [s2_thumbnails], and it will be exported
#'  when it would be more generalised.
#' @param in_rast Input raster (as `Raster*` or `stars` object).
#' @param out_file (optional) Path of the output RGB JPEG image; if NULL
#'  (default), a RasterBrick will be returned.
#' @param bands (optional) 3-length integer argument, with the position of
#'  the three bands to be used respectively for red, green and blue.
#' @param minval (optional) the value corresponding to black (default: 0).
#'  Also a 3-length vector is accepted
#'  (min values for red, green and blue respectively).
#' @param maxval (optional) the value corresponding to white (default: 10000).
#'  Also a 3-length vector is accepted
#'  (max values for red, green and blue respectively).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is JPEG.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#'  In the case a JPEG format is present, the compression indicates the quality
#'  (integer, 0-100).
#'  In the case a GTiff format is present and an integer 0-100 number is provided,
#'  this is interpreted as the quality level of a JPEG compression.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GTiff format was chosen. 
#' @param proc_mode (optional) Character: if `"gdal_calc"`,
#'  `gdal_calc` routines are used to compute indices;
#'  if `"raster"` (default) or `"stars"`, R functions are instead used
#'  (using respectively `raster` or `stars` routines).
#'  See `s2_calcindices()` for further details.
#' @param tmpdir (optional) Path where intermediate files will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @return The path of the output image; alternatively, the output image
#'  as RasterBrick (if `out_rast = NULL`).
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom raster raster
#' @importFrom jsonlite fromJSON
#' @export

stack2rgb <- function(in_rast,
                      out_file = NULL,
                      bands = 1:3,
                      minval = 0,
                      maxval = 1E4,
                      format = "JPEG",
                      compress = "90",
                      bigtiff = FALSE,
                      proc_mode = "raster",
                      tmpdir = NA) {
  
  # Accessory functions to interpret NumPy functions power() and clip()
  clip <- function(x,min,max) {(x+min+2*max+abs(x-min)-abs(x+min-2*max+abs(x-min)))/4}
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="stack2rgb_")
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="stack2rgb_")))
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # Set output file
  return_raster <- if (is.null(out_file)) {
    out_file <- file.path(tmpdir, basename(tempfile(pattern = "stack2rgb_")))
    TRUE
  } else {
    FALSE
  }
  
  # Check minval-maxval length
  if (!(length(minval) == 1 & length(maxval) == 1 |
        length(minval) == 3 & length(maxval) == 3)) {
    print_message(
      type = "error",
      "minval and maxval must be of length 1 or 3."
    )
  }
  
  # Define format, compression and quality
  co <- if (grepl("^[0-9]+$", compress)) {
    if (format == "JPEG") {
      c("-co",  paste0("QUALITY=",compress))
    } else {
      c("-co", "COMPRESS=JPEG", "-co", paste0("JPEG_QUALITY=",compress))
    }
  } else {
    c("-co", paste0("COMPRESS=",compress))
  }
  if (bigtiff == TRUE) {co <- c(co, "-co", "BIGIFF=TRUE")}
  
  # Define formula (one if minval-maxval are unique, three elsewhere)
  sel_formula <- paste0(
    "clip(", if (proc_mode == "gdal_calc") {"A.astype(float),"} else {"v,"},
    minval,",",maxval,")*255/(",maxval,"-",minval,")+",minval
  )

  ## Compute RGB with the selected mode
  # (an intermediate step creating a GeoTiff is required,
  # since gdal_calc is not able to write in JPEG format)
  
  # if minmax is the same for all 3 bands, use a single step with gdal_calc;
  # else, create 3 tiffs and merge them with gdalbuildvrt
  if (any(
    length(sel_formula) == 1,
    all(proc_mode %in% c("raster", "stars"), length(unique(sel_formula)) == 1)
  )) {
    
    interm_path <- file.path(tmpdir, gsub("\\..+$","_temp.tif", basename(out_file)))

    if (proc_mode == "raster") {
      calcindex_raster(
        in_rast,
        sel_formula[1],
        out_file = interm_path,
        NAflag = 0,
        sel_format = "GTiff",
        compress = "LZW",
        datatype = "Byte"
      )
    } else if (proc_mode == "stars") {
      calcindex_stars(
        in_rast,
        sel_formula[1],
        out_file = interm_path,
        NAflag = 0,
        sel_format = "GTiff",
        compress = "LZW",
        datatype = "Byte"
      )
    } else if (proc_mode == "gdal_calc") {
      gdalUtil(
        "calc",
        source = in_rast,
        destination = interm_path,
        formula = gdal_formula,
        options = c(
          "--allBands", "A",
          "--type", "Byte",
          "--NoDataValue", "0",
          "--format", "GTiff"
        ),
        quiet = TRUE
      )
    }
    
  } else {
    
    interm_paths <- sapply(seq_along(minval), function(i) {file.path(
      tmpdir,
      gsub("\\..+$",paste0("_temp",i,".tif"),basename(out_file))
    )})
    interm_path <- gsub("\\_temp1.tif$", "_temp.vrt", interm_paths[1])

    for (i in seq_along(minval)) {
      if (proc_mode == "raster") {
        calcindex_raster(
          brick(in_rast)[[i]],
          sel_formula[i],
          out_file = interm_paths[i],
          NAflag = 0,
          sel_format = "GTiff",
          compress = "LZW",
          datatype = "Byte"
        )
      } else if (proc_mode == "stars") {
        
        calcindex_stars(
          eval(parse(text=paste0("read_stars(in_rast, proxy = TRUE)[,,,",eval(parse(text="i")),"]"))),
          sel_formula[i],
          out_file = interm_paths[i],
          NAflag = 0,
          sel_format = "GTiff",
          compress = "LZW",
          datatype = "Byte"
        )
      } else if (proc_mode == "gdal_calc") {
        gdalUtil(
          "calc",
          source = in_rast,
          destination = interm_paths[i],
          formula = gdal_formula[i],
          options = c(
            "--A_band", i,
            "--type", "Byte",
            "--NoDataValue", "0",
            "--format", "GTiff"
          ),
          quiet = TRUE
        )
      }
    }
 
    gdalUtil(
      "buildvrt",
      source = interm_paths,
      destination = interm_path,
      options = c("-separate"),
      quiet = TRUE
    )
    
  }
  
  gdalUtil(
    "translate",
    source = interm_path,
    destination = out_file,
    options = c("-of", format, co, "-ot", "Byte"),
    quiet = TRUE
  )
  if (exists("interm_paths")) {sapply(interm_paths, unlink)}
  unlink(interm_path)
  
  
  # Return output raster
  if (return_raster) {
    return(brick(out_file))
  } else {
    return(invisible(NULL))
  }
  
  # # old internal (raster::calc) method
  # out_rast <- calc(brick(in_rast), function(x){
  #   c(
  #     as.integer(min(max(x[[bands[1]]],minval),maxval)*255/(maxval-minval)+minval),
  #     as.integer(min(max(x[[bands[2]]],minval),maxval)*255/(maxval-minval)+minval),
  #     as.integer(min(max(x[[bands[3]]],minval),maxval)*255/(maxval-minval)+minval)
  #   )
  # })
  #
  # # Return output raster
  # if (is.null(out_file)) {
  #   return(out_rast)
  # } else {
  #   suppressWarnings(
  #     writeGDAL(
  #       as(out_rast, "SpatialGridDataFrame"),
  #       out_file,
  #       drivername="JPEG",
  #       type="Byte",
  #       options=c("COMPRESS=JPEG")
  #     )
  #   )
  #   return(invisible(NULL))
  # }
  
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
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GTiff format was chosen. 
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @return The path of the output image; alternatively, the output image
#'  as RasterLayer (if `out_rast = NULL`).
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom raster raster
#' @importFrom jsonlite fromJSON
#' @export

raster2rgb <- function(in_rast,
                       out_file = NULL,
                       palette = "generic_ndsi_2",
                       minval = -1,
                       maxval = 1,
                       bigtiff = FALSE,
                       tmpdir = NA) {
  # TODO minval, maxval: now they do not work,
  # add a code to read cpt file, rescale values and save as temp file
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Define builtin palette paths
  palette_builtin <- c(
    "SCL" = system.file("extdata/palettes/SCL.txt", package="sen2r"),
    "NDVI" = system.file("extdata/palettes/NDVI.cpt", package="sen2r"),
    "generic_ndsi" = system.file("extdata/palettes/NDSI.cpt", package="sen2r"),
    "Zscore" = system.file("extdata/palettes/Zscore.cpt", package="sen2r")
  )
  
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
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="raster2rgb_")
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="raster2rgb_")))
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # Rescale palette, if necessary
  if (!names(palette) %in% c("SCL") & (minval!=-1 | maxval!=1)) {
    palette_txt <- strsplit(gsub("^ *(.*) *$","\\1",readLines(palette))," +")
    palette_txt_new <- palette_txt
    for (i in seq_along(palette_txt)) {
      if (length(palette_txt[[i]])==8 & !anyNA(suppressWarnings(as.numeric(palette_txt[[i]])))) {
        palette_txt_new[[i]][c(1,5)] <- (as.numeric(palette_txt[[i]])[c(1,5)]+1)/2*(maxval-minval)+minval
      }
    }
    writeLines(
      sapply(palette_txt_new, paste, collapse=" "),
      palette <- file.path(tmpdir, basename(tempfile(fileext = ".cpt")))
    )
  }
  
  # Set output file
  return_raster <- if (is.null(out_file)) {
    out_file <- file.path(tmpdir, basename(tempfile(pattern = "raster2rgb_")))
    TRUE
  } else {
    FALSE
  }
  
  # Compute RGB with gdaldem
  # (an intermediate step creating a GeoTiff is required,
  # since gdal_calc is not able to write in JPEG format)
  tif_path <- file.path(tmpdir, gsub("\\..+$","_temp.tif",basename(out_file)))
  gdalUtil(
    "demprocessing",
    source = in_rast,
    destination = tif_path,
    processing = "color-relief",
    colorfilename = palette,
    options = c(
      "-of", "GTiff", "-co", "COMPRESS=LZW", # discrete values
      if (bigtiff == TRUE) {c("-co", "BIGTIFF=YES")},
      "-compute_edges"
    ),
    quiet = TRUE
  )
  gdalUtil(
    "translate",
    source = tif_path,
    destination = out_file,
    options = c(
      if (gsub("^.*\\.(.+)$","\\1",out_file) == "png") {
        c("-of", "PNG", "-co", "ZLEVEL=9", "-co", "NBITS=8") # discrete values
      } else if (gsub("^.*\\.(.+)$","\\1",out_file) %in% c("jpg","jpeg")) {
        c("-of", "JPEG", "-co", "QUALITY=90") # continuous values
      }
    ),
    quiet = TRUE
  )
  unlink(tif_path)
  
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
#'  JPEG images with a standard colour palette.
#'  Output images are georeferenced.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the sen2r naming convention
#'  ([safe_shortname]).
#' @param prod_type (optional) Output product (see [safe_shortname] for the
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
#' @param scaleRange (optional) Range of valid values. If not specified
#'  (default), it is automatically retrieved from the product type.
#'  Default ranges for BOA and TOA products are 0 to 8000
#'  (`rgb_type = "SwirNirR"`), 0 to 7500 (`"NirRG"`) and 0 to 2500 (`"RGB"`).
#'  For spectral indices, default range is -1 to 1 for Float products, -10000
#'  to 10000 for Int and 0 to 200 for Byte; for "Zscore" products, default
#'  range is -3 to 3 for Float and -3000 to 3000 for Int.
#'  It can be useful i.e. to stretch BOA "dark" products.
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created. Default is a subdirectory (named
#'  "thumbnails") of the parent directory of each input file.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param proc_mode (optional) Character: if `"gdal_calc"`,
#'  `gdal_calc` routines are used to compute indices;
#'  if `"raster"` or `"stars"`, R functions are instead used
#'  (using respectively `raster` or `stars` routines).
#'  Default (NA) is `"gdal_calc"` if a runtime GDAL is found; `"raster"` elsewhere.
#'  See `s2_calcindices()` for further details.
#' @param overwrite (optional) Logical value: should existing thumbnails be
#'  overwritten? (default: TRUE)
#' @return A vector with the names of the created images.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @export

s2_thumbnails <- function(infiles,
                          prod_type=NA, # TODO implement (for now only NOA / TOA)
                          rgb_type="SwirNirR",
                          dim=1024,
                          scaleRange=NA,
                          outdir=NA,
                          tmpdir=NA,
                          rmtmp=TRUE,
                          proc_mode=NA,
                          overwrite=FALSE) {
  
  # Check that GDAL supports JPEG JFIF format
  # TODO
  
  # Check proc_mode and GDAL external dependency
  if (is.na(proc_mode)) {
    proc_mode <- if (is.null(load_binpaths()$gdal_calc)) {"gdal_calc"} else {"raster"}
  }
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
    tmpdir <- tempfile(pattern="s2thumbnails_")
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="s2thumbnails_")))
  }
  dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  
  # Get files metadata
  if (is.na(prod_type)) {
    infiles_meta <- sen2r_getElements(infiles, format="data.table")
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
        "\\.[^\\.]+$",
        if (sel_prod_type %in% c("SCL")) {".png"} else {".jpg"}, # resp. discrete or continuous values
        basename(sel_infile_path)
      )
    )
    
    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(out_path) | overwrite==TRUE) {
      
      # Consider only the required bands
      if (sel_prod_type %in% c("BOA","TOA")) {
        rgb_bands = switch(
          rgb_type,
          "SwirNirR" = c(11,8,4),
          "NirRG" = c(8,4,3),
          "RGB" = c(4,3,2)
        )
        filterbands_path <- file.path(tmpdir, gsub("\\..+$","_filterbands.vrt",basename(sel_infile_path)))
        gdalUtil(
          "translate",
          source = sel_infile_path,
          destination = filterbands_path,
          options = c(
            "-of", "VRT",
            unlist(lapply(rgb_bands, function(x){c("-b", x)}))
          ),
          quiet = TRUE
        )
      } else {
        filterbands_path <- sel_infile_path
      }
      
      # Resize input if necessary
      sel_infile_size <- raster_metadata(sel_infile_path, "size", format = "list")[[1]]$size
      resized_path <- file.path(tmpdir, gsub(
        "\\..+$",
        if (sel_prod_type %in% c("BOA","TOA")) {"_resized.tif"} else {"_resized.vrt"},
        basename(sel_infile_path)
      )) # GTiff is used for multiband images to avoid problems using gdal_calc (#82)
      if (dim < max(sel_infile_size)) {
        out_size <- round(sel_infile_size * min(dim,max(sel_infile_size)) / max(sel_infile_size))
        gdalUtil(
          "warp",
          source = filterbands_path,
          destination = resized_path,
          options = c(
            "-of", if (sel_prod_type %in% c("BOA","TOA")) {c("GTiff", "-co", "COMPRESS=LZW")} else {"VRT"},
            "-ts", as.vector(out_size),
            "-r", if (sel_prod_type %in% c("SCL")) {"mode"} else {"average"} # resp. discrete or continuous values
          ),
          quiet = TRUE
        )
      } else {
        if (sel_prod_type %in% c("BOA","TOA")) {
          gdalUtil(
            "translate",
            source = filterbands_path,
            destination = resized_path,
            options = c("-of", "GTiff", "-co", "COMPRESS=LZW"),
            quiet = TRUE
          )
        } else {
          resized_path <- filterbands_path
        }
      }
      
      # define scaleRange
      sel_scaleRange <- if (anyNA(scaleRange)) {
        if (sel_prod_type %in% c("BOA","TOA")) {
          c(0, switch(rgb_type, "SwirNirR" = 8000, "NirRG" = 7500, "RGB" = 2500))
        } else if (sel_prod_type %in% c("Zscore","rbias")){
          sel_infile_datatype <- raster_metadata(sel_infile_path)$type
          if (grepl("^Float",sel_infile_datatype)) {
            if (sel_prod_type == "Zscore") {c(-3, 3)} else {c(-300, 300)}
          } else if (grepl("^Int",sel_infile_datatype)) {
            c(-3E3,3E3)
          } else if (grepl("^Byte$",sel_infile_datatype)) {
            c(0,200)
          }
        } else if (sel_prod_type %in% c("SCL")){
          rep(NA,2) # it is ignored
        } else { # spectral indices
          sel_infile_datatype <- raster_metadata(sel_infile_path)$type
          if (grepl("^Float",sel_infile_datatype)) {
            c(-1, 1)
          } else if (grepl("^Int",sel_infile_datatype)) {
            c(-1E4,1E4)
          } else if (grepl("^Byte$",sel_infile_datatype)) {
            c(0,200)
          }
        }
      } else {
        scaleRange
      }
      
      # generate RGB basing on prod_type
      if (sel_prod_type %in% c("BOA","TOA")) {
        
        stack2rgb(
          resized_path,
          out_file = out_path,
          minval = sel_scaleRange[1],
          maxval = sel_scaleRange[2],
          proc_mode = proc_mode,
          tmpdir = tmpdir
        )
        
      } else if (grepl("^((TCI)|(RGB[0-9a-f]{3}[BT]))$" ,sel_prod_type)) {
        
        gdalUtil(
          "translate",
          source = resized_path,
          destination = out_path,
          options = c("-of", "JPEG", "-co", "QUALITY=90", "-a_nodata", "0"),
          quiet = TRUE
        )
        
      } else {
        
        raster2rgb(
          resized_path,
          out_file = out_path,
          palette = if (sel_prod_type %in% c("SCL")) {
            sel_prod_type
          } else if (grepl("\\-Z$",sel_prod_type) | sel_prod_type=="Zscore") {
            "Zscore"
          } else {
            "generic_ndsi"
          },
          minval = sel_scaleRange[1],
          maxval = sel_scaleRange[2],
          tmpdir = tmpdir
        )
        
      }
      
    } # end of overwrite IF cycle
    
    out_names <- c(out_names, out_path)
    
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

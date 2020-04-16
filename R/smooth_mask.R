#' @title Buffer cloud masks
#' @description Internal function (used by [s2_mask]) which smooths
#'  and buffers a 0-1 mask image in order to reduce the roughness of the mask
#'  obtained from SCL classification (which is done pixel by pixel).
#'  See details.
#' @param inmask The path of the input 0-1 mask (where 0 represents the area
#'  to be masked, 1 the clean surface).
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param radius (optional) Numerical (positive): the size (in the unit of
#'  `inmask`, typically metres) to be used as radius for the smoothing
#'  (the higher it is, the more smooth the output mask will result).
#' @param buffer (optional) Numerical (positive or negative): the size of the
#'  buffer (in the unit of `inmask`, typically metres) to be applied to the
#'  masked area after smoothing it (positive to enlarge, negative to reduce).
#' @param namask (optional) The path of an input 0-1 mask where 0 represents
#'  the area of the original file with NA values (which should not be
#'  smoothed / buffered).
#'  Default (NULL) means that no NA values are present.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#' @return The path of the smoothed mask.
#' @importFrom methods is
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0

smooth_mask <- function(
  inmask, 
  tmpdir = tempdir(), 
  radius = 250, 
  buffer = 250, 
  namask = NULL, 
  bigtiff = FALSE
) {
  
  # if inmask is a raster use the path (it should not happen)
  inmask_path0 <- if (is(inmask, "character")) {
    inmask
  } else {
    inmask@file@name
  }
  # if inmask is a raster use the path (it should not happen)
  namask_path <- if (is.null(namask) | is(namask, "character")) {
    namask
  } else {
    namask@file@name
  }
  
  # convert radius and buffer from metres to number of pixels
  # (as required by gdal_fillnodata)
  inmask_res <- mean(raster_metadata(inmask_path0, "res", format = "list")[[1]]$res)
  
  radius_npx <- abs(radius / inmask_res)
  buffer_npx <- buffer / inmask_res
  
  
  # 1. set inmask=1 (clear sky) and namask=0 (nodata) to NA
  inmask_path1 <- file.path(tmpdir,basename(tempfile(pattern = "mask_", fileext = ".tif")))
  if (!is.null(namask)) {
    gdalUtil(
      "calc",
      source = c(inmask_path0, namask_path), 
      destination = inmask_path1,
      formula = "A+1-B",
      options = c(
        "--type", "Byte", "--NoDataValue", "1", "--format", "GTiff", 
        "--co", "COMPRESS=LZW", if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
      ),
      quiet = TRUE
    )
  } else {
    gdalUtil(
      "translate",
      source = inmask_path0,
      destination = inmask_path1,
      options = c(
        "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "1",
        if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
      ),
      quiet = TRUE
    )
  }
  
  if (radius_npx != 0) {
    
    # 2. first positive buffer (1/2 radius)
    inmask_path2 <- gsub("\\.tif$","_2.tif",inmask_path1)
    gdalUtil(
      "fillnodata",
      source = inmask_path1, 
      destination = inmask_path2,
      options = c(
        "-md", radius_npx*3/4, 
        "-si", "0", "-of", "GTiff", "-co", "COMPRESS=LZW"
      ),
      quiet = TRUE
    )
    
    # 3. invert the image: set inmask=0 (clouds) and namask=0 (nodata) to NA
    inmask_path3 <- gsub("\\.tif$","_3.tif",inmask_path1)
    if (!is.null(namask)) {
      inmask_path2b <- gsub("\\.tif$","_2b.tif",inmask_path1)
      gdalUtil(
        "translate",
        source = inmask_path2,
        destination = inmask_path2b,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "none",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
      gdalUtil(
        "calc",
        source = c(inmask_path2b, namask_path), 
        destination = inmask_path3,
        formula = "A*B",
        options = c(
          "--type", "Byte", "--NoDataValue", "1", "--format", "GTiff", 
          "--co", "COMPRESS=LZW", if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    } else {
      gdalUtil(
        "translate",
        source = inmask_path2,
        destination = inmask_path3,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "0",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    }
    
    # 2. second negative buffer (3/4 radius + 5/4 radius)
    inmask_path4 <- gsub("\\.tif$","_4.tif",inmask_path1)
    gdalUtil(
      "fillnodata",
      source = inmask_path3, 
      destination = inmask_path4,
      options = c(
        "-md", radius_npx*2,
        "-si", "0", "-of", "GTiff", "-co", "COMPRESS=LZW"
      ),
      quiet = TRUE
    )
    
    # 5. invert the image: set inmask=1 (clear sky) and namask=0 (nodata) to NA
    inmask_path5 <- gsub("\\.tif$","_5.tif",inmask_path1)
    if (!is.null(namask)) {
      inmask_path4b <- gsub("\\.tif$","_4b.tif",inmask_path1)
      gdalUtil(
        "translate",
        source = inmask_path4,
        destination = inmask_path4b,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "none",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
      gdalUtil(
        "calc",
        source = c(inmask_path4b, namask_path), 
        destination = inmask_path5,
        formula = "A*B+1-B",
        options = c(
          "--type", "Byte", "--NoDataValue", "1", "--format", "GTiff", 
          "--co", "COMPRESS=LZW", if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    } else {
      gdalUtil(
        "translate",
        source = inmask_path4,
        destination = inmask_path5,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "1",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    }
    
  } else {
    inmask_path5 <- inmask_path1
  } # end of IF radius > 0 cycle
  
  # 6. third positive buffer (5/4 radius to complete smooth, buffer to buffer if < 0, to 3/2 buffer if >0)
  inmask_path6 <- gsub("\\.tif$","_6.tif",inmask_path1)
  gdalUtil(
    "fillnodata",
    source = inmask_path5, 
    destination = inmask_path6,
    options = c(
      "-md", radius_npx*5/4+ifelse(buffer_npx>0,buffer_npx*3/2,buffer_npx),
      "-si", "0", "-of", "GTiff", "-co", "COMPRESS=LZW"
    ),
    quiet = TRUE
  )
  
  # 7-8 if buffer_npx > 0
  if (buffer_npx > 0) {
    
    # 7. invert the image: set inmask=0 (clouds) and namask=0 (nodata) to NA
    inmask_path7 <- gsub("\\.tif$","_7.tif",inmask_path1)
    if (!is.null(namask)) {
      inmask_path6b <- gsub("\\.tif$","_6b.tif",inmask_path1)
      gdalUtil(
        "translate",
        source = inmask_path6,
        destination = inmask_path6b,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "none",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
      gdalUtil(
        "calc",
        source = c(inmask_path6b, namask_path), 
        destination = inmask_path7,
        formula = "A*B",
        options = c(
          "--type", "Byte", "--NoDataValue", "1", "--format", "GTiff", 
          "--co", "COMPRESS=LZW", if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    } else {
      gdalUtil(
        "translate",
        source = inmask_path6,
        destination = inmask_path7,
        options = c(
          "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "0",
          if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
        ),
        quiet = TRUE
      )
    }
    
    # 8. fourth negative buffer (1/2)
    inmask_path8 <- gsub("\\.tif$","_8.tif",inmask_path1)
    gdalUtil(
      "fillnodata",
      source = inmask_path7, 
      destination = inmask_path8,
      options = c(
        "-md", buffer_npx/2, 
        "-si", "0", "-of", "GTiff", "-co", "COMPRESS=LZW"
      ),
      quiet = TRUE
    )
    
  }
  
  # 9. remove nodata labels
  inmask_path9 <- gsub("\\.tif$","_9.tif",inmask_path1)
  gdalUtil(
    "translate",
    source = ifelse(buffer_npx>0, inmask_path8, inmask_path6),
    destination = inmask_path9,
    options = c(
      "-of", "GTiff", "-co", "COMPRESS=LZW", "-a_nodata", "none",
      if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
    ),
    quiet = TRUE
  )
  if (!is.null(namask)) {
    inmask_path9b <- gsub("\\.tif$","_9b.tif",inmask_path1)
    gdalUtil(
      "calc",
      source = c(inmask_path9, namask_path), 
      destination = inmask_path9b,
      formula = "A*B",
      options = c(
        "--type", "Byte", "--NoDataValue", "1", "--format", "GTiff", 
        "--co", "COMPRESS=LZW", if (bigtiff==TRUE) {c("-co", "BIGTIFF=YES")}
      ),
      quiet = TRUE
    )
    inmask_path9b
  } else {
    inmask_path9
  }
  
}

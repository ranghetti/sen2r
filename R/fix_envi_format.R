#' @title Fix ENVI outputs
#' @description Internal function which changes some elements of output ENVI
#'  files:
#'  - file extension is set to .dat if .envi (in case of files created
#'      by `writeRaster`) is found, and the header is edited properly,
#'  - and band names are set in the header file (in particular, SR band names
#'      include wavelengths and names like NIR, SWIR; other products shows the
#'      product name as band name);
#'  - SCL headers include information about class names and colours.
#' @param infiles A vector of input filenames, in the
#'  sen2r naming convention ([safe_shortname]) and ENVI format.
#' @return NULL (the function is called for its side effects)
#' @importFrom jsonlite fromJSON
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal

fix_envi_format <- function(infiles) {
  
  # load file extension for ENVI file
  gdal_formats <- fromJSON(
    system.file("extdata/settings/gdal_formats.json",package="sen2r")
  )$drivers
  envi_ext <- gdal_formats[gdal_formats$name=="ENVI","ext"][1]
  
  # list with the names of Sentinel-2 bands
  s2_bands <- list(
    "TOA" = list("A" = list(), "B" = list()),
    "BOA" = list("A" = list(), "B" = list())
  )
  s2_bands[["TOA"]][["A"]][["bandname"]] <- s2_bands[["TOA"]][["B"]][["bandname"]] <- list(
    "B1 Aerosol",
    "B2 Blue",
    "B3 Green",
    "B4 Red",
    "B5 Red-edge 1",
    "B6 Red-edge 2",
    "B7 Red-edge 3",
    c("B8 NIR", "B8a narrow NIR"),
    "B9 Water vapour",
    "B10 Cirrus",
    "B11 SWIR1",
    "B12 SWIR2"
  )
  s2_bands[["BOA"]][["A"]][["bandname"]] <- s2_bands[["BOA"]][["B"]][["bandname"]] <-
    s2_bands[["TOA"]][["B"]][["bandname"]][c(1:9,11:12)]
  s2_bands[["TOA"]][["A"]][["wavelength"]] <- list(
    0.4427, 0.4924, 0.5598, 0.6646, 0.7041, 0.7405, 0.7828, c(0.8328, 0.8647),
    0.9451, 1.3735, 1.6137, 2.2024
  )
  s2_bands[["TOA"]][["B"]][["wavelength"]] <- list(
    0.4422, 0.4921, 0.5590, 0.6649, 0.7038, 0.7391, 0.7797, c(0.8329, 0.8640),
    0.9432, 1.3769, 1.6104, 2.1857
  )
  s2_bands[["BOA"]][["A"]][["wavelength"]] <- s2_bands[["TOA"]][["A"]][["wavelength"]][c(1:9,11:12)]
  s2_bands[["BOA"]][["B"]][["wavelength"]] <- s2_bands[["TOA"]][["B"]][["wavelength"]][c(1:9,11:12)]
  s2_bands[["TOA"]][["A"]][["fwhm"]] <- list(
    0.021, 0.066, 0.036, 0.031, 0.015, 0.015, 0.020, c(0.106, 0.021),
    0.020, 0.031, 0.091, 0.175
  )
  s2_bands[["TOA"]][["B"]][["fwhm"]] <- list(
    0.021, 0.066, 0.036, 0.031, 0.016, 0.015, 0.020, c(0.106, 0.022),
    0.021, 0.030, 0.094, 0.185
  )
  s2_bands[["BOA"]][["A"]][["fwhm"]] <- s2_bands[["TOA"]][["A"]][["fwhm"]][c(1:9,11:12)]
  s2_bands[["BOA"]][["B"]][["fwhm"]] <- s2_bands[["TOA"]][["B"]][["fwhm"]][c(1:9,11:12)]
  s2_bands[["RGB"]] <- c("Red","Green","Blue")
  
  # cycle on infiles
  for (infile in infiles) {
    
    # file metadata
    infile_meta <- sen2r_getElements(infile)
    
    # fix infile name (id the extension was not included, add it)
    infile.dat <- if (grepl(paste0(envi_ext,"$"), infile)) {
      infile
    } else {
      paste0(infile,".",envi_ext)
    }
    
    # 1. check output format
    # (writeRaster uses .envi extension: so, if infile does not exists
    # and infile.envi does, rename it)
    infile.envi <- gsub(paste0("\\.",envi_ext,"$"),".envi",infile.dat)
    if (!file.exists(infile.dat) & file.exists(infile.envi)) {
      file.rename(infile.envi, infile.dat)
      file.rename(paste0(infile.envi,".aux.xml"), paste0(infile.dat,".aux.xml"))
    }
    
    # 2. set band names
    infile.hdr <- gsub(paste0("\\.",envi_ext,"$"),".hdr",infile.dat)
    if (file.exists(infile.hdr)) {
      
      hdr_content <- readLines(infile.hdr)
      rn_0 <- grep("^ *band names ?= ?\\{ *$", hdr_content)
      rns <- grep("^ *Band [0-9]+\\ *[\\,\\}]$", hdr_content)
      filename_rn <- grep(infile.envi, hdr_content, fixed = TRUE)
      
      
      # Set band names
      if (infile_meta$prod_type %in% c("TOA","BOA")) {
        # BOA-TOA: set band names to reflectances
        sel_s2_bands <- s2_bands[[infile_meta$prod_type]][[infile_meta$mission]]
        if (infile_meta$res == "10m") {
          sel_s2_bands[["bandname"]][[8]] <- sel_s2_bands[["bandname"]][[8]][1]
          sel_s2_bands[["wavelength"]][[8]] <- sel_s2_bands[["wavelength"]][[8]][1]
          sel_s2_bands[["fwhm"]][[8]] <- sel_s2_bands[["fwhm"]][[8]][1]
        } else {
          sel_s2_bands[["bandname"]][[8]] <- sel_s2_bands[["bandname"]][[8]][2]
          sel_s2_bands[["wavelength"]][[8]] <- sel_s2_bands[["wavelength"]][[8]][2]
          sel_s2_bands[["fwhm"]][[8]] <- sel_s2_bands[["fwhm"]][[8]][2]
        }
        sel_s2_bands[["bandname"]] <- unlist(sel_s2_bands[["bandname"]])
        sel_s2_bands[["wavelength"]] <- unlist(sel_s2_bands[["wavelength"]])
        sel_s2_bands[["fwhm"]] <- unlist(sel_s2_bands[["fwhm"]])
        rns_length <- switch(infile_meta$prod_type,TOA=12,BOA=11) # bands must be 11 or 12
      } else if (grepl("^RGB", infile_meta$prod_type)) {
        # RGB: set red-green-blue
        sel_s2_bands <- list("bandname" = s2_bands[["RGB"]])
        rns_length <- 3
      } else {
        # other single-band products: set product name as band name
        sel_s2_bands <- list("bandname" = infile_meta$prod_type)
        rns_length <- 1
      }
      
      # If information is retrieved (raw header), replace content
      check_rn <- all(
        all(diff(c(rn_0,rns))==1), # band numbers must be consecutive in the hdr file
        length(rns)==rns_length # check band number
      )
      if (check_rn) {
        hdr_content[rns] <- paste0(
          gsub("^( *)Band [0-9]+(\\ *[\\,\\}])$", "\\1", hdr_content[rns]),
          sel_s2_bands[["bandname"]],
          gsub("^( *)Band [0-9]+(\\ *[\\,\\}])$", "\\2", hdr_content[rns])
        )
        hdr_content[filename_rn] <- gsub(infile.envi, infile.dat, hdr_content[filename_rn], fixed = TRUE)
        if (infile_meta$prod_type %in% c("TOA","BOA")) {
          hdr_content <- c(
            hdr_content,
            paste0("wavelength = {",paste(sel_s2_bands[["wavelength"]], collapse=", "),"}"),
            paste0("fwhm = {",paste(sel_s2_bands[["fwhm"]], collapse=", "),"}"),
            "wavelength units = {um}",
            "default bands = {8, 4, 3}"
          )
        } else if (infile_meta$prod_type == "SCL") {
          # SCL: set class names and colours
          hdr_content <- c(
            hdr_content,
            "classes = 12",
            "class lookup = {",
            paste0("  0,   0,   0, 255,   0,   0,  66,  65,  66,  99,  52,   0,  41, ",
                   "243,  41, 255, 255,   0,   0,   0, 255, 123, 125, 123, 189, 190, ",
                   "189, 255, 255, 255,  99, 203, 255, 255, 154, 255}"),
            "class names = {",
            paste0("No_data, Saturated or defective, Dark area pixels, Cloud shadows, ",
                   "Vegetation, Not vegetated, Water, Unclassified, Cloud (medium ",
                   "probability), Cloud (high probability), Thin cirrus, Snow}")
          )
        }
        writeLines(hdr_content, infile.hdr)
      }
      
    }
    
  } # end of infiles FOR cycle
  
}

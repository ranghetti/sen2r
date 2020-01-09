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
#' @note License: GPL 3.0

fix_envi_format <- function(infiles) {
  
  # load file extension for ENVI file
  gdal_formats <- fromJSON(
    system.file("extdata/settings/gdal_formats.json",package="sen2r")
  )$drivers
  envi_ext <- gdal_formats[gdal_formats$name=="ENVI","ext"][1]
  
  # list with the names of Sentinel-2 bands
  s2_bands <- list("TOA" = list(
    "A" = list(
      "B1 Aerosol (442.7 +- 27 nm)",
      "B2 Blue (492.4 +- 98 nm)",
      "B3 Green (559.8 +- 45 nm)",
      "B4 Red (664.6 +- 38 nm)",
      "B5 Red-edge 1 (704.1 +- 19 nm)",
      "B6 Red-edge 2 (740.5 +- 18 nm)",
      "B7 Red-edge 3 (782.8 +- 28 nm)",
      c("B8 NIR (832.8 +- 145 nm)", "B8a narrow NIR (864.7 +- 33 nm)"),
      "B9 Water vapour (945.1 +- 26 nm)",
      "B10 Cirrus (1373.5 +- 75 nm)",
      "B11 SWIR1 (1613.7 +- 143 nm)",
      "B12 SWIR2 (2202.4 +- 242 nm)"
    ),
    "B" = list(
      "B1 Aerosol (442.2 +- 45 nm)",
      "B2 Blue (492.1 +- 98 nm)",
      "B3 Green (559.0 +- 46 nm)",
      "B4 Red (664.9 +- 39 nm)",
      "B5 Red-edge 1 (703.8 +- 20 nm)",
      "B6 Red-edge 2 (739.1 +- 18 nm)",
      "B7 Red-edge 3 (779.7 +- 28 nm)",
      c("B8 NIR (832.9 +- 133 nm)", "B8a narrow NIR (864.0 +- 32 nm)"),
      "B9 Water vapour (943.2 +- 27 nm)",
      "B10 Cirrus (1376.9 +- 76 nm)",
      "B11 SWIR1 (1610.4 +- 141 nm)",
      "B12 SWIR2 (2185.7 +- 238 nm)"
    )
  ))
  s2_bands[["BOA"]][["A"]] <- s2_bands[["TOA"]][["A"]][c(1:9,11:12)]
  s2_bands[["BOA"]][["B"]] <- s2_bands[["TOA"]][["B"]][c(1:9,11:12)]
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
        sel_s2_bands[[8]] <- if (infile_meta$res == "10m") {sel_s2_bands[[8]][1]} else {sel_s2_bands[[8]][2]}
        sel_s2_bands <- unlist(sel_s2_bands)
        rns_length <- switch(infile_meta$prod_type,TOA=12,BOA=11) # bands must be 11 or 12
      } else if (grepl("^RGB", infile_meta$prod_type)) {
        # RGB: set red-green-blue
        sel_s2_bands <- s2_bands[["RGB"]]
        rns_length <- 3
      } else {
        # other single-band products: set product name as band name
        sel_s2_bands <- infile_meta$prod_type
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
          sel_s2_bands,
          gsub("^( *)Band [0-9]+(\\ *[\\,\\}])$", "\\2", hdr_content[rns])
        )
        hdr_content[filename_rn] <- gsub(infile.envi, infile.dat, hdr_content[filename_rn], fixed = TRUE)
        if (infile_meta$prod_type == "SCL") {
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

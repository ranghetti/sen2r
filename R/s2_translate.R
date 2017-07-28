#' s2_translate
#' @description Convert the S2 product from SAFE format
#' @details The function build a virtual raster from a Sentinel2 SAFE product,
#'  eventually translating it in another spatial format.
#'  For now, only L1C and L2a with long name (< 2016/12/06) are recognised.
#'  Output vrt is at 10m resolution.
#' @param infile `character` Full path of the input SAFE folder (alternatively,
#'  full path of the xml file of the product with metadata).
#' @param outfile `character` (optional) Full name of the output vrt file
#'  (or full existing directory where the vrt file should be created
#'  (default: current directory). If a directory is provided, the file name
#'  will be the same of the SAFE input product.
#' @param format `character` (optional) Format of the output file (in a
#'  format recognised by GDAL). Default value is "VRT" (Virtual Raster).
#' @param utmzone `character`(optional) UTM zone of output products (default:
#'  the first one retrieved from input granules). Note that this function
#'  does not perform reprojections: if no granules refer to the specified
#'  UTM zone, no output is created.
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import import_builtins py_str

# TODO
# - add support for L2A compact name
# - call gdalbuildvrt in a way which ensures that the application is found
# - add uspport for relative paths

s2_translate <- function(infile,
                         outfile=".",
                         format="VRT",
                         utmzone="") {

  res <- c("10m","20m","60m") # resolutions used

  # check output format
  sel_driver <- gdal$GetDriverByName(format)
  if (is.null(py_to_r(sel_driver))) {
    stop(paste0("Format \"",format,"\"is not recognised; ",
                "please use one of the formats supported by your GDAL installation ",
                "(type 'gdalinfo --formats' in a terminal).")) # FIXME replace with R function
  }

    # Retrieve xml required metadata
  infile_meta <- s2_getMetadata(infile, c("xml_main","utm","nameinfo"))

  # retrieve UTM zone
  infile_dir = dirname(infile_meta$xml_main)
  if (utmzone=="") {
    message(paste0("Using UTM zone ",sel_utmzone <- infile_meta$utm[1],"."))
  } else {
    sel_utmzone <- which(infile_meta$utm== as.integer(utmzone))
    if (length(sel_utmzone)==0) {
      warning(paste0("Tiles with UTM zone ",utmzone," are not present: zone ",
                     sel_utmzone <- infile_meta$utm[1]," will be used."))
    }
  }

  # define basename for output files
  out_prefix <- gsub(".SAFE$","",basename(infile_dir)) # FIXME use new naming convention

  ## Create VRT intermediate files
  infile_gdalnames <- paste0("SENTINEL2_L",infile_meta$level,":",infile,":",res,":","EPSG_326",sel_utmzone)
  dir.create(vrt_tmpdir <- tempdir(), showWarnings=FALSE)
  vrt01_names <- file.path(vrt_tmpdir,paste0(out_prefix,"_",res,".vrt"))

  # create separate vrt for files
  if (length(vrt01_names) == length(infile_gdalnames)) {
    py <- import_builtins(convert=FALSE)
    sys <- import("sys",convert=FALSE)
    gdal <- import("osgeo",convert=FALSE)$gdal
    for (i in 1:length(vrt01_names)) {
      vrt_bi <- gdal$Open(infile_gdalnames[i])
      if (is(vrt_bi$GetMetadata("xml:VRT")[[0]], "python.builtin.str")) { # python 3
        writeLines(py_str(vrt_bi$GetMetadata("xml:VRT")[[0]]), vrt01_names[i])
      } else { # python 2
        writeLines(py_str(vrt_bi$GetMetadata("xml:VRT")[[0]]$encode("utf-8")), vrt01_names[i])
      }
    }
  } else {
    stop("Internal error (this should not happen).")
  }

  # create separate vrt for bands
  if ("10m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 \"",tempdir(),"/",out_prefix,"_b02.vrt\" \"",tempdir(),"/",out_prefix,"_10m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 \"",tempdir(),"/",out_prefix,"_b03.vrt\" \"",tempdir(),"/",out_prefix,"_10m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 \"",tempdir(),"/",out_prefix,"_b04.vrt\" \"",tempdir(),"/",out_prefix,"_10m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 4 \"",tempdir(),"/",out_prefix,"_b08.vrt\" \"",tempdir(),"/",out_prefix,"_10m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
  }
  if ("20m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 \"",tempdir(),"/",out_prefix,"_b05.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 \"",tempdir(),"/",out_prefix,"_b06.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 \"",tempdir(),"/",out_prefix,"_b07.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 4 \"",tempdir(),"/",out_prefix,"_b08a.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 5 \"",tempdir(),"/",out_prefix,"_b11.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 6 \"",tempdir(),"/",out_prefix,"_b12.vrt\" \"",tempdir(),"/",out_prefix,"_20m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
  }
  if ("60m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 \"",tempdir(),"/",out_prefix,"_b01.vrt\" \"",tempdir(),"/",out_prefix,"_60m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 \"",tempdir(),"/",out_prefix,"_b09.vrt\" \"",tempdir(),"/",out_prefix,"_60m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 \"",tempdir(),"/",out_prefix,"_b10.vrt\" \"",tempdir(),"/",out_prefix,"_60m.vrt\""),
           intern = Sys.info()["sysname"] == "Windows")
  }

  # create final vrt
  system(
    paste0(
      "gdalbuildvrt -separate ",
      "-resolution highest ",
      "\"",tempdir(),"/",out_prefix,".vrt\" ",
      if ("60m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b01.vrt\" ")},
      if ("10m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b02.vrt\" ")},
      if ("10m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b03.vrt\" ")},
      if ("10m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b04.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b05.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b06.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b07.vrt\" ")},
      if ("10m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b08.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b08a.vrt\" ")},
      if ("60m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b09.vrt\" ")},
      if ("60m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b10.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b11.vrt\" ")},
      if ("20m" %in% res) {paste0("\"",tempdir(),"/",out_prefix,"_b12.vrt\" ")}
    ), intern = Sys.info()["sysname"] == "Windows"
  )

  # create output file
  out_ext <- if (format=="ENVI") {
    "dat"
  } else {
    unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1]
  }
  if (file.exists(outfile) & file.info(outfile)$isdir) {
    outfile <- file.path(outfile,paste0(out_prefix,".",out_ext))
  } else {
    outfile <- paste0(gsub(paste0("\\.",out_ext,"$"),"",outfile),".",out_ext)
  }

  if (format=="VRT") {
    file.copy(paste0(tempdir(),"/",out_prefix,".vrt"), outfile)
  } else {
    system(
      paste0(
        "gdal_translate -of ",format," ",
        "\"",tempdir(),"/",out_prefix,".vrt\" ",
        "\"",outfile,"\""
      ), intern = Sys.info()["sysname"] == "Windows"
    )
  }

}


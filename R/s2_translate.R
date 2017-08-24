#' @title Convert from SAFE format
#' @description The function build a virtual raster from a Sentinel2 SAFE product,
#'  eventually translating it in another spatial format.
#'  For now, only L1C and L2a with long name (< 2016/12/06) are recognised.
#'  Output vrt is at 10m resolution.
#' @param infile Full path of the input SAFE folder (alternatively,
#'  full path of the xml file of the product with metadata).
#' @param outfile (optional) Full name of the output file
#'  (or full existing directory where the file should be created
#'  (default: current directory). If a directory is provided (or if no
#'  value is specified), the file name will follow the short naming
#'  convention adopted in this package (see [s2_shortname]).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default value is "VRT" (Virtual Raster).
#' @param compress (optional) In the case a GTiff format is
#'  chosen, the compression indicated with this parameter is used.
#' @param vrt_rel_paths (optional) Logical: if TRUE (default), the paths
#'  present in the VRT output file are relative to the VRT position;
#'  if FALSE, they are absolute. This takes effect only with
#'  `format = "VRT"`.
#' @param utmzone (optional) UTM zone of output products (default:
#'  the first one retrieved from input granules). Note that this function
#'  does not perform reprojections: if no granules refer to the specified
#'  UTM zone, no output is created.
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import import_builtins py_str

s2_translate <- function(infile,
                         outfile=".",
                         format="VRT",
                         compress="DEFLATE",
                         vrt_rel_paths="TRUE",
                         utmzone="") {

  res <- c("10m","20m","60m") # resolutions used

  # import python modules
  py <- import_builtins(convert=FALSE)
  sys <- import("sys",convert=FALSE)
  gdal <- import("osgeo",convert=FALSE)$gdal

  # check output format
  sel_driver <- gdal$GetDriverByName(format)
  if (is.null(py_to_r(sel_driver))) {
    print_message(
      type="error",
      "Format \"",format,"\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
  }

  # check compression value
  if (format=="GTiff") {
    if (!compress %in% c("JPEG","LZW","PACKBITS","DEFLATE","CCITTRLE",
                         "CCITTFAX3","CCITTFAX4","LZMA","NONE")) {
      print_message(type="warning",
                    "'",toupper(compress),"' is not a valid compression value; ",
                    "the default 'DEFLATE' value will be used.")
    }
  }

  # Check GDAL installation
  check_gdal(abort=TRUE)

  # Retrieve xml required metadata
  infile_meta <- s2_getMetadata(infile, c("xml_main","utm","level","tiles", "jp2list"))

  # retrieve UTM zone
  infile_dir = dirname(infile_meta$xml_main)
  if (utmzone=="") {
    print_message(
      type="message",
      "Using UTM zone ",sel_utmzone <- infile_meta$utm[1],".")
  } else {
    sel_utmzone <- which(infile_meta$utm== as.integer(utmzone))
    if (length(sel_utmzone)==0) {
      print_message(
        type="warning",
        "Tiles with UTM zone ",utmzone," are not present: zone ",
        sel_utmzone <- infile_meta$utm[1]," will be used.")
    }
  }

  # define basename for output files
  out_prefix <- s2_shortname(infile_dir, full.name=FALSE)

  ## Create VRT intermediate files ##
  dir.create(vrt_tmpdir <- tempdir(), showWarnings=FALSE)

browser()
  # select required bands from the list
  jp2df_spectralbands <- infile_meta$jp2list[infile_meta$jp2list$type=="MSI",]
  jp2df_spectralbands <-jp2df_spectralbands[with(jp2df_spectralbands,order(band,res)),]
  jp2df_spectralbands <- jp2df_spectralbands[!duplicated(with(jp2df_spectralbands,paste(band,tile))),]
  jp2_spectralbands <- file.path(infile_dir,jp2df_spectralbands[,"relpath"])

  # TODO check that required bands are present

  # if more than one tile are present in the product, merge them
  if (length(infile_meta$tiles)>1) {
    vrt_spectralbands <- character(0)
    for (sel_band in unique(jp2df_spectralbands$band)) {
      jp2_selband <- jp2_spectralbands[jp2df_spectralbands$band==sel_band]
      vrt_selband <- paste0(tempdir(),"/",out_prefix,"_",sel_band,".vrt")
      vrt_spectralbands <- c(vrt_spectralbands, vrt_selband)
      system(
        paste0(
          Sys.which("gdalbuildvrt")," ",
          "\"",vrt_selband,"\" ",
          paste(paste0("\"",jp2_selband,"\""), collapse=" ")
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )
    }
  } else {
    vrt_spectralbands <- jp2_spectralbands
  }
  # TODO: in this way, in the overlapping areas only one of the two is considered.
  # In L2A there are slight differences (due to different parameters used by sen2cor).
  # A more elaborated function to deal with this situations could be implemented
  # (e.g. average value, maybe weighted on the relative distance from the border of
  # one tile or another).

  # create final vrt
  system(
    paste0(
      Sys.which("gdalbuildvrt")," -separate ",
      "\"",tempdir(),"/",out_prefix,".vrt\" ",
      paste(paste0("\"",vrt_spectralbands,"\""), collapse=" ")
    ),
    intern = Sys.info()["sysname"] == "Windows"
  )

  # create output file
  out_ext <- if (format=="ENVI") {
    "dat"
  } else {
    unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1]
  }
  if (file.exists(outfile) & file.info(outfile)$isdir) {
    outfile <- file.path(expand_path(outfile,parent=dirname(infile_dir)),paste0(out_prefix,".",out_ext))
  } else {
    outfile <- expand_path(paste0(gsub(paste0("\\.",out_ext,"$"),"",outfile),".",out_ext),
                           parent=dirname(infile_dir))
  }

  if (format=="VRT") {
    file.copy(paste0(tempdir(),"/",out_prefix,".vrt"), outfile)
  } else {
    system(
      paste0(
        Sys.which("gdal_translate")," -of ",format," ",
        if (format=="GTiff") {paste0("-co COMPRESS=",toupper(compress)," ")},
        "\"",tempdir(),"/",out_prefix,".vrt\" ",
        "\"",outfile,"\""
      ), intern = Sys.info()["sysname"] == "Windows"
    )
    if (format=="VRT" & vrt_rel_paths==TRUE) {
      gdal_abs2rel(outfile)
    }
  }

  print_message(type="message",
                "Output file created at ",outfile,".")

}


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
#'  convention adopted in this package (see [s2_shortname]). If the
#'  parameter `prod_type` has length > 1, `outfile` must be an existing directory.
#' @param subdirs (optional) Logical: if TRUE, differet output products are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  `prod_type` has length > 1. This parameter takes effect only in `outfile`
#'  is an existing directory.
#' @param prod_type (optional) Vector of types to be produced as outputs
#'  (see [s2_shortname] for the list of accepted values). Default is
#'  reflectance ("TOA" for level 1C, "BOA" for level 2A).
#' @param res (optional) Spatial resolution (one between '10m', '20m' or '60m');
#'  default is '10m'. Notice that, choosing '10m' or '20m', bands with lower
#'  resolution will be rescaled to `res`. Band 08 is used with `res = '10m'`,
#'  band 08A with `res = '20m'` and `res = '60m'`.
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
#' @return A vector with the names of the created output files.
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import import_builtins py_str py_to_r
#' @examples \dontrun{
#' s2_l1c_example <- file.path(
#'   "/existing/path",
#'   "S2A_MSIL1C_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE")
#' s2_l1c_example <- file.path(
#'   "/existing/path",
#'   "S2A_MSIL2A_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE")
#'
#' # Create a single TOA GeoTIFF in the same directory
#' s2_translate(s2_l1c_example, format="GTiff")
#'
#' # Create a single BOA VRT with a custom name
#' s2_translate(s2_l2a_example, "/new/path/example_sentinel2_sr.vrt",
#'   vrt_rel_paths=TRUE)
#'
#' # Create three products (ENVI) in the same directory at 60m resolution
#' s2_translate(s2_example, format="ENVI", prod_type=c("BOA","TCI","SCL"),
#'   res="60m", subdirs=TRUE)
#'}

s2_translate <- function(infile,
                         outfile=".",
                         subdirs=NA,
                         prod_type=NULL,
                         res="10m",
                         format="VRT",
                         compress="DEFLATE",
                         vrt_rel_paths="TRUE",
                         utmzone="") {

  # import python modules
  py <- import_builtins(convert=FALSE)
  sys <- import("sys",convert=FALSE)
  gdal <- import("osgeo",convert=FALSE)$gdal

  # check res (and use the resolutions >= specified res)
  if (!res %in% c("10m","20m","60m")) {
    print_message(
      type="error",
      "\"res\" value is not recognised (accepted values are '10m', '20m' ",
      "and '60m').")
  }
  if (res == "10m") {
    res <- c("10m","20m","60m")
  } else if (res == "20m") {
    res <- c("20m","60m")
  }

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

  # define output directory
  if (file.exists(outfile) & file.info(outfile)$isdir) {
    outdir <- expand_path(outfile, parent=dirname(infile_dir))
    # create subdirs
    if (is.na(subdirs)) {
      subdirs <- ifelse(length(prod_type)>1, TRUE, FALSE)
    }
    if (subdirs) {
      sapply(file.path(outdir,prod_type), dir.create, showWarnings=FALSE)
    }
  } else {
    subdirs <- FALSE
    if (length(prod_type)>1) {
      print_message(
        type="error",
        "\"prod_type\" has length > 1, but \"outdir\" ",
        "is not an existing directory; please check.")
    }
    outdir <- expand_path(dirname(outfile), parent=dirname(infile_dir))
    if (!file.info(outdir)$isdir) {
      print_message(
        type="error",
        "Directory \"",outdir,"\" does not exist.")
    }
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

  ## Create VRT intermediate files ##
  dir.create(vrt_tmpdir <- tempdir(), showWarnings=FALSE)
  # FIXME this cause producing vrt not executable after reboot!
  # solve in one of the followings:
  # 1) do not merge (translate single granules, and merge them after with s2_merge;
  # 2) use a hidden subdir instead of tempdir

  # select default product type if missing
  if (is.null(prod_type)) {
    if (infile_meta$level=="1C") {
      prod_type <- "TOA"
    } else if (infile_meta$level=="2A") {
      prod_type <- "BOA"
    }
  }

  # create a file for each prod_type
  out_names <- character(0) # names of created files
  for (sel_prod in prod_type) {

    if (sel_prod %in% c("BOA","TOA")) {
      sel_type <- "MSI"
    } else {
      sel_type <- sel_prod
    }

    ## define elements of output file names
    # define basename
    if (file.exists(outfile) & file.info(outfile)$isdir) {
      out_prefix <- s2_shortname(infile_dir, prod_type=sel_prod, res=res[1], full.name=FALSE, abort=TRUE)
    } else {
      out_prefix <- paste0(gsub(paste0("\\.",out_ext,"$"),"",basename(outfile)),".",out_ext)
    }
    # define subdir
    out_subdir <- ifelse(subdirs, file.path(outdir,sel_prod), outdir)
    # define extension
    out_ext <- if (format=="ENVI") {
      "dat"
    } else {
      unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1]
    }
    # complete filename
    out_name <- file.path(out_subdir,paste0(out_prefix,".",out_ext))

    # select required bands from the list and order them by resolution
    jp2df_selbands <- infile_meta$jp2list[infile_meta$jp2list$type==sel_type,]
    jp2df_selbands <- jp2df_selbands[with(jp2df_selbands,order(band,res)),]
    # remove lower resolutions and keep only the best resolution for each band
    jp2df_selbands <- jp2df_selbands[as.integer(substr(jp2df_selbands$res,1,2))>=as.integer(substr(res[1],1,2)),]
    jp2df_selbands <- jp2df_selbands[!duplicated(with(jp2df_selbands,paste(band,tile))),]
    # extract vector of paths
    jp2_selbands <- file.path(infile_dir,jp2df_selbands[,"relpath"])

    # TODO check that required bands are present

    # if more than one tile are present in the product, merge them
    if (length(infile_meta$tiles)>1) {
      vrt_selbands <- character(0)
      for (sel_band in unique(jp2df_selbands$band)) {
        jp2_selband <- jp2_selbands[jp2df_selbands$band==sel_band]
        vrt_selband <- paste0(vrt_tmpdir,"/",out_prefix,"_",sel_band,".vrt")
        vrt_selbands <- c(vrt_selbands, vrt_selband)
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
      vrt_selbands <- jp2_selbands
    }
    # TODO: in this way, in the overlapping areas only one of the two is considered.
    # In L2A there are slight differences (due to different parameters used by sen2cor).
    # A more elaborated function to deal with this situations could be implemented
    # (e.g. average value, maybe weighted on the relative distance from the border of
    # one tile or another).

    # create final vrt (of select final raster)
    # for a multiband raster (reflectance), create final vrt
    if (length(vrt_selbands)>1) {
      final_vrt_name <- paste0(vrt_tmpdir,"/",out_prefix,".vrt")
      system(
        paste0(
          Sys.which("gdalbuildvrt")," -separate ",
          "-resolution highest ",
          "\"",final_vrt_name,"\" ",
          paste(paste0("\"",vrt_selbands,"\""), collapse=" ")
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )
    # for a single band raster, skip this step
    } else {
      final_vrt_name <- vrt_selbands
    }

    # create output file
    system(
      paste0(
        Sys.which("gdal_translate")," -of ",format," ",
        if (format=="GTiff") {paste0("-co COMPRESS=",toupper(compress)," ")},
        "\"",final_vrt_name,"\" ",
        "\"",out_name,"\""
      ), intern = Sys.info()["sysname"] == "Windows"
    )
    if (format=="VRT" & vrt_rel_paths==TRUE) {
      gdal_abs2rel(out_name)
    }

    out_names <- c(out_names, out_name)

  } # end of prod_type cycle

  print_message(type="message",
                length(out_names)," output files were correctly created.")
  return(out_names)

}


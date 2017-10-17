#' @title Merge S2 tiles with the same date and orbit
#' @description The function merge the input Sentinel-2 files with
#'  the same date, orbit number, product type and file format.
#'  Outputs are a set of products in the same format of corresponding
#'  input files.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a format managed by
#'  GDAL (use [s2_translate] to do it); their names must be in the
#'  fidolasen-S2 naming convention ([s2_shortname]).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param subdirs (optional) Logical: if TRUE, differet output products are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  `infiles` relate to more than a single product.
#' @param tmpdir (optional) Path where intermediate VRT will be created.
#'  Default is in a hidden subdirectory (called `.vrt`) of the common parent
#'  directory of `infiles`. Set `tmpdir=tempdir()` if you do not want to
#'  keep the intermediate files after reboot.
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is to maintain each input format.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param out_crs (optional) proj4string (character) of the output CRS
#'  (default: the CRS of the first input file). The tiles with CRS different
#'  from `out_crs` will be reprojected (and a warning returned).
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @return A vector with the names of the merged products (just created or
#'  already existing).
#' @importFrom rgdal GDALinfo
#' @importFrom sp CRS
#' @importFrom magrittr "%>%"
#' @importFrom sprawl check_proj4string reproj_extent
#' @importFrom raster compareCRS
#' @import data.table
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


s2_merge <- function(infiles,
                     outdir=".",
                     subdirs=NA,
                     tmpdir=NA,
                     format=NA,
                     compress="DEFLATE",
                     out_crs="",
                     overwrite=FALSE) {

  # Check that files exist
  if (!any(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      "The input files do not exists locally; please check file names and paths.")
  } else if (!all(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      "Some of the input files (\"",
      paste(infiles[!sapply(infiles, file.exists)], collapse="\", \""),
      "\") do not exists locally; please check file names and paths.")
  }

  # Get files metadata
  infiles_meta <- fs2nc_getElements(infiles, format="data.frame")
  # get metadata from GDALinfo (FIXME time expensive; check if it can be speeded up)
  suppressWarnings(
    # infiles_meta_gdal <- sapply(infiles, function(x) {attributes(GDALinfo(x))[c("driver","projection","df")]})
    infiles_meta_gdal <- sapply(infiles, function(x) {attributes(GDALinfo(x))[c("driver","projection")]})
  )
  infiles_meta$format <- unlist(infiles_meta_gdal[1,])
  infiles_meta$proj4string <- sapply(unlist(infiles_meta_gdal[2,]), function(x) {CRS(x)@projargs})
  # infiles_meta$NAflag <- sapply(infiles_meta_gdal[3,], function(x) {
  #   if (x[1,"hasNoDataValue"]==TRUE) {
  #     x[1,"NoDataValue"]
  #   } else {
  #     switch(as.character(x[1,"GDType"]),
  #            UInt16 = "65535",
  #            Byte = "None",
  #            "None")
  #   }
  # })
  # # check input UTM zones
  # infiles_meta$utm <- gsub(".* \\+zone\\=([0-9]+) .*","\\1",infiles_meta$proj4string)
  # if (any(infiles_meta$utm != as.character(as.integer(infiles_meta$utm)))) {
  #   print_message(
  #     type="error",
  #     "Some of the input files is not in a UTM projection.")
  # }

  # if utm zones differ from the selected utm zone, show a warning
  if (out_crs=="") {
    print_message(
      type="message",
      "Using projection \"",infiles_meta$proj4string[1],"\".")
    out_crs <- infiles_meta$proj4string[1]
  }

  # vector which identifies, for each infiles, if its projection is
  # different or not from out_crs
  diffcrs <- sapply(infiles_meta$proj4string, function(x) {
    !compareCRS(CRS(x), CRS(out_crs))
  })

  # Check out_crs
  out_crs <- check_proj4string(out_crs)
  # check the projections of input files
  if (any(diffcrs)) {
    print_message(
      type="warning",
      "Not all the tiles are in the specified projection; ",
      "tiles with different projection will be reprojected.")
  }
  if (is.na(tmpdir)) {
    tmpdir <- file.path(comsub(infiles,"/"),".vrt")
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)

  # create outdir if not existing
  suppressWarnings(outdir <- expand_path(outdir, parent=comsub(infiles,"/"), silent=TRUE))
  dir.create(outdir, recursive=FALSE, showWarnings=FALSE)

  # if out_crs is different from the projection of all input files,
  # reprojected the first file and use as reference for the grid;
  # otherwise, use the first non-reprojected file.
  if (all(diffcrs)) {
    ref_file <- file.path(tmpdir,".ref_grid.vrt")
    system(
      paste0(
        Sys.which("gdalwarp")," ",
        "-overwrite ",
        "-s_srs \"",infiles_meta[1,"proj4string"],"\" ",
        "-t_srs \"",out_crs,"\" ",
        "-of VRT ",
        "\"",infiles[1],"\" ",
        "\"",ref_file,"\""),
      intern = Sys.info()["sysname"] == "Windows"
    )
  } else {
    ref_file <- infiles[which(!diffcrs)[1]]
  }

  # Group by all except id_tile
  infiles_meta_grps <- paste(infiles_meta$mission,
                             infiles_meta$level,
                             infiles_meta$sensing_date,
                             infiles_meta$id_orbit,
                             infiles_meta$prod_type,
                             infiles_meta$res,
                             infiles_meta$file_ext,
                             infiles_meta$format) # FIXME use better syntax (data.table) when the error with data.table will be fixed

  # create subdirs (if requested)
  prod_types <- unique(infiles_meta$prod_type)
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(prod_types)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,prod_types), dir.create, showWarnings=FALSE)
  }

  # merge single output products
  outfiles <- character(0)
  for (infiles_meta_grp in unique(infiles_meta_grps)) {

    sel_infiles <- infiles[infiles_meta_grps == infiles_meta_grp]
    sel_infiles_meta <- infiles_meta[infiles_meta_grps == infiles_meta_grp,]
    sel_diffcrs <- diffcrs[infiles_meta_grps == infiles_meta_grp]

    # Check that there are not duplicated tiles
    if (any(duplicated(sel_infiles_meta$id_tile))) {
      print_message(type="error", "Internal error (duplicated id_tile).")
    }

    # Define output filename
    sel_outfile <- paste0(
      "S2",sel_infiles_meta[1,"mission"],sel_infiles_meta[1,"level"],"_",
      strftime(sel_infiles_meta[1,"sensing_date"],"%Y%m%d"),"_",
      sel_infiles_meta[1,"id_orbit"],"__",
      sel_infiles_meta[1,"prod_type"],"_",
      gsub("m$","",sel_infiles_meta[1,"res"]),".",
      sel_infiles_meta[1,"file_ext"])
    sel_outformat <- ifelse(is.na(format),
                            unique(sel_infiles_meta[,"format"]),
                            format)
    if (length(sel_outformat)>1) {
      print_message(type="error", "Internal error (non unique format).")
    }
    # define subdir
    out_subdir <- ifelse(subdirs, file.path(outdir,sel_infiles_meta[1,"prod_type"]), outdir)

    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(file.path(out_subdir,sel_outfile)) | overwrite==TRUE) {

      # build intermediate reprojected VRTs (if necessary)
      for (i in seq_len(sum(sel_diffcrs))) {
        reproj_vrt <- file.path(
          tmpdir,
          gsub(paste0("\\.",sel_infiles_meta[sel_diffcrs,][i,"file_ext"],"$"),
               "_reproj.vrt",
               basename(sel_infiles[sel_diffcrs][i]))
        )
        gdalwarp_grid(srcfiles = sel_infiles[sel_diffcrs][i],
                      dstfiles = reproj_vrt,
                      ref = ref_file,
                      of = "VRT",
                      r = "near")
        gdal_abs2rel(reproj_vrt)

        # replace input file path with intermediate
        sel_infiles[sel_diffcrs][i] <- reproj_vrt
      }

      # merge tiles
      merged_vrt <- file.path(
        tmpdir,
        gsub(paste0("\\.",sel_infiles_meta[1,"file_ext"],"$"),
             ".vrt",
             sel_outfile))
      system(
        paste0(
          Sys.which("gdalbuildvrt")," ",
          "\"",merged_vrt,"\" ",
          paste(paste0("\"",sel_infiles,"\""), collapse=" ")
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )

      # create output merged file
      system(
        paste0(
          Sys.which("gdal_translate")," ",
          "-of ",sel_outformat," ",
          if (sel_outformat=="GTiff") {paste0("-co COMPRESS=",toupper(compress)," ")},
          "\"",merged_vrt,"\" ",
          "\"",file.path(out_subdir,sel_outfile),"\" "),
        intern = Sys.info()["sysname"] == "Windows"
      )
      if (sel_outformat=="VRT") {
        gdal_abs2rel(file.path(out_subdir,sel_outfile))
      }

    } # end of overwrite IF cycle

    outfiles <- c(outfiles, file.path(out_subdir,sel_outfile))

  }

  return(outfiles)

}

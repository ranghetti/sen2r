#' @title Merge S2 tiles with the same date and orbit
#' @description The function merge the input Sentinel-2 files with
#'  the same date, orbit number, product type and file format.
#'  Outputs are a set of products in the same format of corresponding
#'  input files.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a format managed by
#'  GDAL (use [s2_translate] to do it); their names must be in the
#'  sen2r naming convention ([s2_shortname]).
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
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE).
#'  This parameter takes effect only if the output files are not VRT
#'  (in this case temporary files cannot be deleted, because rasters of source
#'  bands are included within them).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is to maintain each input format.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param vrt_rel_paths (optional) Logical: if TRUE (default on Linux), 
#'  the paths present in the VRT output file are relative to the VRT position;
#'  if FALSE (default on Windows), they are absolute. 
#'  This takes effect only with `format = "VRT"`.
#' @param out_crs (optional) proj4string (character) of the output CRS
#'  (default: the CRS of the first input file). The tiles with CRS different
#'  from `out_crs` will be reprojected (and a warning returned).
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .logfile_message (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @return A vector with the names of the merged products (just created or
#'  already existing).
#' @importFrom rgdal GDALinfo
#' @importFrom magrittr "%>%"
#' @importFrom jsonlite fromJSON
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @import data.table
#' @export
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


s2_merge <- function(infiles,
                     outdir=".",
                     subdirs=NA,
                     tmpdir=NA,
                     rmtmp=TRUE,
                     format=NA,
                     compress="DEFLATE",
                     vrt_rel_paths=NA,
                     out_crs="",
                     parallel = FALSE,
                     overwrite=FALSE,
                     .logfile_message=NA,
                     .log_output=NA) {
  
  # load output formats
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))
  
  # Define vrt_rel_paths
  if (is.na(vrt_rel_paths)) {
    vrt_rel_paths <- Sys.info()["sysname"] != "Windows"
  }
  
  # Check that files exist
  if (!any(sapply(infiles, file.exists))) {
    print_message(
      type="error",
      if (!all(sapply(infiles, file.exists))) {"The "} else {"Some of the "},
      "input files (\"",
      paste(infiles[!sapply(infiles, file.exists)], collapse="\", \""),
      "\") do not exists locally; please check file names and paths.")
  }
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Get files metadata
  infiles_meta <- fs2nc_getElements(infiles, format="data.frame")
  # get metadata from GDALinfo (FIXME time expensive; check if it can be speeded up)
  suppressWarnings(
    # infiles_meta_gdal <- sapply(infiles, function(x) {attributes(GDALinfo(x))[c("driver","projection","df")]})
    infiles_meta_gdal <- sapply(infiles, function(x) {attributes(GDALinfo(x))[c("driver","projection")]})
  )
  infiles_meta$format <- unlist(infiles_meta_gdal[1,])
  infiles_meta$proj4string <- sapply(unlist(infiles_meta_gdal[2,]), function(x) {st_crs2(x)$proj4string})
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
    st_crs2(x) != st_crs2(out_crs)
    # !compareCRS(CRS(x), CRS(out_crs))
  })
  
  # Check out_crs
  out_crs <- st_crs2(out_crs)$proj4string
  # check the projections of input files
  if (any(diffcrs)) {
    print_message(
      type="warning",
      "Not all the tiles are in the specified projection; ",
      "tiles with different projection will be reprojected.")
  }
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- if (format == "VRT") {
      rmtmp <- FALSE # force not to remove intermediate files
      if (!missing(outdir)) {
        autotmpdir <- FALSE # logical: TRUE if tmpdir should be specified 
        # for each out file (when tmpdir was not specified and output files are vrt),
        # FALSE if a single tmpdir should be used (otherwise)
        file.path(outdir, ".vrt")
      } else {
        autotmpdir <- TRUE
        tempfile(pattern="s2merge_")
      }
    } else {
      autotmpdir <- FALSE
      tempfile(pattern="s2merge_")
    }
  } else {
    autotmpdir <- FALSE
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
        binpaths$gdaliwarp," ",
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
  
  # Compute n_cores
  n_cores <- if (is.numeric(parallel)) {
    as.integer(parallel)
  } else if (parallel==FALSE) {
    1
  } else {
    min(parallel::detectCores()-1, length(unique(infiles_meta_grps)), 11) # use at most 11 cores
  }
  if (n_cores<=1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
  }
  
  # merge single output products
  cl <- makeCluster(
    n_cores, 
    type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
  )
  if (n_cores > 1) {registerDoParallel(cl)}
  outfiles <- foreach(
    infiles_meta_grp = unique(infiles_meta_grps), 
    .packages = c("sen2r"), 
    .combine=c, 
    .errorhandling="remove"
  )  %DO% {
    
    # redirect to log files
    if (n_cores > 1 & !is.na(.log_output)) {
      sink(.log_output, split = TRUE, type = "output", append = TRUE)
    }
    if (n_cores > 1 & !is.na(.logfile_message)) {
      sink(.logfile_message, type="message")
    }
    
    sel_infiles <- infiles[infiles_meta_grps == infiles_meta_grp]
    sel_infiles_meta <- infiles_meta[infiles_meta_grps == infiles_meta_grp,]
    sel_diffcrs <- diffcrs[infiles_meta_grps == infiles_meta_grp]
    
    # Check that there are not duplicated tiles
    if (any(duplicated(sel_infiles_meta$id_tile))) {
      print_message(type="error", "Internal error (duplicated id_tile).")
    }
    
    sel_outformat <- ifelse(is.na(format),
                            unique(sel_infiles_meta[,"format"]),
                            format)
    if (length(sel_outformat)>1) {
      print_message(type="error", "Internal error (non unique format).")
    }
    # Define output filename
    sel_outfile <- paste0(
      "S2",sel_infiles_meta[1,"mission"],sel_infiles_meta[1,"level"],"_",
      strftime(sel_infiles_meta[1,"sensing_date"],"%Y%m%d"),"_",
      sel_infiles_meta[1,"id_orbit"],"__",
      sel_infiles_meta[1,"prod_type"],"_",
      gsub("m$","",sel_infiles_meta[1,"res"]),".",
      gdal_formats[gdal_formats$name==sel_outformat,"ext"])
    # define subdir
    out_subdir <- ifelse(subdirs, file.path(outdir,sel_infiles_meta[1,"prod_type"]), outdir)
    
    # if tmpdir should vary for each file, define it
    sel_tmpdir <- if (autotmpdir) {
      file.path(out_subdir, ".vrt")
    } else {
      tmpdir
    }
    dir.create(sel_tmpdir, showWarnings=FALSE)
    
    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(file.path(out_subdir,sel_outfile)) | overwrite==TRUE) {
      
      # build intermediate reprojected VRTs (if necessary)
      for (i in seq_len(sum(sel_diffcrs))) {
        reproj_vrt <- file.path(
          sel_tmpdir,
          gsub(paste0("\\.",sel_infiles_meta[sel_diffcrs,][i,"file_ext"],"$"),
               "_reproj.vrt",
               basename(sel_infiles[sel_diffcrs][i]))
        )
        gdalwarp_grid(srcfiles = sel_infiles[sel_diffcrs][i],
                      dstfiles = reproj_vrt,
                      ref = ref_file,
                      of = "VRT",
                      r = "near")
        if (vrt_rel_paths) {gdal_abs2rel(reproj_vrt)}
        
        # replace input file path with intermediate
        sel_infiles[sel_diffcrs][i] <- reproj_vrt
      }
      
      # merge tiles
      merged_vrt <- file.path(
        sel_tmpdir,
        gsub(paste0("\\.",sel_infiles_meta[1,"file_ext"],"$"),
             ".vrt",
             sel_outfile))
      system(
        paste0(
          binpaths$gdalbuildvrt," ",
          "\"",merged_vrt,"\" ",
          paste(paste0("\"",sel_infiles,"\""), collapse=" ")
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )
      
      # create output merged file
      system(
        paste0(
          binpaths$gdal_translate," ",
          "-of ",sel_outformat," ",
          if (sel_outformat=="GTiff") {paste0("-co COMPRESS=",toupper(compress)," ")},
          "\"",merged_vrt,"\" ",
          "\"",file.path(out_subdir,sel_outfile),"\" "),
        intern = Sys.info()["sysname"] == "Windows"
      )
      if (sel_outformat=="VRT" & vrt_rel_paths) {
        gdal_abs2rel(file.path(out_subdir,sel_outfile))
      }
      
    } # end of overwrite IF cycle
    
    file.path(out_subdir,sel_outfile)
    
  } # end of foreach cycle
  if (n_cores > 1) {stopCluster(cl)}
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  return(outfiles)
  
}

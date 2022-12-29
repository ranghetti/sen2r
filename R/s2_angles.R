#' @title Extract sun angles from SAFE archives
#' @description The function extracts sun angle rasters from 
#'  a SAFE archive, reshaping the original information (5 km resolution)
#'  to the desired Sentinel-2 output resolution.
#'  It was not exported because it is intended to be called by [s2_translate].
#' @param infiles Full paths of the input SAFE folders.
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the directory of `infile`.
#' @param subdirs (optional) Logical: if TRUE, different output products are
#'  placed in separated `outdir` subdirectories; if FALSE, they are placed in
#'  `outdir` directory; if NA (default), subdirectories are created only if
#'  `prod_type` has length > 1.
#' @param tmpdir (optional) Path where intermediate files will be created.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE).
#' @param prod_type (optional) Vector of types (angles) to be produced as outputs.
#'  Default is all the possible types
#'  (`"SZA"` for Sun Angles Grid at Zenith, 
#'  `"SAA"` for Sun Angles Grid at Azimuth, 
#'  `"OZA"` for Viewing Incidence Angles at Zenith averaged Grid,
#'  `"OAA"` for Viewing Incidence Angles at Azimuth averaged Grid).
#' @param res (optional) Spatial resolution (one between `'10m'`, `'20m'` or 
#'  `'60m'`); default is `'10m'`.
#' @param method (optional) A method used to resample the output rasters
#'  (accepted values are valid values accepted by `-r` option of gdalwarp).
#'  Default is `"bilinear"` (linear interpolation).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default value is `"VRT"` (Virtual Raster).
#' @param compress (optional) In the case a GeoTIFF format is
#'  chosen, the compression indicated with this parameter is used.
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GeoTIFF format was chosen. 
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @return A vector with the names of the created output files
#'   (just created or already existing).
#' @author Luigi Ranghetti, phD (2022)
#' @author Marina Ranghetti (2022)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @importFrom XML xmlToList xmlTreeParse
#' @importFrom sf st_crs
#' @importFrom stars st_as_stars write_stars
#' @keywords internal
#' @examples
#' \dontrun{
#' 
#' s2_l2a_example <- file.path(
#'   "/existing/path",
#'   "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNR_20200801T135302.SAFE"
#' )
#'
#' # Create all products
#' s2_angles(s2_l2a_example, outdir = tempdir())
#' 
#' # Create only Viewing Incidence Angles at Azimuth, at 60 m resolution,
#' # with cubic interpolation, in ENVI format
#' s2_angles(
#'   s2_l2a_example, 
#'   outdir = tempdir(), 
#'   prod_type = "OAA", 
#'   res = "60m",
#'   method = "cubic",
#'   format = "ENVI"
#' )
#' }

s2_angles <- function(
    infiles, 
    outdir = ".",
    subdirs = NA,
    tmpdir = NA,
    rmtmp = TRUE,
    prod_type = c("SZA", "OZA", "SAA", "OAA"), 
    res = "10m",
    method = "bilinear",
    format = "VRT",
    compress = "DEFLATE",
    bigtiff = FALSE,
    overwrite = FALSE
) {
  
  # to avoid NOTE on check
  i <- infile_dir <- name <- xml_granules <- NULL
  
  # Checks on arguments were skipped since the function is not exported;
  # arguments are checked in s2_translate, which calls s2_angles).
  
  # exit if nothing is required
  if (length(prod_type) == 0) {return(invisible(character(0)))}
  
  # define internal function to extract angles from xml
  extract_sun_angles <- function(xml, grid, direction) {
    # extract relevant strings
    xml_l <- lapply(
      xml[names(xml) == grid],
      function(x) {unlist(x[[direction]][["Values_List"]])}
    )
    # convert in numeric values (list of layers)
    raw_l <- lapply(
      xml_l,
      function(x) {do.call(rbind, lapply(strsplit(x, " "), as.numeric))}
    )
    # create array with all layers
    # (assuming all matrices to have the same dimension)
    raw_a <- array(unlist(raw_l), dim = c(dim(raw_l[[1]]), length(raw_l)))
    # average layers
    raw_m <- apply(raw_a, 1:2, mean, na.rm=TRUE)
    raw_m
  }
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- if (all(!is.na(format), format == "VRT")) {
      rmtmp <- FALSE # force not to remove intermediate files
      if (!missing(outdir)) {
        file.path(outdir, ".vrt")
      } else {
        tempfile(pattern="s2angles_")
      }
    } else {
      tempfile(pattern="s2angles_")
    }
  } else if (dir.exists(tmpdir)) {
    tmpdir <- file.path(tmpdir, basename(tempfile(pattern="s2angles_")))
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # resolution without "m"
  res_m <- gsub("^([126]0)m$", "\\1", res)
  
  # input metadata
  inmeta <- safe_getMetadata(infiles, c("name", "validname", "prod_type", "version", "mission", "level", "sensing_datetime", "id_baseline", "id_orbit", "id_tile", "creation_datetime", "xml_granules"))
  inmeta$path <- infiles
  
  # check output format
  gdal_formats <- fromJSON(
    system.file("extdata/settings/gdal_formats.json",package="sen2r")
  )$drivers
  sel_driver <- gdal_formats[gdal_formats$name==format,]
  
  # create outdir if not existing (and dirname(outdir) exists)
  suppressWarnings(outdir <- expand_path(outdir, parent=dirname(infile_dir), silent=TRUE))
  if (!dir.exists(dirname(outdir))) {
    print_message(
      type = "error",
      "The parent folder of 'outdir' (",outdir,") does not exist; ",
      "please create it."
    )
  }
  dir.create(outdir, recursive=FALSE, showWarnings=FALSE)
  
  # create subdirs
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(prod_type)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,prod_type), dir.create, showWarnings=FALSE)
  }
  
  # Generate output files for each input SAFE
  out_names <- foreach(i = seq_len(nrow(inmeta)), .combine = c) %do% {
    
    ## Check for already existing products
    sel_out_names_all <- sapply(prod_type, function(a) {
      file.path(
        ifelse(subdirs, file.path(outdir,a), outdir),
        safe_shortname(
          inmeta[i,name], 
          prod_type=a, res=res, 
          full.name=FALSE, ext=sel_driver[1,"ext"]
        )
      )
    })
    sel_out_names <- if (overwrite == TRUE) {
      sel_out_names_all
    } else {
      sel_out_names_all[!file.exists(sel_out_names_all)]
    }
    sel_prod_types <- names(sel_out_names)
    
    ## Read XML
    xml_path <- inmeta[i,xml_granules]
    xml_list <- xmlToList(xmlTreeParse(xml_path, useInternalNodes = TRUE))
    
    ## Extract spatial metadata
    xml_geocoding <- xml_list[["Geometric_Info"]][["Tile_Geocoding"]]
    # extract CRS
    sel_crs <- st_crs(xml_geocoding[["HORIZONTAL_CS_CODE"]])
    # extract file size in pixels and lines
    sel_ts_all <- xml_geocoding[names(xml_geocoding) == "Size"]
    names(sel_ts_all) <- sapply(sel_ts_all, function(x) {x$.attrs["resolution"]})
    sel_ts <- as.integer(c(sel_ts_all[[res_m]][["NROWS"]], sel_ts_all[[res_m]][["NCOLS"]]))
    # extract upper left x-y coordinates and resolutions
    sel_ul_all <- xml_geocoding[names(xml_geocoding) == "Geoposition"]
    names(sel_ul_all) <- sapply(sel_ul_all, function(x) {x$.attrs["resolution"]})
    sel_ul <- as.numeric(c(sel_ul_all[[res_m]][["ULX"]], sel_ul_all[[res_m]][["ULY"]]))
    sel_tr <- as.numeric(c(sel_ul_all[[res_m]][["XDIM"]], sel_ul_all[[res_m]][["YDIM"]]))
    sel_lr <- sel_ul + sel_ts * sel_tr
    
    ## Extract values
    xml_angles <- xml_list[["Geometric_Info"]][["Tile_Angles"]]
    # extract source resolution (assuming all matrices to have the same resolution
    # and to be in metres)
    sel_tr_src <- c(
      as.numeric(xml_angles[["Sun_Angles_Grid"]][["Zenith"]][["COL_STEP"]][["text"]]),
      -as.numeric(xml_angles[["Sun_Angles_Grid"]][["Zenith"]][["ROW_STEP"]][["text"]])
    )
    # extract list of required matrices
    m_angles <- list()
    for (sel_prod in sel_prod_types) {
      sel_grid <- ifelse(
        sel_prod %in% c("SZA", "SAA"),
        "Sun_Angles_Grid", "Viewing_Incidence_Angles_Grids"
      )
      sel_direction <- ifelse(
        sel_prod %in% c("SZA", "OZA"),
        "Zenith", "Azimuth"
      )
      m_angles[[sel_prod]] <- extract_sun_angles(xml_angles, sel_grid, sel_direction)
    }
    
    ## Interpolation
    # create raster at low resolution
    if (length(m_angles) > 0) {
      m_coords <- list(
        "x" = seq(from=sel_ul[1], by=sel_tr_src[1], length.out=dim(m_angles[[1]])[1]),
        "y" = seq(from=sel_ul[2], by=sel_tr_src[2], length.out=dim(m_angles[[1]])[2])
      )
      r_angles <- st_as_stars(
        data.frame(
          expand.grid("y" = m_coords[["y"]], "x" = m_coords[["x"]]),
          sapply(m_angles, as.vector)
        ),
        dims = c("x", "y"),
        y_decreasing = sel_tr[2]<0
      )
      sf::st_crs(r_angles) <- sel_crs
    }
    for (sel_prod in sel_prod_types) {
      if (format != "VRT") {
        print_message(
          type = "message",
          date = TRUE,
          paste0("Interpolating file ", basename(sel_out_names[sel_prod]),"...")
        )
      }
      # write raster at low resolution
      r_selangle_tmpath <- file.path(
        tmpdir, 
        basename(tempfile(pattern = "raster5000_", fileext = ".tif"))
      )
      write_stars(r_angles[sel_prod], r_selangle_tmpath, options="COMPRESS=LZW")
      # interpolate raster at higher resolution
      gdalUtil(
        "warp",
        source = r_selangle_tmpath,
        destination = sel_out_names[sel_prod],
        options = c(
          "-of", format,
          "-dstnodata", s2_defNA(sel_prod),
          "-r", method,
          # "-tr", sel_tr,
          "-ts", sel_ts,
          "-te", c(sel_ul, sel_lr)[c(1,4,3,2)],
          if (format == "GTiff") {c(
            "-co", paste0("COMPRESS=",toupper(compress)),
            "-co", "TILED=YES"
          )},
          if (format=="GTiff" & bigtiff==TRUE) {c("-co", "BIGTIFF=YES")},
          if (overwrite == TRUE) {"-overwrite"}
        ),
        quiet = TRUE
      )
      # fix for envi extension
      if (format=="ENVI") {fix_envi_format(sel_out_names[sel_prod])}
    }
    
    as.vector(sel_out_names_all)
  } # end of infiles FOR cycle
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  print_message(
    type="message",
    length(out_names)," output angle files were correctly created."
  )
  return(out_names)
  
}

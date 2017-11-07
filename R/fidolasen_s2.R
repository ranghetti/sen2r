#' @title Find, download and preprocess Sentinel-2 images
#' @description The function is a wrapper to perform the entire
#'  processing chain to find, download and pre-process Sentinel-2
#'  data. Input is a set of parameters that can be passed with a
#'  list or file (parameter `param_list`) or singularly (see the
#'  descriptions of all the other parameters).
#' @param param_list (optional) List of input parameters:
#'  it can be both an R list or the path of a JSON file.
#'  If some parameters are passed both as elements of `param_list`
#'  and as function arguments, the values passed as function
#'  arguments are considered.
#'  If some parameters are missing in `param_list` and are not
#'  provided as arguments, default values will be used.
#'  Use the function [s2_gui()] to create a complete list of
#'  parameters.
#'  If `param_list` is NULL (default), values given with the
#'  parameters below (or default values for parameters not
#'  provided) are used.
#' @param gui (optional) Logical: if TRUE, function [s2_gui()] is
#'  launched before starting to process in order to set or load parameters;
#'  if FALSE, the function uses parameters passed with `param_list` or
#'  with other function arguments. Default is FALSE if `param_list` is not
#'  NULL, TRUE elsewhere.
#' @param preprocess (optional) Logical: TRUE (default) to perform also
#'  preprocessing steps, FALSE not to (do only find, download
#'  and atmospheric correction).
#' @param s2_levels (optional) Character vector of length 1 or 2, with
#'  Sentinel-2 levels required for processing steps or as output.
#'  This parameter is used only if `preprocess = FALSE` (otherwise, the 
#'  required levels are derived from `list_prods`).
#'  Accepted values: "l1c" and "l2a"; default: "l2a".
#' @param sel_sensor (optional) Character vector of length 1 or 2, with
#' Sentinel-2 sensors to be used.
#'  Accepted values: "s2a" and "s2b"; default: c("s2a","s2b").
#' @param online (optional) Logical: TRUE (default) to search for available
#'  products on SciHub (and download if needed); FALSE to work
#'  only with already downloaded SAFE products.
#' @param overwrite_safe (optional) Logical: TRUE to overwrite existing
#'  products with products found online or manually corrected,
#'  FALSE (default) to skip download and atmospheric correction for
#'  products already existing.
#' @param rm_safe (optional) Character: should SAFE products be deleted after
#'  preprocessing? "yes" means to delete all SAFE; "no" (default)
#'  not to delete; "l1c" to delete only Level-1C products.
#' @param step_atmcorr (optional) Character vector to determine how to obtain
#'  Level-2A SAFE products:
#'  * "auto" (default) means that L2A is first
#'  searched on SciHub: if found, it is dowloaded, if not, the
#'  corresponding Level-1C is downloaded and sen2cor is used to
#'  produce L2A;
#'  * "scihub" means that sen2cor is always used from L1C products
#'  downloaded from SciHub;
#'  * "l2a" means that they are downloaded if available on SciHub,
#'  otherwise they are skipped (sen2cor is never used);
#'  * "no" means that L1C are not considered (processing chain
#'  makes use only of L1C products).
#' @param timewindow (optional) Temporal window for querying: Date object
#'  of length 1 (single day) or 2 (time window). Default is NA in online mode
#'  (meaning that no filters are used, and all found images are processed); in
#'  online mode, default is to process last 90 days.
#'  Only in offline mode, it is possible to pass NA not to
#'  filter by sensing time.
#' @param timeperiod (optional) Character:
#'  * "full" (default) means that all
#'  the images included in the time window are considered;
#'  * "seasonal" means that only the single seasonal periods in the
#'  window are used (i.e., with a time window from 2015-06-01 to
#'  2017-08-31, the periods 2015-06-01 to 2015-08-31, 2016-06-01
#'  to 2016-08-31 and 2017-06-01 to 2017-08-31 are considered).
#' @param extent (optional) Spatial extent on which to clip products (it can
#'  be both the path of a vector file or a geoJSON). 
#'  Default is NA for offline mode (meaning no extent:
#'  all found tiles are entirely used); in online mode, a sample extent is used.
#' @param s2tiles_selected (optional) Character vector with the Sentinel-2
#'  tiles to be considered (default is NA, meaning all the tiles).
#' @param s2orbits_selected (optional) Character vector with the Sentinel-2
#'  orbits to be considered (still to be implemented; for now,
#'  all the accepted values are listed).
#' @param list_prods (optional) Character vector with the values of the
#'  products to be processed (accepted values: "TOA", "BOA", "SCL",
#'  "TCI"). Default is "BOA".
#' @param list_indices (optional) Character vector with the values of the
#'  spectral indices to be computed. Default is no one (NA).
#' @param index_source (optional) Character value: if "BOA" (default), indices
#'  are computed from BOA values; if "TOA", non corrected reflectances
#'  are instead used (be careful to use this setting!).
#' @param mask_type (optional) Character value which determines the categories
#'  in the Srface Classification Map to be masked (see [s2_mask()]
#'  for the accepted values). Default (NA) is not to mask.
#' @param clip_on_extent (optional) Logical: if TRUE (default), output products
#'  and indices are clipped to the selected extent (and resampled/reprojected);
#'  if FALSE, the geometry and extension of the tiles is maintained.
#' @param extent_as_mask (optional) Logical: if TRUE, pixel values outside
#'  the `extent` polygon are set to NA; if FALSE (default), all the values
#'  within the bounding box are maintained.
#' @param reference_path (optional) Path of the raster file to be used as a
#'  reference grid. If NA (default), no reference is used.
#' @param res (optional) Numerifc vector of length 2 with the x-y resolution
#'  for output products. Default: c(10,10). NA means that the resolution
#'  is keeped as native.
#' @param res_s2 (optional) Character value corresponding to the native Sentinel-2
#'  resolution to be used. Accepted values are "10m" (default), "20m"
#'  and "60m".
#' @param unit (optional) Character value corresponding to the unit of measure
#'  with which to interpret the resolution (for now, only "Meter" -
#'  the default value - is supported).
#' @param proj (optional) Character string with the pro4string of the output
#'  resolution. default value (NA) means not to reproject.
#' @param resampling (optional) Resampling method (one of the values supported
#'  by `gdal_translate`: "near" (default), "bilinear", "cubic",
#'  "cubicspline", "lanczos", "average" or "mode").
#' @param resampling_scl (optional) Resampling method for categorical products
#'  (for now, only SCL): one among "near" (default) and "mode".
#' @param outformat (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is "GTiff".
#' @param compression (optional) In the case GTiff is chosen as
#'  output format, the compression indicated with this parameter is
#'  used (default is "DEFLATE").
#' @param overwrite (optional) Logical value: should existing output
#'  files be overwritten? (default: FALSE).
#' @param path_l1c (optional) Path of the directory in which Level-1C SAFE
#'  products are searched and/or downloaded. If not provided (default), a
#'  temporary directory is used.
#' @param path_l2a (optional) Path of the directory in which Level-2A SAFE
#'  products are searched, downloaded and/or generated. If not provided
#'  (default), a temporary directory is used.
#' @param path_tiles (optional) Path of the directory in which Sentinel-2
#'  tiles (as generated by [s2_translate]) are searched and/or generated.
#'  If not provided (default), a temporary directory is used, and files
#'  are generated as virtual rasters; otherwise, they are generated in
#'  the format specified with `outformat` parameter.
#' @param path_merged (optional) Path of the directory in which Sentinel-2
#'  tiles merged by orbit (as generated by [s2_merge]) are searched and/or
#'  generated.
#'  If not provided (default), a temporary directory is used, and files
#'  are generated as virtual rasters; otherwise, they are generated in
#'  the format specified with `outformat` parameter.
#' @param path_out (optional) Path of the directory in which Sentinel-2
#'  output products are searched and/or generated.
#'  If not provided (default), a temporary directory is used.
#' @param path_indices (optional) Path of the directory in which files of
#' spectral indices are searched and/or generated.
#'  If not provided (default), `path_out` is used.
#' @param path_subdirs (optional) Logical: if TRUE (default), a directory
#'  for each output product or spectral index is generated within
#'  `path_tiles`, `path_merged`, `path_out` and `path_indices`; if FALSE,
#'  products are put directly within them.
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom geojsonio geojson_json
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_cast st_read
#' @export


fidolasen_s2 <- function(param_list=NULL,
                         gui=NA,
                         preprocess=NA,
                         s2_levels=NA,
                         sel_sensor=NA,
                         online=NA,
                         overwrite_safe=NA,
                         rm_safe=NA,
                         step_atmcorr=NA,
                         timewindow=NA,
                         timeperiod=NA,
                         extent=NA,
                         s2tiles_selected=NA,
                         s2orbits_selected=NA,
                         list_prods=NA,
                         list_indices=NA,
                         index_source=NA,
                         mask_type=NA,
                         clip_on_extent=NA,
                         extent_as_mask=NA,
                         reference_path=NA,
                         res=NA,
                         res_s2=NA,
                         unit=NA,
                         proj=NA,
                         resampling=NA,
                         resampling_scl=NA,
                         outformat=NA,
                         compression=NA,
                         overwrite=NA,
                         path_l1c=NA,
                         path_l2a=NA,
                         path_tiles=NA,
                         path_merged=NA,
                         path_out=NA,
                         path_indices=NA,
                         path_subdirs=NA) {
  
  
  ### Preliminary settings ###
  
  # import python modules
  # check that python and the required modules are installed
  py <- init_python()

  # internal function: return character(0) instead of NULL
  # (used to build _req names)
  nn <- function(x) {if (is.null(x)) character(0) else x}
  
  ## 1. Read / import parameters ##
  
  # Create parameter list with default values
  # (elements should correspond to the function arguments,
  # except for param_list)
  pm_def <- list(preprocess=TRUE,
                 s2_levels=c("l2a"),
                 sel_sensor=c("s2a","s2b"),
                 online=TRUE,
                 overwrite_safe=FALSE,
                 rm_safe="no",
                 step_atmcorr="auto",
                 timewindow=NA, # below re-defined as last 90 days if online mode
                 timeperiod="full",
                 extent=NA, # below re-defined as sample extent if online mode
                 s2tiles_selected=NA, # below re-defined for online mode
                 s2orbits_selected=NA, # temporary select all orbits (TODO implement)
                 list_prods=c("BOA"),
                 list_indices=NA,
                 index_source="BOA",
                 mask_type=NA,
                 clip_on_extent=TRUE,
                 extent_as_mask=FALSE,
                 reference_path=NA,
                 res=c(10,10),
                 res_s2="10m",
                 unit="Meter",
                 proj=NA,
                 resampling="near",
                 resampling_scl="near",
                 outformat="GTiff",
                 compression="LZW",
                 overwrite=FALSE,
                 path_l1c=NA,
                 path_l2a=NA,
                 path_tiles=NA,
                 path_merged=NA,
                 path_out=NA,
                 path_indices=NA,
                 path_subdirs=TRUE)
  
  # Starting execution
  print_message(
    type = "message",
    date = TRUE,
    "Starting fidolasen execution."
  )
  
  # Import param_list, if provided
  pm <- if (is.null(param_list)) {
    # create with default values
    pm_def
  } else if (is(param_list, "character")) {
    # load json parameter file
    jsonlite::fromJSON(param_list)
    # TODO check package version and parameter names
  } else if (is(param_list, "list")) {
    param_list
    # TODO check parameter names
  }
  
  # Check param_list version
  if (is.null(pm$fidolasen_version)) {
    pm$fidolasen_version <- package_version("0.2.0")
  }
  if (packageVersion("fidolasen") > package_version(pm$fidolasen_version)) {
    open_gui <- print_message(
      type="waiting",
      "The parameter file was created with an old version of the package: ",
      "Type \"G\" (and ENTER) to open the GUI and check that" ,
      "the input parameters are correct, or ENTER to procees anyway ",
      "(this could lead to errors). ",
      "Alternatively, press ESC to interrupt."
    )
    if (length(grep("^[Gg]",open_gui))>0) {
      gui <- TRUE
    }
  }

  # Overwrite parameters passed manually
  # (if some parameters are still missing, copy from default values)
  for (sel_par in names(pm_def)) {
    if (!(length(get(sel_par))==1 & all(is.na(get(sel_par))))) {
      pm[[sel_par]] <- get(sel_par)
    }
    if (length(pm[[sel_par]])==1 & all(is.na(pm[[sel_par]]))) {
      pm[[sel_par]] <- pm_def[[sel_par]]
    }
  }
  
  # # Define sample spatial extent / temporal timewindow if online mode
  # if (pm$online == TRUE) {
  #   if (is.na(pm$extent)) {
  #     pm$extent <- get_extent(
  #       matrix(c(9.4, 45.4, 10.27, 46.1), nrow = 2),
  #       "+init=epsg:4326"
  #     ) %>%
  #       as("sfc_POLYGON") %>%
  #       geojson_json()
  #     # if (is.na(pm$s2tiles_selected)) {
  #     #   pm$s2tiles_selected <- c("32TNR", "32TNS")
  #     # }
  #   }
  #   if (all(is.na(pm$timewindow))) {
  #     pm$timewindow <- c(Sys.Date() - 90, Sys.Date())
  #   }
  # }
  
  
  ## Open GUI (if required)
  # if gui argument was not specified, use default value
  if (is.na(gui)) {
    gui <- if (is.null(param_list)) {TRUE} else {FALSE}
  }
  # open GUI
  if (gui==TRUE) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Launching GUI..."
    )
    
    pm <- .s2_gui(pm, par_fun = "fidolasen_s2")
    if (is.null(pm)) {
      print_message(
        type = "message",
        date = TRUE,
        "Program interrupted by the user (GUI closed)."
      )
      return()
    }
    
    print_message(
      type = "message",
      date = TRUE,
      "Gui closed by the user. Starting processing."
    )
    
  }
  
  # TODO check consistency of parameters
  
  # internal parameters
  dir.create(path_tmp <- tempdir(), showWarnings = FALSE) # consider to add as an optional parameter
  path_out <- if (!is.na(pm$path_out)) {pm$path_out} else {file.path(path_tmp,"out")}
  path_indices <- if (!is.na(pm$path_indices)) {pm$path_indices} else {file.path(path_tmp,"indices")}
  path_tiles <- if (!is.na(pm$path_tiles)) {pm$path_tiles} else {file.path(path_tmp,"tiles")}
  path_merged <- if (!is.na(pm$path_merged)) {pm$path_merged} else {file.path(path_tmp,"merged")}
  path_warped <- if (is.na(pm$mask_type)) {path_out} else {file.path(path_tmp,"warped")}
  
  # accepted products (update together with the same variables in s2_gui())
  l1c_prods <- c("TOA")
  l2a_prods <- c("BOA","SCL","TCI")
  
  # if masking is required, produce also SCL
  list_prods <- if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods){
    c(pm$list_prods,"SCL")
  } else {
    pm$list_prods
  }
  # if some indices is required, compute also TOA or BOA
  if (any(!is.na(pm$list_indices)) & !pm$index_source %in% pm$list_prods){
    list_prods <- c(list_prods, pm$index_source)
  }
  
  # update s2_levels if processing is TRUE (retrieve from products)
  if (pm$preprocess==TRUE) {
    pm$s2_levels <- c(
      if (any(list_prods %in% l1c_prods)) {"l1c"},
      if (any(list_prods %in% l2a_prods)) {"l2a"}
    )
  }
  
  # check that output directories exist
  paths_exist <- sapply(pm[c("path_l1c","path_l2a","path_tiles","path_merged","path_out","path_indices")],
                        function(x){if(is.na(x)){NA}else{file.exists(x)}})
  paths_exist <- paths_exist[!is.na(paths_exist)]
  if (any(!paths_exist)) {
    print_message(
      type="error",
      "The following output ",
      if (sum(!paths_exist)==1) {"directory does "} else {"directories do "},
      "not exist:\n",
      paste(pm[names(paths_exist[!paths_exist])],collapse="\n"),
      ".\nPlease create ",
      if (sum(!paths_exist)==1) {"it "} else {"them "},
      "before continuing."
    )
  }
  
  
  # check output format
  # sel_driver <- py$osgeo$gdal$GetDriverByName(pm$outformat)
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="fidolasen"))
  sel_driver <- gdal_formats[gdal_formats$name==pm$outformat,]
  
  # if (is.null(py_to_r(sel_driver))) {
  if (nrow(sel_driver)==0) {
    print_message(
      type="error",
      "Format \"",pm$outformat,"\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
  }
  # define output extension

  # out_ext <- if (pm$outformat=="ENVI") {
  #   "dat"
  # } else {
  #   # unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1]
  #   unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(py$osgeo$gdal$DMD_EXTENSIONS))," ")," "))[1]
  # }
  out_ext <- sel_driver[1,"ext"]

  ### Find, download and preprocess ###
  
  ## 2. List required products ##
  s2_lists <- list()
  
  if (pm$online == TRUE) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Searching for available SAFE products on SciHub..."
    )

    # if online mode, retrieve list with s2_list() basing on parameters
    if ("l1c" %in% pm$s2_levels) {
      # list of SAFE (L1C) needed for required L1C
      s2_lists[["l1c"]] <- s2_list(spatial_extent = st_read(pm$extent),
                                   time_interval = pm$timewindow,
                                   tile = pm$s2tiles_selected,
                                   level = "L1C")
    }
    if ("l2a" %in% pm$s2_levels) {
      # list of SAFE (L1C or/and L2A) needed for required L2A
      s2_lists[["l2a"]] <- s2_list(spatial_extent = st_read(pm$extent),
                                   time_interval = pm$timewindow,
                                   tile = pm$s2tiles_selected,
                                   level = if (pm$step_atmcorr=="auto") {
                                     "auto"
                                   } else if (pm$step_atmcorr=="l2a") {
                                     "L2A"
                                   } else if (pm$step_atmcorr %in% c("scihub","no")) {
                                     "L1C"
                                   })
    }
    
  } else {
    
    # if offline mode, read the SAFE product list from folders and filter
    if ("l1c" %in% pm$s2_levels) {
      s2_lists[["l1c"]] <- list.files(pm$path_l1c, "\\.SAFE$")
    }
    if ("l2a" %in% pm$s2_levels) {
      s2_lists[["l2a"]] <- if (pm$step_atmcorr=="l2a") {
        list.files(pm$path_l2a, "\\.SAFE$")
      } else if (pm$step_atmcorr %in% c("scihub","no","auto")) { # FIXME "auto"? -> for now, managed as "scihub" (apply sen2cor; if l2a already exists, do nothing)
        list.files(pm$path_l1c, "\\.SAFE$")
      }
    }
    s2_lists <- lapply(s2_lists, function(l) {
      sapply(l, function(x) {
        tryCatch(s2_getMetadata(x, info="nameinfo")$level,
                 error = function(e) NA)
      })
    })
    s2_lists <- lapply(s2_lists, function(l) {l[!is.na(l)]})
    
  }
  s2_list <- unlist(s2_lists)[!duplicated(unlist(lapply(s2_lists, names)))]
  names(s2_list) <- gsub("^l[12][ac]\\.","",names(s2_list))
  
  # If s2_list is empty, exit
  if (length(s2_list)==0) {
    print_message(
      type = "message",
      date = TRUE,
      if (pm$online==FALSE) {
        paste0("No SAFE products which match the settings were found locally; ",
               "please download them or set different spatial/temporal extents.")
      } else {
        paste0("No SAFE products matching the settings were found; ",
               "please set different spatial/temporal extents.")
      },
      " Execution halted."
    )
  }
  
  
  # getting required metadata
  s2_dt <- lapply(names(s2_list), function(x) {
    unlist(s2_getMetadata(x, info="nameinfo")) %>%
      t() %>%
      as.data.frame()
  }) %>%
    rbindlist(fill=TRUE)
  s2_dt[,c("name","url"):=list(names(s2_list),s2_list)]
  s2_dt[,c("sensing_datetime","creation_datetime"):=list(as.POSIXct(sensing_datetime, format="%s"),
                                                         as.POSIXct(creation_datetime, format="%s"))]
  
  s2_dt <- s2_dt[mission %in% toupper(substr(pm$sel_sensor,2,3)),][
    order(-sensing_datetime),]
  if (!any(is.na(pm$timewindow))) {
    s2_dt <- s2_dt[as.Date(sensing_datetime) >= pm$timewindow[1] &
                     as.Date(sensing_datetime) <= pm$timewindow[2],]
  }
  if (all(!is.na(pm$s2tiles_selected))) { # FIXME works only with new products! (add retrieval of all tiles)
    s2_dt <- s2_dt[id_tile %in% pm$s2tiles_selected,]
  }
  if (all(!is.na(pm$s2orbits_selected))) {
    s2_dt <- s2_dt[id_orbit %in% pm$s2orbits_selected,]
  }
  # setorder(s2_dt, -sensing_datetime)
  s2_dt <- s2_dt[!duplicated(s2_dt[,list(mission,level,id_orbit,id_tile)]),]
  s2_list_l1c <- s2_dt[level=="1C",url] # list of required L1C
  s2_list_l2a <- s2_dt[level=="2A",url] # list of required L2A
  names(s2_list_l1c) <- s2_dt[level=="1C",name]
  names(s2_list_l2a) <- s2_dt[level=="2A",name]
  

  ## 3. Download them ##
  # TODO implement ovwerite/skip
  # (now it skips, but analysing each single file)
  
  if (pm$online == TRUE) {
    if (length(s2_list_l2a)>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to download the required level-1C SAFE products."
      )
      
      lapply(pm$s2tiles_selected, function(tile) {
        s2_download(s2_list_l2a,
                    outdir = pm$path_l2a,
                    tile = tile)
      })
      
    }
    if (length(s2_list_l1c)>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to download the required level-2A SAFE products."
      )
      
      lapply(pm$s2tiles_selected, function(tile) {
        s2_download(s2_list_l1c,
                    outdir = pm$path_l1c,
                    tile = tile)
      })
      
    }
  }
  
  # apply sen2cor
  if (pm$step_atmcorr %in% c("auto","scihub")) {
    s2_list_l1c_tocorrect <- if (pm$overwrite_safe==FALSE) {
      s2_list_l1c[
        !gsub(
          "^S2([AB])\\_MSIL1C\\_","S2\\1\\_MSIL2A\\_",
          names(s2_list_l1c)
        ) %in% names(s2_list_l2a)
      ]
    } else {
      s2_list_l1c
    }
    if (length(s2_list_l1c_tocorrect)>0) {
      
      if (sum(!file.path(pm$path_l1c,names(s2_list_l1c_tocorrect)) %>% file.exists()) > 0) {
        print_message(
          type = "message",
          date = TRUE,
          "Starting to correct level-1C SAFE products with sen2cor. ",
          "This operation could take very long time."
        )
      }
      
      s2_list_l2a_corrected <- sen2cor(names(s2_list_l1c_tocorrect),
                                          l1c_dir = pm$path_l1c,
                                          outdir = pm$path_l2a)
      names(s2_list_l2a_corrected) <- basename(s2_list_l2a_corrected)
      s2_list_l2a <- c(s2_list_l2a,s2_list_l2a_corrected)
    }
  }
  
  # delete SAFE, if required
  if (pm$rm_safe == "all") {
    unlink(file.path(pm$path_l1c,names(s2_list_l1c)), recursive=TRUE)
    unlink(file.path(pm$path_l2a,names(s2_list_l2a)), recursive=TRUE)
  } else if (pm$rm_safe == "l1c" & !("l1c" %in% pm$s2_levels)) {
    unlink(file.path(pm$path_l1c,names(s2_list_l1c_tocorrect)), recursive=TRUE)
  }
  
  # if no processing is required, stop here
  if (pm$preprocess == FALSE) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Execution of fidolasen session terminated."
    )
    
    return(safe_fullnames)
    
  }
  
  
  ### Processing steps ###
  
  # do anything if preprocess is not required
  if (pm$preprocess == TRUE) {
    
    
    ## Define output formats
    if (!is.na(pm$path_tiles)) {
      tiles_ext <- out_ext
      tiles_outformat <- pm$outformat
    } else {
      tiles_ext <- "vrt"
      tiles_outformat <- "VRT"
    }
    if (!is.na(pm$path_merged)) {
      merged_ext <- out_ext
      merged_outformat <- pm$outformat
    } else {
      merged_ext <- "vrt"
      merged_outformat <- "VRT"
    }
    if (is.na(pm$mask_type)) {
      warped_ext <- out_ext
      warped_outformat <- pm$outformat
    } else {
      warped_ext <- "vrt"
      warped_outformat <- "VRT"
    }
    
    ## Define output file names and lists ##
    # This section is structured in the following way:
    # 1. Retrieve the file names expected to be present at the
    #    end of the processing chain (suffix _exp):
    #    - tiles_names_
    #    - merged_names_
    #    - warped_names_
    #    - masked_names_
    #    - out_names_ (= warped_names_ or subset(warped_names_)+masked_names_)
    #    - indices_names_
    # 2. Compute the file names expected to be created
    #    (suffixes _req and _new, see below)
    #    (this operation is done in reverse order).
    # Meaning of the suffixes _exp, _req and _new
    # (here and for all the script):
    # _exp: [full] names of the files expected to be present at the
    #       end of the processing chain (already existing or not);
    # _req: names of the files required for the next step
    #       (e.g. tiles_names_req are required to perform s2_merge())
    # _new: names of the required files not existing yet (expected
    #       to be created).
    # With overwrite=TRUE, all these vectors are equal
    # (e.g. merged_names_exp = merged_names_req = merged_names_new),
    # because all is overwritten.
    # The list of the generated file names is at the end of this section.
    
    # expected names for tiles
    tiles_l1c_names_exp <- lapply(names(s2_list_l1c), function(x){
      lapply(list_prods[list_prods %in% l1c_prods], function(p){
        file.path(
          path_tiles,
          if(pm$path_subdirs==TRUE){p}else{""},
          s2_shortname(x, prod_type=p, ext=tiles_ext)
        )
      })
    }) %>% unlist()
    tiles_l2a_names_exp <- lapply(names(s2_list_l2a), function(x){
      lapply(list_prods[list_prods %in% l2a_prods], function(p){
        file.path(
          path_tiles,
          if(pm$path_subdirs==TRUE){p}else{""},
          s2_shortname(x, prod_type=p, ext=tiles_ext)
        )
      })
    }) %>% unlist()
    tiles_names_exp <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_exp},
                         if("l2a" %in% pm$s2_levels) {tiles_l2a_names_exp})
    
    # expected names for merged
    if (length(tiles_names_exp)==0) {
      merged_names_exp <- NULL
    } else {
      merged_names_exp <- data.table(
        fs2nc_getElements(tiles_names_exp, format="data.frame")
      )[,paste0("S2",
                mission,
                level,"_",
                strftime(sensing_date,"%Y%m%d"),"_",
                id_orbit,"__",
                prod_type,"_",
                substr(res,1,2),".",
                file_ext)]
      merged_names_exp <- merged_names_exp[!duplicated(merged_names_exp)]
      merged_names_exp <- gsub(paste0(tiles_ext,"$"),merged_ext,merged_names_exp) %>%
        file.path(path_merged,
                  if(pm$path_subdirs==TRUE){fs2nc_getElements(merged_names_exp, format="data.frame")$prod_type}else{""},
                  .)
    }
    
    # index which is TRUE for SCL products, FALSE for others
    names_merged_exp_scl_idx <- fs2nc_getElements(merged_names_exp,format="data.frame")$prod_type=="SCL"
    # index which is TRUE for products to be atm. masked, FALSE for others
    names_tomask_idx <- if ("SCL" %in% pm$list_prods) {
      !names_merged_exp_scl_idx
    } else {
      names_merged_exp_scl_idx>-1
    }
    
    # expected names for warped products
    warped_names_exp <- if (pm$clip_on_extent==FALSE | length(merged_names_exp)==0) {
      NULL
    } else {
      # if SCL were explicitly required, directly create them as output files (since they are never masked);
      # instead, build only virtual files
      ifelse(
        names_merged_exp_scl_idx & "SCL" %in% pm$list_prods,
        file.path(path_out,
                  if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                  gsub(paste0(merged_ext,"$"),out_ext,basename(merged_names_exp))),
        ifelse(names_merged_exp_scl_idx,
               file.path(path_warped,
                         if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                         gsub(paste0(merged_ext,"$"),out_ext,basename(merged_names_exp))),
               file.path(path_warped,
                         if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                         gsub(paste0(merged_ext,"$"),warped_ext,basename(merged_names_exp))))
      )
    }
    
    # expected names for masked products
    # if clip_on_extent is required, mask warped, otherwise, mask merged
    masked_names_exp <- if (!is.na(pm$mask_type)) {
      if (pm$clip_on_extent==TRUE) {
        file.path(path_out,
                  if(pm$path_subdirs==TRUE){basename(dirname(nn(warped_names_exp[!names_merged_exp_scl_idx])))}else{""},
                  gsub(paste0(warped_ext,"$"),out_ext,basename(nn(warped_names_exp[!names_merged_exp_scl_idx]))))
      } else {
        file.path(path_out,
                  if(pm$path_subdirs==TRUE){basename(dirname(nn(merged_names_exp[!names_merged_exp_scl_idx])))}else{""},
                  gsub(paste0(merged_ext,"$"),out_ext,basename(nn(merged_names_exp[!names_merged_exp_scl_idx]))))
      }
    }
    
    # expected names for output products
    out_names_exp <- if (!is.na(pm$mask_type)) {
      if (pm$clip_on_extent==TRUE) {
        c(warped_names_exp[names_merged_exp_scl_idx], masked_names_exp)
      } else {
        c(merged_names_exp[names_merged_exp_scl_idx], masked_names_exp)
      }
    } else {
      if (pm$clip_on_extent==TRUE) {
        warped_names_exp
      } else {
        merged_names_exp
      }
    }
    
    # expected names for indices
    level_for_indices <- if (all(pm$index_source=="TOA")) {
      "1C"
    } else if (all(pm$index_source=="BOA")) {
      "2A"
    } else {
      c("1C","2A")
    }
    indices_names_exp <- if (length(out_names_exp)==0 | any(is.na(pm$list_indices))) {
      NULL
    } else {
      data.table(
        fs2nc_getElements(out_names_exp, format="data.frame")
      )[level %in% level_for_indices,
        paste0("S2",
               mission,
               level,"_",
               strftime(sensing_date,"%Y%m%d"),"_",
               id_orbit,"__",
               "<index>_",
               substr(res,1,2),".",
               file_ext)] %>%
        expand.grid(pm$list_indices) %>%
        apply(1,function(x){
          file.path(
            if(pm$path_subdirs==TRUE){x[2]}else{""},
            gsub("<index>",x[2],x[1])
          )
        }) %>%
        file.path(path_indices,.) %>%
        gsub(paste0(merged_ext,"$"),out_ext,.)
    }
    
    
    # list of required files and steps
    
    # if overwrite is set to TRUE, works with expected names;
    # otherwise, compute non-existing values
    
    if (pm$overwrite==TRUE) {
      
      indices_names_new <- indices_names_exp
      out_names_req <- if (length(indices_names_exp)==0) {NULL} else {out_names_exp}
      out_names_new <- out_names_exp
      masked_names_new <- masked_names_exp
      warped_names_req <- if (pm$clip_on_extent==FALSE | length(out_names_exp)==0) {
        NULL
      } else {
        warped_names_exp
      }
      warped_names_new <- warped_names_reqout <- warped_names_exp
      merged_names_req <- if (pm$clip_on_extent==TRUE & length(warped_names_exp)==0) {
        NULL
      } else if (pm$clip_on_extent==FALSE & length(out_names_exp)==0) {
        NULL
      } else {
        merged_names_exp
      }
      merged_names_new <- merged_names_exp
      tiles_names_req <- if (length(merged_names_exp)==0) {
        NULL
      } else {
        tiles_names_exp
      }
      tiles_names_new <- tiles_names_exp
      safe_names_l1c_req <- file.path(pm$path_l1c,names(s2_list_l1c))
      safe_names_l2a_req <- file.path(pm$path_l2a,names(s2_list_l2a))
      
    } else {
      
      indices_names_new <- indices_names_exp[!file.exists(nn(indices_names_exp))]
      
      # required output products
      out_basenames_req <- if (length(indices_names_new)==0) {
        NULL
      } else {
        data.table(
          fs2nc_getElements(indices_names_new, format="data.frame")
        )[,paste0("S2",
                  mission,
                  level,"_",
                  strftime(sensing_date,"%Y%m%d"),"_",
                  id_orbit,"__",
                  ifelse(level=="2A","BOA","TOA"),"_",
                  substr(res,1,2),".",
                  out_ext)]
      }
      out_names_req <- out_names_exp[basename(nn(out_names_exp)) %in% out_basenames_req]
      out_names_new <- if (is.na(pm$path_out)) {
        out_names_req
      } else {
        unique(c(
          out_names_req,
          if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods) {
            out_names_exp[fs2nc_getElements(out_names_exp, format="data.frame")$prod_type!="SCL"]
          } else {
            out_names_exp
          }
        ))
      }
      out_names_new <- out_names_new[!file.exists(nn(out_names_new))]
      if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
        # if some output needs to be created with masking, include the creation of SCL
        out_names_new <- unique(c(
          out_names_new,
          out_names_exp[fs2nc_getElements(out_names_exp, format="data.frame")$prod_type=="SCL"]
        ))
      }
      out_names_new <- out_names_new[!file.exists(nn(out_names_new))]
      
      # required masked and warped
      masked_names_new <- if (is.na(pm$mask_type)) {
        NULL
      } else {
        out_names_new[fs2nc_getElements(out_names_new, format="data.frame")$prod_type!="SCL"]
      }
      warped_names_req <- if (pm$clip_on_extent==FALSE | length(out_names_new)==0) {
        NULL
      } else {
        c(file.path(path_warped,
                    if(pm$path_subdirs==TRUE){basename(dirname(out_names_new))}else{""},
                    gsub(paste0(out_ext,"$"),warped_ext,basename(out_names_new))),
          if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
            out_names_exp[fs2nc_getElements(out_names_exp, format="data.frame")$prod_type=="SCL"]
          })
      }
      warped_names_new <- warped_names_req[!file.exists(nn(warped_names_req))]
      
      # required merged
      merged_basenames_req <- c(
        gsub(paste0(warped_ext,"$"),merged_ext,basename(nn(warped_names_new))),
        gsub(paste0(out_ext,"$"),merged_ext,basename(nn(masked_names_new))))
      merged_names_req <- if (pm$clip_on_extent==TRUE) {
        merged_names_exp[basename(nn(merged_names_exp)) %in% merged_basenames_req]
      } else {
        c(file.path(path_merged,
                    if(pm$path_subdirs==TRUE){basename(dirname(nn(out_names_new)))}else{""},
                    gsub(paste0(out_ext,"$"),merged_ext,basename(nn(out_names_new)))),
          if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
            out_names_exp[fs2nc_getElements(out_names_req, format="data.frame")$prod_type=="SCL"]
          })
      }
      merged_names_new <- if (is.na(pm$path_merged)) {
        merged_names_req
      } else {
        unique(c(merged_names_req,merged_names_exp))
      }
      merged_names_new <- merged_names_new[!file.exists(nn(merged_names_new))]
      
      # output of merged_names_req
      warped_names_reqout <- warped_names_exp[merged_names_exp %in% merged_names_req]
      
      # required tiles
      tiles_basenames_req <- gsub(paste0(merged_ext,"$"),tiles_ext,basename(nn(merged_names_new))) %>%
        gsub("\\_\\_","_[A-Z0-9]{5}_",.) %>%
        paste0("^",.,"$")
      tiles_names_req <- tiles_names_exp[unlist(lapply(tiles_basenames_req, grep, basename(nn(tiles_names_exp))))]
      tiles_names_new <- if (is.na(pm$path_tiles)) {
        tiles_names_req
      } else {
        unique(c(tiles_names_req,tiles_names_exp))
      }
      tiles_names_new <- tiles_names_new[!file.exists(nn(tiles_names_new))]
      
      # required SAFE products
      if (length(tiles_names_new)==0) {
        safe_names_l1c_req <- safe_names_l2a_req <- NULL
      } else {
        tiles_dt_new <- data.table(fs2nc_getElements(tiles_names_new,format="data.frame"))
        safe_dt_av <- sapply(c(names(s2_list_l1c),names(s2_list_l2a)), function(x) unlist(s2_getMetadata(x, info="nameinfo"))) %>%
          t() %>% data.table()
        tiles_basenames_av <- safe_dt_av[,paste0("S",
                                                 mission,
                                                 level,"_",
                                                 strftime(as.POSIXct(sensing_datetime, format="%s"),"%Y%m%d"),"_",
                                                 id_orbit,"_",
                                                 id_tile,"_",
                                                 "[A-Z0-9]{3}_",
                                                 "[126]0\\.",
                                                 tiles_ext)]
        tiles_basenames_l1c_av <- tiles_basenames_av[safe_dt_av$level=="1C"]
        tiles_basenames_l2a_av <- tiles_basenames_av[safe_dt_av$level=="2A"]
        safe_names_l1c_req <- if (nrow(tiles_dt_new[level=="1C",])>0) {
          names(s2_list_l1c)[
            lapply(tiles_basenames_l1c_av,
                   function(x){grep(x,tiles_names_new)} %>% length() > 0) %>%
              unlist()
            ] %>%
            file.path(pm$path_l1c,.)
        } else {
          character(0)
        }
        safe_names_l2a_req <- if (nrow(tiles_dt_new[level=="2A",])>0) {
          names(s2_list_l2a)[
            lapply(tiles_basenames_l2a_av,
                   function(x){grep(x,tiles_names_new)} %>% length() > 0) %>%
              unlist()
            ] %>%
            file.path(pm$path_l2a,.)
        } else {
          character(0)
        }
      }
      
    } # end of pm$overwrite FALSE IF cycle
    
    # End of the section of the creation of file names
    # List of the file names:
    # List of all the file names, in order of creation
    # (useful for checks):
    # tiles_l1c_names_exp
    # tiles_l2a_names_exp
    # tiles_names_exp
    # merged_names_exp
    # warped_names_exp
    # masked_names_exp
    # out_names_exp
    # indices_names_exp
    # indices_names_new
    # out_names_req
    # out_names_new
    # masked_names_new
    # warped_names_req
    # warped_names_new
    # merged_names_req
    # merged_names_new
    # tiles_names_req
    # tiles_names_new
    # safe_names_l1c_req
    # safe_names_l2a_req

    
    ## 4. Convert in vrt ##
    if (length(c(safe_names_l1c_req,safe_names_l2a_req))>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to translate SAFE products in custom format."
      )
      
      dir.create(path_tiles, recursive=FALSE, showWarnings=FALSE)
      tiles_l1c_names_out <- tiles_l2a_names_out <- character(0)
      
      if("l1c" %in% pm$s2_levels) {
        list_l1c_prods <- list_prods[list_prods %in% l1c_prods]
        for (sel_prod in safe_names_l1c_req) {
          tiles_l1c_names_out <- c(
            tiles_l1c_names_out,
            s2_translate(infile = sel_prod,
                         outdir = path_tiles,
                         prod_type = list_l1c_prods,
                         format = tiles_outformat,
                         res = pm$res_s2,
                         vrt_rel_paths = TRUE,
                         subdirs = pm$path_subdirs,
                         overwrite = pm$overwrite))
        }
      }
      if("l2a" %in% pm$s2_levels) {
        list_l2a_prods <- list_prods[list_prods %in% l2a_prods]
        for (sel_prod in safe_names_l2a_req) {
          tiles_l2a_names_out <- c(
            tiles_l2a_names_out,
            s2_translate(infile = sel_prod,
                         outdir = path_tiles,
                         prod_type = list_l2a_prods,
                         format = tiles_outformat,
                         res = pm$res_s2,
                         vrt_rel_paths = TRUE,
                         subdirs = pm$path_subdirs,
                         overwrite = pm$overwrite))
        }
      }
      
      tiles_names_out <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_out},
                           if("l2a" %in% pm$s2_levels) {tiles_l2a_names_out})
      # TODO check tiles_names_out - tiles_names_req
      
    } # end of s2_translate IF cycle
    
    
    ## 5. Merge by orbit ##
    if (length(tiles_names_req)>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to merge tiles by orbit."
      )
      
      dir.create(path_merged, recursive=FALSE, showWarnings=FALSE)
      merged_names_out <- s2_merge(tiles_names_req,
                                   path_merged,
                                   subdirs=pm$path_subdirs,
                                   format=merged_outformat,
                                   overwrite=pm$overwrite)
      # TODO check merged_names_out - merged_names_req
      
    } # end of s2_merge IF cycle
    
    
    if (length(merged_names_req)>0) {
      
      ## 6. Clip, rescale, reproject ##
      if (pm$clip_on_extent==TRUE) {
        
        print_message(
          type = "message",
          date = TRUE,
          "Starting to edit geometry (clip, reproject, rescale)."
        )
        
        dir.create(path_warped, recursive=FALSE, showWarnings=FALSE)
        # create mask
        s2_mask_extent <- if (is.na(pm$extent)) {
          NULL
        } else if (pm$extent_as_mask==TRUE) {
          st_read(pm$extent)
        } else {
          suppressWarnings(st_cast(st_read(pm$extent, quiet=TRUE),"LINESTRING"))
        }  # TODO add support for multiple extents
        
        if(pm$path_subdirs==TRUE){
          sapply(unique(dirname(warped_names_reqout)),dir.create,showWarnings=FALSE)
        }
        
        if (any(!file.exists(warped_names_reqout)) | pm$overwrite==TRUE) {
          # index which is TRUE for SCL products, FALSE for others
          names_merged_req_scl_idx <- fs2nc_getElements(merged_names_req,format="data.frame")$prod_type=="SCL"
          fidolasen::gdal_warp(merged_names_req[!names_merged_req_scl_idx],
                               warped_names_reqout[!names_merged_req_scl_idx],
                               of = warped_outformat,
                               ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
                               mask = s2_mask_extent,
                               tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
                               t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
                               r = pm$resampling,
                               overwrite = pm$overwrite) # TODO dstnodata value?
          fidolasen::gdal_warp(merged_names_req[names_merged_req_scl_idx],
                               warped_names_reqout[names_merged_req_scl_idx],
                               of = pm$outformat, # use physical files to speed up next steps
                               ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
                               mask = s2_mask_extent,
                               tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
                               t_srs = if (!is.na(pm$proj)) {pm$proj} else {NULL},
                               r = pm$resampling_scl,
                               overwrite = pm$overwrite)
        }
        
      } # end of gdal_warp IF clip_on_extent cycle
      
      ## 7. Apply mask ##
      # FIXME understand if this should be done before warping (if so, how to manage virtual/physical files?)
      # masked_names <- file.path(path_out,
      #                           if(pm$path_subdirs==TRUE){basename(dirname(warped_names[!names_merged_exp_scl_idx]))}else{""},
      #                           gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names[!names_merged_exp_scl_idx])))
      
      if (!is.na(pm$mask_type)) {
        print_message(
          type = "message",
          date = TRUE,
          "Starting to apply atmospheric masks."
        )
        masked_names_out <- s2_mask(
          if(pm$clip_on_extent==TRUE){warped_names_req}else{merged_names_req},
          if(pm$clip_on_extent==TRUE){warped_names_req}else{merged_names_exp[names_merged_exp_scl_idx]},
          mask_type=pm$mask_type,
          outdir=path_out,
          format=pm$outformat,
          subdirs=pm$path_subdirs,
          overwrite=pm$overwrite,
          parallel=FALSE
        )
      }
      
    } # end of gdal_warp and s2_mask IF cycle
    
    ## 8. Compute spectral indices ##
    # dir.create(file.path(path_out,pm$list_indices), recursive=FALSE, showWarnings = FALSE)
    if (length(out_names_req)>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Computing required spectral indices."
      )
      
      indices_names <- s2_calcindices(out_names_req,
                                      indices=pm$list_indices,
                                      outdir=path_indices,
                                      subdirs=TRUE,
                                      source=pm$index_source,
                                      format=pm$outformat,
                                      overwrite=pm$overwrite)
    }
    
  } # end of pm$preprocess IF cycle
  
  ## 9. remove temporary files
  unlink(path_tmp, recursive = TRUE)
  
  # Exit
  print_message(
    type = "message",
    date = TRUE,
    "Execution of fidolasen session terminated."
  )
  
}

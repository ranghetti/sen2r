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
#'  * "no" means that L2A are not considered (processing chain
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
#'  all found tiles are entirely used); in online mode, a sample extent is used
#'  as default.
#' @param extent_name (optional) Name of the area set as extent, to be used in
#'  the output file names. Default is to leave it blank. The name is an 
#'  alphanumeric string which cannot contain points nor underscores, and that 
#'  cannot be a five-length string with the same structure of a tile ID
#'  (two numeric and three uppercase character values).
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
#'  in the Surface Classification Map to be masked (see [s2_mask()]
#'  for the accepted values). Default (NA) is not to mask.
#' @param max_mask (optional) Numeric value (range 0 to 100), which represents
#'  the maximum percentage of allowed masked surface (by clouds or any other 
#'  type of mask chosen with argument `mask_type`) for producing outputs. 
#'  Images with a percentage of masked surface greater than `max_mask`%
#'  are not processed (the list of expected output files which have not been 
#'  generated is returned as an attribute, named "skipped"). 
#'  Default value is 80.
#'  Notice that the percentage is computed on non-NA values (if input images 
#'  had previously been clipped and masked using a polygon, the percentage is
#'  computed on the surface included in the masking polygons).
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
#' @param index_datatype (optional) Numeric datatype of the ouptut 
#'  spectral indices (see [s2_calcindices].
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
#' @param thumbnails (optional) Logical: if TRUE (default), a thumbnail is
#'  added for each product created. Thumbnails are JPEG or PNG georeferenced
#'  small images (width or height of 1024 pixels) with default colour palettes
#'  (for more details, see the help window in the GUI). They are placed in
#'  a subdirectory of the products names "thumbnails".
#'  If FALSE, they are not created.
#' @param use_python (optional) Logical: if TRUE (default), the presence of
#'  python in the system is checked before running the function; 
#'  if FALSE, this is skipped. Setting this to FALSE can bge useful on 
#'  systems with problems with python, when [sto)] is intended
#'  to be used only for processing existing SAFE files (python is required
#'  in any case to download SAFE).
#' @return A vector with the paths of the files which were created (excluded
#'  the temporary files); NULL otherwise.
#'
#' @import data.table
#' @importFrom geojsonio geojson_json
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_cast st_read st_combine
#' @importFrom sprawl cast_vect
#' @export


sto <- function(param_list=NULL,
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
                extent_name=NA,
                s2tiles_selected=NA,
                s2orbits_selected=NA,
                list_prods=NA,
                list_indices=NA,
                index_source=NA,
                mask_type=NA,
                max_mask=NA,
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
                index_datatype=NA,
                compression=NA,
                overwrite=NA,
                path_l1c=NA,
                path_l2a=NA,
                path_tiles=NA,
                path_merged=NA,
                path_out=NA,
                path_indices=NA,
                path_subdirs=NA,
                thumbnails=TRUE,
                use_python = TRUE) {
  
  
  ### Preliminary settings ###
  
  # import python modules
  # check that python and the required modules are installed
  if (use_python == TRUE) {
    py <- init_python()
  }
  
  # create tempdir
  dir.create(tempdir(), showWarnings=FALSE)
  
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
                 timewindow=c(Sys.Date() - 90, Sys.Date()),
                 timeperiod="full",
                 extent=NA, # below re-defined as sample extent if online mode
                 extent_name="",
                 s2tiles_selected=NA, # below re-defined for online mode
                 s2orbits_selected=NA, # temporary select all orbits (TODO implement)
                 list_prods=c("BOA"),
                 list_indices=NA,
                 index_source="BOA",
                 mask_type=NA,
                 max_mask=100,
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
                 index_datatype="Int16",
                 compression="DEFLATE",
                 overwrite=FALSE,
                 path_l1c=NA,
                 path_l2a=NA,
                 path_tiles=NA,
                 path_merged=NA,
                 path_out=NA,
                 path_indices=NA,
                 path_subdirs=TRUE,
                 thumbnails=TRUE,
                 pkg_version=packageVersion("salto"))
  
  # If it is the first time that the package is used,
  # ask for opening the GUI to install dependencies
  if (interactive() & !file.exists(system.file("extdata","paths.json", package="salto"))) {
    open_check_gui <- NA
    while(is.na(open_check_gui)) {
      open_check_gui_prompt <- print_message(
        type="waiting",
        "It seems you are running this package for the first time. ",
        "Do you want to install the required dependencies using a GUI? (y/n) "
      )
      open_check_gui <- if (grepl("^[Yy]",open_check_gui_prompt)) {
        TRUE
      } else if (grepl("^[Nn]",open_check_gui_prompt)) {
        FALSE
      } else {
        NA
      }
    }
    if (open_check_gui) {check_dependencies()}
  }
  
  # Starting execution
  print_message(
    type = "message",
    date = TRUE,
    "Starting SALTO execution."
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
  
  # Overwrite parameters passed manually
  # (if some parameters are still missing, copy from default values)
  for (sel_par in names(pm_def)[-match("pkg_version",names(pm_def))]) {
    if (!(length(get(sel_par))==1 & all(is.na(get(sel_par))))) {
      pm[[sel_par]] <- get(sel_par)
    }
    if (length(pm[[sel_par]])==1 & all(is.na(pm[[sel_par]]))) {
      pm[[sel_par]] <- pm_def[[sel_par]]
    }
  }
  
  # # Define sample spatial extent if online mode
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
  # }
  
  # if gui argument was not specified, use default value
  if (is.na(gui)) {
    gui <- if (is.null(param_list)) {TRUE} else {FALSE}
  }
  
  # Check param_list version
  if (is.null(pm$pkg_version)) {
    if (!is.null(pm$fidolasen_version)) {
      pm$pkg_version <- pm$fidolasen_version
    } else {
      pm$pkg_version <- package_version("0.2.0")
    }
  }
  if (packageVersion("salto") > package_version(pm$pkg_version)) {
    if (interactive() & !gui) {
      open_gui <- NA
      while(is.na(open_gui)) {
        open_gui_prompt <- print_message(
          type="waiting",
          "\nThe parameter file was created with an old version of the package:\n",
          "would you like to open a GUI and check that the input parameters are correct? (y/n)\n",
          # "Note that continuing without checking them could lead to errors.\n",
          "Alternatively, press ESC to interrupt and check the parameter file manually.\n"
        )
        open_gui <- if (grepl("^[Yy]",open_gui_prompt)) {
          gui <- TRUE
          TRUE
        } else if (grepl("^[Nn]",open_gui_prompt)) {
          FALSE
        } else {
          NA
        }
      }
    } else {
      print_message(
        type="warning",
        "The parameter file was created with an old version of the package ",
        "(this could lead to errors)."
      )
    }
  }

  ## Open GUI (if required)
  if (gui==TRUE) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Launching GUI..."
    )
    
    pm <- .s2_gui(pm, par_fun = "sto")
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
  
  
  ## Check consistency of parameters
  # TODO work in progress
  pm <- check_param_list(pm, type = "error", correct = TRUE)
  # convert from GeoJSON to sf
  if (is(pm$extent, "character")) {
    pm$extent <- st_read(pm$extent, quiet=TRUE)
  }
  # convert from other managed formats
  if (!all(is.na(pm$extent)) & !is(pm$extent, "sf")) {
    pm$extent <- cast_vect(pm$extent, "sfobject")
  }
  
  # check extent_name
  if (grepl("^[0-9A-Z]{5}$",extent_name)) {
    print_message(
      type = "error",
      "\"extent_name\" cannot have the same structure of a tile ID ",
      "(two numeric and by three uppercase character values)."
    )
  } else if (grepl("[\\.\\_]",extent_name)) {
    print_message(
      type = "error",
      "\"extent_name\" cannot contain points nor underscores."
    )
  }
  
  # internal parameters
  dir.create(path_tmp <- tempfile(pattern="dir"), showWarnings = FALSE) # consider to add as an optional parameter
  paths <- c()
  paths["out"] <- if (!is.na(pm$path_out)) {pm$path_out} else {file.path(path_tmp,"out")}
  paths["indices"] <- if (!is.na(pm$path_indices)) {pm$path_indices} else {file.path(path_tmp,"indices")}
  paths["tiles"] <- if (!is.na(pm$path_tiles)) {pm$path_tiles} else {file.path(path_tmp,"tiles")}
  paths["merged"] <- if (!is.na(pm$path_merged)) {pm$path_merged} else {file.path(path_tmp,"merged")}
  paths["warped"] <- if (is.na(pm$mask_type)) {paths["out"]} else {file.path(path_tmp,"warped")}
  
  # accepted products (update together with the same variables in s2_gui() and in compute_s2_names())
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
  
  # check that output parent directories exist, and create required paths
  parent_paths <- sapply(
    pm[c("path_l1c","path_l2a","path_tiles","path_merged","path_out","path_indices")], 
    function(x){if(is.na(x)){NA}else{dirname(x)}}
  ) %>% unique() %>% na.omit() %>% as.character()
  paths_exist <- sapply(parent_paths, file.exists)
  if (any(!paths_exist)) {
    print_message(
      type="error",
      "The following output ",
      if (sum(!paths_exist)==1) {"directory does "} else {"directories do "},
      "not exist:\n",
      paste(names(paths_exist[!paths_exist]),collapse="\n"),
      ".\nPlease create ",
      if (sum(!paths_exist)==1) {"it "} else {"them "},
      "before continuing."
    )
  }
  sapply(
    pm[c("path_l1c","path_l2a","path_tiles","path_merged","path_out","path_indices")],
    function(x) {if(is.na(x)){NA}else{dir.create(x, recursive = FALSE, showWarnings = FALSE)}}
  )
  
  
  # check output format
  # sel_driver <- py$gdal$GetDriverByName(pm$outformat)
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="salto"))
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
  #   unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(py$gdal$DMD_EXTENSIONS))," ")," "))[1]
  # }
  out_ext <- sel_driver[1,"ext"]
  
  
  #### SAFE Part (find, download, correct)
  for (dummy in TRUE) {
    # dummy cycle, created only to allow "break" from this part
    
    ### Find SAFE and compute the names of required files ###
    
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
        s2_lists[["l1c"]] <- s2_list(spatial_extent = pm$extent,
                                     time_interval = pm$timewindow,
                                     tile = pm$s2tiles_selected,
                                     level = "L1C")
      }
      if ("l2a" %in% pm$s2_levels) {
        # list of SAFE (L1C or/and L2A) needed for required L2A
        s2_lists[["l2a"]] <- s2_list(spatial_extent = pm$extent,
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
        } else if (pm$step_atmcorr %in% c("scihub","no")) {
          list.files(pm$path_l1c, "\\.SAFE$")
        } else if (pm$step_atmcorr=="auto") {
          all_l1c <- list.files(pm$path_l1c, "\\.SAFE$")
          all_l2a <- list.files(pm$path_l2a, "\\.SAFE$")
          c(
            all_l2a,
            all_l1c[
              !gsub(
                "\\_OPER\\_","_USER_",
                gsub(
                  "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_","S2\\1\\_\\2MSIL2A\\_",
                  all_l1c
                )
              ) %in% all_l2a
              ]
          )
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
    
    # if s2_list is empty, exit 
    if (length(s2_list)==0) {
      print_message(
        type = "message",
        date = TRUE,
        "No SAFE products found with the parameters set ",
        "(the searching parameters may be too restrictive, ",
        "or the Copernicus Open Access Hub could be unavailable)."
      )
      break
      # return(invisible(NULL))
    }
    
    names(s2_list) <- gsub("^l[12][ac]\\.","",names(s2_list))
    
    # If s2_list is empty, exit
    if (length(s2_list)==0) {
      print_message(
        type = "message",
        date = TRUE,
        if (pm$online==FALSE) {
          paste0("No SAFE products which match the settings were found locally;\n ",
                 "please download them or set different spatial/temporal extents.\n",
                 "Execution halted.")
        } else {
          paste0("No SAFE products matching the settings were found.")
        }
      )
      break
      # return(invisible(NULL))
    }
    
    
    # getting required metadata
    s2_dt <- lapply(names(s2_list), function(x) {
      unlist(s2_getMetadata(x, info="nameinfo")) %>%
        t() %>%
        as.data.frame(stringsAsFactors=FALSE)
    }) %>%
      rbindlist(fill=TRUE)
    s2_dt[,c("name","url"):=list(names(s2_list),s2_list)]
    s2_dt[,c("sensing_datetime","creation_datetime"):=list(as.POSIXct(sensing_datetime, format="%s"),
                                                           as.POSIXct(creation_datetime, format="%s"))]
    if (is.null(s2_dt$id_tile)) {
      s2_dt$id_tile <- as.character(NA)
    }
    
    s2_dt <- s2_dt[mission %in% toupper(substr(pm$sel_sensor,2,3)),][
      order(-sensing_datetime),]
    if (!anyNA(pm$timewindow)) {
      s2_dt <- s2_dt[as.Date(sensing_datetime) >= pm$timewindow[1] &
                       as.Date(sensing_datetime) <= pm$timewindow[2],]
    }
    # if pm$s2tiles_selected contains NA, do not filter on tiles now;
    # otherwise, filter on tiles but keep also NA not to discard old name products.
    # (products will be filtered later: #filter2)
    if (all(!is.na(pm$s2tiles_selected))) {
      s2_dt <- s2_dt[id_tile %in% c(pm$s2tiles_selected,NA),]
    }
    if (all(!is.na(pm$s2orbits_selected))) {
      s2_dt <- s2_dt[id_orbit %in% pm$s2orbits_selected,]
    }
    # setorder(s2_dt, -sensing_datetime)
    s2_list_l1c <- s2_dt[level=="1C",url] # list of required L1C
    s2_list_l2a <- s2_dt[level=="2A",url] # list of required L2A
    names(s2_list_l1c) <- s2_dt[level=="1C",name]
    names(s2_list_l2a) <- s2_dt[level=="2A",name]
    
    # add expected L2A names (after sen2cor)
    if (pm$step_atmcorr %in% c("auto","scihub")) {
      s2_list_l1c_tocorrect <- if (pm$overwrite_safe==FALSE) {
        s2_list_l1c[
          !gsub(
            "\\_OPER\\_","_USER_",
            gsub(
              "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_","S2\\1\\_\\2MSIL2A\\_",
              names(s2_list_l1c)
            )
          ) %in% names(s2_list_l2a)
          ]
      } else {
        s2_list_l1c
      }
      if (length(s2_list_l1c_tocorrect)>0) {
        s2_list_l2a_tobecorrected <- gsub(
          "\\_OPER\\_","_USER_",
          gsub(
            "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_","S2\\1\\_\\2MSIL2A\\_",
            names(s2_list_l1c_tocorrect)
          )
        )
        names(s2_list_l2a_tobecorrected) <- basename(s2_list_l2a_tobecorrected)
        s2_list_l2a_exp <- c(s2_list_l2a,s2_list_l2a_tobecorrected)
      } else {
        s2_list_l2a_exp <- s2_list_l2a
      }
    } else {
      s2_list_l1c_tocorrect <- character()
      s2_list_l2a_exp <- s2_list_l2a
    }
    
    # If s2_list is empty, exit (second time)
    if (nrow(s2_dt)==0) {
      print_message(
        type = "message",
        date = TRUE,
        if (pm$online==FALSE) {
          paste0("No SAFE products which match the settings were found locally; ",
                 "please download them or set different tile or orbit IDs.\n",
                 "Execution halted."
          )
        } else {
          paste0("No SAFE products matching the settings were found.")
        }
      )
      break
      # return(invisible(NULL))
    }
    
    
    # if preprocess is required, define output names
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
      if (pm$index_source %in% pm$list_prods) {
        sr_masked_ext <- out_ext
        sr_masked_outformat <- pm$outformat
      } else {
        sr_masked_ext <- "vrt"
        sr_masked_outformat <- "VRT"
      }
      
      # Import path of files to ignore, if exists
      # (see comment at #ignorePath)
      ignorelist <- if (is(param_list, "character")) {
        ignorelist_path <- gsub("\\.json$","_ignorelist.txt",param_list)
        if (file.exists(ignorelist_path)) {
          readLines(ignorelist_path)
        } else {
          character()
        }
      } else {
        character()
      }
      
      # compute names for required files (SAFE req)
      print_message(type = "message", date = TRUE, "Computing output names...")
      s2names <- compute_s2_paths(
        pm=pm, 
        s2_list_l1c=s2_list_l1c, s2_list_l2a=s2_list_l2a_exp, 
        paths=paths, 
        list_prods=list_prods, 
        out_ext=out_ext, tiles_ext=tiles_ext, 
        merged_ext=merged_ext, warped_ext=warped_ext, sr_masked_ext=sr_masked_ext,
        ignorelist = ignorelist
      )
      
      # Check if processing is needed
      if (all(sapply(s2names[c(
        "indices_names_new", "out_names_new", "masked_names_new", 
        "warped_names_new", "merged_names_new", "tiles_names_new"
      )], length) == 0)) {
        print_message(
          type = "message",
          date = TRUE,
          "All the required output files already exist; nothing to do.\n ",
          "To reprocess, run sto() with the argument overwrite = TRUE\n ",
          "or specify a different output directory."
        )
        return(invisible(NULL))
      }
      
    } # end of pm$preprocess==TRUE IF cycle (names of required files)
    
    
    ### SAFE processing: download and atmospheric correction ###
    
    ## Generate the list of required SAFE
    if (pm$preprocess==TRUE) {
      # if preprocess is required, only the SAFE necessary to generate new files are considered
      s2_list_l2a_req <- s2_list_l2a[names(s2_list_l2a) %in% basename(nn(s2names$safe_names_l2a_req))]
      safe_names_l2a_reqout <- s2names$safe_names_l2a_req[!basename(nn(s2names$safe_names_l2a_req)) %in% names(s2_list_l2a)]
      safe_names_l1c_tocorrect <- gsub(
        "\\_USER\\_","_OPER_",
        gsub(
          "^S2([AB])\\_((?:USER\\_PRD\\_)?)MSIL2A\\_","S2\\1\\_\\2MSIL1C\\_",
          basename(nn(safe_names_l2a_reqout))
        )
      )
      s2_list_l1c_req <- s2_list_l1c[
        names(s2_list_l1c) %in% c(safe_names_l1c_tocorrect,basename(nn(s2names$safe_names_l1c_req)))
        ]
      s2_dt <- s2_dt[name %in% c(names(s2_list_l1c_req),names(s2_list_l2a_req)),]
      s2_list_l1c <- s2_list_l1c_req
      s2_list_l2a <- s2_list_l2a_req
    }
    
    ## 3. Download required SAFE ##
    # TODO implement ovwerite/skip
    # (now it skips, but analysing each single file)
    
    if (pm$online == TRUE) {
      if (length(s2_list_l2a)>0) {
        
        print_message(
          type = "message",
          date = TRUE,
          "Starting to download the required level-2A SAFE products."
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
          "Starting to download the required level-1C SAFE products."
        )
        
        lapply(pm$s2tiles_selected, function(tile) {
          s2_download(s2_list_l1c,
                      outdir = pm$path_l1c,
                      tile = tile)
        })
        # FIXME this operation can be very long with oldname products but tiled:
        # Sentinel-download.py scans within single xml files and discharges
        # products without the selected tile, but for some reasons this
        # operation can be very time consuming. Find a way to avoid it.
        
      }
    }
    
    # second filter on tiles (#filter2)
    s2_dt$id_tile <- lapply(file.path(ifelse(s2_dt$level=="1C",pm$path_l1c,pm$path_l2a),s2_dt[,name]), function(x) {
      tryCatch(s2_getMetadata(x, "tiles"), error = function(e) {NULL})
    }) %>%
      sapply(paste, collapse = " ") %>% as.character()
    if (all(!is.na(pm$s2tiles_selected)) & nrow(s2_dt)>0) {
      # filter "elegant" using strsplit (fails with empty s2_dt)
      s2_dt <- s2_dt[sapply(strsplit(s2_dt$id_tile," "), function(x){
        any(x %in% pm$s2tiles_selected)
      }),]
      # # filter "ugly" with regexp
      # s2_dt <- s2_dt[grep(paste0("(",paste(pm$s2tiles_selected,collapse=")|("),")"), s2_dt$id_tile),]
    }
    
    # remove duplicates (often for different creation dates, or same sensing dates and different sensing hours)
    # (placed here causes downloading more than the required tiles, but it is the only method to be sure not to exclude
    # some products with the required tiles and include others without them)
    s2_dt <- s2_dt[order(-creation_datetime),]
    s2_dt <- s2_dt[
      !duplicated(
        s2_dt[, list(
          mission,
          level,
          id_orbit,
          id_tile=ifelse(is.na(id_tile),sample(1E5),id_tile), # if id_tile is not specified do not remove duplicates, because different products can rely to different tiles
          as.Date(sensing_datetime)
        )]
      ),
      ]
    
    # redefine s2_list_l1c/l2a
    s2_list_l1c <- s2_dt[level=="1C",url] # list of required L1C
    s2_list_l2a <- s2_dt[level=="2A",url] # list of required L2A
    names(s2_list_l1c) <- s2_dt[level=="1C",name]
    names(s2_list_l2a) <- s2_dt[level=="2A",name]
    
    ## Apply sen2cor
    if (pm$step_atmcorr %in% c("auto","scihub")) {
      
      s2_list_l1c_tocorrect <- if (pm$overwrite_safe==FALSE) {
        s2_list_l1c[
          !gsub(
            "\\_OPER\\_","_USER_",
            gsub(
              "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_","S2\\1\\_\\2MSIL2A\\_",
              names(s2_list_l1c)
            )
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
                                         outdir = pm$path_l2a,
                                         tiles = pm$s2tiles_selected,
                                         parallel = TRUE)
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
    
    # if no processing is required, stop here # TODO see #TODO3 (end of file)
    if (pm$preprocess == FALSE) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Execution of SALTO session terminated."
      )
      
      unlink(path_tmp, recursive = TRUE) # probabily only empty directories will be deleted
      
      return(invisible(
        c(file.path(pm$path_l1c,names(s2_list_l1c)),
          file.path(pm$path_l2a,names(s2_list_l2a)))
      ))
      
    }
    
  } # end of SAFE dummy FOR cycle
  
  if (pm$preprocess == FALSE) {
    return(invisible(NULL))
  }
  
  # update names for output files (after #filter2)
  print_message(type = "message", date = TRUE, "Updating output names...")
  s2names <- compute_s2_paths(
    pm=pm, 
    s2_list_l1c=s2_list_l1c, s2_list_l2a=s2_list_l2a, 
    paths=paths, 
    list_prods=list_prods, 
    out_ext=out_ext, tiles_ext=tiles_ext, 
    merged_ext=merged_ext, warped_ext=warped_ext, sr_masked_ext=sr_masked_ext,
    ignorelist = ignorelist
  )
  
  
  ### GDAL processing: convert SAFE, merge tiles, warp, mask and compute indices ###
  
  ## 4. Convert in vrt ##
  if (length(c(s2names$safe_names_l1c_req,s2names$safe_names_l2a_req))>0) {
    
    
    print_message(
      type = "message",
      date = TRUE,
      "Starting to translate SAFE products in custom format."
    )
    
    dir.create(paths["tiles"], recursive=FALSE, showWarnings=FALSE)
    tiles_l1c_names_out <- tiles_l2a_names_out <- character(0)
    
    if("l1c" %in% pm$s2_levels) {
      list_l1c_prods <- list_prods[list_prods %in% l1c_prods]
      for (sel_prod in s2names$safe_names_l1c_req) {
        tiles_l1c_names_out <- c(
          tiles_l1c_names_out,
          trace_function(
            s2_translate,
            infile = sel_prod,
            outdir = path["tiles"],
            tmpdir = file.path(path_tmp,"tmp_tiles"),
            prod_type = list_l1c_prods,
            format = tiles_outformat,
            tiles = pm$s2tiles_selected,
            res = pm$res_s2,
            subdirs = pm$path_subdirs,
            overwrite = pm$overwrite,
            trace_files = s2names$tiles_names_new
          )
        )
        # s2_translate(infile = sel_prod,
        #              outdir = paths["tiles"],
        #              prod_type = list_l1c_prods,
        #              format = tiles_outformat,
        #              res = pm$res_s2,
        #              subdirs = pm$path_subdirs,
        #              overwrite = pm$overwrite))
        
      }
    }
    if("l2a" %in% pm$s2_levels) {
      list_l2a_prods <- list_prods[list_prods %in% l2a_prods]
      for (sel_prod in s2names$safe_names_l2a_req) {
        tiles_l2a_names_out <- c(
          tiles_l2a_names_out,
          trace_function(
            s2_translate,
            infile = sel_prod,
            tmpdir = file.path(path_tmp,"tmp_tiles"),
            outdir = paths["tiles"],
            prod_type = list_l2a_prods,
            format = tiles_outformat,
            res = pm$res_s2,
            subdirs = pm$path_subdirs,
            overwrite = pm$overwrite,
            trace_files = s2names$tiles_names_new
          )
        )
        # tiles_l2a_names_out <- c(
        #   tiles_l2a_names_out,
        #   s2_translate(infile = sel_prod,
        #                outdir = paths["tiles"],
        #                prod_type = list_l2a_prods,
        #                format = tiles_outformat,
        #                res = pm$res_s2,
        #                subdirs = pm$path_subdirs,
        #                overwrite = pm$overwrite))
      }
    }
    
    tiles_names_out <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_out},
                         if("l2a" %in% pm$s2_levels) {tiles_l2a_names_out})
    # TODO check tiles_names_out - merged_names_new
    
  } # end of s2_translate IF cycle
  
  
  ## 5. Merge by orbit ##
  if (sum(file.exists(nn(s2names$tiles_names_req)))>0) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Starting to merge tiles by orbit."
    )
    
    dir.create(paths["merged"], recursive=FALSE, showWarnings=FALSE)
    merged_names_out <- trace_function(
      s2_merge,
      infiles = s2names$tiles_names_req[file.exists(s2names$tiles_names_req)], # TODO add warning when sum(!file.exists(s2names$merged_names_new))>0
      outdir = paths["merged"],
      subdirs = pm$path_subdirs,
      tmpdir = file.path(path_tmp,"tmp_merged"),
      format = merged_outformat,
      overwrite = pm$overwrite,
      trace_files = s2names$merged_names_new
    )
    # merged_names_out <- s2_merge(s2names$merged_names_new,
    #                              paths["merged"],
    #                              subdirs=pm$path_subdirs,
    #                              format=merged_outformat,
    #                              overwrite=pm$overwrite)
    # TODO check merged_names_out - s2names$merged_names_req
    
  } # end of s2_merge IF cycle
  
  
  if (sum(file.exists(nn(s2names$merged_names_req)))>0) {
    
    ## 6. Clip, rescale, reproject ##
    if (pm$clip_on_extent==TRUE) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to edit geometry (clip, reproject, rescale)."
      )
      
      dir.create(paths["warped"], recursive=FALSE, showWarnings=FALSE)
      # create mask
      s2_mask_extent <- if (anyNA(pm$extent$geometry)) {
        NULL
      } else if (pm$extent_as_mask==TRUE) {
        pm$extent %>% st_combine() # TODO remove this when multiple extents will be allowed
      } else {
        suppressWarnings(st_cast(pm$extent,"LINESTRING")) %>% st_combine() # TODO remove this when multiple extents will be allowed
      }  # TODO add support for multiple extents
      
      if(pm$path_subdirs==TRUE){
        sapply(unique(dirname(s2names$warped_names_reqout)),dir.create,showWarnings=FALSE)
      }
      
      if (any(!file.exists(s2names$warped_names_reqout)) | pm$overwrite==TRUE) {
        # index which is TRUE for SCL products, FALSE for others
        names_merged_req_scl_idx <- fs2nc_getElements(s2names$merged_names_req,format="data.frame")$prod_type=="SCL"
        # here trace_function() is not used, since argument "tr" matches multiple formal arguments.
        # manual cycle is performed.
        tracename_gdalwarp <- start_trace(s2names$warped_names_reqout[!names_merged_req_scl_idx], "gdal_warp")
        trace_gdalwarp <- tryCatch(
          gdal_warp(
            s2names$merged_names_req[!names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
            s2names$warped_names_reqout[!names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
            of = warped_outformat,
            ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
            mask = s2_mask_extent,
            tr = if (!anyNA(pm$res)) {pm$res} else {NULL},
            t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
            r = pm$resampling,
            dstnodata = s2_defNA(
              sapply(s2names$merged_names_req[!names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
                     function(x){fs2nc_getElements(x)$prod_type})
            ),
            co = if (warped_outformat=="GTiff") {paste0("COMPRESS=",pm$compression)},
            overwrite = pm$overwrite
          ), # TODO dstnodata value?
          error = print
        )
        if (is(trace_gdalwarp, "error")) {
          clean_trace(tracename_gdalwarp)
          stop(trace_gdalwarp)
        } else {
          end_trace(tracename_gdalwarp)
        }
        tracename_gdalwarp <- start_trace(s2names$warped_names_reqout[!names_merged_req_scl_idx], "gdal_warp")
        trace_gdalwarp <- tryCatch(
          gdal_warp(
            s2names$merged_names_req[names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
            s2names$warped_names_reqout[names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
            of = pm$outformat, # use physical files to speed up next steps
            ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
            mask = s2_mask_extent,
            tr = if (!anyNA(pm$res)) {pm$res} else {NULL},
            t_srs = if (!is.na(pm$proj)) {pm$proj} else {NULL},
            r = pm$resampling_scl,
            dstnodata = s2_defNA(
              sapply(s2names$merged_names_req[names_merged_req_scl_idx & file.exists(s2names$merged_names_req)],
                     function(x){fs2nc_getElements(x)$prod_type})
            ),
            co = if (pm$outformat=="GTiff") {paste0("COMPRESS=",pm$compression)},
            overwrite = pm$overwrite
          ), # TODO dstnodata value?
          error = print
        )
        if (is(trace_gdalwarp, "error")) {
          clean_trace(tracename_gdalwarp)
          stop(trace_gdalwarp)
        } else {
          end_trace(tracename_gdalwarp)
        }
        # gdal_warp(s2names$merged_names_req[!names_merged_req_scl_idx],
        #           s2names$warped_names_reqout[!names_merged_req_scl_idx],
        #           of = warped_outformat,
        #           ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
        #           mask = s2_mask_extent,
        #           tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
        #           t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
        #           r = pm$resampling,
        #           overwrite = pm$overwrite) # TODO dstnodata value?
        # gdal_warp(s2names$merged_names_req[names_merged_req_scl_idx],
        #           s2names$warped_names_reqout[names_merged_req_scl_idx],
        #           of = pm$outformat, # use physical files to speed up next steps
        #           ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
        #           mask = s2_mask_extent,
        #           tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
        #           t_srs = if (!is.na(pm$proj)) {pm$proj} else {NULL},
        #           r = pm$resampling_scl,
        #           overwrite = pm$overwrite)
      }
      
    } # end of gdal_warp IF clip_on_extent cycle
    
    ## 7. Apply mask ##
    # FIXME understand if this should be done before warping (if so, how to manage virtual/physical files?)
    # masked_names <- file.path(paths["out"],
    #                           if(pm$path_subdirs==TRUE){basename(dirname(warped_names[!names_merged_exp_scl_idx]))}else{""},
    #                           gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names[!names_merged_exp_scl_idx])))
    
    if (!is.na(pm$mask_type)) {
      print_message(
        type = "message",
        date = TRUE,
        "Starting to apply atmospheric masks."
      )
      
      # index which is TRUE for SCL products, FALSE for others
      names_warped_exp_scl_idx <- fs2nc_getElements(s2names$warped_names_exp,format="data.frame")$prod_type=="SCL"
      names_warped_req_scl_idx <- fs2nc_getElements(s2names$warped_names_req,format="data.frame")$prod_type=="SCL"
      # index which is TRUE for products to be atm. masked, FALSE for others
      names_warped_tomask_idx <- if ("SCL" %in% pm$list_prods) {
        names_warped_req_scl_idx>-1
      } else {
        !names_warped_req_scl_idx
      }
      
      # if SR outformat is different (because BOA was not required,
      # bur some indices are) launch s2_mask separately
      masked_names_infiles <- if (pm$clip_on_extent==TRUE) {
        s2names$warped_names_req[names_warped_tomask_idx & file.exists(s2names$warped_names_req)]
      } else {
        s2names$merged_names_req[names_merged_tomask_idx & file.exists(s2names$merged_names_req)]
      }
      masked_names_infiles_sr_idx <- any(!is.na(pm$list_indices)) & 
        !pm$index_source %in% pm$list_prods & 
        sapply(masked_names_infiles, function(x){
          fs2nc_getElements(x)$prod_type==pm$index_source
        })
      
      masked_names_out_nsr <- if (length(masked_names_infiles[!masked_names_infiles_sr_idx])>0) {
        trace_function(
          s2_mask,
          infiles = masked_names_infiles[!masked_names_infiles_sr_idx],
          maskfiles = if (pm$clip_on_extent==TRUE) {
            s2names$warped_names_exp[names_warped_exp_scl_idx]
          } else {
            s2names$merged_names_exp[names_merged_exp_scl_idx]
          },
          mask_type = pm$mask_type,
          max_mask = pm$max_mask,
          outdir = paths["out"],
          tmpdir = file.path(path_tmp,"tmp_masked"),
          format = pm$outformat,
          compress = pm$compression,
          subdirs = pm$path_subdirs,
          overwrite = pm$overwrite,
          parallel = FALSE, # TODO pass as parameter
          trace_files = s2names$out_names_new
        )
      } else {character(0)}
      masked_names_out_sr <- if (length(masked_names_infiles[masked_names_infiles_sr_idx])>0) {
        trace_function(
          s2_mask,
          infiles = masked_names_infiles[masked_names_infiles_sr_idx],
          maskfiles = if (pm$clip_on_extent==TRUE) {
            s2names$warped_names_exp[names_warped_exp_scl_idx]
          } else {
            s2names$merged_names_exp[names_merged_exp_scl_idx]
          },
          mask_type = pm$mask_type,
          max_mask = pm$max_mask,
          outdir = paths["out"],
          tmpdir = file.path(path_tmp,"tmp_masked"),
          format = sr_masked_outformat,
          compress = pm$compression,
          subdirs = pm$path_subdirs,
          overwrite = pm$overwrite,
          parallel = TRUE, # TODO pass as parameter
          trace_files = s2names$out_names_new
        )
      } else {character(0)}
      masked_names_out <- c(masked_names_out_nsr, masked_names_out_sr)
    }
    
  } # end of gdal_warp and s2_mask IF cycle
  
  ## 8. Compute spectral indices ##
  # dir.create(file.path(paths["out"],pm$list_indices), recursive=FALSE, showWarnings = FALSE)
  if (sum(file.exists(nn(s2names$out_names_req)))>0) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Computing required spectral indices."
    )
    
    dir.create(paths["indices"], recursive=FALSE, showWarnings=FALSE)
    indices_names <- trace_function(
      s2_calcindices,
      infiles = s2names$out_names_req[file.exists(s2names$out_names_req)],
      indices = pm$list_indices,
      outdir = paths["indices"],
      subdirs = TRUE,
      source = pm$index_source,
      format = pm$outformat,
      dataType = pm$index_datatype,
      compress = pm$compression,
      overwrite = pm$overwrite,
      trace_files = s2names$indices_names_new
    )
    # indices_names <- s2_calcindices(s2names$out_names_req,
    #                                 indices=pm$list_indices,
    #                                 outdir=paths["indices"],
    #                                 subdirs=TRUE,
    #                                 source=pm$index_source,
    #                                 format=pm$outformat,
    #                                 overwrite=pm$overwrite)
  }
  
  # check file which have been created
  names_out <- unique(unlist(s2names[c(
    "tiles_names_new", "merged_names_new", "warped_names_new",
    "masked_names_new", "out_names_new", "indices_names_new"
  )]))
  # exclude temporary files
  names_out <- names_out[!grepl(path_tmp, names_out, fixed=TRUE)]
  names_out_created <- names_out[file.exists(names_out)]
  
  
  ## 9. create thumbnails
  
  if (thumbnails==TRUE) {
    
    thumb_names_req <- names_out_created
    
    if (length(thumb_names_req)>0) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Generating thumbnails."
      )
      
      # define expected output names
      thumb_names_new <- file.path(
        dirname(thumb_names_req), 
        "thumbnails",
        sapply(
          basename(thumb_names_req), 
          function(x) {
            gsub(
              "\\..+$",
              if (fs2nc_getElements(x)$prod_type %in% c("SCL")) {".png"} else {".jpg"},
              x
            )
          }
        )
      )
      
      thumb_names_out <- trace_function(
        s2_thumbnails,
        infiles = thumb_names_req,
        tmpdir = file.path(path_tmp,"tmp_thumbnails"),
        trace_files = c(thumb_names_new,paste0(thumb_names_new,".aux.xml"))
      )
      
    }
    
  } # end of thumbnails IF cycle
  
  
  ## 10. remove temporary files
  unlink(path_tmp, recursive = TRUE)
  
  # check if some files were not created
  names_missing <- names_out[!file.exists(names_out)]
  
  # Note down the list of non created files (#ignorePath)
  # (sometimes not all the output files are correctly created, i.e. because of
  # old name SAFE products which do not include all the tiles.
  # To prevent to try to create these files every time the function is called 
  # with the same parameter file, if param_list is a path, this list is noted
  # in a hidden file, so to ignore them during next executions. 
  # To try it again, delete the file or set overwrite = TRUE).
  if (length(names_missing)>0) {
    if (is(param_list, "character")) {
      write(names_missing, ignorelist_path, append=TRUE)
    }
    print_message(
      type="warning",
      "Some files were not created ",
      "(probably because the cloud coverage was higher than \"max_mask\"):\n\"",
      paste(names_missing,collapse="\"\n\""),
      if (is(param_list, "character")) {paste0(
        "\"\nThese files will be skipped during next executions ",
        "from the current parameter file (\"",param_list,"\").\n",
        "To try again to build them, remove the file \"",
        ignorelist_path,"\"."
      )} else {paste0(
        "\""
      )}
    )
  }
  
  # Exit
  print_message(
    type = "message",
    date = TRUE,
    "Execution of SALTO session terminated."
  )
  
  # Return output file paths
  return(names_out_created)
  # TODO add also SAFE created files (here and at line #TODO3)
  
}

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
#' @param apihub Path of the text file containing credentials
#'  of scihub account. If NA (default) the default credentials
#'  (username "user", password "user") will be used.
#' @param downloader (optional) Character value corresponding to the executable
#'  which should be used to download SAFE products. It could be one among 
#'  "wget" (default) and "aria2". 
#'  If aria2 is not installed, Wget will be used instead.
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
#' @param max_cloud_safe (optional) Integer number (0-100) containing 
#'  the maximum cloud level of each SAFE to be considered (default: no filter).
#'  It it used to limit the research of SAFE products to "good" images, 
#'  so it is applied only to non-existing archives (existing SAFE are always 
#'  used).
#'  In thish sense, this parameter is different from `max_mask`, which can be
#'  used to set a maximum cloud coverage over output extents.
#'  Notice also that this value is used to filter on the basis of the metadata
#'  "Cloud cover percentage" associated to each SAFE, so it is not based
#'  on the cloud mask defined with the processing options.
#' @param timewindow (optional) Temporal window for querying: Date object
#'  of length 1 (single day) or 2 (time window). Default is NA, meaning that
#'  no filters are used if online = FALSE, and all found images are processed;
#'  if online = TRUE, last 90 days are processed.
#'  Is it possible to pass also integer (or difftime) values, which are 
#'  interpreted as the last n days.
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
#'  the output file names. Default is "sen2r" The name is an 
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
#' @param list_rgb (optional) Character vector with the values of the
#'  RGB images to be produced.
#'  Images are in the form RGBrgbx, when:
#'  - x is B (if source is BOA) or T (is source is TOA);
#'  - r g and b are the the number of the bands to be used respectively
#'      for red, green and blue, in hexadecimal format.
#'      Notice that this is the [actual number name of the bands](
#'      https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial):
#'      so, to use i.e. BOA band 11 (1610nm) use the value "b", even if band 11 is
#'      the 10th band of a BOA product (because band 10 is missing).
#'  Default is no one (NA).
#' @param list_indices (optional) Character vector with the values of the
#'  spectral indices to be computed. Default is no one (NA).
#' @param index_source (optional) Character value: if "BOA" (default), indices
#'  are computed from BOA values; if "TOA", non corrected reflectances
#'  are instead used (be careful to use this setting!).
#' @param rgb_ranges (optional) Range of valid values to be used for RGB products.
#'  Values must be provided in the same scale used within SAFE and BOA/TOA
#'  products (0-10000, corresponding to reflectances * 10000).
#'  If can be a 2-length integer vector (min-max for all the 3 bands) or a 6-length vector or 
#'  3x2 matrix (min red, min green, min blue, max red, max green, max blue).
#'  Default is to use c(0,2500) for bands 2, 3 and 4; c(0,7500) for other bands.
#'  In case `list_rgb` is a vector of length > 1, `rgb_ranges` must be a list 
#'  of the same length (otherwise, the same range vlaues will be used for all the RGB
#'  products).
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
#'  This parameter is different from `max_cloud_safe`, because:
#'  1. it is computed over the selected extent;
#'  2. it is computed basing on the cloud mask defined as above.
#'  Notice that the percentage is computed on non-NA values (if input images 
#'  had previously been clipped and masked using a polygon, the percentage is
#'  computed on the surface included in the masking polygons).
#' @param mask_smooth (optional) Numeric positive value: the smoothing radius
#'  (expressed in unit of measure of the output projection, typically metres)
#'  to be applied to the cloud mask by function [s2_mask]. 
#' @param mask_buffer (optional) Numeric value: the buffering radius
#'  (expressed in unit of measure of the output projection, typically metres)
#'  to be applied to the cloud mask by function [s2_mask]. 
#'  Default value (0) means that no buffer is applied; a positive value causes
#'  an enlargement of the masked area; a negative value cause a reducement.
#' @param clip_on_extent (optional) Logical: if TRUE (default), output products
#'  and indices are clipped to the selected extent (and resampled/reprojected);
#'  if FALSE, the geometry and extension of the tiles is maintained.
#' @param extent_as_mask (optional) Logical: if TRUE, pixel values outside
#'  the `extent` polygon are set to NA; if FALSE (default), all the values
#'  within the bounding box are maintained.
#' @param reference_path (optional) Path of the raster file to be used as a
#'  reference grid. If NA (default), no reference is used.
#' @param res (optional) Numerifc vector of length 2 with the x-y resolution
#'  for output products. Default (NA) means that the resolution
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
#' @param rgb_outformat (optional) Format of the output RGB products (in a
#'  format recognised by GDAL). Default is "GTiff".
#' @param index_datatype (optional) Numeric datatype of the ouptut 
#'  spectral indices (see [s2_calcindices].
#' @param compression (optional) In the case GTiff is chosen as
#'  output format, the compression indicated with this parameter is
#'  used (default is "DEFLATE").
#' @param rgb_compression (optional) In the case GTiff is chosen as
#'  output format for RGB products, the compression indicated 
#'  with this parameter is used (default is "DEFLATE"). 
#'  In the cases GTiff or JPEG are chosen as output format for RGB products,
#'  this parameter can also be a 1-100 integer value, which is interpreted
#'  as the compression level for a JPEG compression.
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
#' @param path_rgb (optional) Path of the directory in RGB products
#'  are searched and/or generated.
#'  If not provided (default), `path_out` is used.
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
#' @param parallel (optional) Logical or integer: if TRUE (default), some
#'  functions ([sen2cor], [s2_mask] and [s2_calcindices] for now) 
#'  are executed using multiple cores in order to speed up the execution.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE, the processing chain is forced to run with a single core
#'  (this can be useful if multiple [sen2r] instances are run in parallel).
#'  This argument can be set only in commandline mode, not using the GUI.
#' @param processing_order (optional) Character string:
#'  - "1" or "by_step" (default) is the legacy mode, in which download and processing 
#'      (merging tiles, warping, masking clouds, etc.) are performed step by step.
#'      If `parallel = TRUE`, parallelisation is applied within each step
#'      (when internal functions can manage it).
#'      This is the only accepted value in case `preprocess = FALSE`.
#'  - "2" or "by_date" is a new experimental mode in which output products are grouped 
#'      by sensing date; download and processing are performed date by date.
#'      Parallelisation is performed like in mode 1 (within each step).
#'      Although this is slightly slower than mode 1, it allow to delete SAFE
#'      products and/or temporary files after each date, so it is suitable
#'      for machines in which disk space is limited.
#'  - "3" or "mixed" is another experimental mode in which download and atmospheric 
#'      correction are performed first; then, processing (merging tiles, 
#'      warping, masking clouds, etc.) is performed step by step.
#'      If `parallel = TRUE`, `sen2cor` is first parallelised (one core per 
#'      SAFE), then one core per group is used for processing steps.
#'      This mode is normally the faster one.
#'  - "4" or "by_groups" is similar to mode 2, but groups are larger (groups of 
#'      n dates, where n is the number of cores which is used).
#'      Parallelisation is performed like in mode 3 within each group.
#'      It allows to take advantages both from speeding-up of mode 3 (although 
#'      slightly slower) and better disk space managing of mode 2.
#' @param use_python (optional) Logical: if TRUE (default), the presence of
#'  python in the system is checked before running the function; 
#'  if FALSE, this is skipped. Setting this to FALSE can bge useful on 
#'  systems with problems with python, when [sen2r()] is intended
#'  to be used only for processing existing SAFE files (python is required
#'  in any case to download SAFE).
#' @param tmpdir (optional) Path where intermediate files will be created.
#'  Default is a temporary directory (unless `outformat = "VRT"`: in this case,
#'  default is a subdirectory named ".vrt" within `path_out`).
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE). `rmtmp` is forced to `FALSE` if `outformat = "VRT"`.
#' @param log (optional) Character string with the path where the package 
#'  messages will be redirected. 
#'  Default (NA) is not to redirect (use standard output).
#'  A two-length character with tho paths (which can also coincide)
#'  can be used to redirect also the output: in this case, the first path 
#'  is the path for messages, the second one for the output.
#' @return A vector with the paths of the files which were created (excluded
#'  the temporary files); NULL otherwise.
#'  The vector includes two attributes: 
#'  - `cloudcovered` with the list of imags not created due to the higher 
#'      percentage of cloud covered pixels;
#'  - `missing` with the list of imags not created due to other reasons.
#'
#' @import data.table
#' @importFrom utils packageVersion
#' @importFrom geojsonio geojson_json
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_cast st_read st_combine st_as_sf st_is_valid
#' @importFrom methods formalArgs
#' @importFrom stats na.omit
#' @export


sen2r <- function(param_list = NULL,
                  gui = NA,
                  preprocess = TRUE,
                  s2_levels = c("l1c","l2a"),
                  sel_sensor = c("s2a","s2b"),
                  online = TRUE,
                  apihub = NA,
                  downloader = "wget",
                  overwrite_safe = FALSE,
                  rm_safe = "no",
                  step_atmcorr = "auto",
                  max_cloud_safe = 100,
                  timewindow = NA,
                  timeperiod = "full",
                  extent = NA, # below re-defined as sample extent if online mode
                  extent_name = "sen2r",
                  s2tiles_selected = NA, # below re-defined for online mode
                  s2orbits_selected = NA, # temporary select all orbits (TODO implement)
                  list_prods = c("BOA"),
                  list_rgb = NA,
                  list_indices = NA,
                  index_source = "BOA",
                  rgb_ranges = NA,
                  mask_type = NA,
                  max_mask = 100,
                  mask_smooth = 0,
                  mask_buffer = 0,
                  clip_on_extent = TRUE,
                  extent_as_mask = FALSE,
                  reference_path = NA,
                  res = NA,
                  res_s2 = "10m",
                  unit = "Meter",
                  proj = NA,
                  resampling = "near",
                  resampling_scl = "near",
                  outformat = "GTiff",
                  rgb_outformat = "GTiff",
                  index_datatype = "Int16",
                  compression = "DEFLATE",
                  rgb_compression = "90",
                  overwrite = FALSE,
                  path_l1c = NA,
                  path_l2a = NA,
                  path_tiles = NA,
                  path_merged = NA,
                  path_out = NA,
                  path_rgb = NA,
                  path_indices = NA,
                  path_subdirs = TRUE,
                  thumbnails = TRUE,
                  parallel = TRUE,
                  processing_order = "by_step",
                  use_python = TRUE,
                  tmpdir = NA,
                  rmtmp = TRUE, 
                  log = NA) {
  
  # sink to external files
  log_output <- log[2]
  log_message <- log[1]
  if (!is.na(log_output)) {
    dir.create(dirname(log_output), showWarnings=FALSE)
    sink(log_output, split = TRUE, type = "output", append = TRUE)
  }
  if (!is.na(log_message)) {
    dir.create(dirname(log_message), showWarnings=FALSE)
    logfile_message = file(log_message, open = "a")
    sink(logfile_message, type="message")
  }
  
  # filter names of passed arguments
  sen2r_args <- formalArgs(sen2r:::.sen2r)
  sen2r_args <- sen2r_args[!sen2r_args %in% c(".log_message",".log_output",".only_list_names")]
  pm_arg_passed <- logical(0)
  for (i in seq_along(sen2r_args)) {
    pm_arg_passed[i] <- !do.call(missing, list(sen2r_args[i]))
  }
  
  # launch the function
  sen2r:::.sen2r(
    param_list = param_list,
    pm_arg_passed = pm_arg_passed,
    gui = gui,
    preprocess = preprocess,
    s2_levels = s2_levels,
    sel_sensor = sel_sensor,
    online = online,
    apihub = apihub,
    downloader = downloader,
    overwrite_safe = overwrite_safe,
    rm_safe = rm_safe,
    step_atmcorr = step_atmcorr,
    max_cloud_safe = max_cloud_safe,
    timewindow = timewindow,
    timeperiod = timeperiod,
    extent = extent,
    extent_name = extent_name,
    s2tiles_selected = s2tiles_selected,
    s2orbits_selected = s2orbits_selected,
    list_prods = list_prods,
    list_rgb = list_rgb,
    list_indices = list_indices,
    index_source = index_source,
    rgb_ranges = rgb_ranges,
    mask_type = mask_type,
    max_mask = max_mask,
    mask_smooth = mask_smooth,
    mask_buffer = mask_buffer,
    clip_on_extent = clip_on_extent,
    extent_as_mask = extent_as_mask,
    reference_path = reference_path,
    res = res,
    res_s2 = res_s2,
    unit = unit,
    proj = proj,
    resampling = resampling,
    resampling_scl = resampling_scl,
    outformat = outformat,
    rgb_outformat = rgb_outformat,
    index_datatype = index_datatype,
    compression = compression,
    rgb_compression = rgb_compression,
    overwrite = overwrite,
    path_l1c = path_l1c,
    path_l2a = path_l2a,
    path_tiles = path_tiles,
    path_merged = path_merged,
    path_out = path_out,
    path_rgb = path_rgb,
    path_indices = path_indices,
    path_subdirs = path_subdirs,
    thumbnails = thumbnails,
    parallel = parallel,
    processing_order = processing_order,
    use_python = use_python,
    tmpdir = tmpdir,
    rmtmp = rmtmp,
    .log_message = log_message, .log_output = log_output,
    .only_list_names = FALSE
  )
  
  # stop sinking
  # n_sink_output <- sink.number("output")
  # while (n_sink_output > 0) {
  #   sink(type = "output")
  #   n_sink_output <- sink.number("output")
  # }
  # n_sink_message <- sink.number("message")
  # while (n_sink > 2) {
  #   sink(type = "message"); close(logfile_message)
  #   n_sink_message <- sink.number("message")
  # }
  if (!is.na(log_output)) {
    sink(type = "output")
  }
  if (!is.na(log_message)) {
    sink(type = "message"); close(logfile_message)
  }
  
}

# Internal function, which is the "real" sen2r() function insider the use of sink
# (this workaround was used in order to manage final sink() in those cases 
# in which return() is used inside the function.)
# TODO: manage also errors (.sen2r inside a trycatch; in case of errors, stop
# passing the error message)
.sen2r <- function(param_list,
                   pm_arg_passed, # TODO workaround ($473), fix
                   gui,
                   preprocess,
                   s2_levels,
                   sel_sensor,
                   online,
                   apihub,
                   downloader,
                   overwrite_safe,
                   rm_safe,
                   step_atmcorr,
                   max_cloud_safe,
                   timewindow,
                   timeperiod,
                   extent,
                   extent_name,
                   s2tiles_selected,
                   s2orbits_selected,
                   list_prods,
                   list_rgb,
                   list_indices,
                   index_source,
                   rgb_ranges,
                   mask_type,
                   max_mask,
                   mask_smooth,
                   mask_buffer,
                   clip_on_extent,
                   extent_as_mask,
                   reference_path,
                   res,
                   res_s2,
                   unit,
                   proj,
                   resampling,
                   resampling_scl,
                   outformat,
                   rgb_outformat,
                   index_datatype,
                   compression,
                   rgb_compression,
                   overwrite,
                   path_l1c,
                   path_l2a,
                   path_tiles,
                   path_merged,
                   path_out,
                   path_rgb,
                   path_indices,
                   path_subdirs,
                   thumbnails,
                   parallel,
                   processing_order,
                   use_python,
                   tmpdir,
                   rmtmp,
                   .log_message = NA, 
                   .log_output = NA,
                   .only_list_names = FALSE) {
  
  # to avoid NOTE on check
  . <- NULL
  
  ### Preliminary settings ###
  
  # If it is the first time that the package is used,
  # ask for opening the GUI to install dependencies
  if (interactive() & !file.exists(system.file("extdata","paths.json", package="sen2r"))) {
    open_check_gui <- NA
    while(is.na(open_check_gui)) {
      open_check_gui_prompt <- print_message(
        type="waiting",
        # "It seems you are running this package for the first time. ",
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
    if (open_check_gui) {
      check_sen2r_deps()
      print_message(
        type="message",
        "Dependencies checked; please restart sen2r() now."
      )
      return(invisible(NULL))
    }
  }
  
  # Starting execution
  print_message(
    type = "message",
    date = TRUE,
    "Starting sen2r execution."
  )
  
  # import python modules
  # check that python and the required modules are installed
  if (use_python == TRUE) {
    py <- init_python()
  }
  
  
  ## 1. Read / import parameters ##
  
  # Read arguments with default values
  pm_def <- formals(sen2r::sen2r) # use "sen2r" instead of ".sen2r" because this one has no defaults
  pm_def <- pm_def[!names(pm_def) %in% c("log")] # remove "log" (argument of sen2r(), not .sen2r())
  # select arguments which are not parameters
  pm_def <- sapply(pm_def[!names(pm_def) %in% c("param_list","gui","use_python","tmpdir","rmtmp")], eval)
  
  # filter names of passed arguments
  sen2r_args <- formalArgs(sen2r:::.sen2r)
  sen2r_args <- sen2r_args[!sen2r_args %in% c(".log_message",".log_output")]
  # FIXME $473 pm_arg_passed computed in the main function (otherwise nothing was missing).
  # This is not elegant, find a better way to do it.
  #   pm_arg_passed <- logical(0)
  #   for (i in seq_along(sen2r_args)) {
  #     pm_arg_passed[i] <- !do.call(missing, list(sen2r_args[i]))
  #   }
  # Read arguments with passed values
  pm_arg <- sapply(sen2r_args[pm_arg_passed], function(x){
    do.call(get, list(x))
  }, simplify=FALSE)
  # select arguments which are not parameters
  pm_arg <- pm_arg[!names(pm_arg) %in% c("param_list","gui","use_python","tmpdir","rmtmp")]
  
  # Import param_list, if provided
  pm_list <- if (is(param_list, "character")) {
    # load json parameter file
    jsonlite::fromJSON(param_list)
    # TODO check package version and parameter names
  } else if (is(param_list, "list")) {
    param_list
    # TODO check parameter names
  } else {
    list("pkg_version" = packageVersion("sen2r"))
  }
  
  # Create the ultimate parameter list (pm):
  # based on pm_def, overwrite first with pm_list and then with pm_arg
  pm <- pm_def
  pm[names(pm_list)] <- pm_list
  pm[names(pm_arg)] <- pm_arg
  
  # if gui argument was not specified, use default value
  if (is.na(gui)) {
    gui <- if (is.null(param_list)) {TRUE} else {FALSE}
  }
  
  # Check param_list version
  if (is.null(pm_list$pkg_version)) {
    if (!is.null(pm_list$fidolasen_version)) {
      pm_list$pkg_version <- pm_list$fidolasen_version
    } else {
      pm_list$pkg_version <- package_version("0.2.0")
    }
  }
  if (packageVersion("sen2r") > package_version(pm_list$pkg_version)) {
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
    
    # Check parameters before opening the GUI
    pm <- check_param_list(pm, type = "error", correct = TRUE)
    
    pm <- .s2_gui(pm, par_fun = "sen2r")
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
  if (is(pm$extent, "character") | is(pm$extent, "geojson")) {
    pm$extent <- st_read(pm$extent, quiet=TRUE)
  } else if (is(pm$extent, "Spatial")) {
    pm$extent <- st_as_sf(pm$extent)
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
  
  # check and manage parallel (workaroud)
  # TODO add parallel to the GUI, and threat as a normal parameter
  if (is.null(pm$parallel)) {pm$parallel <- parallel}
  if (is.null(pm$processing_order)) {pm$processing_order <- processing_order}
  # determine if parallelisation muts be applied to groups or to steps
  if (!pm$processing_order %in% c(
    1,"by_step", 
    2,"by_date", 
    3,"mixed", 
    4,"by_groups"
  )) {
    print_message(
      type = "warning",
      "processing_order = \"",pm$processing_order,"\"not recognised, ",
      "using default \"by_step\"."
    )
    pm$processing_order <- "by_step"
  }
  # here defined:
  # parallel_steps: related to internal parallelisation of functions
  # parallel_groups_A: related to "level 1" (download, sen2cor, processing) parallelisation
  # parallel_groups_B: related to "level 2" (processing) parallelisation of each group_A
  if (pm$parallel == TRUE | is.numeric(pm$parallel)) {
    if (pm$processing_order %in% c(1,"by_step", 2,"by_date")) {
      parallel_groups_A <- FALSE
      parallel_groups_B <- FALSE
      parallel_steps <- pm$parallel
    } else if (pm$processing_order %in% c(3,"mixed", 4,"by_groups")) {
      parallel_groups_A <- FALSE
      parallel_groups_B <- pm$parallel
      parallel_steps <- FALSE
    }
  } else {
    parallel_groups_A <- FALSE
    parallel_groups_B <- FALSE
    parallel_steps <- FALSE
  }
  
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    # if outformat is VRT, set as a subdirectory of path_out
    tmpdir <- if (
      pm$outformat == "VRT" & 
      !all(is.na(pm[c("path_out","path_rgb","path_indices","path_tiles","path_merged")]))
    ) {
      # use path_out if it is not NA, otherwise path_indices, otherwise path_tiles, otherwise path_merged
      main_dir <- unlist(pm[c("path_out","path_rgb","path_indices","path_tiles","path_merged")])[
        !is.na(pm[c("path_out","path_rgb","path_indices","path_tiles","path_merged")])
        ][1]
      dir.create(main_dir, showWarnings=FALSE)
      file.path(main_dir, ".vrt")
    } else {
      tempfile(pattern="sen2r_")
    }
  }
  if (pm$outformat == "VRT") {
    rmtmp <- FALSE # force not to remove intermediate files
  }
  dir.create(tmpdir, showWarnings=FALSE)
  
  
  # internal parameters
  paths <- c()
  paths["out"] <- if (!is.na(pm$path_out)) {pm$path_out} else {file.path(tmpdir,"out")}
  paths["rgb"] <- if (!is.na(pm$path_rgb)) {pm$path_rgb} else {file.path(tmpdir,"rgb")}
  paths["indices"] <- if (!is.na(pm$path_indices)) {pm$path_indices} else {file.path(tmpdir,"indices")}
  paths["tiles"] <- if (!is.na(pm$path_tiles)) {pm$path_tiles} else {file.path(tmpdir,"tiles")}
  paths["merged"] <- if (!is.na(pm$path_merged)) {pm$path_merged} else {file.path(tmpdir,"merged")}
  paths["warped"] <- if (is.na(pm$mask_type)) {paths["out"]} else {file.path(tmpdir,"warped")}
  
  # accepted products (update together with the same variables in s2_gui() and in compute_s2_names())
  l1c_prods <- c("TOA")
  l2a_prods <- c("BOA","SCL","TCI")
  
  # if masking is required, produce also SCL
  list_prods <- if (!is.na(pm$mask_type)) {
    unique(c(pm$list_prods, "SCL"))
  } else {
    pm$list_prods
  }
  # if some RGB are required, compute also TOA or BOA
  if (any(!is.na(pm$list_rgb))) {
    list_prods <- unique(c(
      list_prods, 
      paste0(unique(substr(pm$list_rgb,7,7)),"OA")
    ))
  }
  # if some indices are required, compute also TOA or BOA
  if (any(!is.na(pm$list_indices))) {
    list_prods <- unique(c(list_prods, pm$index_source))
  }
  list_prods <- list_prods[!is.na(list_prods)]
  
  # update s2_levels if processing is TRUE (retrieve from products)
  if (pm$preprocess==TRUE) {
    pm$s2_levels <- c(
      if (any(list_prods %in% l1c_prods)) {"l1c"},
      if (any(list_prods %in% l2a_prods)) {"l2a"}
    )
  }
  
  # check that output parent directories exist, and create required paths
  parent_paths <- sapply(
    pm[c("path_l1c","path_l2a","path_tiles","path_merged","path_out","path_rgb","path_indices")], 
    function(x){if(is.na(nn(x))){NA}else{dirname(x)}}
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
    pm[c("path_l1c","path_l2a","path_tiles","path_merged","path_out","path_rgb","path_indices")],
    function(x) {if(is.na(nn(x))){NA}else{dir.create(x, recursive = FALSE, showWarnings = FALSE)}}
  )
  
  
  # check output format
  # sel_driver <- py$gdal$GetDriverByName(pm$outformat)
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))
  sel_driver <- gdal_formats[gdal_formats$name==pm$outformat,]
  if (is.null(pm$rgb_outformat)) {pm$rgb_outformat <- pm$outformat} # to avoid errors
  if (is.null(pm$rgb_compression)) {pm$rgb_compression <- pm$compression} # to avoid errors
  sel_rgb_driver <- gdal_formats[gdal_formats$name==pm$rgb_outformat,]
  
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
  if (nrow(sel_rgb_driver)==0) {
    print_message(
      type="error",
      "Format \"",pm$rgb_outformat,"\" is not recognised; ",
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
  main_ext <- sel_driver[1,"ext"]
  rgb_ext <- sel_rgb_driver[1,"ext"]
  
  
  #### SAFE Part (find, download, correct)
  # if preprocess is required, define output formats
  if (pm$preprocess == TRUE) {  
    
    ## Define output formats
    if (!anyNA(pm$list_prods)) {
      out_ext <- main_ext
      out_outformat <- pm$outformat
    } else {
      out_ext <- "vrt"
      out_outformat <- "VRT"
    }
    if (!is.na(pm$path_tiles)) {
      tiles_ext <- main_ext
      tiles_outformat <- pm$outformat
    } else {
      tiles_ext <- "vrt"
      tiles_outformat <- "VRT"
    }
    if (!is.na(pm$path_merged)) {
      merged_ext <- main_ext
      merged_outformat <- pm$outformat
    } else {
      merged_ext <- "vrt"
      merged_outformat <- "VRT"
    }
    if (is.na(pm$mask_type)) {
      warped_ext <- out_ext
      warped_outformat <- out_outformat
    } else {
      warped_ext <- "vrt"
      warped_outformat <- "VRT"
    }
    if (pm$index_source %in% pm$list_prods) {
      sr_masked_ext <- main_ext
      sr_masked_outformat <- pm$outformat
    } else {
      sr_masked_ext <- "vrt"
      sr_masked_outformat <- "VRT"
    }
    
    # Import path of files to ignore, if exists
    # (see comment at #ignorePath)
    ignorelist <- if (is(param_list, "character")) {
      ignorelist_path <- gsub("\\.json$","_ignorelist.txt",param_list)
      cloudlist_path <- gsub("\\.json$","_cloudlist.txt",param_list)
      ignorelist0 <- if (file.exists(ignorelist_path)) {
        readLines(ignorelist_path)
      } else {
        character()
      }
      cloudlist0 <- if (file.exists(cloudlist_path)) {
        readLines(cloudlist_path)
      } else {
        character()
      }
      c(ignorelist0, cloudlist0)
    } else {
      character()
    }
    
  }
  
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
                                     time_period = pm$timeperiod,
                                     tile = pm$s2tiles_selected,
                                     level = "L1C",
                                     max_cloud = pm$max_cloud_safe,
                                     apihub = pm$apihub)
      }
      if ("l2a" %in% pm$s2_levels) {
        # list of SAFE (L1C or/and L2A) needed for required L2A
        s2_lists[["l2a"]] <- s2_list(spatial_extent = pm$extent,
                                     time_interval = pm$timewindow,
                                     time_period = pm$timeperiod,
                                     tile = pm$s2tiles_selected,
                                     level = if (pm$step_atmcorr=="auto") {
                                       "auto"
                                     } else if (pm$step_atmcorr=="l2a") {
                                       "L2A"
                                     } else if (pm$step_atmcorr %in% c("scihub","no")) {
                                       "L1C"
                                     },
                                     max_cloud = pm$max_cloud_safe,
                                     apihub = pm$apihub)
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
          list.files(pm$path_1c, "\\.SAFE$")
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
          tryCatch(safe_getMetadata(x, info="nameinfo")$level,
                   error = function(e) NA)
        })
      })
      s2_lists <- lapply(s2_lists, function(l) {l[!is.na(l)]})
      
    }
    s2_list <- unlist(s2_lists)[!duplicated(unlist(lapply(s2_lists, names)))]
    rm(s2_lists)
    
    # If s2_list is empty, exit
    if (length(s2_list)==0) {
      print_message(
        type = "message",
        date = TRUE,
        if (pm$online==FALSE) {
          "No SAFE products which match the settings were found locally."
        } else {
          "No SAFE products matching the settings were found."
        }
      )
    }
    
    if (length(nn(s2_list))>0) {
      names(s2_list) <- gsub("^l[12][ac]\\.","",names(s2_list))
    }
    
    ## Searching for existing local SAFE equivalent to online ones
    # (SAFE with the same metadata, except from baseline and ingestion date)
    
    # getting required metadata
    s2_dt <- lapply(names(s2_list), function(x) {
      unlist(safe_getMetadata(x, info="nameinfo")) %>%
        t() %>%
        as.data.frame(stringsAsFactors=FALSE)
    }) %>%
      rbindlist(fill=TRUE)
    if (nrow(s2_dt)==0) {
      # generate column names for empty dt (to avoid errors)
      s2_dt <- as.data.table(safe_getMetadata(
        "S2A_MSIL2A_20000101T000000_N0200_R001_T01TAA_20000101T000000.SAFE", 
        info="nameinfo"
      ), stringsAsFactors = FALSE)[-1,]
    }
    s2_dt[,c("name","url"):=list(nn(names(s2_list)),nn(s2_list))]
    s2_dt[,c("sensing_datetime","creation_datetime"):=
            list(as.POSIXct(sensing_datetime, format="%s"),
                 as.POSIXct(creation_datetime, format="%s"))]
    
    # list existing products and get metadata
    s2_existing_list <- list.files(unique(c(pm$path_l1c,pm$path_l2a)), "\\.SAFE$")
    if (length(s2_existing_list) > 0 & pm$online == TRUE) {
      s2_isvalid <- sapply(s2_existing_list, safe_isvalid, info="nameinfo")
      s2_existing_list <- s2_existing_list[s2_isvalid]
      s2_existing_dt <- lapply(s2_existing_list, function(x) {
        unlist(safe_getMetadata(x, info="nameinfo")) %>%
          t() %>%
          as.data.frame(stringsAsFactors=FALSE)
      }) %>%
        rbindlist(fill=TRUE)
      s2_existing_dt[,"name":=s2_existing_list]
      s2_existing_dt[,c("sensing_datetime","creation_datetime"):=
                       list(as.POSIXct(sensing_datetime, format="%s"),
                            as.POSIXct(creation_datetime, format="%s"))]
      
      # make a vector with only metadata to be used for the comparison
      s2_meta_pasted <- s2_dt[,list("V1" = paste(
        mission, 
        level, 
        strftime(sensing_datetime,"%y%m%d"), 
        id_orbit, 
        ifelse(version=="compact", id_tile, "oldname")
      ))]$V1
      s2_existing_meta_pasted <- s2_existing_dt[,list("V1" = paste(
        mission, 
        level, 
        strftime(sensing_datetime,"%y%m%d"), 
        id_orbit, 
        ifelse(version=="compact", id_tile, "oldname_existing")
      ))]$V1
      s2_existing_list_touse <- s2_existing_dt[s2_existing_meta_pasted %in% s2_meta_pasted,]$name
      # s2_existing_list_touse cannot contain oldname products, since they are 
      # always checked in case new tiles are required
      
      # replace found SAFE with existing equivalent ones
      s2_dt[
        !is.na(match(s2_meta_pasted, s2_existing_meta_pasted)),
        name := s2_existing_list[na.omit(match(s2_meta_pasted, s2_existing_meta_pasted))]
        ]
      s2_dt[!is.na(match(s2_meta_pasted, s2_existing_meta_pasted)), url:=""]
    }
    
    # removing duplicated products
    # (in case of products with different baseline / ingestion time, keep the most recent one)
    s2_dt <- if (!is.null(s2_dt$id_baseline)) {
      s2_dt[order(-sensing_datetime, -creation_datetime, -id_baseline),]
    } else {
      s2_dt[order(-sensing_datetime, -creation_datetime),]
    }
    s2_dt <- s2_dt[!duplicated(paste(
      prod_type, version, mission, level,
      sensing_datetime, id_orbit, ifelse(version=="compact", id_tile, "oldname")
    )),]
    
    # continue editing metadata
    if (is.null(s2_dt$id_tile)) {
      s2_dt$id_tile <- as.character(NA)
    }
    
    s2_dt <- s2_dt[mission %in% toupper(substr(pm$sel_sensor,2,3)),]
    if (!anyNA(pm$timewindow)) {
      s2_dt <- s2_dt[as.Date(sensing_datetime) >= pm$timewindow[1] &
                       as.Date(sensing_datetime) <= pm$timewindow[2],]
    }
    # if pm$s2tiles_selected contains NA, do not filter on tiles now;
    # otherwise, filter on tiles but keep also NA not to discard old name products.
    # (products will be filtered later: #filter2)
    if (all(!is.na(pm$s2tiles_selected))) {
      s2_dt <- s2_dt[id_tile %in% c(as.character(pm$s2tiles_selected),NA),]
    } else if (all(st_is_valid(pm$extent))) {
      # if no tiles were specified, select only tiles which overlap the extent
      # (this to prevent to use unuseful SAFE in offline mode)
      s2tiles <- s2_tiles()
      s2tiles_sel <- s2tiles[lengths(st_intersects(s2tiles, st_transform(pm$extent, st_crs(s2tiles)))) > 0,]
      s2_dt <- s2_dt[id_tile %in% s2tiles_sel$tile_id,]
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
              "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_([0-9T]{15})\\_N[0-9]{4}\\_",
              "S2\\1\\_\\2MSIL2A\\_\\3\\_NXXXX\\_",
              names(s2_list_l1c)
            )
          ) %in% gsub("\\_N[0-9]{4}\\_", "_NXXXX_", names(s2_list_l2a))
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
    
    # if preprocess is required, define output names
    if (pm$preprocess == TRUE) {  
      
      # Import path of files to ignore, if exists
      # (see comment at #ignorePath)
      ignorelist <- if (is(param_list, "character")) {
        ignorelist_path <- gsub("\\.json$","_ignorelist.txt",param_list)
        cloudlist_path <- gsub("\\.json$","_cloudlist.txt",param_list)
        ignorelist0 <- if (file.exists(ignorelist_path)) {
          readLines(ignorelist_path)
        } else {
          character()
        }
        cloudlist0 <- if (file.exists(cloudlist_path)) {
          readLines(cloudlist_path)
        } else {
          character()
        }
        c(ignorelist0, cloudlist0)
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
        out_ext=out_ext, index_ext=main_ext, tiles_ext=tiles_ext, merged_ext=merged_ext, 
        warped_ext=warped_ext, rgb_ext = rgb_ext, sr_masked_ext=sr_masked_ext,
        force_tiles = TRUE,
        ignorelist = if (exists("ignorelist")) {ignorelist} else {NULL}
      )
      
      # Check if processing is needed
      if (all(sapply(s2names[c(
        "indices_names_new", "rgb_names_new", "out_names_new", "masked_names_new", 
        "warped_names_new", "merged_names_new", "tiles_names_new"
      )], length) == 0)) {
        if (all(sapply(s2names[c(
          "indices_names_exp", "rgb_names_exp", "out_names_exp", "masked_names_exp", 
          "warped_names_exp", "merged_names_exp", "tiles_names_exp"
        )], length) == 0)) {
          print_message(
            type = "message",
            date = TRUE,
            "No output products matching the settings were found; \nplease ",
            if (pm$online == FALSE) {"try in online mode, \nor "},
            "specify less restrictive settings."
          )
        } else {
          print_message(
            type = "message",
            date = TRUE,
            "All the required output files already exist; nothing to do.\n",
            "To reprocess, run sen2r() with the argument overwrite = TRUE,\nor ",
            if (pm$online == FALSE) {"try running sen2r() in online mode, or "},
            "specify a different output directory."
          )
        }
        return(invisible(NULL))
      }
      
    } # end of pm$preprocess==TRUE IF cycle (names of required files)
    
    # if list_sen2r_paths(): stop here
    if (.only_list_names == TRUE) {return(s2names)}
    
    ### SAFE processing: download and atmospheric correction ###
    
    ## Generate the list of required SAFE
    if (pm$preprocess==TRUE) {
      # if preprocess is required, only the SAFE necessary to generate new files are considered
      s2_list_l2a_req <- s2_list_l2a[
        names(s2_list_l2a) %in% basename(nn(s2names$safe_names_l2a_req))
        ]
      safe_names_l2a_reqout <- s2names$safe_names_l2a_req[
        !gsub("\\_N[0-9]{4}\\_", "_NXXXX_", basename(nn(s2names$safe_names_l2a_req))) %in% 
          gsub("\\_N[0-9]{4}\\_", "_NXXXX_", names(s2_list_l2a))
        ]
      safe_names_l1c_tocorrect <- gsub(
        "\\_USER\\_","_OPER_",
        gsub(
          "^S2([AB])\\_((?:USER\\_PRD\\_)?)MSIL2A\\_","S2\\1\\_\\2MSIL1C\\_",
          basename(nn(safe_names_l2a_reqout))
        )
      )
      s2_list_l1c_req <- s2_list_l1c[
        gsub("\\_N[0-9]{4}\\_", "_NXXXX_", names(s2_list_l1c)) %in% 
          gsub("\\_N[0-9]{4}\\_", "_NXXXX_", c(safe_names_l1c_tocorrect,basename(nn(s2names$safe_names_l1c_req))))
        ]
      s2_dt <- s2_dt[name %in% c(names(s2_list_l1c_req),names(s2_list_l2a_req)),]
      s2_list_l1c <- s2_list_l1c_req
      s2_list_l2a <- s2_list_l2a_req
    }
    
    # Compute the maximum number of cores to be used
    max_n_cores <- if (is.numeric(pm$parallel)) {
      as.integer(pm$parallel)
    } else if (pm$parallel == FALSE) {
      1
    } else {
      min(parallel::detectCores()-1, 8) # use at most 8 cores,
    }
    
    if (pm$processing_order %in% c(2,"by_date", 4,"by_groups")) {
      
      # Create processing groups
      sen2r_dates_A <- if (pm$preprocess == TRUE) {
        sort(unique(sen2r_getElements(
          unlist(s2names[grep("_new",names(s2names))]), format = "data.frame"
        )$sensing_date))
      } else {
        s2names <- list()
        sort(unique(as.Date(s2_dt$sensing_datetime)))
      }
      sen2r_groups_A <- if (pm$processing_order %in% c(2,"by_date")) {
        setNames(as.list(sen2r_dates_A), sen2r_dates_A)
      } else if (pm$processing_order %in% c(4,"by_groups")) {
        suppressWarnings(split(
          sen2r_dates_A, 
          seq_len(ceiling(length(sen2r_dates_A)/max_n_cores))
        ))
      }

      # build groups
      s2names_groups_A <- lapply(sen2r_groups_A, function(d) {
        sapply(s2names, function(v) {
          tryCatch(
            v[sen2r_getElements(v, format = "data.frame")$sensing_date %in% d],
            error = function(e) {
              v[sapply(v, function(x) {
                strftime(safe_getMetadata(x, info="nameinfo")$sensing_datetime, "%Y-%m-%d")
              }, USE.NAMES = FALSE) %in% as.character(d)]
            })
        }, simplify = FALSE)
      })
      s2_list_l1c_groups_A <- lapply(sen2r_groups_A, function(d) {
        s2_list_l1c[
          sapply(names(s2_list_l1c), function(s) {
            strftime(safe_getMetadata(s, info="nameinfo")$sensing_datetime, "%Y-%m-%d")
          }) %in% as.character(d)
          ]
      })
      s2_list_l2a_groups_A <- lapply(sen2r_groups_A, function(d) {
        s2_list_l2a[
          sapply(names(s2_list_l2a), function(s) {
            strftime(safe_getMetadata(s, info="nameinfo")$sensing_datetime, "%Y-%m-%d")
          }) %in% as.character(d)
          ]
      })
      s2_dt_groups_A <- lapply(sen2r_groups_A, function(d) {
        s2_dt[
          sapply(names(s2_list_l2a), function(s) {
            strftime(safe_getMetadata(s, info="nameinfo")$sensing_datetime, "%Y-%m-%d")
          }) %in% as.character(d),
          ]
      })
      names(s2names_groups_A) <- names(s2_list_l1c_groups_A) <-
        names(s2_list_l2a_groups_A) <- names(s2_dt_groups_A) <- names(sen2r_groups_A)
      
    } else if (pm$processing_order %in% c(1,"by_step", 3,"mixed")) {
      
      s2names_groups_A <- list(s2names)
      s2_list_l1c_groups_A <- list(s2_list_l1c)
      s2_list_l2a_groups_A <- list(s2_list_l2a)
      s2_dt_groups_A <- list(s2_dt)
      names(s2names_groups_A) <- names(s2_list_l1c_groups_A) <-
        names(s2_list_l2a_groups_A) <- names(s2_dt_groups_A) <- "unique"
    
    }

    # Define n_apihubs, being the number of product groups within each group_A
    apihubs <- read_scihub_login(pm$apihub)
    n_apihubs <- min(nrow(apihubs), length(s2names_groups_A)) # this because
    # it takes no sense to use more apihubs than groups
    if (n_apihubs > 1 & pm$processing_order %in% c(2,"by_date")) {
      pm$apihub <- file.path(tmpdir,paste0("apihub_",seq_len(n_apihubs),".txt"))
      for (i in seq_len(n_apihubs)) {
        write_scihub_login(apihubs[i,1], apihubs[i,2], pm$apihub[i], append = FALSE)
      }
      if (pm$parallel == TRUE | is.numeric(pm$parallel)) {
        parallel_groups_A <- pm$parallel
        parallel_groups_B <- FALSE
        parallel_steps <- FALSE
      }
    }
    # they will be used in case of parallel execution of groups_A

    # Initialise foreach cycle A
    # Compute n_cores
    n_cores_A <- if (is.numeric(parallel_groups_A)) {
      as.integer(parallel_groups_A)
    } else if (parallel_groups_A == FALSE) {
      1
    } else {
      min(max_n_cores, length(s2names_groups_A), n_apihubs)
      # limiting to n_apihubs because it takes no sense to use N cores
      # for groups of n elements (N > n)
    }
    if (n_cores_A<=1) {
      `%DO_A%` <- `%do%`
      n_cores_A <- 1
    } else {
      `%DO_A%` <- `%dopar%`
      cl <- makeCluster(
        n_cores_A, 
        type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
      )
      registerDoParallel(cl)
    }
    # Run processing by group
    # 2 nested cycle:
    # one internal FOREACH with n_cores_A groups,
    # oe external with groups
    # (this is necessary to ensure that 2 groups with the same apihub 
    # cannot be executed in the same time)
    outnames_list_A1 <- foreach(
      sel_group_A = suppressWarnings(split(
        seq_len(length(s2names_groups_A)), 
        seq_len(ceiling(length(s2names_groups_A)/n_apihubs))
      ))
    ) %do% {

    outnames_list_A2 <- foreach(
      sel_s2names = s2names_groups_A[sel_group_A], 
      sel_s2_list_l1c = s2_list_l1c_groups_A[sel_group_A],
      sel_s2_list_l2a = s2_list_l2a_groups_A[sel_group_A],
      sel_s2_dt = s2_dt_groups_A[sel_group_A],
      i_group_A = match(names(s2names_groups_A[sel_group_A]), names(s2names_groups_A)), 
      sel_apihub_path = pm$apihub,
      .packages = c("sf", "sen2r")
    ) %DO_A% {

    # redirect to log files
    if (n_cores_A > 1) {
      if (!is.na(.log_output)) {
        sink(.log_output, split = TRUE, type = "output", append = TRUE)
      }
      if (!is.na(.log_message)) {
        logfile_message = file(.log_message, open = "a")
        sink(logfile_message, type="message")
      }
    }
    
    if (length(s2names_groups_A) > 1) {
      sen2r:::print_message(
        type="message", 
        date=TRUE,
        "Processing group ",i_group_A," of ",length(s2names_groups_A),"..."
      )
    }

    # Define per-group parameters
    tmpdir_groupA <- file.path(tmpdir, basename(tempfile(pattern="group")))
    dir.create(tmpdir_groupA, recursive = FALSE, showWarnings = FALSE)
    paths["L1C"] <- if (!is.na(pm$path_l1c)) {pm$path_l1c} else {file.path(tmpdir_groupA,"SAFE")}
    paths["L2A"] <- if (!is.na(pm$path_l2a)) {pm$path_l2a} else {file.path(tmpdir_groupA,"SAFE")}
    paths["out"] <- if (!is.na(pm$path_out)) {pm$path_out} else {file.path(tmpdir_groupA,"out")}
    paths["rgb"] <- if (!is.na(pm$path_rgb)) {pm$path_rgb} else {file.path(tmpdir_groupA,"rgb")}
    paths["indices"] <- if (!is.na(pm$path_indices)) {pm$path_indices} else {file.path(tmpdir_groupA,"indices")}
    paths["tiles"] <- if (!is.na(pm$path_tiles)) {pm$path_tiles} else {file.path(tmpdir_groupA,"tiles")}
    paths["merged"] <- if (!is.na(pm$path_merged)) {pm$path_merged} else {file.path(tmpdir_groupA,"merged")}
    paths["warped"] <- if (is.na(pm$mask_type)) {paths["out"]} else {file.path(tmpdir_groupA,"warped")}

    ## 3. Download required SAFE ##
    # TODO implement ovwerite/skip
    # (now it skips, but analysing each single file)
    
    if (pm$online == TRUE) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to download the required level-2A SAFE products."
      )
      
      # If all products are compactname, launch a single s2_download() instance
      if (all(sapply(names(sel_s2_list_l2a), function(x) {
        safe_getMetadata(x, "nameinfo")$version
      }) == "compact")) {
        s2_download(
          sel_s2_list_l2a[!names(sel_s2_list_l2a) %in% list.files(paths["L2A"], "\\.SAFE$")],
          outdir = paths["L2A"],
          downloader = pm$downloader,
          apihub = sel_apihub_path
        )
      } else { # otherwise, launch one per tile
        lapply(pm$s2tiles_selected, function(tile) {
          if (length(pm$s2tiles_selected) > 1) {
            print_message(
              type = "message",
              date = TRUE,
              "Cycle ",grep(tile, pm$s2tiles_selected),
              " of ",length(pm$s2tiles_selected),
              " (tile ",tile,
              if (grep(tile, pm$s2tiles_selected)==1) {
                " and products with a compact name)"
              } else {
                " within products with an old long name)"
              }
            )
          }
          s2_download(
            sel_s2_list_l2a, # ask to download all the images, 
            # and not only the non existing ones,
            # because of the cycle on tiles
            # (with compactname products, all the zips are downloaded
            # during the first execution, since argument "tile" is 
            # ignored).
            outdir = paths["L2A"],
            downloader = pm$downloader,
            tile = tile,
            apihub = sel_apihub_path
          )
        })
      }
      
      print_message(
        type = "message", 
        date = TRUE, 
        "Download of level-2A SAFE products terminated."
      )
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to download the required level-1C SAFE products."
      )
      
      # If all products are compactname, launch a single s2_download() instance
      if (all(sapply(names(sel_s2_list_l1c), function(x) {
        safe_getMetadata(x, "nameinfo")$version
      }) == "compact")) {
        s2_download(
          sel_s2_list_l1c[!names(sel_s2_list_l1c) %in% list.files(paths["L1C"], "\\.SAFE$")],
          outdir = paths["L1C"],
          downloader = pm$downloader
        )
      } else { # otherwise, launch one per tile
        lapply(pm$s2tiles_selected, function(tile) {
          if (length(pm$s2tiles_selected) > 1) {
            print_message(
              type = "message",
              date = TRUE,
              "Cycle ",grep(tile, pm$s2tiles_selected),
              " of ",length(pm$s2tiles_selected),
              " (tile ",tile,
              if (grep(tile, pm$s2tiles_selected)==1) {
                " and products with a compact name)"
              } else {
                " within products with an old long name)"
              }
            )
          }
          s2_download(
            sel_s2_list_l1c, # ask to download all the images, 
            # and not only the non existing ones,
            # because of the cycle on tiles
            # (with compactname products, all the zips are downloaded
            # during the first execution, since argument "tile" is 
            # ignored).
            outdir = paths["L1C"],
            downloader = pm$downloader,
            tile = tile
          )
        })
      }
      # FIXME this operation can be very long with oldname products but tiled:
      # Sentinel-download.py scans within single xml files and discharges
      # products without the selected tile, but for some reasons this
      # operation can be very time consuming. Find a way to avoid it.
      
      print_message(
        type = "message", 
        date = TRUE, 
        "Download of level-1C SAFE products terminated."
      )
      
    }
    
    # second filter on tiles (#filter2)
    sel_s2_dt$id_tile <- lapply(
      file.path(paths[ifelse(sel_s2_dt$level=="1C","L1C","L2A")], sel_s2_dt[,name]), 
      function(x) {
        tryCatch(safe_getMetadata(x, "tiles"), error = function(e) {NULL})
      }
    ) %>%
      sapply(paste, collapse = " ") %>% as.character()
    if (all(!is.na(pm$s2tiles_selected)) & nrow(sel_s2_dt)>0) {
      # filter "elegant" using strsplit (fails with empty sel_s2_dt)
      sel_s2_dt <- sel_s2_dt[sapply(strsplit(sel_s2_dt$id_tile," "), function(x){
        any(x %in% pm$s2tiles_selected)
      }),]
      # # filter "ugly" with regexp
      # sel_s2_dt <- sel_s2_dt[grep(paste0("(",paste(pm$s2tiles_selected,collapse=")|("),")"), sel_s2_dt$id_tile),]
    }
    
    # remove duplicates (often for different creation dates, or same sensing dates and different sensing hours)
    # (placed here causes downloading more than the required tiles, but it is the only method to be sure not to exclude
    # some products with the required tiles and include others without them)
    sel_s2_dt <- sel_s2_dt[order(-creation_datetime),]
    sel_s2_dt <- sel_s2_dt[
      !duplicated(
        sel_s2_dt[, list(
          mission,
          level,
          id_orbit,
          id_tile=ifelse(is.na(id_tile),sample(1E5),id_tile), # if id_tile is not specified do not remove duplicates, because different products can rely to different tiles
          as.Date(sensing_datetime)
        )]
      ),
      ]
    
    # redefine sel_s2_list_l1c/l2a
    sel_s2_list_l1c <- sel_s2_dt[level=="1C",url] # list of required L1C
    sel_s2_list_l2a <- sel_s2_dt[level=="2A",url] # list of required L2A
    names(sel_s2_list_l1c) <- sel_s2_dt[level=="1C",name]
    names(sel_s2_list_l2a) <- sel_s2_dt[level=="2A",name]
    
    ## Apply sen2cor
    if (pm$step_atmcorr %in% c("auto","scihub")) {
      
      sel_s2_list_l1c_tocorrect <- if (pm$overwrite_safe==FALSE) {
        sel_s2_list_l1c[
          !gsub(
            "\\_OPER\\_","_USER_",
            gsub(
              "^S2([AB])\\_((?:OPER\\_PRD\\_)?)MSIL1C\\_","S2\\1\\_\\2MSIL2A\\_",
              names(sel_s2_list_l1c)
            )
          ) %in% names(sel_s2_list_l2a)
          ]
      } else {
        sel_s2_list_l1c
      }
      
      if (length(sel_s2_list_l1c_tocorrect)>0) {
        
        if (sum(!file.path(paths["L1C"],names(sel_s2_list_l1c_tocorrect)) %>% file.exists()) > 0) {
          print_message(
            type = "message",
            date = TRUE,
            "Starting to correct level-1C SAFE products with sen2cor. ",
            "This operation could take very long time."
          )
        }
        
        sel_s2_list_l2a_corrected <- sen2cor(
          names(sel_s2_list_l1c_tocorrect),
          l1c_dir = paths["L1C"],
          outdir = paths["L2A"],
          tiles = pm$s2tiles_selected,
          parallel = pm$parallel,
          tmpdir = if (Sys.info()["sysname"] == "Windows") {
            file.path(tmpdir_groupA, "sen2cor")
          } else if (any(attr(mountpoint(tmpdir_groupA), "protocol") %in% c("cifs", "nsfs"))) {
            # if tmpdir is on a SAMBA mountpoint over Linux, 
            # use a tmeporary directory different from the specified one
            NA
          } else {
            file.path(tmpdir_groupA, "sen2cor")
          }, 
          .log_message = .log_message, .log_output = .log_output,
          rmtmp = TRUE # SAFE temporary archives are always deleted
        )
        names(sel_s2_list_l2a_corrected) <- basename(sel_s2_list_l2a_corrected)
        sel_s2_list_l2a <- c(sel_s2_list_l2a,sel_s2_list_l2a_corrected)
      }
      
    }
    
    # delete SAFE, if required
    if (!("l1c" %in% pm$s2_levels) & pm$rm_safe %in% c("all","l1c")) {
      unlink(file.path(paths["L1C"],names(sel_s2_list_l1c_tocorrect)), recursive=TRUE)
    }
    
    # if no processing is required, stop here # TODO see #TODO3 (end of file)
    if (pm$preprocess == FALSE) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Execution of sen2r session terminated."
      )
      
      return(invisible(
        c(file.path(paths["L1C"],names(sel_s2_list_l1c)),
          file.path(paths["L2A"],names(sel_s2_list_l2a)))
      ))
      
    }
    
  
  if (pm$preprocess == FALSE) {
    return(invisible(NULL))
  }
  
  # update names for output files (after #filter2)
  print_message(type = "message", date = TRUE, "Updating output names...")
  sel_s2names <- compute_s2_paths(
    pm=pm, 
    s2_list_l1c = if (exists("sel_s2_list_l1c")) {sel_s2_list_l1c} else {character(0)},
    s2_list_l2a = if (exists("sel_s2_list_l2a")) {sel_s2_list_l2a} else {character(0)},
    paths=paths, 
    list_prods=list_prods, 
    out_ext=out_ext, index_ext=main_ext, tiles_ext=tiles_ext, merged_ext=merged_ext, 
    warped_ext=warped_ext, rgb_ext = rgb_ext, sr_masked_ext=sr_masked_ext,
    force_tiles = FALSE,
    ignorelist = if (exists("ignorelist")) {ignorelist} else {NULL}
  )
  
  
  ### GDAL processing: convert SAFE, merge tiles, warp, mask and compute indices ###
  
  # Create processing groups
  if (pm$processing_order %in% c(2,"by_date", 3,"mixed", 4,"by_groups")) {
    
    # build groups
    sen2r_dates_B <- sort(unique(nn(sen2r_getElements(
      unlist(sel_s2names[grep("_new",names(sel_s2names))]), format = "data.frame"
    )$sensing_date)))
    
    s2names_groups_B <- lapply(sen2r_dates_B, function(d) {
      sapply(sel_s2names, function(v) {
        tryCatch(
          v[sen2r_getElements(v, format = "data.frame")$sensing_date == d],
          error = function(e) {
            v[as.Date(sapply(v, function(x) {
              strftime(safe_getMetadata(x, info="nameinfo")$sensing_datetime, "%Y-%m-%d")
            }, USE.NAMES = FALSE)) == d]
          })
      }, simplify = FALSE)
    })
    names(s2names_groups_B) <- sen2r_dates_B
    
  } else if (pm$processing_order %in% c(1,"by_step")) {
    
    s2names_groups_B <- list(sel_s2names)
    
  }
  
  # Initialise foreach cycle 2
  # Compute n_cores_B
  n_cores_B <- if (is.numeric(parallel_groups_B)) {
    as.integer(parallel_groups_B)
  } else if (parallel_groups_B == FALSE) {
    1
  } else {
    min(max_n_cores, length(s2names_groups_B)) # use at most 8 cores
  }
  if (n_cores_B<=1) {
    `%DO_B%` <- `%do%`
    n_cores_B <- 1
  } else {
    `%DO_B%` <- `%dopar%`
    cl <- makeCluster(
      n_cores_B, 
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )
    registerDoParallel(cl)
  }
  # Run processing by group
  outnames_list_B <- foreach(
    sel_s2names = s2names_groups_B, 
    i_group_B = seq_along(s2names_groups_B), 
    .packages = c("sf", "sen2r")
  ) %DO_B% {

  # redirect to log files
  if (n_cores_B > 1) {
    if (!is.na(.log_output)) {
      sink(.log_output, split = TRUE, type = "output", append = TRUE)
    }
    if (!is.na(.log_message)) {
      logfile_message = file(.log_message, open = "a")
      sink(logfile_message, type="message")
    }
  }
    
  if (length(s2names_groups_B) > 1) {
    sen2r:::print_message(
      type="message", 
      date=TRUE,
      "Processing group ",i_group_B," of ",length(s2names_groups_B),"..."
    )
  }
    
  ## 4. Convert in vrt ##
  if (length(c(sel_s2names$safe_names_l1c_req,sel_s2names$safe_names_l2a_req))>0) {
    
    
    print_message(
      type = "message",
      date = TRUE,
      "Starting to translate SAFE products in custom format."
    )
    
    dir.create(paths["tiles"], recursive=FALSE, showWarnings=FALSE)
    tiles_l1c_names_out <- tiles_l2a_names_out <- character(0)
    
    if("l1c" %in% pm$s2_levels) {
      list_l1c_prods <- list_prods[list_prods %in% l1c_prods]
      for (sel_prod in sel_s2names$safe_names_l1c_req) {
        tiles_l1c_names_out <- c(
          tiles_l1c_names_out,
          trace_function(
            s2_translate,
            infile = sel_prod,
            outdir = paths["tiles"],
            tmpdir = file.path(tmpdir_groupA, "s2_translate_l1c"), 
            rmtmp = FALSE,
            prod_type = list_l1c_prods,
            format = tiles_outformat,
            tiles = pm$s2tiles_selected,
            res = pm$res_s2,
            subdirs = pm$path_subdirs,
            overwrite = pm$overwrite,
            trace_files = sel_s2names$tiles_names_new
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
      for (sel_prod in sel_s2names$safe_names_l2a_req) {
        tiles_l2a_names_out <- c(
          tiles_l2a_names_out,
          trace_function(
            s2_translate,
            infile = sel_prod,
            tmpdir = file.path(tmpdir_groupA, "s2_translate_l2a"), 
            rmtmp = FALSE,
            outdir = paths["tiles"],
            prod_type = list_l2a_prods,
            format = tiles_outformat,
            tiles = pm$s2tiles_selected,
            res = pm$res_s2,
            subdirs = pm$path_subdirs,
            overwrite = pm$overwrite,
            trace_files = sel_s2names$tiles_names_new
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
  if (sum(file.exists(nn(sel_s2names$tiles_names_req)))>0) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Starting to merge tiles by orbit."
    )
    
    dir.create(paths["merged"], recursive=FALSE, showWarnings=FALSE)
    merged_names_out <- trace_function(
      s2_merge,
      infiles = sel_s2names$tiles_names_req[file.exists(sel_s2names$tiles_names_req)], # TODO add warning when sum(!file.exists(sel_s2names$merged_names_new))>0
      outdir = paths["merged"],
      subdirs = pm$path_subdirs,
      tmpdir = file.path(tmpdir_groupA, "s2_merge"), 
      rmtmp = FALSE,
      format = merged_outformat,
      parallel = if (merged_outformat=="VRT") {FALSE} else {parallel_steps},
      overwrite = pm$overwrite,
      .log_message = .log_message, .log_output = .log_output,
      trace_files = sel_s2names$merged_names_new
    )
    # merged_names_out <- s2_merge(sel_s2names$merged_names_new,
    #                              paths["merged"],
    #                              subdirs=pm$path_subdirs,
    #                              format=merged_outformat,
    #                              overwrite=pm$overwrite)
    # TODO check merged_names_out - sel_s2names$merged_names_req
    
  } # end of s2_merge IF cycle
  
  
  if (sum(file.exists(nn(sel_s2names$merged_names_req)))>0) {
    
    ## 6. Clip, rescale, reproject ##
    if (pm$clip_on_extent==TRUE) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Starting to edit geometry (clip, reproject, rescale)."
      )
      
      dir.create(paths["warped"], recursive=FALSE, showWarnings=FALSE)
      # create mask
      s2_mask_extent <- if (is(pm$extent, "vector") && is.na(pm$extent)) {
        NULL
      } else if (anyNA(pm$extent$geometry)) { # FIXME check on telemod tiffs
        NULL
      } else if (pm$extent_as_mask==TRUE) {
        pm$extent %>% st_combine() # TODO remove this when multiple extents will be allowed
      } else {
        suppressWarnings(st_cast(st_cast(pm$extent,"POLYGON"), "LINESTRING")) %>%
          st_combine() # TODO remove this when multiple extents will be allowed
      }  # TODO add support for multiple extents
      
      if(pm$path_subdirs==TRUE){
        sapply(unique(dirname(sel_s2names$warped_names_reqout)),dir.create,showWarnings=FALSE)
      }
      
      if (any(!file.exists(sel_s2names$warped_names_reqout)) | pm$overwrite==TRUE) {
        # index which is TRUE for SCL products, FALSE for others
        names_merged_req_scl_idx <- sen2r_getElements(sel_s2names$merged_names_req,format="data.frame")$prod_type=="SCL"
        # here trace_function() is not used, since argument "tr" matches multiple formal arguments.
        # manual cycle is performed.
        tracename_gdalwarp <- start_trace(sel_s2names$warped_names_reqout[!names_merged_req_scl_idx], "gdal_warp")
        trace_gdalwarp <- tryCatch(
          gdal_warp(
            sel_s2names$merged_names_req[!names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
            sel_s2names$warped_names_reqout[!names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
            of = warped_outformat,
            ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
            mask = s2_mask_extent,
            tr = if (!anyNA(pm$res)) {pm$res} else {NULL},
            t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
            r = pm$resampling,
            dstnodata = s2_defNA(
              sapply(sel_s2names$merged_names_req[!names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
                     function(x){sen2r_getElements(x)$prod_type})
            ),
            co = if (warped_outformat=="GTiff") {paste0("COMPRESS=",pm$compression)},
            overwrite = pm$overwrite,
            tmpdir = file.path(tmpdir_groupA, "gdal_warp"), 
            rmtmp = FALSE
          ), # TODO dstnodata value?
          error = print
        )
        if (is(trace_gdalwarp, "error")) {
          clean_trace(tracename_gdalwarp)
          stop(trace_gdalwarp)
        } else {
          end_trace(tracename_gdalwarp)
        }
        tracename_gdalwarp <- start_trace(sel_s2names$warped_names_reqout[!names_merged_req_scl_idx], "gdal_warp")
        trace_gdalwarp <- tryCatch(
          gdal_warp(
            sel_s2names$merged_names_req[names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
            sel_s2names$warped_names_reqout[names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
            of = out_outformat, # use physical files to speed up next steps
            ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
            mask = s2_mask_extent,
            tr = if (!anyNA(pm$res)) {pm$res} else {NULL},
            t_srs = if (!is.na(pm$proj)) {pm$proj} else {NULL},
            r = pm$resampling_scl,
            dstnodata = s2_defNA(
              sapply(sel_s2names$merged_names_req[names_merged_req_scl_idx & file.exists(sel_s2names$merged_names_req)],
                     function(x){sen2r_getElements(x)$prod_type})
            ),
            co = if (out_outformat=="GTiff") {paste0("COMPRESS=",pm$compression)},
            overwrite = pm$overwrite,
            tmpdir = file.path(tmpdir_groupA, "gdal_warp"), 
            rmtmp = FALSE
          ), # TODO dstnodata value?
          error = print
        )
        if (is(trace_gdalwarp, "error")) {
          clean_trace(tracename_gdalwarp)
          stop(trace_gdalwarp)
        } else {
          end_trace(tracename_gdalwarp)
        }
        # gdal_warp(sel_s2names$merged_names_req[!names_merged_req_scl_idx],
        #           sel_s2names$warped_names_reqout[!names_merged_req_scl_idx],
        #           of = warped_outformat,
        #           ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
        #           mask = s2_mask_extent,
        #           tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
        #           t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
        #           r = pm$resampling,
        #           overwrite = pm$overwrite) # TODO dstnodata value?
        # gdal_warp(sel_s2names$merged_names_req[names_merged_req_scl_idx],
        #           sel_s2names$warped_names_reqout[names_merged_req_scl_idx],
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
    
    if (!is.na(pm$mask_type) & length(sel_s2names$masked_names_new)>0) {
      print_message(
        type = "message",
        date = TRUE,
        "Starting to apply cloud masks."
      )
      
      # index which is TRUE for SCL products, FALSE for others
      names_exp_scl_idx <- sen2r_getElements(
        if (pm$clip_on_extent==TRUE) {
          sel_s2names$warped_names_exp
        } else {
          sel_s2names$merged_names_exp
        },
        format="data.frame"
      )$prod_type=="SCL"
      names_req_scl_idx <- sen2r_getElements(
        if (pm$clip_on_extent==TRUE) {
          sel_s2names$warped_names_req
        } else {
          sel_s2names$merged_names_req
        },
        format="data.frame"
      )$prod_type=="SCL"
      # index which is TRUE for products to be atm. masked, FALSE for others
      names_tomask_idx <- if ("SCL" %in% pm$list_prods) {
        names_req_scl_idx>-1
      } else {
        !names_req_scl_idx
      }
      
      # if SR outformat is different (because BOA was not required,
      # bur some indices are) launch s2_mask separately
      masked_names_infiles <- if (pm$clip_on_extent==TRUE) {
        sel_s2names$warped_names_req[names_tomask_idx & file.exists(sel_s2names$warped_names_req)]
      } else {
        sel_s2names$merged_names_req[names_tomask_idx & file.exists(sel_s2names$merged_names_req)]
      }
      masked_names_infiles_sr_idx <- any(!is.na(pm$list_indices)) & 
        !pm$index_source %in% pm$list_prods & 
        sapply(masked_names_infiles, function(x){
          sen2r_getElements(x)$prod_type==pm$index_source
        })
      
      masked_names_out_nsr <- if (length(masked_names_infiles[!masked_names_infiles_sr_idx])>0) {
        trace_function(
          s2_mask,
          infiles = masked_names_infiles[!masked_names_infiles_sr_idx],
          maskfiles = if (pm$clip_on_extent==TRUE) {
            sel_s2names$warped_names_exp[names_exp_scl_idx]
          } else {
            sel_s2names$merged_names_exp[names_exp_scl_idx]
          },
          smooth = pm$mask_smooth,
          buffer = pm$mask_buffer,
          mask_type = pm$mask_type,
          max_mask = pm$max_mask,
          outdir = paths["out"],
          tmpdir = file.path(tmpdir_groupA, "s2_mask"),
          rmtmp = FALSE,
          format = out_outformat,
          compress = pm$compression,
          subdirs = pm$path_subdirs,
          overwrite = pm$overwrite,
          parallel = parallel_steps,
          .log_message = .log_message, .log_output = .log_output,
          trace_files = sel_s2names$out_names_new
        )
      } else {character(0)}
      masked_names_out_sr <- if (length(masked_names_infiles[masked_names_infiles_sr_idx])>0) {
        trace_function(
          s2_mask,
          infiles = masked_names_infiles[masked_names_infiles_sr_idx],
          maskfiles = if (pm$clip_on_extent==TRUE) {
            sel_s2names$warped_names_exp[names_exp_scl_idx]
          } else {
            sel_s2names$merged_names_exp[names_exp_scl_idx]
          },
          mask_type = pm$mask_type,
          smooth = pm$mask_smooth,
          buffer = pm$mask_buffer,
          max_mask = pm$max_mask,
          outdir = paths["out"],
          tmpdir = file.path(tmpdir_groupA, "s2_mask"), 
          rmtmp = FALSE,
          format = sr_masked_outformat,
          compress = pm$compression,
          subdirs = pm$path_subdirs,
          overwrite = pm$overwrite,
          parallel = parallel_steps,
          .log_message = .log_message, .log_output = .log_output,
          trace_files = sel_s2names$out_names_new
        )
      } else {character(0)}
      masked_names_out <- c(masked_names_out_nsr, masked_names_out_sr)
      masked_names_notcreated <- c(
        attr(masked_names_out_nsr, "toomasked"),
        attr(masked_names_out_sr, "toomasked")
      )
    }
    
  } # end of gdal_warp and s2_mask IF cycle
  
  
  ## 8a. Create RGB products ##
  rgb_names_infiles <- if (pm$clip_on_extent==TRUE) {
    sel_s2names$warped_names_reqforrgb
  } else {
    sel_s2names$merged_names_reqforrgb
  }
  
  if (sum(file.exists(nn(rgb_names_infiles)))>0) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Producing required RGB images."
    )
    
    dir.create(paths["rgb"], recursive=FALSE, showWarnings=FALSE)
    rgb_names <- trace_function(
      s2_rgb,
      infiles = rgb_names_infiles[file.exists(rgb_names_infiles)],
      rgb_bands = lapply(
        strsplit(unique(gsub("^RGB([0-9a-f]{3})[BT]$","\\1",pm$list_rgb)),""), 
        function(x) {strtoi(paste0("0x",x))}
      ),
      scaleRange = pm$rgb_ranges,
      outdir = paths["rgb"],
      subdirs = pm$path_subdirs,
      format = pm$rgb_outformat,
      compress = pm$rgb_compression,
      tmpdir = file.path(tmpdir_groupA, "s2_rgb"),
      rmtmp = FALSE,
      parallel = parallel_steps,
      overwrite = pm$overwrite,
      .log_message = .log_message, .log_output = .log_output,
      trace_files = sel_s2names$rgb_names_new
    )
    
  }
  
  
  ## 8. Compute spectral indices ##
  # dir.create(file.path(paths["out"],pm$list_indices), recursive=FALSE, showWarnings = FALSE)
  if (sum(file.exists(nn(sel_s2names$out_names_req)))>0) {
    
    print_message(
      type = "message",
      date = TRUE,
      "Computing required spectral indices."
    )
    
    dir.create(paths["indices"], recursive=FALSE, showWarnings=FALSE)
    indices_names <- trace_function(
      s2_calcindices,
      infiles = sel_s2names$out_names_req[file.exists(sel_s2names$out_names_req)],
      indices = pm$list_indices,
      outdir = paths["indices"],
      subdirs = pm$path_subdirs,
      tmpdir = file.path(tmpdir_groupA, "s2_calcindices"),
      source = pm$index_source,
      format = pm$outformat,
      dataType = pm$index_datatype,
      compress = pm$compression,
      overwrite = pm$overwrite,
      parallel = parallel_steps,
      .log_message = .log_message, .log_output = .log_output,
      trace_files = sel_s2names$indices_names_new
    )
    
    # indices_names <- s2_calcindices(sel_s2names$out_names_req,
    #                                 indices=pm$list_indices,
    #                                 outdir=paths["indices"],
    #                                 subdirs=TRUE,
    #                                 source=pm$index_source,
    #                                 format=pm$outformat,
    #                                 overwrite=pm$overwrite)
  }
  
  # If some input file is not present due to higher cloud coverage,
  # build the names of the indices / RGB images not created for the same reason
  if (exists("masked_names_notcreated")) {
    if (length(masked_names_notcreated)>0 & length(sel_s2names$out_names_req)>0) {
      indices_names_notcreated <- data.table(
        sen2r_getElements(masked_names_notcreated, format="data.frame")
      )[prod_type == pm$index_source,
        paste0("S2",
               mission,
               level,"_",
               strftime(sensing_date,"%Y%m%d"),"_",
               id_orbit,"_",
               if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
               "<index>_",
               substr(res,1,2),".",
               out_ext)] %>%
        expand.grid(pm$list_indices) %>%
        apply(1,function(x){
          file.path(
            if(pm$path_subdirs==TRUE){x[2]}else{""},
            gsub("<index>",x[2],x[1])
          )
        }) %>%
        file.path(paths["indices"],.) %>%
        gsub(paste0(merged_ext,"$"),out_ext,.)
      # rgb_names_notcreated <- data.table(
      #   sen2r_getElements(masked_names_notcreated, format="data.frame")
      # )[prod_type %in% c("BOA","TOA"),
      #   paste0("S2",
      #          mission,
      #          level,"_",
      #          strftime(sensing_date,"%Y%m%d"),"_",
      #          id_orbit,"_",
      #          if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
      #          "<rgb",substr(prod_type,1,1),">_",
      #          substr(res,1,2),".",
      #          out_ext)] %>%
      #   expand.grid(pm$list_rgb) %>%
      #   apply(1,function(x) {
      #     # consider only if sources (BOA/TOA) are coherent
      #     if (gsub("^.+<rgb([BT])>.+$","\\1",x[1]) == substr(x[2],7,7)) {
      #       file.path(
      #         if(pm$path_subdirs==TRUE){x[2]}else{""},
      #         gsub("<rgb[BT]>",x[2],x[1])
      #       )
      #     } else {
      #       character(0)
      #     }
      #   }) %>%
      #   unlist() %>%
      #   file.path(paths["rgb"],.) %>%
      #   gsub(paste0(merged_ext,"$"),out_ext,.)
    }
  }
  
  # check file which have been created
  names_out <- unique(unlist(sel_s2names[c(
    "tiles_names_new", "merged_names_new", "warped_names_new",
    "masked_names_new", "out_names_new", "rgb_names_new", "indices_names_new"
  )]))
  # exclude temporary files
  names_out <- names_out[!grepl(tmpdir_groupA, names_out, fixed=TRUE)]
  names_out_created <- names_out[file.exists(nn(names_out))]
  
  
  ## 9. create thumbnails
  
  if (pm$thumbnails==TRUE) {
    
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
              if (sen2r_getElements(x)$prod_type %in% c("SCL")) {".png"} else {".jpg"},
              x
            )
          }
        )
      )
      
      thumb_names_out <- trace_function(
        s2_thumbnails,
        infiles = thumb_names_req,
        tmpdir = file.path(tmpdir_groupA, "s2_thumbnails"), 
        rmtmp = FALSE,
        trace_files = c(thumb_names_new,paste0(thumb_names_new,".aux.xml"))
      )
      
    }
    
  } # end of thumbnails IF cycle
  
  # stop sinking
  if (n_cores_B > 1) {
    if (!is.na(.log_output)) {
      sink(type = "output")
    }
    if (!is.na(.log_message)) {
      sink(type = "message"); close(logfile_message)
    }
  }
  
  # check if some files were not created
  list(
    "out" = names_out,
    "out_created" = names_out_created,
    "cloudcovered" = nn(c(
      if(exists("masked_names_notcreated")) {masked_names_notcreated},
      if(exists("indices_names_notcreated")) {indices_names_notcreated}#,
      # if(exists("rgb_names_notcreated")) {rgb_names_notcreated}
    ))
  )
  
  } # end of s2names_groups_B FOREACH cycle
  if (n_cores_B > 1) {
    stopCluster(cl)
  }

  ## 10. remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir_groupA, recursive=TRUE)
  }
  # delete SAFE, if required
  if (pm$rm_safe == "all") {
    unlink(file.path(paths["L1C"],names(sel_s2_list_l1c)), recursive=TRUE)
    unlink(file.path(paths["L2A"],names(sel_s2_list_l2a)), recursive=TRUE)
  } else if (pm$rm_safe == "l1c") {
    unlink(file.path(paths["L1C"],names(sel_s2_list_l1c)), recursive=TRUE)
  }
  
  if (n_cores_A > 1) {
    stopCluster(cl)
  }
  list(
    "out" = as.vector(unlist(lapply(outnames_list_B, function(x){x$out}))),
    "out_created" = as.vector(unlist(lapply(outnames_list_B, function(x){x$out_created}))),
    "cloudcovered" = as.vector(unlist(lapply(outnames_list_B, function(x){x$cloudcovered})))
  )

  } # end of s2names_groups_A FOREACH 2/2 cycle (2 cycles)
  if (pm$preprocess == FALSE | .only_list_names == TRUE) {
    outnames_list_A2
  } else {
    list(
      "out" = as.vector(unlist(lapply(outnames_list_A2, function(x){x$out}))),
      "out_created" = as.vector(unlist(lapply(outnames_list_A2, function(x){x$out_created}))),
      "cloudcovered" = as.vector(unlist(lapply(outnames_list_A2, function(x){x$cloudcovered})))
    )
  }
  
  } # end of s2names_groups_A FOREACH 1/2 cycle (2 cycles)

  if (pm$preprocess == FALSE | .only_list_names == TRUE) {
    return(unlist(outnames_list_A1))
  }
    
  # check if some files were not created
  names_out <- as.vector(unlist(lapply(outnames_list_A1, function(x){x$out})))
  names_out_created <- as.vector(unlist(lapply(outnames_list_A1, function(x){x$out_created})))
  names_cloudcovered <- as.vector(unlist(lapply(outnames_list_A1, function(x){x$cloudcovered})))
  names_missing <- names_out[!file.exists(nn(names_out))]
  names_missing <- names_missing[!names_missing %in% names_cloudcovered]
  names_cloudcovered <- names_cloudcovered[!grepl(tmpdir, names_cloudcovered, fixed=TRUE)]
  
  # Add attributes related to files not created
  attr(names_out_created, "cloudcovered") <- names_cloudcovered
  attr(names_out_created, "missing") <- names_missing
  
  # Note down the list of non created files (#ignorePath)
  # Sometimes not all the output files are correctly created:
  # the main reason is the cloud coverage higher than the maximum allowed
  # value (argument "max_mask"), but also some other unexpected reasons could
  # happen, i.e. because of old name SAFE products which do not include all the tiles.
  # To prevent to try to create these files every time the function is called 
  # with the same parameter file, if param_list is a path, this list is noted
  # in two hidden files ( one per file not created because of cloud coverage,
  # one other for all the other reasons) so to ignore them during next executions. 
  # To try it again, delete the files or set overwrite = TRUE).
  if (length(names_missing)>0) {
    ignorelist_path <- gsub("\\.json$","_ignorelist.txt",param_list)
    if (is(param_list, "character")) {
      write(names_missing, ignorelist_path, append=TRUE)
    }
    print_message(
      type="warning",
      "Some files were not created:\n\"",
      paste(names_missing,collapse="\"\n\""),"\"",
      if (is(param_list, "character")) {paste0(
        "\"\nThese files will be skipped during next executions ",
        "from the current parameter file (\"",param_list,"\").\n",
        "To try again to build them, remove the file \"",
        ignorelist_path,"\"."
      )}
    )
  }
  if (length(names_cloudcovered)>0) {
    cloudlist_path <- gsub("\\.json$","_cloudlist.txt",param_list)
    if (is(param_list, "character")) {
      write(names_cloudcovered, cloudlist_path, append=TRUE)
    }
    print_message(
      type="message",
      "Some files were not created ",
      "because the cloud coverage was higher than \"max_mask\":\n\"",
      paste(names_cloudcovered,collapse="\"\n\""),"\"",
      if (is(param_list, "character")) {paste0(
        "\"\nThe list of these files was written in a hidden file ",
        "(\"",cloudlist_path,"\"), ",
        "so to be skipped during next executions."
      )}
    )
  }
  
  # Exit
  print_message(
    type = "message",
    date = TRUE,
    "Execution of sen2r session terminated."
  )
  
  # Return output file paths
  return(names_out_created)
  # TODO add also SAFE created files (here and at line #TODO3)
  
}

#' @description `list_sen2r_paths` is a wrapper of [sen2r], which runs
#'  [sen2r] until [compute_s2_paths] is called, returning the same list.
#'  It is a simple way to call [compute_s2_paths] with the same arguments
#'  if [sen2r].
#' @param param_list `sen2r` argument (refer to [sen2r] documentation).
#' @param gui `sen2r` argument (refer to [sen2r] documentation).
#' @param preprocess `sen2r` argument (refer to [sen2r] documentation).
#' @param s2_levels `sen2r` argument (refer to [sen2r] documentation).
#' @param sel_sensor `sen2r` argument (refer to [sen2r] documentation).
#' @param online `sen2r` argument (refer to [sen2r] documentation).
#' @param apihub `sen2r` argument (refer to [sen2r] documentation).
#' @param downloader `sen2r` argument (refer to [sen2r] documentation).
#' @param overwrite_safe `sen2r` argument (refer to [sen2r] documentation).
#' @param rm_safe `sen2r` argument (refer to [sen2r] documentation).
#' @param step_atmcorr `sen2r` argument (refer to [sen2r] documentation).
#' @param max_cloud_safe `sen2r` argument (refer to [sen2r] documentation).
#' @param timewindow `sen2r` argument (refer to [sen2r] documentation).
#' @param timeperiod `sen2r` argument (refer to [sen2r] documentation).
#' @param extent `sen2r` argument (refer to [sen2r] documentation).
#' @param extent_name `sen2r` argument (refer to [sen2r] documentation).
#' @param s2tiles_selected `sen2r` argument (refer to [sen2r] documentation).
#' @param s2orbits_selected `sen2r` argument (refer to [sen2r] documentation).
#' @param list_rgb `sen2r` argument (refer to [sen2r] documentation).
#' @param list_indices `sen2r` argument (refer to [sen2r] documentation).
#' @param index_source `sen2r` argument (refer to [sen2r] documentation).
#' @param rgb_ranges `sen2r` argument (refer to [sen2r] documentation).
#' @param mask_type `sen2r` argument (refer to [sen2r] documentation).
#' @param max_mask `sen2r` argument (refer to [sen2r] documentation).
#' @param mask_smooth `sen2r` argument (refer to [sen2r] documentation).
#' @param mask_buffer `sen2r` argument (refer to [sen2r] documentation).
#' @param clip_on_extent `sen2r` argument (refer to [sen2r] documentation).
#' @param extent_as_mask `sen2r` argument (refer to [sen2r] documentation).
#' @param reference_path `sen2r` argument (refer to [sen2r] documentation).
#' @param res `sen2r` argument (refer to [sen2r] documentation).
#' @param res_s2 `sen2r` argument (refer to [sen2r] documentation).
#' @param unit `sen2r` argument (refer to [sen2r] documentation).
#' @param proj `sen2r` argument (refer to [sen2r] documentation).
#' @param resampling `sen2r` argument (refer to [sen2r] documentation).
#' @param resampling_scl `sen2r` argument (refer to [sen2r] documentation).
#' @param outformat `sen2r` argument (refer to [sen2r] documentation).
#' @param index_datatype `sen2r` argument (refer to [sen2r] documentation).
#' @param compression `sen2r` argument (refer to [sen2r] documentation).
#' @param overwrite `sen2r` argument (refer to [sen2r] documentation).
#' @param path_l1c `sen2r` argument (refer to [sen2r] documentation).
#' @param path_l2a `sen2r` argument (refer to [sen2r] documentation).
#' @param path_tiles `sen2r` argument (refer to [sen2r] documentation).
#' @param path_merged `sen2r` argument (refer to [sen2r] documentation).
#' @param path_out `sen2r` argument (refer to [sen2r] documentation).
#' @param path_indices `sen2r` argument (refer to [sen2r] documentation).
#' @param path_subdirs `sen2r` argument (refer to [sen2r] documentation).
#' @param thumbnails `sen2r` argument (refer to [sen2r] documentation).
#' @param parallel `sen2r` argument (refer to [sen2r] documentation).
#' @param processing_order `sen2r` argument (refer to [sen2r] documentation).
#' @param use_python `sen2r` argument (refer to [sen2r] documentation).
#' @param tmpdir `sen2r` argument (refer to [sen2r] documentation).
#' @param rmtmp `sen2r` argument (refer to [sen2r] documentation).
#' @param log `sen2r` argument (refer to [sen2r] documentation).
#' @rdname compute_s2_paths

list_sen2r_paths <- function(param_list = NULL,
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
                             extent = NA,
                             extent_name = "sen2r",
                             s2tiles_selected = NA,
                             s2orbits_selected = NA,
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
                             index_datatype = "Int16",
                             compression = "DEFLATE",
                             overwrite = FALSE,
                             path_l1c = NA,
                             path_l2a = NA,
                             path_tiles = NA,
                             path_merged = NA,
                             path_out = NA,
                             path_indices = NA,
                             path_subdirs = TRUE,
                             thumbnails = TRUE,
                             parallel = TRUE,
                             processing_order = "by_step",
                             use_python = TRUE,
                             tmpdir = NA,
                             rmtmp = TRUE, 
                             log = NA) {
  
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
    index_datatype = index_datatype,
    compression = compression,
    overwrite = overwrite,
    path_l1c = path_l1c,
    path_l2a = path_l2a,
    path_tiles = path_tiles,
    path_merged = path_merged,
    path_out = path_out,
    path_indices = path_indices,
    path_subdirs = path_subdirs,
    thumbnails = thumbnails,
    parallel = parallel,
    processing_order = processing_order,
    use_python = use_python,
    tmpdir = tmpdir,
    rmtmp = rmtmp,
    .log_message = NA, .log_output = NA,
    .only_list_names = TRUE
  )
  
}

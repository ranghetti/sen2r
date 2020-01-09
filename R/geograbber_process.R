#' @title Preprocess (clip, reproject, reshape, mask) SAFE products
#' @description The function is a simplified wrapper of [sen2r()]
#'  to preprocess existing SAFE products. No online research is performed,
#'  neither sen2cor is applied to existing L1C products.
#'  Output products are GeoTIFF files clipped to a desired 
#' @param path_safe Path of the directory in which SAFE
#'  products are searched.
#' @param path_out Path of the directory in which Sentinel-2
#'  output products are generated.
#' @param bbox (optional) Four-length numeric vector with the bounding box 
#'  of the output products (`xmin`, `ymin`, `xmax`, `ymax`), in geographic coordinates.
#' @param list_prods (optional) Character vector with the values of the
#'  products to be processed (accepted values: `"TOA"`, `"BOA"`, `"SCL"`,
#'  `"TCI"`). Default is `"BOA"`.
#' @param res (optional) Numeric vector of length 2 with the x-y resolution
#'  for output products. NA (default) means that the resolution
#'  is kept as native.
#' @param ... Other additional [sen2r] arguments:
#'  `extent_name`, `list_indices`, `index_source`, `mask_type`, `max_mask`, 
#'  `mask_smooth`, `mask_buffer`, `clip_on_extent`, `extent_as_mask`, 
#'  `reference_path`, `res_s2`, `unit`, `proj`, `resampling`, `resampling_scl`, 
#'  `outformat`, `index_datatype`, `compression`, `overwrite`, `path_tiles`, 
#'  `path_merged`, `path_indices`, `path_subdirs`, `thumbnails`, 
#'  `parallel`, `tmpdir`, `rmtmp`.
#'  See [sen2r] documentation for details.
#' @importFrom geojsonio geojson_json
#' @importFrom methods as is
#' @importFrom sf st_bbox st_as_sfc

geograbber_process <- function(path_safe,
                               path_out,
                               bbox = NA,
                               list_prods = c("BOA"),
                               res = NA,
                               ...
) {
  
  # Check bbox and generate extent
  extent <- if (all(is.na(bbox))) {
    NA
  } else {
    if (length(bbox)!=4 | !is(bbox, "numeric")) {
      print_message(
        type = "error",
        "Argument bbox is not valid: check that it is a numeric 4-length ",
        "vector (or leave it as NA)."
      )
    }
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    geojson_json(st_as_sfc(st_bbox(bbox, crs = 4326)))
  }
  
  # Launch processing
  sen2r(
    # parameters passed by this function
    extent = extent,
    list_prods = list_prods,
    res = res,
    path_l1c = path_safe,
    path_l2a = path_safe,
    path_out = path_out,
    # fixed parameters (related to search an d manage SAFE)
    param_list = NULL,
    gui = FALSE,
    preprocess = TRUE,
    s2_levels = NA,
    sel_sensor = c("s2a","s2b"),
    online = FALSE,
    overwrite_safe = FALSE,
    rm_safe = FALSE,
    step_atmcorr = "no",
    timewindow = NA,
    timeperiod = "full",
    s2tiles_selected = NA,
    s2orbits_selected = NA,
    use_python = FALSE,
    # other additional parameters
    ...
  )
  
}

#' @title Preprocess (clip, reproject, reshape, mask) SAFE products
#' @description The function is a simplified wrapper of [fidolasen_s2()]
#'  to preprocess existing SAFE products. No online research is performed,
#'  neither sen2cor is applied to existing L1C products.
#'  Output products are GeoTIFF files clipped to a desired 
#' @param path_safe Path of the directory in which SAFE
#'  products are searched.
#' @param path_out Path of the directory in which Sentinel-2
#'  output products are generated.
#' @param bbox (optional) Four-length numeric vector with the bounding box 
#'  of the output products (xmin, ymin, xmax, ymax).
#' @param list_prods (optional) Character vector with the values of the
#'  products to be processed (accepted values: "TOA", "BOA", "SCL",
#'  "TCI"). Default is "BOA".
#' @param mask_type (optional) Character value which determines the categories
#'  in the Srface Classification Map to be masked (see [s2_mask()]
#'  for the accepted values). Default (NA) is not to mask.
#' @param res (optional) Numerifc vector of length 2 with the x-y resolution
#'  for output products. NA (default) means that the resolution
#'  is keeped as native.
#' @param proj (optional) Character string with the pro4string of the output
#'  resolution. default value (NA) means not to reproject.
#' @param resampling (optional) Resampling method (one of the values supported
#'  by `gdal_translate`: "near" (default), "bilinear", "cubic",
#'  "cubicspline", "lanczos", "average" or "mode").
#' @param overwrite (optional) Logical value: should existing output
#'  files be overwritten? (default: FALSE).
#' @importFrom geojsonio geojson_json
#' @importFrom sprawl check_proj4string get_extent
#' @importFrom methods as is

geograbber_process <- function(path_safe,
                               path_out,
                               bbox       = NA,
                               list_prods = c("BOA"),
                               mask_type  = NA,
                               res        = NA,
                               proj       = NA,
                               resampling = "near",
                               overwrite  = FALSE
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
    geojson_json(
      as(
        get_extent(
          matrix(bbox,nrow=2), 
          check_proj4string(4326)
        ),
        "sfc_POLYGON"
      )
    )
  }

  # Launch processing
  fidolasen_s2(
    # parameters passed by this function
    extent            = extent,
    list_prods        = list_prods,
    mask_type         = mask_type,
    res               = res,
    proj              = proj,
    resampling        = resampling,
    overwrite         = overwrite,
    path_l1c          = path_safe,
    path_l2a          = path_safe,
    path_out          = path_out,
    # fixed parameters
    param_list        = NULL,
    gui               = FALSE,
    preprocess        = TRUE,
    s2_levels         = NA,
    sel_sensor        = c("s2a","s2b"),
    online            = FALSE,
    overwrite_safe    = FALSE,
    rm_safe           = FALSE,
    step_atmcorr      = "no",
    timewindow        = NA,
    timeperiod        = "full",
    s2tiles_selected  = NA,
    s2orbits_selected = NA,
    list_indices      = NA,
    index_source      = NA,
    clip_on_extent    = TRUE,
    extent_as_mask    = FALSE,
    reference_path    = NA,
    res_s2            = "10m",
    unit              = "Meter",
    resampling_scl    = if(resampling=="near"){"near"}else{"mode"},
    outformat         = "GTiff",
    compression       = "DEFLATE",
    path_tiles        = NA,
    path_merged       = NA,
    path_indices      = NA,
    path_subdirs      = TRUE,
    use_python        = FALSE
  )
  
}



#' @title Build an example JSON parameter file
#' @description Function used to write JSON parameter file.
#'  A function is provided instead than a json file to ensure
#'  directories to match the user folder tree.
#' @param json_path Path of the output file. Default is to save it on a
#'  temporary file, whose path is returned.
#' @param overwrite Logical value: should existing output file be
#'  overwritten? (default: TRUE)
#' @return The path of the created file.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom jsonlite toJSON
#' @examples
#' sen2r:::build_example_param_file()

build_example_param_file <- function(
  json_path = tempfile(fileext = "_sen2r_params.json"),
  overwrite = TRUE
) {
  
  # Check json_path
  if (json_path == "") {json_path <- NULL}
  if (!attr(path_check(dirname(json_path)), "isvalid")) {
    print_message(
      type = "error",
      "Path '",dirname(json_path),"' is missing or not writable."
    )
  }
  
  # Delete existing file
  if (file.exists(json_path)) {
    if (overwrite == TRUE) {
      file.remove(json_path)
    } else {
      print_message(
        type = "error",
        "File '",json_path,"' already exists."
      )
    }
  }
  
  writeLines(
    jsonlite::toJSON(
      list(
        "preprocess" = TRUE,
        "s2_levels" = c("l1c", "l2a"),
        "sel_sensor" = c("s2a", "s2b"),
        "online" = TRUE,
        "downloader" = "builtin",
        "overwrite_safe" = FALSE,
        "rm_safe" = "no",
        "max_cloud_safe" =100,
        "step_atmcorr" = "l2a",
        "timewindow" = c("2017-07-03", "2017-07-03"),
        "timeperiod" = "full",
        "extent" = system.file("data/vector/barbellino.geojson", package = "sen2r"),
        "s2tiles_selected" = NA,
        "s2orbits_selected" = NA,
        "list_prods" = c("TOA", "BOA", "SCL"),
        "list_indices" = c("MSAVI2", "NDVI"),
        "list_rgb" = c("RGB432B", "RGB432T", "RGB843B"),
        "rgb_ranges" = list(
          c(0, 2500),
          c(0, 2500),
          matrix(c(0, 0, 0, 7500, 2500, 2500), ncol = 2)
        ),
        "index_source" = "BOA",
        "mask_type" = NA,
        "max_mask" = 80,
        "mask_smooth" = 0,
        "mask_buffer" = 0,
        "clip_on_extent" = TRUE,
        "extent_as_mask" = FALSE,
        "extent_name" = "sen2r",
        "reference_path" = NA,
        "res" = NA,
        "res_s2" ="10m",
        "unit" = "Meter",
        "proj"= NA,
        "resampling" = "near",
        "resampling_scl" = "near",
        "outformat" = "GTiff",
        "rgb_outformat" = "GTiff",
        "index_datatype" = "Int16",
        "compression" = "DEFLATE",
        "rgb_compression" = "DEFLATE",
        "overwrite" = FALSE,
        "path_l1c" = file.path(dirname(attr(load_binpaths(), "path")), "safe"),
        "path_l2a" = file.path(dirname(attr(load_binpaths(), "path")), "safe"),
        "path_tiles"= NA,
        "path_merged"= NA,
        "path_out" = tempfile(pattern = "sen2r_out_"),
        "path_rgb" = "",
        "path_indices" = "",
        "path_subdirs"= TRUE,
        "thumbnails" = TRUE,
        "log"= NA,
        "parallel" = FALSE,
        "processing_order" = "by_groups",
        "pkg_version" = as.character(packageVersion("sen2r"))
      ),
      pretty = TRUE
    ),
    json_path
  )
  
  return(json_path)
  
}

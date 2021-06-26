cat("\n---- Test build_example_param_file() ----")

testthat::test_that(
  "Build an example parameter file", {
    json_path <- build_example_param_file()
    testthat::expect_true(file.exists(json_path))
    list_param <- jsonlite::fromJSON(json_path)
    testthat::expect_equal(
      names(list_param),
      c("preprocess", "s2_levels", "sel_sensor", "online", "server", "order_lta", 
        "downloader", "overwrite_safe", "rm_safe", "max_cloud_safe",
        "step_atmcorr", "sen2cor_use_dem", "sen2cor_gipp", 
        "timewindow", "timeperiod", "extent", 
        "s2tiles_selected", "s2orbits_selected", "list_prods", "list_indices", 
        "list_rgb",  "rgb_ranges", "index_source", "mask_type", "max_mask",
        "mask_smooth", "mask_buffer", "clip_on_extent", "extent_as_mask", 
        "extent_name", "reference_path", "res", "res_s2", "unit", "proj",
        "resampling", "resampling_scl", "outformat", "rgb_outformat",
        "index_datatype", "compression", "rgb_compression", "overwrite", 
        "path_l1c", "path_l2a", "path_tiles", "path_merged", "path_out", 
        "path_rgb", "path_indices", "path_subdirs", "thumbnails", "log", 
        "parallel", "processing_order", "pkg_version"
      )
    )
  }
)

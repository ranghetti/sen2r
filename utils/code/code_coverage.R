# Code for manual package coverage
Sys.setenv("NOT_CRAN"="true")
covr::package_coverage(
  line_exclusions = c(
    "R/s2_gui.R",
    "R/install_gui_deps.R",
    "R/add_rgb_image.R",
    "R/give_write_permission.R",
    "R/check_sen2r_deps.R",
    "R/check_gui_deps.R",
    "R/create_indices_db.R", 
    "R/helpers_extent.R",
    "R/gdal_formats_db.R",
    "R/path_check.R", 
    "R/list_sen2r_paths.R",
    "R/check_param_list.R",
    "R/convert_datatype.R",
    "R/dontuse.R"
  )
)

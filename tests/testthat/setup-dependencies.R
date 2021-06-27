if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  
  # Install or check internal dependencies
  # install_sen2cor()
  # if (Sys.info()["sysname"] == "Windows") {sen2r::install_aria2()}
  if (Sys.info()["sysname"] == "Linux") {sen2r::check_gdal()}
  check_gcloud(force = TRUE, abort = FALSE)
  
  s2tiles <- s2_tiles()
  
}

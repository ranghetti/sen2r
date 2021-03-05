# Install or check internal dependencies
install_sen2cor()
if (Sys.info()["sysname"] == "Windows") {sen2r::install_aria2()}
sen2r::check_gdal()

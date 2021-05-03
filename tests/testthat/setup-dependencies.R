# Install or check internal dependencies
# install_sen2cor()
# if (Sys.info()["sysname"] == "Windows") {sen2r::install_aria2()}
if (Sys.info()["sysname"] == "Linux") {sen2r::check_gdal()}
s2tiles <- s2_tiles()

# Return "relevant" dependency versions
message("Version of package 'sf': ",packageVersion("sf"))
message("Version of package 's2': ",packageVersion("s2"))
message("Version of package 'wk': ",packageVersion("wk"))

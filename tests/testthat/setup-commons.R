# Define common paths
ref_dir <- normalizePath(system.file("extdata/out", package = "sen2r"))
safe_dir <- file.path(dirname(normalizePath(attr(load_binpaths(), "path"), mustWork = FALSE)), "safe")
dir.create(safe_dir, showWarnings = FALSE)

# Silence clock check (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

# Perform full tests?
# If so, all the original tests are performed 
# (this is useful for codecov and after major releases).
# If neither, only fast and strategic tests are executed.
full_tests <- FALSE
skip_full_tests <- function() {skip_if(full_tests == FALSE, "Full tests disabled")}
# Perform tests calling GDAL?
# On some specific isolated machines, calling sf::gdal_utils() 
# causes segmentation faults.
# Functions which use GDAL are not tested in those cases.
gdal_tests <- Sys.info()["user"] != "ripley"
skip_gdal_tests <- function() {skip_if(gdal_tests == FALSE, "GDAL tests disabled")}

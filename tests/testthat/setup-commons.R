# Define common paths
ref_dir <- system.file("extdata/out", package = "sen2r")
safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)

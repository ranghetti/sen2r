# Define common paths
ref_dir <- normalizePath(system.file("extdata/out", package = "sen2r"))
safe_dir <- file.path(dirname(normalizePath(attr(load_binpaths(), "path"))), "safe")
dir.create(safe_dir, showWarnings = FALSE)

# Silence clock check (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)


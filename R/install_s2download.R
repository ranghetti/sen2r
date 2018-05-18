#' @title Clone s2download and install sen2cor docker.
#' @description [s2download](https://github.com/ranghetti/s2download)
#'  is a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by this package.
#'  This function clones them and installs a docker with sen2cor.
#' @details This function is deprecated, since `s2download` scripts were
#'  integrated in the package.
#' @param inst_path Path where
#'  [s2download](https://github.com/ranghetti/s2download) will be cloned
#'  (default: a subdirectory of this package).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import_from_path import_builtins py_str use_python py_module_available py_to_r

install_s2download <- function(inst_path = NA) {
  .install_s2download(
    inst_path = inst_path, 
    interactive = TRUE
  )
}

.install_s2download <- function(inst_path = NA,
                                interactive = TRUE) {
  
  # define remote position of s2download
  s2download_ref <- "devel"
  s2download_url <- paste0("https://github.com/ranghetti/s2download/archive/",s2download_ref,".zip")
  
  # define inst_path (where to install or update)
  if (is.na(inst_path)) {
    inst_path <- file.path(system.file(package="sen2r"),"s2download")
  }
  if (file.exists(inst_path) & !file.info(inst_path)$isdir) {
    print_message(
      type="error",
      inst_path," already exists and it is a file; please provide a different value (or leave blank).")
  }
  if (length(list.files(inst_path))>0) {
    if (interactive & interactive()) {
      print_message(
        type="waiting", 
        paste0(inst_path," already exists and will be erased: ENTER to proceed or ESC to cancel...")
      )
    } else {
      print_message(
        type="warning",
        inst_path," already exists and will be erased.")
    }
    unlink(inst_path,recursive=TRUE)
  } else if (dir.exists(inst_path)) {
    unlink(inst_path)
  }
  
  
  # clone the package and import the module
  s2download_zippath <- file.path(dirname(inst_path), "s2download.zip")
  download.file(s2download_url, destfile = s2download_zippath)
  unzip(zipfile = s2download_zippath,
        exdir   = dirname(inst_path),
        unzip   = "internal") %>%
    suppressWarnings()
  file.rename(file.path(dirname(inst_path),paste0("s2download-",s2download_ref)), inst_path)
  unlink(s2download_zippath)
  # system(paste0(binpaths$git," clone ",s2download_git," ",inst_path))
  install_s2download_dependencies <- tryCatch(
    import_from_path("install_dependencies", inst_path, convert=FALSE), 
    error = function(e){e}
  )
  if (is(install_s2download_dependencies, "error")) {
    install_s2download_dependencies <- import_from_path(
      "install_dependencies", 
      normalize_path(paste0(inst_path,"/")), 
      convert=FALSE
    )
  }
  
  # clone dependent repositories
  install_s2download_dependencies$download_repo(c("ranghetti","Sentinel-download"))
  
  # TODO check on errors (bot in python some of them does not appear as errors)
  # and message in case all run ok.
  
  # Save a text file with the directory where s2download has been cloned
  binpaths <- load_binpaths()
  binpaths$s2download <- normalize_path(inst_path)
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  
}

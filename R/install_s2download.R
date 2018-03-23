#' @title Clone s2download and install sen2cor docker.
#' @description [s2download](https://github.com/ranghetti/s2download)
#'  is a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by this package.
#'  This function clones them and installs a docker with sen2cor.
#' @param inst_path Path where
#'  [s2download](https://github.com/ranghetti/s2download) will be cloned
#'  (default: a subdirectory of this package).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
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
  
  # define the required binary dependencies
  dependencies <- c("python","wget")
  
  # define inst_path (where to install or update)
  if (is.na(inst_path)) {
    inst_path <- file.path(system.file(package="salto"),"s2download")
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
  
  # check that git, python2 and wget are installed
  binpaths_file <- file.path(system.file("extdata",package="salto"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    sapply(dependencies,function(x){x=NULL})
  }
  
  # add missing binaries to binpaths
  for (d in dependencies[!dependencies %in% names(binpaths)]) {
    binpaths[[d]] <- normalizePath(Sys.which(d))
  }
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
  
  missing_dep <- dependencies[binpaths[dependencies]==""]
  if (length(missing_dep)>0) {
    
    if (Sys.info()["sysname"] != "Windows") {
      
      # On Linux, send an error if something is missing
      print_message(
        type="error",
        "Some dependencies (",paste(missing_dep,collapse=", "),") were not found in your system; ",
        "please install them or update your system PATH. "
      )
      
    } else {
      
      # On Windows, download and install (them) or inform how to install them)
      
      # Download wget
      suppressMessages(install_wget())
      
      # If other ones are missing:
      if (length(missing_dep[missing_dep!="wget"])>0) {
        print_message(
          type="error",
          "Some dependencies (",paste(missing_dep,collapse=", "),") were not found in your system; ",
          "please install them or update your system PATH.",
          if ("python" %in% missing_dep) {paste0(
            "\nTo install python, we suggest to use the OSGeo4W installer ",
            "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
            if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
            "to choose the \"Advanced install\" and to check ",
            "the package \"gdal-python\"."
          )}
        )
      }
      
    }
  } #TODO pip2 not working to install gitPython
  
  # checks the python version and import modules
  py <- init_python()
  
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
      normalizePath(paste0(inst_path,"/")), 
      convert=FALSE
    )
  }
  
  # clone dependent repositories
  install_s2download_dependencies$download_repo(c("ranghetti","Sentinel-download"))
  
  # TODO check on errors (bot in python some of them does not appear as errors)
  # and message in case all run ok.
  
  # Save a text file with the directory where s2download has been cloned
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("s2download" = NULL)
  }
  binpaths$s2download <- normalizePath(inst_path)
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
  
}

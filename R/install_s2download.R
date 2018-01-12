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

install_s2download <- function(inst_path=NA) {
  
  # define remote position of s2download
  s2download_git <- "https://github.com/ranghetti/s2download.git"
  
  # define the required binary dependencies
  dependencies <- c("git","python2","wget")
  # dependencies <- c("git","docker-compose","python2","wget")
  
  # define inst_path (where to install or update)
  if (is.na(inst_path)) {
    inst_path <- file.path(system.file(package="fidolasen"),"s2download")
  }
  if (!file.exists(inst_path)) {
    dir.create(inst_path, recursive=TRUE)
  } else if (!file.info(inst_path)$isdir) {
    print_message(
      type="error",
      inst_path," already exists and it is a file; please provide a different value (or leave blank).")
  }
  if (length(list.files(inst_path))>0) {
    if (interactive()) {
      print_message(
        type="waiting",
        inst_path," already exists and will be erased: ENTER to proceed or ESC to cancel...")
    } else {
      print_message(
        type="warning",
        inst_path," already exists and will be erased.")
    }
    unlink(inst_path,recursive=TRUE)
    dir.create(inst_path)
  }
  
  # check that git is installed
  missing_dep <- dependencies[Sys.which(dependencies)==""]
  if (length(missing_dep)>0) {
    print_message(
      type="error",
      "Some dependencies (",paste(missing_dep,collapse=", "),") were not found in your system; ",
      "please install them or update your system PATH. ",
      if ("docker-compose" %in% missing_dep){
        paste0("\n(Some systems require, after installing docker-compose, to manually enable ",
               "the service 'docker'; to do this use the command ",
               "'sudo systemctl enable docker; sudo systemctl start docker')")
      })
  } #TODO pip2 not working to install gitPython
  
  # # check the user to be in the "docker" group
  # user_groups <- unlist(strsplit(system(paste("groups", system("whoami",intern=TRUE)), intern=TRUE), " "))
  # if (!"docker" %in% user_groups) {
  #   print_message(
  #     type="error",
  #     "Current user '",system("whoami",intern=TRUE),"' is not in the group 'docker' ",
  #     "(this is required to run sen2cor in a docker). ",
  #     "Please add it before installing s2download (you can do it with the command ",
  #     "'sudo usermod -a -G docker ",system("whoami",intern=TRUE),"' ;",
  #     "some systems requires to logout and re-login to take effects).")
  # }
  
  # checks the python version and import modules
  py <- init_python()
  
  # clone the package and import the module
  system(paste0(Sys.which("git")," clone ",s2download_git," ",inst_path))
  install_s2download_dependencies <- tryCatch(
    import_from_path("install_dependencies", inst_path, convert=FALSE), 
    error = print
  )
  if (is(install_s2download_dependencies, "error")) {
    install_s2download_dependencies <- import_from_path(
      "install_dependencies", 
      paste0(normalizePath(inst_path),"/"), 
      convert=FALSE
    )
  }
  
  # clone dependent repositories
  # install_s2download_dependencies$clone_repo(c("ranghetti","fetchLandsatSentinelFromGoogleCloud"))
  install_s2download_dependencies$clone_repo(c("ranghetti","Sentinel-download"))
  # install_s2download_dependencies$clone_sen2cor_docker()
  # install_s2download_dependencies$build_sen2cor_docker()
  
  # TODO check on errors (bot in python some of them does not appear as errors)
  # and message in case all run ok.
  
  # Save a text file with the directory where s2download has been cloned
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("s2download" = NULL)
  }
  binpaths$s2download <- normalizePath(inst_path)
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
  
}

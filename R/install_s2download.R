#' @title Clone s2download and install sen2cor docker.
#' @description s2download is a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by RSPrePro.
#'  This function clones them and installs a docker with sen2cor.
#' @details The function installs both Sentinel-download (to download
#'  Sentinel-2 images from SciHub), fetchLandsatSentinelFromGoogleCloud
#'  (to download from Google Cloud) and sen2cor_docker (to run
#'  sen2cor in a docker which ensure its functionality).
#'  This last function works only on Linux systems.
#'
#'  TODO In future this last function will be separated, in order to
#'  allow the download to run also on Windows.
#'
#'  Note that first run can be very time consuming, since the docker
#'  for sen2cor have to be downloaded and built.
#' @param inst_path `character` Path where s2download will be cloned
#'  (default: a subdirectory of the RSPrePro package).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import import_builtins py_str use_python py_module_available py_to_r

install_s2download <- function(inst_path=NA) {

  # define remote position of s2download
  s2download_git <- "https://github.com/ggranga/s2download.git"

  # define the required python modules and binary dependencies
  py_modules <- c("os","sys","git","subprocess","re","numpy","zipfile")
  dependencies <- c("git","docker-compose","python2","wget")

  # define inst_path (where to install or update)
  if (is.na(inst_path)) {
    inst_path <- file.path(system.file(package="RSPrePro"),"s2download")
  }
  if (!file.exists(inst_path)) {
    dir.create(inst_path, recursive=TRUE)
  } else if (!file.info(inst_path)$isdir) {
    print_message(
      type="error",
      inst_path," already exists and it is a file; please provide a different value (or leave blank).")
  }
  if (length(list.files(inst_path))>0) {
    print_message(
      type="waiting",
      inst_path," already exists and will be erased: ENTER to proceed or ESC to cancel...")
    unlink(inst_path,recursive=TRUE)
    dir.create(inst_path)
  }

  # check that docker-compose and git are installed
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

  # check the user to be in the "docker" group
  user_groups <- unlist(strsplit(system(paste("groups", system("whoami",intern=TRUE)), intern=TRUE), " "))
  if (!"docker" %in% user_groups) {
    print_message(
      type="error",
      "Current user '",system("whoami",intern=TRUE),"' is not in the group 'docker' ",
      "(this is required to run sen2cor in a docker). ",
      "Please add it before installing s2download (you can do it with the command ",
      "'sudo usermod -a -G docker ",system("whoami",intern=TRUE),"' ;",
      "some systems requires to logout and re-login to take effects).")
  }

  # checks the python version
  # (install_denepdencies.py runs also with python3, but using python2
  # for compatibility with s2download.py)
  use_python(Sys.which("python2")[1])
  py_missing <- py_modules[!sapply(py_modules,py_module_available)]
  if (length(py_missing)>0) {
    print_message(
      type="error",
      "Some modules (",paste(py_missing,collapse=", "),") are missing in your python distribution. ",
      "Please install them before continuing (depending on your distribution, you can find ",
      "packaged version of them, otherwise you can install them manually with ",
      "'sudo pip2 install ",paste(py_missing,collapse=" "),"' - pip2 is required).")
  } #TODO pip2 not working to install gitPython

  # import python modules
  py <- import_builtins(convert=FALSE)
  sys <- import("sys",convert=FALSE)

  # clone the package and import the module
  system(paste0(Sys.which("git")," clone ",s2download_git," ",inst_path))
  if (!inst_path %in% py_to_r(sys$path)) {
    sys$path$insert(py$int(0),inst_path)
  }
  install_s2download_dependencies <- import("install_dependencies", convert=FALSE)

  # clone dependent repositories
  install_s2download_dependencies$clone_repo(c("ggranga","fetchLandsatSentinelFromGoogleCloud"))
  install_s2download_dependencies$clone_repo(c("ggranga","Sentinel-download"))
  install_s2download_dependencies$clone_sen2cor_docker()
  install_s2download_dependencies$build_sen2cor_docker()

  # TODO check on errors (bot in python some of them does not appear as errors)
  # and message in case all run ok.

  # Save a text file with the directory where s2download has been cloned
  writeLines(inst_path, file.path(system.file(package="RSPrePro"),"s2download_path.txt"))

}

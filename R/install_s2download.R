#' install_s2download
#' @description Clone s2download and install sen2cor docker.
#' @details s2download is a collection of python scripts used to download
#'  and correct Sentinel-2 images, and it is required by RSPrePro.
#'  This function clones them and installs a docker with sen2cor.
#' @param inst_path `character` Path where s2download will be cloned
#'  (default: a subdirectory of the RSPrePro package).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import import_builtins py_str

install_s2download <- function(inst_path=NA) {

  # import python modules
  py <- import_builtins(convert=FALSE)
  sys <- import("sys",convert=FALSE)

  # define remote position of s2download
  s2download_git <- "https://github.com/ggranga/s2download.git"

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
  for (req_bin in c("git","docker-compose")) {
    if (Sys.which(req_bin)[1]=="") {
      print_message(
        type="error",
        req_bin," was not found in your system; please install it or update your system PATH.")
    }
  }

  # check that the user is in the "docker" group (root required!)
  user_groups <- unlist(strsplit(system(paste("groups", system("whoami",intern=TRUE)), intern=TRUE), " "))
  if (!"docker" %in% user_groups) {
    print_message(
      type="error",
      "Current user '",system("whoami",intern=TRUE),"' is not in the group 'docker' ",
      "(this is required to run sen2cor in a docker). ",
      "Please add it before installing s2download (you can do it with the command ",
      "'sudo usermod -a -G docker ",system("whoami",intern=TRUE),"' )")
  }

  # clone the package and import the module
  system(paste0("git clone ",s2download_git," ",inst_path))


  if (!inst_path %in% py_to_r(sys$path)) {
    sys$path$insert(py$int(0),inst_path)
  }
browser()
setwd(inst_path)
install_s2download_dependencies <- import("install_dependencies", convert=FALSE)
  # TODO: add checks on python modules!

  # clone dependent repositories

  install_s2download_dependencies$clone_repo(c("ggranga","fetchLandsatSentinelFromGoogleCloud"))
  install_s2download_dependencies$clone_repo(c("ggranga","Sentinel-download"))
  # install_s2download_dependencies$clone_sen2cor_docker()

  # exit
  print_message(
    type="messsage",
    "s2download and dependencies have been correctly installed.")

}

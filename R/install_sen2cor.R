#' @title Download and install sen2cor.
#' @description This function download and install standalone version of
#'  [sen2cor 2.4.0](http://step.esa.int/main/third-party-plugins-2/sen2cor).
#' @param sen2cor_dir (optional) Path where sen2cor will be installed
#'  (default: a subdirectory of the package path).
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom utils download.file unzip
#' @importFrom magrittr '%>%'
#' @export
#' @examples \dontrun{
#' install_sen2cor()
#' }

install_sen2cor <- function(sen2cor_dir=NA, force = FALSE) {
  
  # sen2cor version
  sen2cor_version <- package_version("2.4.0")
  
  # check if it is already installed
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("sen2cor" = NULL)
  }
  if (force != TRUE & !is.null(binpaths$sen2cor)) {
    sen2cor_bin <- binpaths$sen2cor 
    if (file.exists(sen2cor_bin)) {
      print_message(
        type = "message",
        "sen2cor is already installed; to overwrite, set force = TRUE."
      )
      return(invisible(NULL))
    }
  }
  
  # define sen2cor_dir (where to install or update)
  if (is.na(sen2cor_dir)) {
    sen2cor_dir <- file.path(system.file(package="fidolasen"),"sen2cor")
  }
  if (!file.exists(sen2cor_dir)) {
    dir.create(sen2cor_dir, recursive=FALSE, showWarnings = FALSE)
  } else if (!file.info(sen2cor_dir)$isdir) {
    print_message(
      type="error",
      sen2cor_dir," already exists and it is a file; please provide a different value (or leave blank).")
  }
  if (length(list.files(sen2cor_dir))>0) {
    print_message(
      type="waiting",
      sen2cor_dir," already exists and will be erased: ENTER to proceed or ESC to cancel...")
    unlink(sen2cor_dir,recursive=TRUE)
    dir.create(sen2cor_dir)
  }
  
  # Set path
  if (Sys.info()["sysname"] == "Linux") {
    sen2cor_url <- paste0("http://step.esa.int/thirdparties/sen2cor/",
                          sen2cor_version,
                          "/Sen2Cor-",sen2cor_version,"-Linux64.run")
  } else if (Sys.info()["sysname"] == "Windows") {
    sen2cor_url <- paste0("http://step.esa.int/thirdparties/sen2cor/",
                          sen2cor_version,
                          "/Sen2Cor-",sen2cor_version,"-win64.zip")
  } else {
    print_message(
      type = "error",
      "Installing sen2cor on ", Sys.info()["sysname"], " was not yet implemented."
    )
  }
  sen2cor_installer <- file.path(sen2cor_dir, basename(sen2cor_url))
  
  # download, extract and delete archive
  download.file(sen2cor_url, destfile = sen2cor_installer)
  if (Sys.info()["sysname"] == "Linux") {
    curr_dir <- getwd()
    setwd(sen2cor_dir)
    # os.chmod(sen2cor_installer, 0755)
    system(
      paste0("/bin/bash ./",basename(sen2cor_url)," --quiet --nox11 --target ./"),
      intern = FALSE
    )
    unlink(sen2cor_installer)
    setwd(curr_dir)
    sen2cor_bin <- file.path(sen2cor_dir, "bin", "L2A_Process")
  } else if (Sys.info()["sysname"] == "Windows") {
    unzip(zipfile = sen2cor_installer,
          exdir   = sen2cor_dir,
          unzip   = "internal") %>%
      suppressWarnings()
    unlink(sen2cor_installer)
    sen2cor_bin <- file.path(sen2cor_dir, "bin", "Sen2Cor-2.4.0-win64", "L2A_Process.bat")
  }

  # Save a text file with the L2A_Process path,
  # including also paths of GDAL apps
  binpaths$sen2cor <- normalizePath(sen2cor_bin)
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
  
}

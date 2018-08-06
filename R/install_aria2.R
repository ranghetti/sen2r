#' @title Download and install aria2.
#' @description This function download and install standalone version of
#'  [aria2](https://aria2.github.io) for Windows.
#' @param aria2_dir (optional) Path where aria2 executable will be installed
#'  (default: the package path).
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @return The path of aria2
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @examples \dontrun{
#' install_aria2()
#' }

install_aria2 <- function(aria2_dir = system.file(package="sen2r"), 
                         force = FALSE) {
  
  # define the versions to download (for Windows)
  aria2_ver <- package_version("1.34.0")
  
  # run only on Windows
  if (Sys.info()["sysname"] != "Windows") {
    print_message(
      type = "warning",
      "This function is only for Windows."
    )
    return(invisible(NULL))
  }
  
  # check if it is already installed
  binpaths <- load_binpaths()
  if (force != TRUE & !is.null(binpaths$aria2c)) {
    aria2_bin <- binpaths$aria2c
    if (file.exists(aria2_bin)) {
      print_message(
        type = "message",
        "aria2 is already installed; to overwrite, set force = TRUE."
      )
      return(invisible(NULL))
    }
  }
  
  # Download aria2
  aria2_url <- paste0(
    "https://github.com/aria2/aria2/releases/download/release-", aria2_ver, 
    "/aria2-", aria2_ver,
    if (Sys.info()["machine"]=="x86-64") {"-win-64bit"} else {"-win-32bit"}, 
    "-build1.zip"
  )
  aria2_zip <- normalize_path(file.path(aria2_dir,"aria2.zip"), mustWork=FALSE)
  aria2_path <- normalize_path(file.path(aria2_dir,"aria2c.exe"), mustWork=FALSE)
  
  # download.file(aria2_url, aria2_zip) # problem with https
  system(
    paste(binpaths$wget, aria2_url, "-O", aria2_zip),
    intern = Sys.info()["sysname"] == "Windows"
  )
  if (file.exists(aria2_zip)) {
    aria2_filelist <- unzip(
      zipfile = aria2_zip,
      list = TRUE
    )
    aria2_filename <- aria2_filelist$Name[
      grep(paste0(basename(aria2_path),"$"), aria2_filelist$Name)
    ]
    unzip(
      zipfile = aria2_zip,
      files = aria2_filename,
      exdir = dirname(aria2_path),
      junkpaths = TRUE,
      unzip = "internal"
    )
    unlink(aria2_zip)
  }
  if (file.exists(aria2_path)) {
    binpaths$aria2c <- aria2_path
    writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  }
  # # add to the path
  # system(paste0('setx PATH "',binpaths$aria2c,'"'), intern=TRUE)
  
  aria2_path
  
}

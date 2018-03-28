#' @title Download and install wget.
#' @description This function download and install standalone version of
#'  [GNU Wget](https://eternallybored.org/misc/wget) for Windows.
#' @param wget_dir (optional) Path where wget executable will be installed
#'  (default: the package path).
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @return The path of wget
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @examples \dontrun{
#' install_wget()
#' }

install_wget <- function(wget_dir = system.file(package="sen2r"), 
                         force = FALSE) {
  
  # define the versions to download (for Windows)
  wget_ver <- package_version("1.19.4")

  # run only on Windows
  if (Sys.info()["sysname"] != "Windows") {
    print_message(
      type = "warning",
      "This function is only for Windows."
    )
    return(invisible(NULL))
  }
  
  # check if it is already installed
  binpaths_file <- file.path(system.file("extdata",package="sen2r"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("wget" = NULL)
  }
  if (force != TRUE & !is.null(binpaths$wget)) {
    wget_bin <- binpaths$wget 
    if (file.exists(wget_bin)) {
      print_message(
        type = "message",
        "wget is already installed; to overwrite, set force = TRUE."
      )
      return(invisible(NULL))
    }
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }

  # Download wget
    wget_url <- paste0(
      "https://eternallybored.org/misc/wget/releases/wget-", wget_ver, 
      if (Sys.info()["machine"]=="x86-64") {"-win64"} else {"-win32"}, ".zip"
    )
    wget_zip <- normalize_path(file.path(wget_dir,"wget.zip"), mustWork=FALSE)
    wget_path <- normalize_path(file.path(wget_dir,"wget.exe"), mustWork=FALSE)
    download.file(wget_url, wget_zip)
    if (file.exists(wget_zip)) {
      unzip(zipfile = wget_zip,
            files   = basename(wget_path),
            exdir   = dirname(wget_path),
            unzip   = "internal")
      unlink(wget_zip)
    }
    if (file.exists(wget_path)) {
      binpaths$wget <- wget_path
      writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), binpaths_file)
    }
    # # add to the path
    # system(paste0('setx PATH "',binpaths$wget,'"'), intern=TRUE)
    
    wget_path
  
}

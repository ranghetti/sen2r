#' @title Download and install aria2.
#' @description This function download and install standalone version of
#'  [aria2](https://aria2.github.io) for Windows.
#' @param aria2_dir Path where aria2 executable will be installed.
#' @param force (optional) Logical: if TRUE, install even if it is already 
#'  installed (default is FALSE).
#' @return The path of aria2
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom httr RETRY write_disk progress
#' @export
#' @examples
#' \dontrun{
#' # Run only on Windows
#' install_aria2(aria2_dir = tempdir())
#' # ( use a non-temporary folder path instead of tempdir() )
#' }

install_aria2 <- function(aria2_dir, force = FALSE) {
  
  # define the versions to download (for Windows)
  aria2_ver <- package_version("1.34.0")
  arch_ver <- if (Sys.info()["machine"]=="x86-64") {"64"} else {"32"}
  
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
    "/aria2-", aria2_ver, "-win-", arch_ver, "bit-build1.zip"
  )
  aria2_zip <- normalize_path(file.path(aria2_dir,"aria2.zip"), mustWork=FALSE)
  aria2_path <- normalize_path(file.path(aria2_dir,"aria2c.exe"), mustWork=FALSE)
  
  out_bar <- if (inherits(stdout(), "terminal")) {
    NULL
  } else {
    file(out_bar_path <- tempfile(), open = "a")
  }
  RETRY(
    verb = "GET",
    url = aria2_url,
    times = 5, pause_cap = 8,
    progress(con = if (length(out_bar) > 0) {out_bar} else {stdout()}),
    write_disk(aria2_zip, overwrite = TRUE)
  )
  if (length(out_bar) > 0) {
    close(out_bar)
    invisible(file.remove(out_bar_path))
  }
  
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

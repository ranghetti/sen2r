#' @title Download and install (or link) Sen2Cor
#' @description [install_sen2cor()] downloads and installs a standalone version of
#'  [Sen2Cor](http://step.esa.int/main/third-party-plugins-2/sen2cor).
#' @param sen2cor_dir Path where sen2cor will be installed or searched
#'  (by default it is a subdirectory `"sen2cor"` of the default sen2r directory).
#' @param version (optional) Character: Sen2Cor version (one among
#'  '2.5.5' - default - and '2.8.0').
#' @param force (optional) Logical: if TRUE, installs sen2cor even if it is already
#'  found in sen2cor_dir (default is FALSE).
#' @return NULL (the function is called for its side effects)
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom utils download.file unzip
#' @importFrom httr GET write_disk
#' @export
#' @examples
#' \dontrun{
#' install_sen2cor(sen2cor_dir = tempdir())
#' # ( use a non-temporary folder path instead of tempdir() )
#' }

install_sen2cor <- function(
  sen2cor_dir = NA, 
  version = "2.5.5", 
  force = FALSE
) {
  .install_sen2cor(
    sen2cor_dir = sen2cor_dir,
    version = version,
    force = force,
    interactive = TRUE
  )
}

.install_sen2cor <- function(
  sen2cor_dir,
  version="2.5.5",
  force = FALSE,
  interactive = TRUE
) {
  
  # check Sen2Cor version
  if (!as.character(version) %in% c("2.5.5", "2.8.0")) {
    print_message(
      type = "error",
      "Only Sen2Cor versions '2.5.5' and '2.8.0' are currently supported."
    )
  }
  version <- package_version(version)
  
  # check if it is already installed
  binpaths <- load_binpaths()
  if (force != TRUE & !is.null(binpaths$sen2cor)) {
    sen2cor_bin <- binpaths$sen2cor
    if (file.exists(sen2cor_bin)) {
      print_message(
        type = "message",
        "Sen2Cor is already installed; to overwrite, set force = TRUE."
      )
      return(invisible(NULL))
    }
  }
  
  # define sen2cor_dir (where to install or update)
  if (is.na(sen2cor_dir)) {
    sen2cor_dir <- file.path(dirname(attr(binpaths, "path")), "sen2cor")
  }
  if (!file.exists(sen2cor_dir)) {
    dir.create(sen2cor_dir, recursive = FALSE, showWarnings = FALSE)
  } else if (!file.info(sen2cor_dir)$isdir) {
    print_message(
      type="error",
      sen2cor_dir," already exists and it is a file; please provide a different value (or leave blank).")
  }
  if (length(list.files(sen2cor_dir))>0) {
    if (interactive & interactive()) {
      print_message(
        type="waiting",
        sen2cor_dir," already exists and will be erased: ENTER to proceed or ESC to cancel...")
    } else {
      print_message(
        type="warning",
        sen2cor_dir," already exists and will be erased.")
    }
    unlink(sen2cor_dir,recursive=TRUE)
    dir.create(sen2cor_dir)
  }
  
  # Set path
  if (Sys.info()["sysname"] == "Linux") {
    sen2cor_url <- paste0("http://step.esa.int/thirdparties/sen2cor/",
                          version,
                          "/Sen2Cor-",
                          str_pad2(version[,1],2,"left","0"),".",
                          str_pad2(version[,2],2,"left","0"),".",
                          str_pad2(version[,3],2,"left","0"),
                          "-Linux64.run")
  } else if (Sys.info()["sysname"] == "Windows") {
    sen2cor_url <- paste0("http://step.esa.int/thirdparties/sen2cor/",
                          version,
                          "/Sen2Cor-",
                          str_pad2(version[,1],2,"left","0"),".",
                          str_pad2(version[,2],2,"left","0"),".",
                          str_pad2(version[,3],2,"left","0"),
                          "-win64.zip")
  } else if (Sys.info()["sysname"] == "Darwin") {
    sen2cor_url <- paste0("http://step.esa.int/thirdparties/sen2cor/",
                          version,
                          "/Sen2Cor-",
                          str_pad2(version[,1],2,"left","0"),".",
                          str_pad2(version[,2],2,"left","0"),".",
                          str_pad2(version[,3],2,"left","0"),
                          "-Darwin64.run")
  } else {
    print_message(
      type = "error",
      "Installing Sen2Cor on ", Sys.info()["sysname"], " was not yet implemented."
    )
  }
  sen2cor_installer <- file.path(sen2cor_dir, basename(sen2cor_url))
  
  # download, extract and delete archive
  GET(sen2cor_url, write_disk(sen2cor_installer, overwrite=TRUE))
  # download.file(sen2cor_url, destfile = sen2cor_installer)
  if (Sys.info()["sysname"] %in% c("Linux","Darwin")) {
    curr_dir <- getwd()
    on.exit(setwd(curr_dir))
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
    suppressWarnings(unzip(
      zipfile = sen2cor_installer,
      exdir = sen2cor_dir,
      unzip = "internal"
    ))
    unlink(sen2cor_installer)
    sen2cor_bin <- file.path(
      sen2cor_dir,
      gsub("\\.zip$","",basename(sen2cor_installer)),
      "L2A_Process.bat"
    )
    if (!file.exists(sen2cor_bin)) {
      print_message(
        type = "error",
        "Something went wrong during the installation of Sen2Cor, ",
        "try reinstalling it or contact the package maintainer."
      )
    }
  }
  
  # fix bug #71
  script_tofix_path <- file.path(
    if (Sys.info()["sysname"] == "Windows") {dirname(sen2cor_bin)} else {dirname(dirname(sen2cor_bin))},
    "lib/python2.7/site-packages/sen2cor/L2A_Tables.py"
  )
  if (file.exists(script_tofix_path)) {
    script_tofix <- readLines(script_tofix_path)
    linenumber_tofix <- grep("t2a_split[2] + '_' + t2a_split[1] + '_' + t1c_split[10]", script_tofix, fixed=TRUE)
    if (length(linenumber_tofix)>0) {
      script_tofix[linenumber_tofix] <- gsub(
        "t1c_split[10]", "t1c_split[-1]",
        script_tofix[linenumber_tofix],
        fixed = TRUE
      )
      writeLines(script_tofix, script_tofix_path)
    }
  }
  
  # Save a text file with the L2A_Process path,
  # including also paths of GDAL apps
  binpaths$sen2cor <- normalize_path(sen2cor_bin)
  writeLines(toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  
  # reset sen2r GIPP XML to the default Sen2Cor values
  # (this is necessary to avoid errors in case of reinstallation 
  # of a different Sen2cor version)
  gipp_init(force = TRUE, dem_warning = TRUE)

}


.sen2cor_exists <- function(sen2cor_dir) {
  # Check if Sen2Cor exists in the provided directory
  file.exists(file.path(
    sen2cor_dir,
    if (Sys.info()["sysname"] == "Windows") {"L2A_Process.bat"} else {"bin/L2A_Process"}
  ))
}


#' @name link_sen2cor
#' @rdname install_sen2cor
#' @description `link_sen2cor()` links an existing standalone version of
#'  [Sen2Cor](http://step.esa.int/main/third-party-plugins-2/sen2cor) to sen2r.
#' @export
link_sen2cor <- function(sen2cor_dir) {
  
  # Exit if Sen2Cor does not exist in the provided directory
  if (!.sen2cor_exists(sen2cor_dir)) {
    print_message(type = "error", "Sen2Cor was not found here.")
  }
  
  binpaths <- load_binpaths()
  binpaths$sen2cor <- normalize_path(file.path(
    sen2cor_dir,
    if (Sys.info()["sysname"] == "Windows") {"L2A_Process.bat"} else {"bin/L2A_Process"}
  ))
  writeLines(toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  # get Sen2Cor version
  sen2cor_version_raw0 <- system(paste(binpaths$sen2cor, "-h"), intern = TRUE)
  sen2cor_version_raw1 <- sen2cor_version_raw0[grep(
    "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version:",
    sen2cor_version_raw0
  )]
  sen2cor_version <- gsub(
    "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version: ([2-9]+\\.[0-9]+)\\.[0-9]+,.*$",
    "\\1",
    sen2cor_version_raw1
  )
  
  # reset sen2r GIPP XML to the default Sen2Cor values
  # (this is necessary to avoid errors in case of reinstallation 
  # of a different Sen2cor version)
  gipp_init(force = TRUE, dem_warning = TRUE)
  
  return(invisible(NULL))
  
}

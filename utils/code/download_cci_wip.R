# Development of this function was stopped, since unzip() is not able do decompress files > 4 GB

download_cci <- function(sen2cor_dir = NA, downloader = "builtin") {
  
  # define sen2cor_dir (where to install or update)
  binpaths <- load_binpaths()
  if (is.na(sen2cor_dir)) {
    if (is.null(binpaths$sen2cor)) {
      print_message(
        type = "error",
        "Sen2Cor was not found; install it first."
      )
    }
    sen2cor_dir <- dirname(dirname(binpaths$sen2cor))
  }
  
  # Define CCI url
  cci_url <- "ftp://geo10.elie.ucl.ac.be/v207/ESACCI-LC-L4-ALL-FOR-SEN2COR.zip"
  
  # Exit if Sen2Cor does not exist in the provided directory
  if (!.sen2cor_exists(sen2cor_dir)) {
    print_message(type = "error", "Sen2Cor was not found here.")
  }
  
  auxdata_path <- list.files(
    sen2cor_dir, "aux_data", 
    recursive = TRUE, full.names = TRUE, include.dirs = TRUE
  )[1]
  zip_path <- file.path(auxdata_path, "ESACCI-LC-L4-ALL-FOR-SEN2COR.zip")
  
  if (is.na(auxdata_path)) {
    print_message(
      type = "error",
      "Sen2Cor seems not to be installed correctly; CCI was not downloaded."
    )
  }
  
  # check downloader
  if (!downloader %in% c("builtin", "aria2", "aria2c")) {
    print_message(
      type = "warning",
      "Downloader \"",downloader,"\" not recognised ",
      "(builtin will be used)."
    )
    downloader <- "builtin"
  }
  
  # check if aria2 is available, switch to builtin
  if (downloader %in% c("aria2", "aria2c") && is.null(load_binpaths()$aria2c)) {
    print_message(
      type = "warning",
      "Downloader \"",downloader,"\" was not found in your system. ",
      "Builtin downloader will be used (see the documentation at ",
      "https://sen2r.ranghetti.info/articles/installation ",
      "to see how to install it)."
    )
    downloader <- "builtin"
  }
  
  if (downloader %in% c("builtin", "wget")) { # wget left for compatibility
    
    download <- httr::RETRY(
      verb = "GET",
      url = cci_url,
      times = 10,
      httr::progress(),
      # httr::write_disk(zip_path, overwrite = TRUE)
    )
    
  } else if (grepl("^aria2c?$", downloader)) {
    
    binpaths <- load_binpaths("aria2")
    if (Sys.info()["sysname"] != "Windows") {
      link_aria <- gsub("/\\$value", "/\\\\$value", link)
    } else {
      link_aria <- link
    }
    aria_string <- paste0(
      binpaths$aria2c, 
      " -d ",auxdata_path,
      " -o ", basename(zip_path),
      " ", "\"", cci_url, "\"",
      " --allow-overwrite --file-allocation=none --retry-wait=2",
      " --max-tries=10"
    )
    download <- try({
      system(aria_string, intern = Sys.info()["sysname"] == "Windows")
    })
    
  }
  
  if (inherits(download, "try-error")) {
    suppressWarnings(file.remove(zip_path))
    suppressWarnings(file.remove(paste0(zip_path,".aria2")))
    print_message(
      type = "error",
      "Download of file", basename(zip_path), "failed more than 10 times. ",
      "Internet connection may be down."
    )
  } else {
    zip_content <- unzip(zip_path, list = TRUE)
    safe_newname <- unique(gsub("(^[^\\/]+)\\/.*$", "\\1", zip_content$Name))
    # unzip
    unzip(zip_path, exdir = dirname(zip_path))
    file.remove(zip_path)
  }
  
  
  
}


remove_cci <- function(sen2cor_dir) {
  
  # Exit if Sen2Cor does not exist in the provided directory
  if (!.sen2cor_exists(sen2cor_dir)) {
    print_message(type = "error", "Sen2Cor was not found here.")
  }
  
  
}

check_cci <- function(sen2cor_dir) {
  
  # Exit if Sen2Cor does not exist in the provided directory
  if (!.sen2cor_exists(sen2cor_dir)) {
    print_message(type = "error", "Sen2Cor was not found here.")
  }
  
  
}
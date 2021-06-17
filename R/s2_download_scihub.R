.s2_download_scihub <- function(
  s2_prodlist, s2_meta, outdir, apihub, service, downloader, abort, overwrite
) {
  
  # to avoid NOTE on check
  i <- mission <- level <- sensing_datetime <- 
    id_orbit <- id_tile <- footprint <- NULL
  
  # Check connection
  if (!check_scihub_connection()) {
    print_message(
      type = "error", 
      "Impossible to reach the SciHub server ",
      "(internet connection or SciHub may be down)." 
    )
  }
  
  # read credentials
  if (length(s2_prodlist) > 0) {
    creds <- read_scihub_login(apihub)
  }
  
  # check the used service
  if (!service %in% c("apihub", "dhus", NA)) {
    print_message(
      type = "error",
      "Argument 'service' can be only \"apihub\" or \"dhus\"; ",
      "leaving the input URLs as are."
    )
  } else if (!is.na(service)) {
    s2_prodlist <- gsub(
      "^https://((scihub)|(apihub)).copernicus.eu/((apihub)|(dhus))/odata",
      paste0("https://",ifelse(service=="dhus","scihub","apihub"),
             ".copernicus.eu/",service,"/odata"),
      s2_prodlist
    )
  }
  
  foreach(
    i = seq_along(s2_prodlist), 
    .combine = c
  ) %do% {
    
    link <- s2_prodlist[i]
    zip_path <- file.path(outdir, paste0(names(s2_prodlist[i]),".zip"))
    safe_path <- gsub("\\.zip$", "", zip_path)
    
    # if footprint exists, check if existing SAFEs are actually equivalent
    if (!is.null(s2_meta$footprint)) {
      # regular expression to detect if equivalent products already exist
      safe_regex <- s2_meta[i,paste0(
        "^S",mission,"\\_MSIL",level,"\\_",strftime(sensing_datetime,"%Y%m%dT%H%M%S"),
        "\\_N[0-9]{4}\\_R",id_orbit,"\\_T",id_tile,"\\_[0-9]{8}T[0-9]{6}\\.SAFE$"
      )]
      safe_existing <- list.files(dirname(zip_path), safe_regex, full.names = TRUE)
      safe_existing <- safe_existing[safe_isvalid(safe_existing)]
      # check centroids
      safe_existing_footprints <- safe_getMetadata(safe_existing, "footprint")
      safe_existing <- try({
        safe_existing_centroids <- st_transform(
          st_centroid(st_transform(st_as_sfc(safe_existing_footprints, crs = 4326), 3857)),
          4326
        )
        safe_centroid <- st_transform(
          st_centroid(st_transform(st_as_sfc(s2_meta[i,footprint], crs = 4326), 3857)),
          4326
        )
        # remove SAFE with the same (approximatively) centroid
        safe_existing[
          apply(round(st_coordinates(safe_existing_centroids), 2), 1, paste, collapse = " ") ==
            apply(round(st_coordinates(safe_centroid), 2), 1, paste, collapse = " ")
        ]
      }, silent = TRUE)
      if (inherits(safe_existing, "try-error")) {safe_existing <- character()}
    } else {
      # if footprints are not available, avoid checking 
      safe_existing <- safe_path[dir.exists(safe_path)]
    }
    
    if (any(overwrite == TRUE, length(safe_existing) == 0)) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Downloading Sentinel-2 image ", i,
        " of ",length(s2_prodlist)," (",basename(safe_path),")..."
      )
      
      if (downloader %in% c("builtin", "wget")) { # wget left for compatibility
        
        out_bar <- if (all(inherits(stdout(), "terminal"), interactive())) {
          NULL
        } else {
          file(out_bar_path <- tempfile(), open = "a")
        }
        
        times_429 <- 10 # if 429 "too many requests", retry up to 10 times
        while (times_429 > 0) {
          download <- RETRY(
            verb = "GET",
            url = as.character(link),
            config = authenticate(creds[1,1], creds[1,2]),
            times = 5, pause_cap = 8,
            progress(con = if (length(out_bar) > 0) {out_bar} else {stdout()}),
            write_disk(zip_path, overwrite = TRUE)
          )
          times_429 <-if (download$status_code != 429) {0} else {times_429 - 1}
        }
        
        if (length(out_bar) > 0) {
          close(out_bar)
          invisible(file.remove(out_bar_path))
        }
      } else if (grepl("^aria2c?$", downloader)) {
        
        binpaths <- load_binpaths("aria2")
        if (Sys.info()["sysname"] != "Windows") {
          link_aria <- gsub("/\\$value", "/\\\\$value", link)
        } else {
          link_aria <- link
        }
        aria_string <- paste0(
          binpaths$aria2c, " -x 2 --check-certificate=false -d ",
          dirname(zip_path),
          " -o ", basename(zip_path),
          " ", "\"", as.character(link_aria), "\"",
          " --allow-overwrite --file-allocation=none --retry-wait=2",
          " --http-user=", "\"", creds[1,1], "\"",
          " --http-passwd=", "\"", creds[1,2], "\"",
          " --max-tries=10"
        )
        download <- try({
          system(aria_string, intern = Sys.info()["sysname"] == "Windows")
        })
        
      }
      
      # check if the user asked to download a LTA product
      download_is_lta <- if (inherits(download, "response")) {
        download$status_code == 202
      } else if (inherits(download, "integer")) {
        download == 22
      } else FALSE
      if (download_is_lta) {
        # TODO
      }
      
      if (inherits(download, "try-error")) {
        suppressWarnings(file.remove(zip_path))
        suppressWarnings(file.remove(paste0(zip_path,".aria2")))
        print_message(
          type = ifelse(abort == TRUE, "error", "warning"),
          "Download of file", link, "failed more than 10 times ",
          "(internet connection or SciHub may be down)."
        )
        safe_newname <- character(0)
      } else {
        # check md5
        check_md5 <- tryCatch({
          requireNamespace("tools")
          sel_md5 <- RETRY(
            verb = "GET",
            url = gsub("\\$value$", "Checksum/Value/$value", as.character(link)),
            config = authenticate(creds[1,1], creds[1,2]),
            write_disk(md5file <- tempfile(), overwrite = TRUE)
          )
          md5 <- toupper(readLines(md5file, warn = FALSE)) == 
            toupper(tools::md5sum(zip_path))
          file.remove(md5file)
          md5
        }, error = function(e) {logical(0)})
        if (any(!check_md5 %in% c(TRUE, FALSE), length(check_md5) == 0)) {
          print_message(
            type = "warning",
            "File ", names(link), " cannot be checked",
            if (!requireNamespace("tools", quietly = TRUE)) {
              "(package \"tools\" needs to be installed)"
            },
            ". Please verify if the download was successful."
          )
        } else if (!check_md5) {
          file.remove(zip_path)
          print_message(
            type = ifelse(abort == TRUE, "error", "warning"),
            "Download of file ", names(link), " was incomplete (Md5sum check failed). ",
            "Please retry to launch the download."
          )
          safe_newname <- character(0)
        }
        # remove existing SAFE
        if (dir.exists(safe_path)) {
          unlink(safe_path, recursive = TRUE)
        }
        # update SAFE name (possible different processing date)
        zip_content <- try(unzip(zip_path, list = TRUE), silent = TRUE)
        if (inherits(zip_content, "try-error")) {
          file.remove(zip_path)
          print_message(
            type = ifelse(abort == TRUE, "error", "warning"),
            "File ", names(link), " cannot be unzipped. ",
            "Please retry later to launch the download."
          )
          safe_newname <- character(0)
        } else {
          safe_newname <- setNames(
            link, 
            unique(gsub("(^[^\\/]+)\\/.*$", "\\1", zip_content$Name))
          )
          # unzip
          unzip(zip_path, exdir = dirname(zip_path))
          file.remove(zip_path)
        }
      }
      
    } else {
      
      print_message(
        type = "message",
        date = TRUE,
        "Skipping Sentinel-2 image ", i,
        " of ",length(s2_prodlist)," ",
        "since the corresponding folder already exists."
      )
      
      
      # safe_existing_meta <- safe_getMetadata(safe_existing, info = "nameinfo")
      # safe_newname <- safe_existing_meta$name[
      #   order(nn(safe_existing_meta$creation_datetime), decreasing = TRUE)[1]
      #   ]
      safe_newname <- setNames(link, basename(safe_path))
      
    }
    
    # return to foreach
    as(safe_newname, "safelist")
    
  }
}

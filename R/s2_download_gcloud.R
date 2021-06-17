#' @importFrom foreach foreach "%do%"
#' @importFrom methods as
#' @importFrom stats setNames
#' @import data.table
.s2_download_gcloud <- function(s2_prodlist, s2_meta, outdir, overwrite) {
  
  ## Check gcloud
  check_gcloud() # stop in case of problems
  binpaths <- load_binpaths()
  
  foreach(
    i = seq_along(s2_prodlist),
    .combine = c
  ) %do% {
    
    link <- s2_prodlist[i]
    safe_path <- file.path(outdir, paste0(names(s2_prodlist[i])))
    
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
      
      dir.create(safe_tmpdir <- tempfile(tmpdir = outdir, pattern = "LOCK_"))
      download <- try({
        system(paste0(binpaths$gsutil," -m -q cp -r ",link," ",safe_tmpdir))
        safe_newname <- list.files(safe_tmpdir)
        if (all(overwrite == TRUE, dir.exists(file.path(outdir, safe_newname)))) {
          unlink(file.path(outdir, safe_newname), recursive = TRUE)
        }
        file.rename(
          list.files(safe_tmpdir, "\\.SAFE$", full.names = TRUE)[1],
          file.path(outdir, safe_newname)
        )
        file.remove(safe_tmpdir)
      })
      
      if (inherits(download, "try-error")) {
        suppressWarnings(file.remove(safe_tmpdir))
        print_message(
          type = "error",
          "Download of file", link, "failed. ",
          "Internet connection may be down, ",
          "or \"gsutil\" may not be correctly installed."
        )
      } else {
        # Post-download operations: add missing subdirectories
        if (dir.exists(file.path(outdir, safe_newname))) {
          if (!dir.exists(file.path(outdir, safe_newname,"HTML"))) {
            dir.create(file.path(outdir, safe_newname,"HTML"))
          }
          if (!dir.exists(file.path(outdir, safe_newname,"AUX_DATA"))) {
            dir.create(file.path(outdir, safe_newname,"AUX_DATA"))
          }
          granule_dir <- list.files(
            file.path(outdir, safe_newname, "GRANULE"),
            "^L([12][AC])\\_T([A-Z0-9]{5})\\_A([0-9]{6})\\_([0-9]{8}T[0-9]{6})$",
            full.names=TRUE
          )
          if (!dir.exists(file.path(granule_dir,"AUX_DATA"))) {
            dir.create(file.path(granule_dir,"AUX_DATA"))
          }
        }
      }
      
      safe_newname <- setNames(link, safe_newname)
      
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

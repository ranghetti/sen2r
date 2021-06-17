#' @importFrom XML htmlTreeParse saveXML xmlRoot
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach foreach "%do%"
#' @importFrom sf gdal_utils
#' @import data.table
.s2_list_gcloud <- function(time_intervals, tile, orbit, level, max_cloud) {
  
  ## Check gcloud
  check_gcloud() # stop in case of problems
  binpaths <- load_binpaths()
  
  ## List SAFE
  sen2r:::print_message(
    type="message", date=TRUE,
    "Querying Google Cloud (this can take a very long time)..."
  )
  if ("auto" %in% level) { # level must be "L1C", "L2A" or both
    level <- c("L1C", "L2A")
  }
  gc_dirs <- list()
  if ("L1C" %in% level) {
    gc_dirs[["L1C"]] <- file.path(
      "gs://gcp-public-data-sentinel-2/tiles",
      substr(tile,1,2), substr(tile,3,3), substr(tile,4,5)
    )
  }
  if ("L2A" %in% level) {
    gc_dirs[["L2A"]] <- file.path(
      "gs://gcp-public-data-sentinel-2/L2/tiles",
      substr(tile,1,2), substr(tile,3,3), substr(tile,4,5)
    )
  }
  gc_urls <- unlist(lapply(gc_dirs, function(x) {
    y <- unlist(lapply(paste0(binpaths$gsutil," ls ",x), system, intern = TRUE))
    y[grepl("^gs://.+\\.SAFE/?$", y)]
  }))
  
  gc_meta <- safe_getMetadata(gc_urls, info = "nameinfo")
  gc_meta[,date:=as.Date(sensing_datetime)]
  gc_meta[,sensing_datetime:=as.POSIXct(sensing_datetime)]
  gc_meta$url <- gc_urls
  
  ## Filter by arguments
  # (this is done also after merging, but it is repeated here in order to
  # limit the slow "gsutil" calls)
  # date
  gc_meta <- rbindlist(foreach(j = seq_len(nrow(time_intervals))) %do% {
    gc_meta[date >= time_intervals[j,1] & date <= time_intervals[j,2]]
  })
  # orbit
  if (!is.null(orbit)) {
    gc_meta <- gc_meta[as.integer(id_orbit) %in% as.integer(orbit),]
  }
  
  # use products in compact format (old products are present in both the formats)
  # (this can be very long)
  if (nrow(gc_meta) == 0) {return(data.table())}
  if (inherits(stdout(), "terminal")) {pb <- txtProgressBar(0, nrow(gc_meta), style = 3)}
  gc_suburls <- sapply(gc_meta$url, function(x) {
    x1 <- system(paste0(binpaths$gsutil," ls ",paste0(x, "GRANULE")), intern = TRUE)
    if (inherits(stdout(), "terminal")) {setTxtProgressBar(pb, which(x==gc_meta$url))}
    x1[grepl("/$", x1)][1]
  })
  gc_meta$version <- safe_getMetadata(gc_suburls, allow_oldnames = TRUE, info = "version")
  gc_meta <- gc_meta[version=="compact",]
  gc_meta <- gc_meta[order(date, -level, creation_datetime),]
  gc_meta <- gc_meta[!duplicated(paste(date, id_orbit, id_tile)),]
  
  # additional metadata TODO
  if (nrow(gc_meta) == 0) {return(data.table())}
  if (inherits(stdout(), "terminal")) {pb <- txtProgressBar(0, nrow(gc_meta), style = 3)}
  gc_xml_paths <- sapply(gc_meta$url, function(x) {
    x1 <- system(paste0(binpaths$gsutil," ls ",paste0(x, "MTD_MSI*.xml")), intern = TRUE)
    system(paste0(binpaths$gsutil," -m -q cp -r ",x1[1]," ",x2 <- tempfile(fileext=".xml")))
    if (inherits(stdout(), "terminal")) {setTxtProgressBar(pb, which(x==gc_meta$url))}
    x2
  })
  gc_xml_gdal <- lapply(gc_xml_paths, function(x) {suppressWarnings(
    strsplit(gdal_utils("info", x, quiet = TRUE), "\\n")[[1]]
  )})
  gc_xml_list <- lapply(gc_xml_paths, function(x) {
    xmlRoot(htmlTreeParse(x, useInternalNodes = TRUE))[["body"]][[1]]
  })
  
  gc_meta$footprint <- do.call(c, lapply(gc_xml_gdal, function(x) {
    gsub(
      "^ *FOOTPRINT=", "",
      x[which(grepl("^ *FOOTPRINT=", x))][1]
    )
  }))
  
  # Extract / fitler clouds
  gc_meta$clouds <- sapply(gc_xml_list, function(x) {
    as.numeric(saveXML(
      x[["quality_indicators_info"]][["cloud_coverage_assessment"]][["text"]]
    ))
  })
  gc_meta <- gc_meta[clouds<=max_cloud,]
  
  gc_meta$ingestion_datetime <- as.POSIXct(NA)
  gc_meta$uuid <- as.character(NA)
  gc_meta$online <- TRUE
  
  out_dt <- gc_meta[,list(
    name, url, mission, level, id_tile, id_orbit,
    sensing_datetime, ingestion_datetime, clouds, footprint, uuid, online
  )]
  
  out_dt
  
}

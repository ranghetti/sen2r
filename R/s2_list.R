#' @title Retrieve list of available products.
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of
#'  `s2download` (see [import_s2download])
#'  python function only to retrieve the list of files, without
#'  downloading and correcting them.
#' @param spatial_extent A valid spatial object object of class `sf`,
#'  `sfc` or `sfg`
#' @param tile Single Sentinel-2 Tile string (5-length character)
#' @param orbit Single Sentinel-2 orbit number
#' @param time_interval a temporal vector (class [POSIXct] or
#'  [Date]) of length 1 (specific day) or 2 (time interval).
#' @param time_period (optional) Character:
#'  * "full" (default) means that all
#'  the images included in the time window are considered;
#'  * "seasonal" means that only the single seasonal periods in the
#'  window are used (i.e., with a time window from 2015-06-01 to
#'  2017-08-31, the periods 2015-06-01 to 2015-08-31, 2016-06-01
#'  to 2016-08-31 and 2017-06-01 to 2017-08-31 are considered).
#' @param level Character vector with one of the following:
#'     - "auto" (default): check if level-2A is available on SciHub:
#'         if so, list it; if not, list the corresponding level-1C
#'         product
#'     - "L1C": list available level-1C products
#'     - "L2A": list available level-2A products
#' @param ignore_ingestion_time (optional) Logical: if TRUE (default),
#'  the research is performed basing only on the sensing date and time
#'  (the time in which the image was acquired), ignoring the ingestion date
#'  and time (the time the image was ingested). 
#'  If FALSE, products with the ingestion time specified with `time_interval`
#'  are first of all filtered, and them the research is performed on the sensing
#'  time among them. 
#'  `ignore_ingestion_time = TRUE` ensures to perform a complete research, but 
#'  it is slower; setting it to `FALSE` speeds up, although some products could 
#'  be ignored (but generally the ingestion date is the same of the sensing date).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of scihub account. If NA (default) the default credentials
#'  (username "user", password "user") will be used.
#' @param max_cloud Integer number (0-100) containing the maximum cloud
#'  level of the tiles to be listed (default: no filter).
#' @return A vector of available products (being each element an URL,
#'  and its name the product name).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate py_to_r r_to_py
#' @importFrom magrittr "%>%"
#' @importFrom sf st_bbox st_read st_centroid st_polygon st_transform
#'
#' @examples \dontrun{
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2016-05-01", "2017-07-30"))
#' 
#' # Full-period list
#' example_s2_list <- s2_list(
#'   spatial_extent = pos, 
#'   tile = "32TNR", 
#'   time_interval = time_window, 
#'   orbit = "065"
#' )
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' as.vector(sort(sapply(names(example_s2_list), function(x) {
#'   strftime(safe_getMetadata(x,"nameinfo")$sensing_datetime)
#' })))
#' 
#' # Seasonal-period list
#' example_s2_list <- s2_list(
#'   spatial_extent = pos, 
#'   tile = "32TNR", 
#'   time_interval = time_window, 
#'   time_period = "seasonal"
#' )
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' as.vector(sort(sapply(names(example_s2_list), function(x) {
#'   strftime(safe_getMetadata(x,"nameinfo")$sensing_datetime)
#' })))
#' }

s2_list <- function(spatial_extent=NULL, tile=NULL, orbit=NULL, # spatial parameters
                    time_interval=NULL, time_period = "full", # temporal parameters
                    level="auto",
                    ignore_ingestion_time = TRUE,
                    apihub=NA,
                    max_cloud=100) {
  
  # to avoid NOTE on check
  . <- i <- NULL
  
  # convert input NA arguments in NULL
  for (a in c("spatial_extent","tile","orbit","time_interval","apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # check if spatial_extent was provided
  spatial_extent_exists <- if (!exists("spatial_extent")) {
    FALSE
  } else if (is.null(spatial_extent)) {
    FALSE
  } else if (is(spatial_extent, "POLYGON")) {
    if (length(spatial_extent)==0) {
      FALSE
    } else {
      TRUE
    }
  } else {
    TRUE
  }
  
  # if not, retrieve it from tile
  if (!spatial_extent_exists) {
    if (is.null(tile)) {
      print_message(
        type = "error",
        "At least one parameter among spatial_extent and tile must be specified."
      )
    } else {
      # extract and import tiles kml
      s2tiles <- s2_tiles()
      # take the the selected tiles as extent
      # (this will result in the selection of more tiles, cause to overlapping 
      # areas; it is filtered in s2_download, but it is slow: FIXME).
      # It is not possible to use tile centroids, because tile of external areas
      # of orbits could not be included).
      spatial_extent <- suppressWarnings(
        s2tiles[s2tiles$tile_id %in% tile,] #%>%
        # sf::st_centroid()
      )
    }
  }
  
  # checks on inputs
  spatext <- st_bbox(st_transform(spatial_extent, 4326))
  
  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (spatext["xmin"]==spatext["xmax"] || spatext["ymin"]==spatext["ymax"]) {
    lon <- mean(spatext["xmin"], spatext["xmax"])
    lat <- mean(spatext["ymin"], spatext["ymax"])
    lonmin <- lonmax <- latmin <- latmax <- NULL
  } else {
    lonmin <- spatext["xmin"]; lonmax <- spatext["xmax"]
    latmin <- spatext["ymin"]; latmax <- spatext["ymax"]
    lon <- lat <- NULL
  }
  
  # checks on dates
  # TODO add checks on format
  if (length(time_interval)==1) {
    time_interval <- rep(time_interval,2)
  }
  # split time_interval in case of seasonal download
  time_intervals <- if (time_period == "full") {
    data.frame(
      "start" = strftime(time_interval[1], "%Y%m%d"), 
      "end" = strftime(time_interval[2], "%Y%m%d"),
      stringsAsFactors = FALSE
    )
  } else if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_interval[1], time_interval[2], by="year"), "%Y%m%d"),
      "end" = strftime(rev(seq(time_interval[2], time_interval[1], by="-1 year")), "%Y%m%d"),
      stringsAsFactors = FALSE
    )
  }
  
  # convert orbits to integer
  if (is.null(orbit)) {
    orbit <- list(NULL)
  } else {
    orbit <- as.integer(orbit)
    if (anyNA(orbit)) {
      orbit <- list(NULL)
    }
  }
  
  # import s2download
  s2download <- import_s2download(convert=FALSE)
  
  # read the path of wget
  binpaths <- load_binpaths("wget")
  
  # link to apihub
  if (is.null(apihub)) {
    apihub <- file.path(s2download$inst_path,"apihub.txt")
  }
  if (!file.exists(apihub)) {
    print_message(
      type="error",
      "File apihub.txt with the SciHub credentials is missing."
    ) # TODO build it
  }
  
  # set corr_type
  corr_type <- switch(
    level,
    auto = "auto",
    L1C  = "no",
    L2A  = "scihub",
    "auto")
  
  # run the research of the list of products
  av_prod_tuple <- lapply(orbit, function(o) {
    lapply(seq_len(nrow(time_intervals)), function(i) {
      # py_capture_output was added not only to allow threating the python output
      # as R message, but also because, on Windows, the following error was returned
      # if the function was launched outside py_capture_output:
      # Error in py_call_impl(callable, dots$args, dots$keywords) : 
      #   IOError: [Errno 9] Bad file descriptor
      # from a python console, the error did not appear (only inside reticulate).
      py_output <- reticulate::py_capture_output(
        py_return <- s2download$s2_download(
          lat=lat, lon=lon, latmin=latmin, latmax=latmax, lonmin=lonmin, lonmax=lonmax,
          start_date=time_intervals[i,1], end_date=time_intervals[i,2],
          start_ingest_date=if (ignore_ingestion_time==FALSE) {time_intervals[i,1]} else {r_to_py(NULL)},
          end_ingest_date=if (ignore_ingestion_time==FALSE) {time_intervals[i,2]} else {r_to_py(NULL)},
          # start_ingest_date=time_intervals[i,1],
          # end_ingest_date=time_intervals[i,2],
          tile=r_to_py(tile),
          orbit=r_to_py(o),
          apihub=apihub,
          max_cloud=max_cloud,
          list_only=TRUE,
          max_records=0, # TODO this is possible after an addition in Sentinel-download python script:
          # cycle on product requests (one per 100 products) is interrupted after
          # the first request of length 0.
          corr_type=corr_type,
          downloader_path=dirname(binpaths$wget)
        )
      )
      # message(py_output) # do not show the output of the products found
      # as formatted by Hagolle
      py_return
    })
  })
  
  av_prod_list <- lapply(av_prod_tuple, lapply, function(x) {py_to_r(x)[[1]]})
  if (is.character(try(unlist(av_prod_list)))) {
    av_prod_list <- unlist(av_prod_list)
    names(av_prod_list) <- unlist(lapply(av_prod_tuple, lapply, function(x) {py_to_r(x)[[2]]}))
  } else {
    av_prod_list <- character(0)
  }
  
  # filter on tiles
  # (filtering within python code does not take effect with list_only=TRUE)
  # The filter is applied only on compactname products
  # (using sen2r(), a complete filter on tiles is applied after downloading the product;
  # however, s2_download() would correctly download only required tiles)
  
  if (!is.null(tile) & length(av_prod_list)>0) {
    av_prod_tiles <- lapply(names(av_prod_list), function(x) {
      safe_getMetadata(x, info="nameinfo")$id_tile %>%
        ifelse(is.null(.), NA, .) 
    }) %>%
      unlist()
    av_prod_list <- av_prod_list[av_prod_tiles %in% tile | is.na(av_prod_tiles)]
  }
  
  return(av_prod_list)
  
  # TODO add all the checks!
  
}

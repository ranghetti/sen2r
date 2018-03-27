#' @title Retrieve list of available products.
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of
#'  [s2download](https://github.com/ranghetti/s2download)
#'  python function only to retrieve the list of files, without
#'  downloading and correcting them.
#' @param spatial_extent A valid spatial object object of class `sf`,
#'  `sfc` or `sfg`
#' @param tile Single Sentinel-2 Tile string (5-length character)
#' @param orbit Single Sentinel-2 orbit number
#' @param time_interval a temporal vector (class [POSIXct] or
#'  [Date]) of length 1 (specific day) or 2 (time interval).
#' @param level Character vector with one of the following:
#'     - "auto" (default): check if level-2A is available on SciHub:
#'         if so, list it; if not, list the corresponding level-1C
#'         product
#'     - "L1C": list available level-1C products
#'     - "L2A": list available level-2A products
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of scihub account. If NULL (default) the default credentials
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
#' pos <- sp::SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=sp::CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' example_s2_list <- s2_list(spatial_extent=pos, tile="32TQQ", time_interval=time_window)
#' print(example_s2_list)
#' }

s2_list <- function(spatial_extent=NULL, tile=NULL, orbit=NULL, # spatial parameters
                    time_interval=NULL, # temporal parameters
                    level="auto",
                    apihub=NULL,
                    max_cloud=110) {
  
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
      s2tiles_kmz <- system.file("extdata","vector","s2_tiles.kmz",package="sen2r")
      s2tiles_kml <- gsub("\\.kmz$",".kml",s2tiles_kmz)
      if (!file.exists(s2tiles_kml)) {
        unzip(zipfile = s2tiles_kmz,
              files   = basename(s2tiles_kml),
              exdir   = dirname(s2tiles_kml),
              unzip   = "internal")
      }
      s2tiles <- st_read(s2tiles_kml, stringsAsFactors=FALSE, quiet=TRUE)
      # take the the selected tiles as extent
      # (this will result in the selection of more tiles, cause to overlapping 
      # areas; it is filtered in s2_download, but it is slow: FIXME).
      # It is not possible to use tile centroids, because tile of external areas
      # of orbits could not be included).
      spatial_extent <- suppressWarnings(
        s2tiles[s2tiles$Name %in% tile,] #%>%
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
  # convert in format taken by th function
  time_interval <- strftime(time_interval,"%Y%m%d")
  
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
  binpaths_file <- file.path(system.file("extdata",package="sen2r"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("wget" = install_wget())
  }
  
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
    # py_capture_output was added not only to allow threating the python output
    # as R message, but also because, on Windows, the following error was returned
    # if the function was launched outside py_capture_output:
    # Error in py_call_impl(callable, dots$args, dots$keywords) : 
    #   IOError: [Errno 9] Bad file descriptor
    # from a python console, the error did not appear (only inside reticulate).
    py_output <- reticulate::py_capture_output(
      py_return <- s2download$s2_download(
        lat=lat, lon=lon, latmin=latmin, latmax=latmax, lonmin=lonmin, lonmax=lonmax,
        start_date=time_interval[1], end_date=time_interval[2],
        tile=r_to_py(tile),
        orbit=r_to_py(o),
        apihub=apihub,
        max_cloud=max_cloud,
        list_only=TRUE,
        max_records=0, # TODO this is possible after an addition in Sentinel-download python script:
        # cycle on product requests (one per 100 products) is interrupted after
        # the first request of length 0.
        corr_type=corr_type,
        wget_path=dirname(binpaths$wget)
      )
    )
    message(py_output)
    py_return
  })
  
  av_prod_list <- unlist(lapply(av_prod_tuple, function(x) {py_to_r(x)[[1]]}))
  names(av_prod_list) <- unlist(lapply(av_prod_tuple, function(x) {py_to_r(x)[[2]]}))
  
  # filter on tiles
  # (filtering within python code does not take effect with list_only=TRUE)
  # The filter is applied only on compactname products
  # (using sen2r(), a complete filter on tiles is applied after downloading the product;
  # however, s2_download() would correctly download only required tiles)
  
  if (!is.null(tile) & !is.null(av_prod_list)) {
    av_prod_tiles <- lapply(names(av_prod_list), function(x) {
      s2_getMetadata(x, info="nameinfo")$id_tile %>%
        ifelse(is.null(.), NA, .) 
    }) %>%
      unlist()
    av_prod_list <- av_prod_list[av_prod_tiles %in% tile | is.na(av_prod_tiles)]
  }
  
  return(av_prod_list)
  
  # TODO add all the checks!
  
}

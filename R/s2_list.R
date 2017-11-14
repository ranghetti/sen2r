#' @title Retrieve list of available products.
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of
#'  [s2download](https://github.com/ranghetti/s2download)
#'  python function only to retrieve the list of files, without
#'  downloading and correcting them.
#' @param spatial_extent A valid spatial object managed by [sprawl::get_extent]
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
#' @importFrom sprawl get_extent reproj_extent
#' @importFrom magrittr "%>%"
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

  # checks on inputs
  spatext <- get_extent(spatial_extent) %>%
    reproj_extent("+init=epsg:4326")

  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (spatext@extent["xmin"]==spatext@extent["xmax"] | spatext@extent["ymin"]==spatext@extent["ymax"]) {
    lon <- mean(spatext@extent["xmin"], spatext@extent["xmax"])
    lat <- mean(spatext@extent["ymin"], spatext@extent["ymax"])
    lonmin <- lonmax <- latmin <- latmax <- NULL
  } else {
    lonmin <- spatext@extent["xmin"]; lonmax <- spatext@extent["xmax"]
    latmin <- spatext@extent["ymin"]; latmax <- spatext@extent["ymax"]
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
    if (any(is.na(orbit))) {
      orbit <- list(NULL)
    }
  }

  # import s2download
  s2download <- import_s2download(convert=FALSE)

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
    
    s2download$s2_download(
      lat=lat, lon=lon, latmin=latmin, latmax=latmax, lonmin=lonmin, lonmax=lonmax,
      start_date=time_interval[1], end_date=time_interval[2],
      tile=r_to_py(tile),
      orbit=r_to_py(o),
      apihub=apihub,
      max_cloud=max_cloud,
      list_only=TRUE,
      corr_type=corr_type)
  })

  av_prod_list <- unlist(lapply(av_prod_tuple, function(x) {py_to_r(x)[[1]]}))
  names(av_prod_list) <- unlist(lapply(av_prod_tuple, function(x) {py_to_r(x)[[2]]}))

  return(av_prod_list)

  # TODO add all the checks!

}

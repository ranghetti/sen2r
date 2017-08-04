#' @title Retrieve list of available products.
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of s2downoad
#'  python function.
#' @param coords TODO
#' @param extent TODO
#' @param tile TODO
#' @param orbit TODO
#' @param start_date TODO
#' @param end_date TODO
#' @param corr_type TODO
#' @param apihub TODO
#' @param max_cloud TODO
#' @return A vector of available products (being each element an URL,
#'  and its name the product name)
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import py_to_r
#' @importFrom sprawl get_extent
#' @importFrom MODIStsp reproj_bbox
#'
#' @examples \dontrun{
#' pos <- SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' s2_list(spatial_extent=pos, tile="32TQQ",
#'         time_interval=time_window,
#'         apihub=file.path(system.file(package="RSPrePro"),"s2download","apihub.txt"))
#' }
#'

s2_list <- function(spatial_extent=NULL, tile=NULL, orbit=NULL, # spatial parameters
                    time_interval=NULL, # temporal parameters
                    corr_type="auto",
                    apihub=NULL,
                    max_cloud=110) {

  # checks on inputs
  # spatial_extent must be a valid sp object
  # TODO for now, take the bounding box and no controls are added
  extent_extent <- get_extent(spatial_extent)@extent
  # FIXME aggiungi riproiezione dopo aver modificato MODIStsp_reproj_bbox in sprawl

  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (extent_extent["xmin"]==extent_extent["xmax"] | extent_extent["ymin"]==extent_extent["ymax"]) {
    lon <- mean(extent_extent["xmin"], extent_extent["xmax"])
    lat <- mean(extent_extent["ymin"], extent_extent["ymax"])
    lonmin <- lonmax <- latmin <- latmax <- NULL
  } else {
    lonmin <- extent_extent["xmin"]; lonmax <- extent_extent["xmax"]
    latmin <- extent_extent["ymin"]; latmax <- extent_extent["ymax"]
    lon <- lat <- NULL
  }

  # checks on dates
  # time_interval must be a single date or a two length date
  # TODO add checks on format
  if (length(time_interval)==1) {
    time_interval <- rep(time_interval,2)
  }
  # convert in format taken by th function
  time_interval <- strftime(time_interval,"%Y%m%d")

  # import s2download
  s2download <- import_s2download(convert=FALSE)

  apihub <- file.path(s2download$inst_path,"apihub.txt")
  if (!file.exists(apihub)) {
    print_message(type="error","File apihub.txt with the SciHub credentials is missing.") # TODO build it
  }

  # run the research of the list of products
  av_prod_tuple <- s2download$s2_download(
    lat=lat, lon=lon, latmin=latmin, latmax=latmax, lonmin=lonmin, lonmax=lonmax,
    start_date=time_interval[1] ,end_date=time_interval[2],
    orbit=orbit, apihub=apihub, max_cloud=max_cloud,
    tile=tile, list_only=TRUE,
    corr_type="auto")

  av_prod_list <- py_to_r(av_prod_tuple)[[1]]
  names(av_prod_list) <- py_to_r(av_prod_tuple)[[2]]

  return(av_prod_list)

  # TODO add all the checks!

}

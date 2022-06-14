#' @title Download S2 products.
#' @description The function downloads S2 products.
#'  Input filenames must be elements obtained with
#'  [s2_list] function
#'  (each element must be a URL, and the name the product name).
#' @param s2_prodlist Named character: list of the products to be downloaded,
#'  in the format `safelist` (see [safelist-class]).
#'  Alternatively, it can be the path of a JSON file exported by [s2_order].
#' @param downloader Executable to use to download products
#'  (default: "builtin"). Alternatives are "builtin" or "aria2"
#'  (this requires aria2c to be installed).
#' @param apihub Path of the `apihub.txt` file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param service Character: it can be `"dhus"` or `"apihub"`, in which cases
#'  the required service is forced instead that the one present in the URLs
#'  passed through argument `s2_prodlist`.
#'  If NA (default), the service present in the URLs is maintained.
#' @param tile Deprecated argument
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created (default: current directory).
#' @param order_lta Logical: if TRUE (default), products which are not available
#'  for direct download are ordered from the Long Term Archive;
#'  if FALSE, they are simply skipped.
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case of errors during downloads; if FALSE, a warning is shown and
#'  download of subsequent products continues.
#' @param overwrite Logical value: should existing output archives be
#'  overwritten? (default: FALSE)
#' @return Vector character with the list ot the output products
#'  (being downloaded or already existing).
#'
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @author Lorenzo Busetto, phD (2019)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom httr RETRY authenticate progress write_disk
#' @importFrom foreach foreach "%do%"
#' @export
#'
#' @examples
#' \dontrun{
#' single_s2 <- paste0("https://apihub.copernicus.eu/apihub/odata/v1/",
#'   "Products(\'c7142722-42bf-4f93-b8c5-59fd1792c430\')/$value")
#' names(single_s2) <- "S2A_MSIL1C_20170613T101031_N0205_R022_T32TQQ_20170613T101608.SAFE"
#' # (this is equivalent to:
#' # single_s2 <- example_s2_list[1]
#' # where example_s2_list is the output of the example of the
#' # s2_list() function)
#'
#' # Download the whole product
#' s2_download(single_s2, outdir=tempdir())
#'
#' #' # Download the whole product - using aria2
#' s2_download(single_s2, outdir=tempdir(), downloader = "aria2")
#'
#' # Download more products, ordering the ones stored in the Long Term Archive
#' pos <- sf::st_sfc(sf::st_point(c(-57.8815,-51.6954)), crs = 4326)
#' time_window <- as.Date(c("2018-02-21", "2018-03-20"))
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' s2_download(list_safe, outdir=tempdir())
#' }

s2_download <- function(
  s2_prodlist = NULL,
  downloader = "builtin",
  apihub = NA,
  service = NA,
  tile = NULL,
  outdir = ".",
  order_lta = TRUE,
  abort = TRUE,
  overwrite = FALSE
) {
  
  # warn for deprecated arguments
  if (!missing("tile")) {
    warning("argument 'tile' is deprecated and will not be used")
  }
  
  .s2_download(
    s2_prodlist = s2_prodlist,
    downloader = downloader,
    apihub = apihub,
    service = service,
    outdir = outdir,
    order_lta = order_lta,
    abort = abort,
    overwrite = overwrite,
    .s2_availability = NULL
  )
  
}

# internal function, used internally in order not to repeat the check
# for online availability
.s2_download <- function(
  s2_prodlist = NULL,
  downloader = "builtin",
  apihub = NA,
  service = NA,
  outdir = ".",
  order_lta = TRUE,
  abort = TRUE,
  overwrite = FALSE,
  .s2_availability = NULL
) {
  
  # to avoid NOTE on check
  footprint <- NULL
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # exit if empty
  if (length(nn(s2_prodlist)) == 0) {
    return(invisible(NULL))
  }
  
  # check input format
  s2_prodlist <- as(s2_prodlist, "safelist")
  # TODO add input checks
  s2_meta <- safe_getMetadata(s2_prodlist, info = "nameinfo")
  if (!is.null(attr(s2_prodlist, "footprint"))) {
    s2_meta[,footprint:=attr(s2_prodlist, "footprint")]
  }
  
  # check input server
  s2_server <- ifelse(
    grepl("^http.+Products\\(.+\\)/\\$value$", s2_prodlist),
    "scihub",
    ifelse(
      grepl("^gs://gcp-public-data-sentinel-2", s2_prodlist),
      "gcloud",
      "unrecognised"
    )
  )
  
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
  
  # check outdir
  if (!dir.exists(outdir)) {
    print_message(
      type = "error",
      "Output directory does not exist."
    )
  }
  
  # Split products to be downloaded from products to be ordered
  s2_availability <- if (is.null(.s2_availability)) {
    print_message(
      type = "message",
      date = TRUE,
      "Check if products are available for download..."
    )
    safe_is_online(s2_prodlist, verbose = FALSE, apihub = apihub)
  } else {
    .s2_availability
  }
  
  # Split products basing on download method
  s2_toorder <- which(!s2_availability)
  s2_todownload_scihub <- which(s2_availability & s2_server == "scihub")
  s2_todownload_gcloud <- which(s2_server == "gcloud")
  
  # Order products stored from the Long Term Archive
  if (order_lta == TRUE) {
    ordered_products <- .s2_order(
      s2_prodlist[s2_toorder], 
      .s2_availability = s2_availability[s2_toorder], 
      service = service,
      apihub = apihub
    )
  }
  
  ## Download products available for download on SciHub
  safe_prodlist_scihub <- if (length(s2_todownload_scihub) > 0) {
    .s2_download_scihub(
      s2_prodlist = s2_prodlist[s2_todownload_scihub], 
      s2_meta = s2_meta[s2_todownload_scihub,],
      outdir = outdir, 
      apihub = apihub, 
      service = service,
      downloader = downloader, 
      abort = abort,
      overwrite = overwrite
    )
  } else {character(0)}
  
  ## Download products available for download on GCloud
  safe_prodlist_gcloud <- if (length(s2_todownload_gcloud) > 0) {
    .s2_download_gcloud(
      s2_prodlist = s2_prodlist[s2_todownload_gcloud],
      s2_meta = s2_meta[s2_todownload_gcloud,],
      outdir = outdir,
      overwrite = overwrite
    )
  } else {character(0)}
  
  safe_prodlist <- as(
    c(safe_prodlist_scihub, safe_prodlist_gcloud)[
      order(c(s2_todownload_scihub, s2_todownload_gcloud))
    ],
    "safelist"
  )
  
  return(safe_prodlist)
  
}

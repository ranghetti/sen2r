#' @title Apply .
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of s2downoad
#'  python function only to retrieve the list of files, without
#'  downloading and correcting them.
#' @param l1c_prodlist TODO
#' @param l1c_dir TODO
#' @param out_dir TODO
#' @param n_procs TODO
#' @param overwrite TODO
#' @return A vector of available products (being each element an URL,
#'  and its name the product name)
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate import
#'
#' @examples \dontrun{
#' pos <- sp::SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=sp::CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' example_s2_list <- s2_list(spatial_extent=pos, tile="32TQQ", time_interval=time_window)
#' s2_download(example_s2_list, out_dir=tempdir())
#' s2_sen2cor(names(example_s2_list)[1], l1c_dir=tempdir(), out_dir=tempdir())
#'
#' }
#'

s2_sen2cor <- function(l1c_prodlist=NULL, l1c_dir=NULL, out_dir=NULL, n_procs=1, overwrite=FALSE) {

  # import s2download
  s2download <- import_s2download(convert=FALSE)

  # if l1c_dir is defined, append to product names
  if (!is.null(l1c_dir)) {
    l1c_prodlist <- file.path(l1c_dir,l1c_prodlist)
  }

  # check that all products are in the same directory (FIXME allow differents <- need to change call_sen2cor)
  if (length(unique(dirname(l1c_prodlist)))>1) {
    print_message(
      type="error",
      "All input products must be in the same folder (this will be fixed to allow different ones).")
  }

  # TODO check that all input names are L12C
  # (to do it, edit s2_getMetadata() in order to accept also prod naems without files)

  s2download$call_sen2cor(l1c_dir=unique(dirname(l1c_prodlist)),
                          l2a_dir=out_dir,
                          l1c_list=basename(l1c_prodlist),
                          n_procs=n_procs,
                          overwrite=overwrite)

}

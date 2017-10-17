#' @title Correct L1C products using sen2cor
#' @description The function uses sen2cor from the docker to manually correct L1C products.
#' @param l1c_prodlist List of L1C product names to be corrected. They can be both
#'  product names with full/relative path or only names of SAFE products (in this case, also
#'  l1c_dir argument must be provided). SAFE products must be unzipped.
#'  Note that, at this stage, all products must be in the same directory (this will be fixed).
#' @param l1c_dir Full or relative path of input L1C products.
#' @param outdir Directory where output L2A products will be placed.
#' @param n_procs Number of processors (`integer`) to use (default is 1, single processor).
#' @param overwrite Logical value: should existing output L2A products be overwritten?
#'  (default: FALSE)
#' @return Vector character with the list ot the output products (being corrected or already
#'  existing)
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#'
#' @examples \dontrun{
#' pos <- sp::SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=sp::CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' example_s2_list <- s2_list(spatial_extent=pos, tile="32TQQ", time_interval=time_window)
#' s2_download(example_s2_list, outdir=tempdir())
#' s2_sen2cor(names(example_s2_list)[1], l1c_dir=tempdir(), outdir=tempdir())
#' }

s2_sen2cor <- function(l1c_prodlist=NULL, l1c_dir=NULL, outdir=NULL, n_procs=1, overwrite=FALSE) {

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
                          l2a_dir=outdir,
                          l1c_list=basename(l1c_prodlist),
                          n_procs=as.integer(n_procs),
                          overwrite=overwrite)

  # return the names of corrected products (or already existing products)
  expected_l2a_prodlist <- file.path(
    outdir,
    gsub("^S2([AB])\\_MSIL1C\\_","S2\\1\\_MSIL2A\\_",basename(l1c_prodlist))
  )
  existing_l2a_prodlist <- expected_l2a_prodlist[file.exists(expected_l2a_prodlist)]
  return(existing_l2a_prodlist)

}

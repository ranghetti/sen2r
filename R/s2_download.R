#' @title Download S2 products.
#' @description The function downloads a single S2 product.
#'  Input filename must be an element obtained with s2_list() function
#'  (the content must be a URL, and the name the product name).
#' @param filename TODO
#' @param downloader TODO
#' @param apihub TODO
#' @param tile TODO
#' @param write_dir TODO
#' @return A vector of available products (being each element an URL,
#'  and its name the product name)
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#'
#' @examples \dontrun{
#' pos <- SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' s2_list(spatial_extent=pos, tile="32TQQ",
#'         time_interval=time_window,
#'         apihub=file.path(system.file(package="RSPrePro"),"s2download","apihub.txt"))
#' }
#'

s2_download <- function(filename=NULL,
                        downloader="wget",
                        apihub=NULL,
                        tile=NULL,
                        write_dir=".") {

  # import s2download
  s2download <- import_s2download(convert=FALSE)

  # TODO add checks on the format of filename (one element output of s2_list)

  # link to apihub
  if (is.null(apihub)) {
    apihub <- file.path(s2download$inst_path,"apihub.txt")
  }
  if (!file.exists(apihub)) {
    print_message(type="error","File apihub.txt with the SciHub credentials is missing.") # TODO build it
  }

  s2download$download_s2product(filename=names(filename),
                                link=filename,
                                downloader=downloader,
                                apihub=apihub,
                                tile=NULL,
                                no_download=FALSE,
                                write_dir='.',
                                file_list=NULL)

  return(NULL)

}

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
#' single_s2 <- "https://scihub.copernicus.eu/apihub/odata/v1/Products('c7142722-42bf-4f93-b8c5-59fd1792c430')/\\$value"
#' names(single_s2) <- "S2A_MSIL1C_20170613T101031_N0205_R022_T32TQQ_20170613T101608.SAFE"
#' # (this is equivalent to:
#' # single_s2 <- example_s2_list[1]
#' # where example_s2_list is the output of the example of the
#' # s2_list() function)
#'
#' # Download the whole product
#' s2_download(single_s2, write_dir=tempdir()')
#'
#' # Download a specific tile
#' s2_download(single_s2, tile="32TQQ", write_dir=tempdir()')
#' # (for products with compact names, the two produce equivalent
#' # results, while the forst downloads a SAFE archive, the second
#' # downloades single product files)
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

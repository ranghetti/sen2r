#' @title Download S2 products.
#' @description The function downloads a single S2 product.
#'  Input filename must be an element obtained with
#'  [s2_list] function
#'  (the content must be a URL, and the name the product name).
#' @param s2_prodlist List of the products to be downloaded
#'  (this must be the output of [s2_list] function).
#' @param downloader Executable to use to download products
#'  (default: "wwget").
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of scihub account. If NULL (default) the default credentials
#'  (username "user", password "user") will be used.
#' @param tile Single Sentinel-2 Tile string (5-length character)
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created (default: current directory).
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate r_to_py
#' @export
#'
#' @examples \dontrun{
#' single_s2 <- paste0("https://scihub.copernicus.eu/apihub/odata/v1/",
#'   "Products(\'c7142722-42bf-4f93-b8c5-59fd1792c430\')/\\$value")
#' names(single_s2) <- "S2A_MSIL1C_20170613T101031_N0205_R022_T32TQQ_20170613T101608.SAFE"
#' # (this is equivalent to:
#' # single_s2 <- example_s2_list[1]
#' # where example_s2_list is the output of the example of the
#' # s2_list() function)
#'
#' # Download the whole product
#' s2_download(single_s2, outdir=tempdir())
#'
#' # Download a specific tile
#' s2_download(single_s2, tile="32TQQ", outdir=tempdir())
#' # (for products with compact names, the two above commands produce equivalent
#' # results: the first one downloads a SAFE archive, while the second one
#' # downloads single product files)
#'
#' # Download a serie of products
#' pos <- sp::SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=sp::CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' example_s2_list <- s2_list(spatial_extent=pos, tile="32TQQ", time_interval=time_window)
#' s2_download(example_s2_list, outdir=tempdir())
#' }

s2_download <- function(s2_prodlist=NULL,
                        downloader="wget",
                        apihub=NULL,
                        tile=NULL,
                        outdir=".") {

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

  for (i in 1:length(s2_prodlist)) {
    
    link <- s2_prodlist[i]
    filename <- names(s2_prodlist[i])
    # download archive for compactname products
    if (s2_getMetadata(filename, "nameinfo")$version=="compact") {
      py_tile <- r_to_py(NULL)
      unzip_tile <- TRUE
    } else {
      py_tile <- r_to_py(tile)
      unzip_tile <- FALSE
    }

    print_message(
      type = "message",
      date = TRUE,
      "Downloading product ",i," of ",length(s2_prodlist),
      " (",filename,")..."
    )
    
    trace_function(
      s2download$download_s2product,
      filename      = filename,
      link          = link,
      downloader    = downloader,
      apihub        = apihub,
      tile          = py_tile,
      no_download   = FALSE,
      write_dir     = outdir,
      file_list     = NULL,
      trace_funname = "s2download",
      trace_files   = file.path(outdir,c(filename,paste0(filename,".zip")))
    )
    # s2download$download_s2product(filename=filename,
    #                               link=link,
    #                               downloader=downloader,
    #                               apihub=apihub,
    #                               tile=py_tile,
    #                               no_download=FALSE,
    #                               write_dir=outdir,
    #                               file_list=NULL)

  }

  return(NULL)

}

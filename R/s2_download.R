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
#' @param overwrite Logical value: should existing output archives be
#'  overwritten? (default: FALSE)
#' @return Vector character with the list ot the output products
#'  (being downloaded or already existing).
#'
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @author Lorenzo Busetto, phD (2019) \email{lbusett@@gmail.com}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom httr RETRY authenticate progress write_disk
#' @importFrom foreach foreach "%do%"
#' @export
#'
#' @examples
#' \dontrun{
#' single_s2 <- paste0("https://scihub.copernicus.eu/apihub/odata/v1/",
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
  overwrite = FALSE,
  .s2_availability = NULL
) {
  
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
  
  # Order products stored from the Long Term Archive
  if (order_lta == TRUE) {
    ordered_products <- .s2_order(
      s2_prodlist, 
      .s2_availability = s2_availability, 
      apihub = apihub
    )
  }
  
  # Split products basing on download method
  s2_todownload_scihub <- which(s2_availability & s2_server == "scihub")
  s2_todownload_gcloud <- which(s2_server == "gcloud")
  
  ## Download products available for download on SciHub
  safe_prodlist <- .s2_download_scihub(
    s2_prodlist = s2_prodlist[s2_todownload_scihub], 
    s2_meta = s2_meta[s2_todownload_scihub,],
    outdir = outdir, 
    apihub = apihub, 
    service = service,
    downloader = downloader, 
    overwrite = overwrite
  )
  
  ## Download products available for download on GCloud
  if (length(s2_todownload_gcloud) > 0) {
    if (eval(parse(text = 'requireNamespace("sen2r.extras", quietly = TRUE)'))) {
      safe_prodlist_gcloud <- eval(parse(text = paste0(
        "sen2r.extras::.s2_download_gcloud(",
        "  s2_prodlist = s2_prodlist[s2_todownload_gcloud],",
        "  s2_meta = s2_meta[s2_todownload_gcloud,],",
        "  outdir = outdir,",
        "  overwrite = overwrite",
        ")"
      )))
      safe_prodlist <- as(c(safe_prodlist, safe_prodlist_gcloud), "safelist")
    }
  }
  
  
  return(safe_prodlist)
  
}

# internal function with the "core" download method from SciHub
.s2_download_scihub <- function(
  s2_prodlist, s2_meta, outdir, apihub, service, downloader, overwrite
) {
  
  # to avoid NOTE on check
  i <- mission <- level <- sensing_datetime <- id_orbit <- id_tile <- NULL
  
  # read credentials
  if (length(s2_prodlist) > 0) {
    creds <- read_scihub_login(apihub)
  }
  
  # check the used service
  if (!service %in% c("apihub", "dhus", NA)) {
    print_message(
      type = "error",
      "Argument 'service' can be only \"apihub\" or \"dhus\"; ",
      "leaving the input URLs as are."
    )
  } else if (!is.na(service)) {
    s2_prodlist <- gsub(
      "^https://scihub.copernicus.eu/((apihub)|(dhus))/odata",
      paste0("https://scihub.copernicus.eu/",service,"/odata"),
      s2_prodlist
    )
  }
  
  foreach(
    i = seq_along(s2_prodlist), 
    .combine = c
  ) %do% {
    
    link <- s2_prodlist[i]
    zip_path <- file.path(outdir, paste0(names(s2_prodlist[i]),".zip"))
    safe_path <- gsub("\\.zip$", "", zip_path)
    
    # regular expression to detect if equivalent products already exist
    safe_regex <- s2_meta[i,paste0(
      "^S",mission,"\\_MSIL",level,"\\_",strftime(sensing_datetime,"%Y%m%dT%H%M%S"),
      "\\_N[0-9]{4}\\_R",id_orbit,"\\_T",id_tile,"\\_[0-9]{8}T[0-9]{6}\\.SAFE$"
    )]
    safe_existing <- list.files(dirname(zip_path), safe_regex, full.names = TRUE)
    
    if (any(overwrite == TRUE, length(safe_existing) == 0)) {
      
      print_message(
        type = "message",
        date = TRUE,
        "Downloading Sentinel-2 image ", i,
        " of ",length(s2_prodlist)," (",basename(safe_path),")..."
      )
      
      if (downloader %in% c("builtin", "wget")) { # wget left for compatibility
        
        out_bar <- if (inherits(stdout(), "terminal")) {
          NULL
        } else {
          file(out_bar_path <- tempfile(), open = "a")
        }
        download <- RETRY(
          verb = "GET",
          url = as.character(link),
          config = authenticate(creds[1], creds[2]),
          times = 5, pause_cap = 8,
          progress(con = if (length(out_bar) > 0) {out_bar} else {stdout()}),
          write_disk(zip_path, overwrite = TRUE)
        )
        if (length(out_bar) > 0) {
          close(out_bar)
          invisible(file.remove(out_bar_path))
        }
      } else if (grepl("^aria2c?$", downloader)) {
        
        binpaths <- load_binpaths("aria2")
        if (Sys.info()["sysname"] != "Windows") {
          link_aria <- gsub("/\\$value", "/\\\\$value", link)
        } else {
          link_aria <- link
        }
        aria_string <- paste0(
          binpaths$aria2c, " -x 2 --check-certificate=false -d ",
          dirname(zip_path),
          " -o ", basename(zip_path),
          " ", "\"", as.character(link_aria), "\"",
          " --allow-overwrite --file-allocation=none --retry-wait=2",
          " --http-user=", "\"", creds[1], "\"",
          " --http-passwd=", "\"", creds[2], "\"",
          " --max-tries=10"
        )
        download <- try({
          system(aria_string, intern = Sys.info()["sysname"] == "Windows")
        })
        
      }
      
      # check if the user asked to download a LTA product
      download_is_lta <- if (inherits(download, "response")) {
        download$status_code == 202
      } else if (inherits(download, "integer")) {
        download == 22
      } else FALSE
      if (download_is_lta) {
        # TODO
      }
      
      if (inherits(download, "try-error")) {
        suppressWarnings(file.remove(zip_path))
        suppressWarnings(file.remove(paste0(zip_path,".aria2")))
        print_message(
          type = "error",
          "Download of file", link, "failed more than 10 times. ",
          "Internet connection or SciHub may be down."
        )
      } else {
        # check md5
        check_md5 <- tryCatch({
          requireNamespace("tools")
          sel_md5 <- RETRY(
            verb = "GET",
            url = gsub("\\$value$", "Checksum/Value/$value", as.character(link)),
            config = authenticate(creds[1], creds[2]),
            write_disk(md5file <- tempfile(), overwrite = TRUE)
          )
          md5 <- toupper(readLines(md5file, warn = FALSE)) == 
            toupper(tools::md5sum(zip_path))
          file.remove(md5file)
          md5
        }, error = function(e) {logical(0)})
        if (any(!check_md5 %in% c(TRUE, FALSE), length(check_md5) == 0)) {
          print_message(
            type = "warning",
            "File ", names(link), " cannot be checked",
            if (!requireNamespace("tools", quietly = TRUE)) {
              "(package \"tools\" needs to be installed)"
            },
            ". Please verify if the download was successful."
          )
        } else if (!check_md5) {
          file.remove(zip_path)
          print_message(
            type = "error",
            "Download of file ", names(link), " was incomplete (Md5sum check failed). ",
            "Please retry to launch the download."
          )
        }
        # remove existing SAFE
        if (dir.exists(safe_path)) {
          unlink(safe_path, recursive = TRUE)
        }
        # update SAFE name (possible different processing date)
        zip_content <- unzip(zip_path, list = TRUE)
        safe_newname <- unique(gsub("(^[^\\/]+)\\/.*$", "\\1", zip_content$Name))
        # unzip
        unzip(zip_path, exdir = dirname(zip_path))
        file.remove(zip_path)
      }
      
    } else {
      
      print_message(
        type = "message",
        date = TRUE,
        "Skipping Sentinel-2 image ", i,
        " of ",length(s2_prodlist)," ",
        "since the corresponding folder already exists."
      )
      
      safe_existing_meta <- safe_getMetadata(safe_existing, info = "nameinfo")
      safe_newname <- safe_existing_meta$name[
        order(nn(safe_existing_meta$creation_datetime), decreasing = TRUE)[1]
        ]
      
    }
    
    # return to foreach
    as(setNames(link, safe_newname), "safelist")
    
  }
}

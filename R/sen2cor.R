#' @title Correct L1C products using sen2cor
#' @description The function uses sen2cor to manually correct L1C products.
#'  Standalone version of 
#'  [sen2cor 2.4.0](http://step.esa.int/main/third-party-plugins-2/sen2cor)
#'  is used.
#' @param l1c_prodlist List of L1C product names to be corrected. They can be both
#'  product names with full/relative path or only names of SAFE products (in this case, also
#'  l1c_dir argument must be provided). SAFE products must be unzipped.
#'  Note that, at this stage, all products must be in the same directory (this will be fixed).
#' @param l1c_dir Full or relative path of input L1C products.
#'  If NULL (default), `l1c_prodlist` must already be a vector of full paths.
#' @param outdir Directory where output L2A products will be placed.
#'  If NULL (default), each product is left in the parent directory of `l1c_prodlist`.
#' @param proc_dir (optional) Directory where processing is applied.
#'  If NA (default), processing is done in `l1c_dir` and output L2A product is 
#'  then moved to `outdir`, unless `l1c_dir` is a subdirectory of a SAMBA mountpoint under Linux:
#'  in this case, L1C input products are copied in a temporary directory, 
#'  processing is done there and then L2A is moved to `outdir`. 
#'  This is required under Linux systems when `l1c_dir` is a subdirectory of
#'  a unit mounted with SAMBA, otherwise sen2cor would produce empty L2A products.
#' @param n_procs Number of processors (`integer`) to use (default is 1, single processor).
#' @param overwrite Logical value: should existing output L2A products be overwritten?
#'  (default: FALSE)
#' @return Vector character with the list ot the output products (being corrected or already
#'  existing)
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom parallel detectCores makeCluster stopCluster
#' @export
#'
#' @examples \dontrun{
#' pos <- sp::SpatialPoints(data.frame("x"=12.0,"y"=44.8), proj4string=sp::CRS("+init=epsg:4326"))
#' time_window <- as.Date(c("2017-05-01","2017-07-30"))
#' example_s2_list <- s2_list(spatial_extent=pos, tile="32TQQ", time_interval=time_window)
#' s2_download(example_s2_list, outdir=tempdir())
#' sen2cor(names(example_s2_list)[1], l1c_dir=tempdir(), outdir=tempdir())
#' }

sen2cor <- function(l1c_prodlist=NULL, l1c_dir=NULL, outdir=NULL, proc_dir=NA, n_procs=1, overwrite=FALSE) {
  
  # install sen2cor is not already done
  install_sen2cor() %>% suppressMessages()
  
  # load sen2cor executable path
  sen2cor_bin <- fromJSON(
    file.path(
      system.file("extdata",package="fidolasen"),
      "sen2cor_path.json"
    )
  )$sen2cor
  
  # if l1c_dir is defined, append to product names
  if (!is.null(l1c_dir)) {
    l1c_prodlist <- file.path(l1c_dir,l1c_prodlist)
  }
  
  # # check that all products are in the same directory (FIXME allow differents <- need to change call_sen2cor)
  # if (length(unique(dirname(l1c_prodlist)))>1) {
  #   print_message(
  #     type="error",
  #     "All input products must be in the same folder (this will be fixed to allow different ones).")
  # }
  
  # check that proc_dir is not on a mountpoint
  if (!is.na(proc_dir)) {
    if (Sys.info()["sysname"] != "Windows" & 
        !is.null(mountpoint(proc_dir, "cifs"))) {
      print_message(
        type = "warning",
        proc_dir, "is on a SAMBA mounted unit, so it will not be used."
      )
      proc_dir <- NULL
    }
  }
  
  # accept only input names which are L1C
  l1c_prodlist_level <- sapply(l1c_prodlist, function(x) {
    tryCatch(
      s2_getMetadata(x, info = "level"),
      error = function(y){"wrong"}
    )
  })
  l1c_prodlist <- l1c_prodlist[l1c_prodlist_level=="1C"]
  
  # if no products were found, exit
  if (length(l1c_prodlist) == 0) {
    print_message(
      type = "warning",
      "No valid L1C products were found,"
    )
    return(NULL)
  }
  
  ## Cycle on each file
  # if parallel==TRUE, use doParallel
  if (n_procs>1) {
    `%DO%` <- `%dopar%`
    n_cores <- min(parallel::detectCores()-1, length(l1c_prodlist), 7) # use at most 7 cores
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
  } else {
    `%DO%` <- `%do%`
  }
  
  # cycle on eacjh product
  l2a_prodlist <- c()
  foreach(i=seq_along(l1c_prodlist), 
          .combine=c,
          .export = "mountpoint",
          .packages='fidolasen') %DO% {
    
    sel_l1c <- l1c_prodlist[i]
    sel_l2a <- file.path(
      if (is.null(outdir)) {dirname(sel_l1c)} else {outdir},
      gsub("^S2([AB])\\_MSIL1C\\_","S2\\1_MSIL2A_",basename(sel_l1c))
    ) # path of the L2A product where it should be placed definitively
    
    # proceed only if overwrite==TRUE, or if file does not exist
    if (overwrite==TRUE | !file.exists(sel_l2a)) {
      
      # if output exists (and overwrite==TRUE), delete it
      unlink(sel_l2a)
      
      # if sel_l1c is on a SAMBA mountpoint, set proc_dir to a temporary directory
      sel_proc_dir <- proc_dir
      if (is.na(proc_dir)) {
        if (Sys.info()["sysname"] != "Windows" & 
            !is.null(mountpoint(sel_l1c, "cifs"))) {
          sel_proc_dir <- tempdir()
        }
      }
      
      # if proc_dir is [manually or automatically] set, copy sel_l1c
      if (!is.na(sel_proc_dir)) {
        dir.create(sel_proc_dir, recursive=FALSE, showWarnings=FALSE)
        file.copy(sel_l1c, sel_proc_dir, recursive=TRUE)
        sel_l1c <- file.path(sel_proc_dir, basename(sel_l1c))
      }
      
      # apply sen2cor
      system(
        paste(sen2cor_bin, sel_l1c),
        intern = Sys.info()["sysname"] == "Windows"
      )
      sen2cor_out_l2a <- file.path(
        dirname(sel_l1c),
        gsub("^S2([AB])\\_MSIL1C\\_","S2\\1_MSIL2A_",basename(sel_l1c))
      ) # path of the L2A product where it is placed by sen2cor
      
      # move output to the required output directory
      if (sen2cor_out_l2a != sel_l2a) {
        file.copy(sen2cor_out_l2a, sel_l2a, recursive=TRUE)
        file.remove(sen2cor_out_l2a, recursive=TRUE)
      }
      
    } # end IF cycle on overwrite

    l2a_prodlist <- c(l2a_prodlist, sel_l2a)
    
  } # end cycle on each product
  
  if (n_procs>1) {
    stopCluster(cl)
  }
  
  return(l2a_prodlist)
  
}

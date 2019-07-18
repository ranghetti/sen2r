#' @title Parallel masking over SR products
#' @description Internal function to apply cloud mask to SR products.
#'  It is used inside `s2_mask` only on multiband virtual products
#'  (because if the output is a physical file there is no gain in using
#'  this procedure).
#' @param in_rast Input raster Stack.
#' @param in_mask Input Raster to be used as mask (pixesl with values 1 are not
#'  masked, pixels with values 0 are masked).
#' @param outpath Path of the output file.
#' @param tmpdir (optional) Path where intermediate VRT will be created.
#'  Default is in a temporary directory.
#' @param binpaths list of paths of binaries.
#' @param NAflag (optional) NA value to be used where mask is applied.
#' @param parallel (optional) Logical: if TRUE, masking is conducted using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#' @param minrows (optional) Argument to be passed to [raster::blockSize].
#' @param datatype (optional) data type of the output raster.
#' @param overwrite (optional) Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `s2_mask()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `s2_mask()`).
#' @return NULL
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom raster blockSize getValues nlayers raster writeStart writeStop writeValues
#' @importFrom foreach foreach "%dopar%" "%do%"
#' @author Lorenzo Busetto, phD (2018) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

maskapply_parallel <- function(in_rast, 
                               in_mask, 
                               outpath, 
                               tmpdir = NA,
                               binpaths,
                               NAflag = 0, 
                               parallel = TRUE, 
                               minrows = NULL, 
                               datatype = "INT2S",
                               overwrite = FALSE,
                               .log_message=NA,
                               .log_output=NA) {

  # to avoid NOTE on check
  i <- NULL
  
  #this function applies a mask by multiplying the mask with the input raster
  # processing is done by chinks of lines. Changing minrows affects the dimensions
  # of "chunks" and affects (slightly) speed, and majorly memory footprint
  maskapply <- function(x, y, na, out_file = '', n_cores, datatype, minrows = NULL, ...) {
    
    out <- raster(x)
    out <- writeStart(out, out_file, NAflag=na, ...)
    if (is.null(minrows)) {
      bs <- blockSize(out)
    } else {
      bs <- blockSize(out, minrows = minrows)
    }
    for (i in seq_len(bs$n)) {
      # message("Processing chunk ", i, " of ", bs$n)
      v   <- getValues(x, row = bs$row[i], nrows = bs$nrows[i])
      m   <- getValues(y, row = bs$row[i], nrows = bs$nrows[i])
      out <- writeValues(out, m*v+(1-m)*na, bs$row[i])
    }
    out <- writeStop(out)
    return(out)
  }
  
  # Skip if outpath already exists and overwrite is FALSE
  if (file.exists(outpath) & overwrite==FALSE) {
    print_message(
      type="warning",
      "\"",basename(outpath),"\" already exists; skipping."
    )
    return(invisible(NULL))
  }
  
  # This cycles in foreach over each band of the input raster and applies tha mask band by band
  # Va verificato i lfunzionamentento della keyword "datataype" !!!
  m  <- in_mask
  
  # Compute n_cores
  n_cores <- if (is.numeric(parallel)) {
    min(as.integer(parallel), nlayers(in_rast))
  } else if (parallel==FALSE) {
    1
  } else {
    min(parallel::detectCores()-1, nlayers(in_rast), 11) # use at most 11 cores
  }
  if (n_cores<=1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
  }
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="maskapply_")
  }
  dir.create(tmpdir, showWarnings=FALSE)
  
  #NOTE: FORK is more efficient on Linux because it does not make copies;
  # on windows we should use "PSOCK".
  if (n_cores > 1) {
    cl <- makeCluster(
      n_cores, 
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )
    registerDoParallel(cl)
  }
  out_paths <- foreach(i = seq_len(nlayers(in_rast)), .packages = c("raster"), .combine=c)  %DO% {
    
    # redirect to log files
    if (n_cores > 1) {
      if (!is.na(.log_output)) {
        sink(.log_output, split = TRUE, type = "output", append = TRUE)
      }
      if (!is.na(.log_message)) {
        logfile_message = file(.log_message, open = "a")
        sink(logfile_message, type="message")
      }
    }
    
    # run code
    out_path <- file.path(tmpdir, paste0(basename(tempfile(pattern = "maskapply_")), "_b" , i, ".tif"))
    r <- in_rast[[i]]
    s <- maskapply(r, m, 
                   na = NAflag,
                   out_file = out_path,
                   n_cores,  datatype, minrows, 
                   format = 'GTiff', overwrite = TRUE, options = c("COMPRESS=LZW"))
    
    # stop sinking
    if (n_cores > 1) {
      if (!is.na(.log_output)) {
        sink(type = "output")
      }
      if (!is.na(.log_message)) {
        sink(type = "message"); close(logfile_message)
      }
    }
    
    out_path
    
  } # end of FOREACH cycle
  if (n_cores > 1) {
    stopCluster(cl)
  }
  
  # write output VRT
  system(
    paste0(
      binpaths$gdalbuildvrt," ",
      # "-vrtnodata ",NAflag," ",
      " -separate ",
      "\"",outpath,"\" ",
      paste(paste0("\"",out_paths,"\""), collapse=" ")
    ),
    intern = Sys.info()["sysname"] == "Windows"
  )
  
  return(invisible(NULL))

}
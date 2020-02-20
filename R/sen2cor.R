#' @title Correct L1C products using Sen2Cor
#' @description The function uses Sen2Cor to manually correct L1C products.
#'  Standalone version of
#'  [sen2cor](http://step.esa.int/main/third-party-plugins-2/sen2cor)
#'  (version 2.8.0 or 2.5.5) is used.
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
#'  in this case, L1C input products are copied in a temporary directory
#'  (specified with argument `tmpdir`),
#'  processing is done there and then L2A is moved to `outdir`.
#'  This is required under Linux systems when `l1c_dir` is a subdirectory of
#'  a unit mounted with SAMBA, otherwise Sen2Cor would produce empty L2A products.
#' @param tmpdir (optional) Path where processing is performed if a temporary
#'  working directory is required (see argument `proc_dir`). Be sure `tmpdir`
#'  not to be a SAMBA mountpoint under Linux.
#'  Default is a temporary directory.
#'  If `tmpdir` is a non-empty folder, a random subdirectory will be used.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param gipp (optional) Ground Image Processing Parameters (GIPP)
#'  to be passed to Sen2Cor.
#'  It is possible to specify both the path of an existing XML file 
#'  or a list of parameters in the form `parameter_name = "value"`, where 
#'  `parameter_name` is the name of the parameter as specified in the 
#'  XML file (case insensitive), and `"value"` is the character value 
#'  which the user wants to set 
#'  (notice that, in the case the user wants to specify the value `NONE`,
#'  both `"NONE"` and `NA` can be used, but not `NULL`, which has the effect
#'  to maintain the value specified in the XML file).
#'  
#'  For details about the GIPP parameters, refer to the Sen2Cor documentation
#'  (v. [2.5.5](http://step.esa.int/main/third-party-plugins-2/sen2cor/sen2cor_v2-5-5/) 
#'  or [2.8.0](http://step.esa.int/main/third-party-plugins-2/sen2cor/sen2cor_v2-8/):
#'  see the "Schemas of the GIPP file" at the end of each page).
#'  _Note_: this argument takes effect only in the current execution of 
#'  `sen2cor()` function.
#' @param use_dem (optional) Logical: 
#'  if TRUE, Sen2Cor is set to use a Digital Elevation Model for topographic 
#'  correction (reflecting what is done for Level-2A SAFE images provided by ESA Hub);
#'  if FALSE, it is set not to perform topographic correction (reflecting the 
#'  current default Sen2Cor behaviour);
#'  if NA (default), the option set in the XML GIPP configuration file 
#'  used by sen2r (stored in the default sen2r settings directory) is respected;
#'  in case the user never edited it,
#'  the current default setting is not to perform topographic correction.
#'  
#'  _Notes_: 
#'  1. if TRUE, the path used to read or store DEM files 
#'      and the online source used to download missing DEM tiles
#'      are respectively the `DEM_Directory` and `DEM_Reference` parameters
#'      set in the default sen2r GIPP XML file (the user can read them
#'      with the function `read_gipp(c("DEM_Directory", "DEM_Reference"))`).
#'      In case one or both these parameters were set to `"NONE"`, 
#'      a subdirectory `"srtm90"` of the default sen2r directory is used as
#'      DEM directory, and/or the [CGIAR SRTM 90m](http://srtm.csi.cgiar.org/) 
#'      is set as online source.
#'      To set another directory or reference, use argument `gipp` in the form
#'      `gipp = list(DEM_Directory = tempdir(), DEM_Reference ="another_reference", ...)`
#'      (replacing `tempdir()` with the desired path and specifying the online resource).
#'  2. Currently the default value is NA in order to grant backward 
#'      compatibility. In a future release of sen2r, the default value will be
#'      set to TRUE, so to grant homogeneity between Level-2A products downloaded
#'      from ESA Hub and generated using Sen2Cor.
#' @param tiles Vector of Sentinel-2 Tile strings (5-length character) to be
#'  processed (default: process all the tiles found in the input L1C products).
#' @param parallel (optional) Logical: if TRUE, Sen2Cor instances are launched
#'  in parallel using multiple cores; if FALSE (default), they are launched in
#'  series on a single core.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#' @param overwrite Logical value: should existing output L2A products be overwritten?
#'  (default: FALSE)
#' @param kill_errored Logical: experimental feature allowing killing dead Sen2Cor
#'  processes, so leaving `sen2cor()` continuing processing on the remaining 
#'  products. Set to TRUE to activate it (default is FALSE).
#'  This experimental feature is available only on Unix systems.
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `sen2r()`).
#' @return Vector character with the list ot the output products
#'  (being corrected or already existing).
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom tools pskill
#' @export
#'
#' @examples
#' \dontrun{
#' # Download an L1C SAFE product
#' example_s2_list <- s2_list(
#'   spatial_extent = st_sfc(st_point(c(12.0, 44.8)), crs=st_crs(4326)), 
#'   tile = "32TQQ", 
#'   time_interval = as.Date(c("2017-05-01","2017-07-30"))
#' )
#' s2_download(example_s2_list, outdir = tempdir())
#' 
#' # Correct it applying a topographic correction
#' sen2cor(
#'   names(example_s2_list)[1], 
#'   l1c_dir = tempdir(), 
#'   outdir = tempdir(),
#'   use_dem = TRUE
#' )
#' }

sen2cor <- function(
  l1c_prodlist = NULL, 
  l1c_dir = NULL, 
  outdir = NULL, 
  proc_dir = NA,
  tmpdir = NA, 
  rmtmp = TRUE,
  gipp = NULL,
  use_dem = NA,
  tiles = NULL, 
  parallel = FALSE, 
  overwrite = FALSE,
  kill_errored = FALSE,
  .log_message = NA, 
  .log_output = NA
) {
  
  # to avoid NOTE on check
  i <- creation_datetime <- id_baseline <- path <- name <- NULL
  
  # load Sen2Cor executable path
  binpaths <- tryCatch(
    load_binpaths("sen2cor"),
    warning = stop
  )
  
  # Use a copy of the default GIPP XML, setting parameters properly
  gipp_curr_path <- tempfile(pattern = "L2A_GIPP_", fileext = ".xml")
  set_gipp(gipp = gipp, gipp_path = gipp_curr_path, use_dem = use_dem)
  
  # get version
  sen2cor_version_raw0 <- system(paste(binpaths$sen2cor, "-h"), intern = TRUE)
  sen2cor_version_raw1 <- sen2cor_version_raw0[grep(
    "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version:",
    sen2cor_version_raw0
  )]
  sen2cor_version <- package_version(gsub(
    "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version: ([2-9]+\\.[0-9]+\\.[0-9]+),.*$",
    "\\1",
    sen2cor_version_raw1
  ))
  
  # tiles NA -> NULL
  if (!is.null(tiles)) {
    if (all(is.na(tiles))) {tiles <- character(0)}
  }
  
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
  if (!is.na(proc_dir) & Sys.info()["sysname"] != "Windows") {
    if (any(attr(mountpoint(proc_dir), "protocol") %in% c("cifs", "nsfs"))) {
      print_message(
        type = "warning",
        proc_dir, "is on a SAMBA mounted unit, so it will not be used."
      )
      proc_dir <- NA
    }
  }
  
  # accept only input names which are L1C
  l1c_prodlist_level <- safe_getMetadata(
    l1c_prodlist, "level", 
    format = "vector", abort = FALSE, simplify = TRUE
  )
  l1c_prodlist <- l1c_prodlist[which(l1c_prodlist_level == "1C")]
  
  # if no products were found, exit
  if (length(l1c_prodlist) == 0) {
    print_message(
      type = "warning",
      "No valid L1C products were found,"
    )
    return(invisible(NULL))
  }
  
  # Check kill_errored
  if (all(kill_errored == TRUE, Sys.info()["sysname"] == "Windows")) {
    print_message(
      type = "warning",
      "'kill_errored' is available only for Unix systems; skipping it."
    )
    kill_errored <- FALSE
  }
  
  ## Cycle on each file
  # if parallel==TRUE, use doParallel
  n_cores <- if (is.numeric(parallel)) {
    min(as.integer(parallel), length(l1c_prodlist))
  } else if (parallel==FALSE) {
    1
  } else {
    min(parallel::detectCores()-1, length(l1c_prodlist), 8) # use at most 8 cores
  }
  # if (parallel==FALSE | Sys.info()["sysname"] == "Windows" | n_cores<=1) {
  if (parallel==FALSE | n_cores<=1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
  }
  
  # cycle on eacjh product
  l2a_prodlist <- foreach(
    i=seq_along(l1c_prodlist),
    .combine=c,
    .export = "mountpoint",
    .packages='sen2r'
  ) %DO% {
    
    # redirect to log files
    if (n_cores > 1) { # nocov start
      if (!is.na(.log_output)) {
        sink(.log_output, split = TRUE, type = "output", append = TRUE)
      }
      if (!is.na(.log_message)) {
        logfile_message = file(.log_message, open = "a")
        sink(logfile_message, type="message")
      }
    } # nocov end
    
    # set paths
    sel_l1c <- l1c_prodlist[i]
    sel_l2a <- file.path(
      if (is.null(outdir)) {dirname(sel_l1c)} else {outdir},
      gsub(
        "_MSIL1C\\_", "_MSIL2A_",
        gsub("_OPER_", "_USER_", basename(sel_l1c))
      )
    ) # path of the L2A product where it should be placed definitively
    
    # Check if a "comparable" file already excists
    # (in case using Sen2cor 2.8.0, this is necessary in order not to 
    # re-correct L1C products every time)
    sel_l2a_regex <- gsub(
      "^S(2[AB])\\_MSIL([12][AC])\\_([0-9]{8}T[0-9]{6})\\_N([0-9]{4})\\_R([0-9]{3})\\_T([A-Z0-9]{5})\\_([0-9]{8}T[0-9]{6})\\.SAFE$",
      "^S\\1\\\\_MSIL\\2\\\\_\\3\\\\_N[0-9]{4}\\\\_R\\5\\\\_T\\6\\\\_[0-9]{8}T[0-9]{6}\\\\.SAFE$",
      basename(sel_l2a)
    )
    sel_l2a_exi <- list.files(dirname(sel_l2a), sel_l2a_regex, full.names = TRUE)
    # exclude invalid SAFEs
    sel_l2a_exi <- sel_l2a_exi[safe_isvalid(sel_l2a_exi)]
    sel_l2a_meta <- safe_getMetadata(
      sel_l2a_exi, 
      info = c("creation_datetime", "id_baseline", "tiles")
    )
    if (nrow(sel_l2a_meta) > 0) {
      # order by ingestion date (recent first) and baseline (locally corrected and newer first)
      sel_l2a_meta <- sel_l2a_meta[
        order(-creation_datetime, -id_baseline), 
        path := file.path(dirname(sel_l2a), name)
        ]
      # replace the defined sel_l2a with the actual safe to be used
      sel_l2a <- sel_l2a_meta[1,path] 
    }
    
    
    ## Set the tiles vectors (existing, required, ...)
    # existing L1C tiles within input product
    sel_l1c_tiles_existing <- safe_getMetadata(
      list.files(file.path(sel_l1c,"GRANULE")),
      allow_oldnames = TRUE,
      "id_tile", format = "vector", simplify = TRUE
    )
    # L2A tiles already existing
    sel_l2a_tiles_existing <- safe_getMetadata(
      list.files(file.path(sel_l2a,"GRANULE")),
      allow_oldnames = TRUE,
      "id_tile", format = "vector", simplify = TRUE
    )
    # L2A tiles required as output (already existing or not)
    sel_l2a_tiles_required <- if (length(tiles)==0) {
      sel_l1c_tiles_existing
    } else {
      sel_l1c_tiles_existing[sel_l1c_tiles_existing %in% tiles]
    }
    # L2A tiles to be corrected (= required and not yet existing)
    sel_l2a_tiles_tocorrect <- sel_l2a_tiles_required[
      !sel_l2a_tiles_required %in% sel_l2a_tiles_existing
      ]
    # L1C tiles not required to be corrected
    sel_l1c_tiles_unnecessary <- sel_l1c_tiles_existing[
      !sel_l1c_tiles_existing %in% sel_l2a_tiles_tocorrect
      ]
    # L1C tiles to be manually excluded before applying sen2cor in order
    # not to process them (= tiles present within input L1C product
    # and not already esisting as L2A, because in this case sen2cor would
    # automatically skip them)
    sel_l1c_tiles_toavoid <- sel_l1c_tiles_unnecessary[
      !sel_l1c_tiles_unnecessary %in% sel_l2a_tiles_existing
      ]
    
    ## Continue only if not all the existing tiles have already been corrected
    ## (or if the user chosed to overwrite)
    if (overwrite==TRUE | length(sel_l2a_tiles_tocorrect)>0) {
      
      # if output exists (and overwrite==TRUE), delete it
      if (overwrite==TRUE) {
        unlink(sel_l2a, recursive = TRUE)
      }
      
      # set a temporary proc_dir in the following three cases:
      # 1) if some L1C tiles need to be manually excluded;
      # 2) if dirname(sel_l1c)!=dirname(sel_l2a) & input L1C was already partially corrected
      #    (to avoid the risk to process more tiles than required);
      # 3) if sel_l1c is on a SAMBA mountpoint (because sen2cor does not process
      #    correctly in this case)
      sel_proc_dir <- proc_dir
      if (is.na(proc_dir)) {
        if (
          length(sel_l1c_tiles_toavoid)>0 | # 1
          dirname(sel_l1c)!=dirname(sel_l2a) & length(sel_l2a_tiles_existing)>0 | # 2
          Sys.info()["sysname"] != "Windows" & any(attr(suppressWarnings(mountpoint(sel_l1c)), "protocol") %in% c("cifs", "nsfs")) # 3
        ) {
          if (is.na(tmpdir)) {
            tmpdir <- tempfile(pattern="sen2cor_")
          } else if (dir.exists(tmpdir)) {
            tmpdir <- file.path(tmpdir, basename(tempfile(pattern="sen2cor_")))
          }
          dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
          # check that tmpdir is not on a mountpoint
          if (Sys.info()["sysname"] != "Windows") {
            if (any(attr(mountpoint(tmpdir), "protocol") %in% c("cifs", "nsfs"))) {
              print_message(
                type = "error",
                tmpdir, "is on a SAMBA mounted unit, so it can not be used."
              )
            }
          }
          sel_proc_dir <- tmpdir
        }
      }
      
      # if proc_dir is [manually or automatically] set, copy sel_l1c
      if (!is.na(sel_proc_dir)) {
        use_tmpdir <- TRUE
        dir.create(sel_proc_dir, recursive=FALSE, showWarnings=FALSE)
        if (length(sel_l1c_tiles_unnecessary)>0) {
          # if some tiles is unnecessary, copy only necessary files
          dir.create(file.path(sel_proc_dir,basename(sel_l1c)))
          sel_l1c_files <- list.files(sel_l1c, recursive=FALSE, full.names=FALSE)
          file.copy(
            file.path(sel_l1c,sel_l1c_files[!grepl("^GRANULE$",sel_l1c_files)]),
            file.path(sel_proc_dir,basename(sel_l1c)),
            recursive = TRUE
          )
          dir.create(file.path(sel_proc_dir,basename(sel_l1c),"GRANULE"))
          file.copy(
            file.path(sel_l1c,"GRANULE",names(sel_l2a_tiles_tocorrect)),
            file.path(sel_proc_dir,basename(sel_l1c),"GRANULE"),
            recursive = TRUE
          )
        } else {
          # else, copy the whole product
          file.copy(sel_l1c, sel_proc_dir, recursive=TRUE)
        }
        sel_l1c <- file.path(sel_proc_dir, basename(sel_l1c))
      } else {
        use_tmpdir <- FALSE
      }
      
      # path of the L2A product where it is placed by sen2cor
      sen2cor_out_l2a <- file.path(
        dirname(sel_l1c),
        gsub(
          "_MSIL1C\\_", "_MSIL2A_",
          gsub("_OPER_", "_USER_", basename(sel_l1c))
        )
      )
      
      # command arguments
      sen2cor_args <- c(
        "--GIP_L2A", gipp_curr_path,
        if (sen2cor_version>=package_version("2.8.0")) {c("--output_dir", sen2cor_out_l2a)},
        if (sen2cor_version<package_version("2.8.0")) {"--refresh"},
        sel_l1c
      )
      
      # apply sen2cor
      trace_paths <- if (use_tmpdir) {
        c(sel_l1c, sen2cor_out_l2a)
      } else if (overwrite==TRUE | !file.exists(sen2cor_out_l2a)) {
        sen2cor_out_l2a
      } else {
        file.path(sen2cor_out_l2a,"GRANULE",names(sel_l2a_tiles_tocorrect))
      }
      sel_trace <- start_trace(trace_paths, "sen2cor")
      
      sel_sen2cor_timestart <- Sys.time()
      print_message(
        type = "message", date = TRUE,
        "Launching Sen2Cor on ",basename(sel_l1c)," ",
        "(",i,"/",length(l1c_prodlist),")..."
      )
      
      # Launch with exec_background() or system(), 
      # depending on the presence of package "sys"
      if (requireNamespace("sys", quietly = TRUE)) {
        sel_sen2cor_log_output <- if (!is.na(.log_output)) {
          tempfile(pattern = "sen2cor_log_output_", fileext = ".txt")
        } else {NA}
        sel_sen2cor_log_message <- if (!is.na(.log_message)) {
          tempfile(pattern = "sen2cor_log_message_", fileext = ".txt")
        } else {NA}
        sel_sen2cor_pid_1 <- sys::exec_background(
          binpaths$sen2cor,
          args = sen2cor_args,
          std_out = sel_sen2cor_log_output,
          std_err = sel_sen2cor_log_message
        )
      } else {
        system(
          paste(binpaths$sen2cor, paste(sen2cor_args, collapse = " ")),
          intern = FALSE,
          wait = kill_errored == FALSE
          # intern = Sys.info()["sysname"] == "Windows",
        )
      }
      
      if (kill_errored == TRUE) {
        Sys.sleep(10) # wait until all processes started
        # Retrieve the correct PID
        # (sel_sen2cor_pid_1 is the PID of L2A_Process bash script,
        # while we need the PID of the secodn launched python utility)
        psaux_raw0 <- system(paste0("ps aux"), intern = TRUE)
        psaux_raw1 <- psaux_raw0[grepl(sel_l1c, psaux_raw0)]
        psaux_raw2 <- strsplit(psaux_raw1, " +")
        sel_sen2cor_pids <- sapply(psaux_raw2, function(x){x[[2]]})
        sel_sen2cor_pid_2 <- if (length(sel_sen2cor_pids) > 0) {
          psaux_raw2[[which(sel_sen2cor_pids==max(sel_sen2cor_pids))]][2]
        } else {
          1E9 # dummy nonexistent PID 
        }
        # check every 10 seconds
        while (tools::pskill(sel_sen2cor_pid_2, 0)) {Sys.sleep(10)}
      } else if (requireNamespace("sys", quietly = TRUE)) {
        sys::exec_status(sel_sen2cor_pid_1, wait = TRUE)
      }
      
      print_message(
        type = "message", date = TRUE,
        "Running Sen2Cor on ", basename(sel_l1c)," finished after ",
        format(Sys.time() - sel_sen2cor_timestart, digits = 3),"."
      )
      
      if (all(exists("sel_sen2cor_log_output"), !is.na(.log_output))) {
        sen2r:::print_message(
          type = "string", date = TRUE,
          "Sen2Cor log on ", basename(sel_l1c),":"
        )
        readLines(sel_sen2cor_log_output)
      }
      if (all(exists("sel_sen2cor_log_message"), !is.na(.log_message))) {
        sen2r:::print_message(
          type = "string", date = TRUE,
          "Some errors occurred running Sen2Cor on ", basename(sel_l1c),":\n",
          paste(readLines(sel_sen2cor_log_message), collapse = "\n")
        )
      }
      
      if (TRUE) { # TODO define a way to check if sen2cor ran correctly
        end_trace(sel_trace)
      } else {
        clean_trace(sel_trace)
      }
      
      # correct sen2cor_out_l2a
      # (starting from v. 2.8.0, L2A baseline and creation date are different from L1C ones)
      if (sen2cor_version>=package_version("2.8.0")) {
        sen2cor_out_l2a_basename <- list.files(sen2cor_out_l2a, "\\.SAFE$")
        file.rename(
          file.path(sen2cor_out_l2a,sen2cor_out_l2a_basename),
          file.path(dirname(sen2cor_out_l2a),sen2cor_out_l2a_basename)
        )
        unlink(sen2cor_out_l2a, recursive = TRUE)
        sen2cor_out_l2a <- file.path(dirname(sen2cor_out_l2a),sen2cor_out_l2a_basename)
        sel_l2a <- file.path(dirname(sel_l2a), sen2cor_out_l2a_basename)
      }
      
      # move output to the required output directory
      if (use_tmpdir | dirname(sen2cor_out_l2a)!=dirname(sel_l2a)) {
        file.copy(sen2cor_out_l2a, dirname(sel_l2a), recursive=TRUE, overwrite=TRUE)
        if (rmtmp == TRUE) {
          unlink(sen2cor_out_l2a, recursive=TRUE)
        }
      }
      if (use_tmpdir & rmtmp == TRUE) {
        unlink(sel_l1c, recursive=TRUE)
      }
      
    } # end IF cycle on overwrite
    
    # stop sinking
    if (n_cores > 1) { # nocov start
      if (!is.na(.log_output)) {
        sink(type = "output")
      }
      if (!is.na(.log_message)) {
        sink(type = "message"); close(logfile_message)
      }
    } # nocov end
    
    sel_l2a
    
  } # end FOREACH cycle on each product
  if (n_cores > 1) { # nocov start
    stopCluster(cl)
  } # nocov end
  
  # Remove temporary directory
  if (rmtmp == TRUE & !is.na(tmpdir)) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  return(l2a_prodlist)
  
}

#' @title Find, download and preprocess Sentinel-2 images
#' @description The function is a wrapper to perform the entire
#'  processing chain to find, download and pre-process Sentinel-2
#'  data. Input is a set of parameters that can be passed with a
#'  list or file (parameter `param_list`) or singularly (see the
#'  descriptions of all the other parameters).
#' @param param_list List of input parameters: it can be both an
#'  R list or the path of a JSON file.
#'  If some parameters are passed both as elements of `param_list`
#'  and as function arguments, the values passed as function
#'  arguments are considered.
#'  If some parameters are missing in `param_list` and are not
#'  provided as arguments, default values will be used.
#'  Use the function [s2_gui()] to create a complete list of
#'  parameters.
#' @param sel_sensor =NA,
#' @param online =NA,
#' @param overwrite_safe =NA,
#' @param rm_safe =NA,
#' @param step_atmcorr =NA,
#' @param steps_reqout =NA,
#' @param timewindow =NA,
#' @param timewindow =NA,
#' @param timeperiod =NA,
#' @param extent =NA,
#' @param s2tiles_selected =NA,
#' @param list_prods =NA,
#' @param reference_path =NA,
#' @param res =NA,
#' @param unit =NA,
#' @param proj =NA,
#' @param resampling =NA,
#' @param outformat =NA,
#' @param compression =NA,
#' @param overwrite =NA,
#' @param path_l1c =NA,
#' @param path_l2a =NA,
#' @param path_tiles =NA,
#' @param path_out =NA,
#' @param path_tiles_subdirs =NA,
#' @param path_out_subdirs =NA


fidolasen_s2 <- function(param_list=NULL,
                         sel_sensor=NA,
                         online=NA,
                         overwrite_safe=NA,
                         rm_safe=NA,
                         step_atmcorr=NA,
                         steps_reqout=NA,
                         timewindow=NA,
                         timeperiod=NA,
                         extent=NA,
                         s2tiles_selected=NA,
                         list_prods=NA,
                         reference_path=NA,
                         res=NA,
                         unit=NA,
                         proj=NA,
                         resampling=NA,
                         outformat=NA,
                         compression=NA,
                         overwrite=NA,
                         path_l1c=NA,
                         path_l2a=NA,
                         path_tiles=NA,
                         path_out=NA,
                         path_tiles_subdirs=NA,
                         path_out_subdirs=NA) {


  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal


  ## 1. Read / import parameters ##

  # Create parameter list with default values
  # (elements should correspond to the function arguments,
  # except for param_list)
  pm_def <- list(sel_sensor=c("s2a","s2b"),
                 online=TRUE,
                 overwrite_safe=FALSE,
                 rm_safe="no",
                 step_atmcorr="auto",
                 steps_reqout=c("safe","clipped"),
                 timewindow=c(Sys.Date()-90, Sys.Date()), # last 90 days
                 timeperiod="full",
                 extent=get_extent(matrix(c(9.4,45.4,10.27,46.1),nrow=2),"+init=epsg:4326"),
                 s2tiles_selected=c("32TNR","32TNS"),
                 list_prods=c("BOA"),
                 reference_path=NA,
                 res=c(10,10),
                 unit="Meter",
                 proj=NA,
                 resampling="near",
                 outformat="GTiff",
                 compression="LZW",
                 overwrite=FALSE,
                 path_l1c="",
                 path_l2a="",
                 path_tiles="",
                 path_out="",
                 path_tiles_subdirs=TRUE,
                 path_out_subdirs=TRUE)

  # Import param_list, if provided
  pm <- if (is.null(param_list)) {
    # create with default values
    pm_def
  } else if (is(param_list, "character")) {
    # load json parameter file
    jsonlite::read_json(param_list)
    # TODO check package version and parameter names
  } else if (is(param_list, "list")) {
    param_list
    # TODO check parameter names
  }

  # Overwrite parameters passed manually
  # (if some parameters are still missing, copy from default values)
  for (sel_par in names(pm_def)) {
    if (!is.na(get(sel_par))) {
      pm[[sel_par]] <- get(sel_par)
    }
    if (is.na(pm[[sel_par]])) {
      pm[[sel_par]] <- pm_def[[sel_par]]
    }
  }

  # TODO check consistency of parameters

  # internam parameters
  path_vrt <- tempdir() # consider to add as an optional parameter
  path_out_subdirs <- TRUE

  # check output format
  sel_driver <- gdal$GetDriverByName(pm$outformat)
  if (is.null(py_to_r(sel_driver))) {
    print_message(
      type="error",
      "Format \"",pm$outformat,"\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
  }
  # define output extension
  out_ext <- if (pm$outformat=="ENVI") {
    "dat"
  } else {
    unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1]
  }



  ## 2. List required products ##
  s2_list <- s2_list(spatial_extent = pm$extent,
                     time_interval = pm$timewindow,
                     tile = pm$s2tiles_selected,
                     level = if (pm$step_atmcorr=="auto") {
                       "auto"
                     } else if (pm$step_atmcorr=="l2a") {
                       "L2A"
                     } else if (pm$step_atmcorr %in% c("scihub","no")) {
                       "L1C"
                     })
  # getting required metadata
  s2_dt <- sapply(names(s2_list), function(x) unlist(s2_getMetadata(x, info="nameinfo"))) %>%
    t() %>% data.table()
  s2_dt[,c("name","url"):=list(names(s2_list),s2_list)]
  s2_dt[,c("sensing_datetime","creation_datetime"):=list(as.POSIXct(sensing_datetime, format="%s"),
                                                         as.POSIXct(creation_datetime, format="%s"))]
  s2_dt[id_tile %in% pm$s2tiles_selected,][order(sensing_datetime),]
  s2_dt <- s2_dt[id_tile %in% pm$s2tiles_selected & id_orbit %in% sel_orbits,][order(-sensing_datetime),]
  setorder(s2_dt, -sensing_datetime)
  s2_dt <- s2_dt[!duplicated(s2_dt[,list(id_orbit,id_tile)]) &  # consider only most recent tiles
                   # id_orbit %in% pm$s2orbits_selected &       # use only selected orbits (TODO not yet implemented)
                   id_tile %in% pm$s2tiles_selected,]           # use only selected tiles
  s2_list_l1c <- s2_dt[level=="1C",url]
  s2_list_l2a <- s2_dt[level=="2A",url]
  names(s2_list_l1c) <- s2_dt[level=="1C",name]
  names(s2_list_l2a) <- s2_dt[level=="2A",name]


  ## 3. Download them ##
  if (length(s2_list_l2a)>0) {
    lapply(pm$s2tiles_selected, function(tile) {
      s2_download(s2_list_l2a,
                  outdir = pm$path_l2a,
                  tile = tile)
    })
  }
  if (length(s2_list_l1c)>0) {
    lapply(pm$s2tiles_selected, function(tile) {
      s2_download(s2_list_l1c,
                  outdir = pm$path_l1c,
                  tile = tile)
    })
    s2_sen2cor(list.files(pm$path_l1c,"\\.SAFE$"),
               l1c_dir = pm$path_l1c,
               outdir = pm$path_l2a,
               n_procs = 1) # TODO implement multicore
  }


  ## 4. Convert in vrt ##
  dir.create(pm$path_tiles, recursive=FALSE, showWarnings=FALSE)
  # l2a_names <- list.files(pm$path_l2a, paste0(sel_tiles,".+\\.SAFE$"), full.names=TRUE)
  l2a_names <- names(s2_list_l2a)
  if ("tiles" %in% pm$steps_reqout) {
    tiles_ext <- out_ext
    tiles_outformat <- pm$outformat
  } else {
    tiles_ext <- "vrt"
    tiles_outformat <- "VRT"
  }

  for (sel_prod in l2a_names) {
    if (any(!file.exists(
      file.path(pm$path_tiles,pm$list_prods,basename(s2_shortname(sel_prod, ext=tiles_ext)))
    )) | pm$overwrite==TRUE) {
      s2_translate(infile = sel_prod,
                   outdir = pm$path_tiles,
                   prod_type = pm$steps_reqout,
                   format = tiles_outformat,
                   res = if (pm$res < 20) { # TODO manage Degree pm$unit
                     "10m"
                   } else if (pm$res < 60) {
                     "20m"
                   } else {
                     "60m"
                   },
                   vrt_rel_paths = TRUE)
    }
  }


  ## 5. Merge by orbit ##
  tiles_names <- list.files(pm$path_tiles, recursive=TRUE, full.names=TRUE)
  s2_merge(vrt_01_names, vrt_02_path)

  ## 5) clip, rescale, reproject
  vrt_02_names <- list.files(vrt_01_path, recursive=TRUE, full.names=TRUE)
  vrt_03_path <- out_path
  sapply(subdirs <- unique(basename(dirname(vrt_02_names))), function(x) {
    dir.create(file.path(vrt_03_path,x),showWarnings=FALSE)
  })
  sel_outfiles <- file.path(vrt_03_path,
                            basename(dirname(vrt_02_names)),
                            gsub("\\.vrt$","\\.dat",basename(vrt_02_names)))
  if (any(!file.exists(sel_outfiles))) {
    fidolasen::gdal_warp(vrt_02_names[!file.exists(sel_outfiles)],
                         sel_outfiles[!file.exists(sel_outfiles)],
                         ref = sel_ref,
                         of = out_format)
  }

  ## 5) Apply mask
  sapply(subdirs <- c("BOA"), function(x) {
    dir.create(file.path(out_path,paste0(x,"_masked")),showWarnings=FALSE)
  })
  boa_masked_names <- file.path(out_path,"BOA_masked",basename(sel_outfiles[grep("_BOA_",sel_outfiles)]))
  if (any(!file.exists(boa_masked_names))) {
    boa_masked_names_ <- s2_mask(sel_outfiles[grep("_BOA_",sel_outfiles)][!file.exists(boa_masked_names)],
                                 sel_outfiles[grep("_SCL_",sel_outfiles)],
                                 mask_type="cloud_medium_proba",
                                 outdir=file.path(out_path,"BOA_masked"),
                                 format=out_format,
                                 subdirs=FALSE)
  }

  ## 5) Compute spectral indices
  indices_names <- unlist(lapply(sel_indices,function(i){file.path(out_path,i,gsub("BOA",i,basename(boa_masked_names)))}))
  dir.create(file.path(out_path,sel_indices), recursive=FALSE, showWarnings = FALSE)
  if (any(!file.exists(indices_names))) {
    indices_names <- s2_calcindices(boa_masked_names[!file.exists(indices_names)], sel_indices, out_path, subdirs=TRUE, format="ENVI", dataType="Float32")
  }












}

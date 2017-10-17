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
#' @param preprocess =NA,
#' @param s2_levels =NA,
#' @param sel_sensor =NA,
#' @param online =NA,
#' @param overwrite_safe =NA,
#' @param rm_safe =NA,
#' @param step_atmcorr =NA,
#' @param timewindow =NA,
#' @param timeperiod =NA,
#' @param extent =NA,
#' @param s2tiles_selected =NA,
#' @param s2orbits_selected =NA,
#' @param extent_as_mask =NA,
#' @param list_prods =NA,
#' @param list_indices =NA,
#' @param index_source =NA,
#' @param mask_type =NA,
#' @param reference_path =NA,
#' @param res =NA,
#' @param res_s2 =NA,
#' @param unit =NA,
#' @param proj =NA,
#' @param resampling =NA,
#' @param resampling_scl =NA,
#' @param outformat =NA,
#' @param compression =NA,
#' @param overwrite =NA,
#' @param path_l1c =NA,
#' @param path_l2a =NA,
#' @param path_tiles =NA,
#' @param path_merged =NA,
#' @param path_out =NA,
#' @param path_indices =NA,
#' @param path_subdirs =NA,


fidolasen_s2 <- function(param_list=NULL,
                         preprocess=NA,
                         s2_levels=NA,
                         sel_sensor=NA,
                         online=NA,
                         overwrite_safe=NA,
                         rm_safe=NA,
                         step_atmcorr=NA,
                         timewindow=NA,
                         timeperiod=NA,
                         extent=NA,
                         s2tiles_selected=NA,
                         s2orbits_selected=NA,
                         extent_as_mask=NA,
                         list_prods=NA,
                         list_indices=NA,
                         index_source=NA,
                         mask_type=NA,
                         reference_path=NA,
                         res=NA,
                         res_s2=NA,
                         unit=NA,
                         proj=NA,
                         resampling=NA,
                         resampling_scl=NA,
                         outformat=NA,
                         compression=NA,
                         overwrite=NA,
                         path_l1c=NA,
                         path_l2a=NA,
                         path_tiles=NA,
                         path_merged=NA,
                         path_out=NA,
                         path_indices=NA,
                         path_subdirs=NA) {


  ### Preliminary settings ###

  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal

  ## 1. Read / import parameters ##

  # Create parameter list with default values
  # (elements should correspond to the function arguments,
  # except for param_list)
  pm_def <- list(preprocess=TRUE,
                 s2_levels=c("l2a"),
                 sel_sensor=c("s2a","s2b"),
                 online=TRUE,
                 overwrite_safe=FALSE,
                 rm_safe="no",
                 step_atmcorr="auto",
                 timewindow=c(Sys.Date()-90, Sys.Date()), # last 90 days
                 timeperiod="full",
                 extent=get_extent(matrix(c(9.4,45.4,10.27,46.1),nrow=2),"+init=epsg:4326") %>% as("sfc_POLYGON") %>% geojson_json(),
                 s2tiles_selected=c("32TNR","32TNS"),
                 s2orbits_selected=str_pad(c(1:143),3,"left","0"), # temporary select all orbits (TODO implement)
                 extent_as_mask=FALSE,
                 list_prods=c("BOA"),
                 list_indices=NA,
                 index_source="BOA",
                 mask_type=NA,
                 reference_path=NA,
                 res=c(10,10),
                 res_s2="10m",
                 unit="Meter",
                 proj=NA,
                 resampling="near",
                 resampling_scl="near",
                 outformat="GTiff",
                 compression="LZW",
                 overwrite=FALSE,
                 path_l1c=NA,
                 path_l2a=NA,
                 path_tiles=NA,
                 path_merged=NA,
                 path_out=NA,
                 path_indices=NA,
                 path_subdirs=TRUE)

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

  # internal parameters
  dir.create(path_tmp <- tempdir(), showWarnings = FALSE) # consider to add as an optional parameter
  path_tmp <- "/home/lranghetti/nas-s4a/nr_working/luigi/data/s2tsp/171002_fidolasen_build/tmpdir" # FIXME FIXME FIXME remove!
  path_out <- if (!is.na(pm$path_out)) {pm$path_out} else {file.path(path_tmp,"out")}
  path_indices <- if (!is.na(pm$path_indices)) {pm$path_indices} else {file.path(path_tmp,"indices")}
  path_tiles <- if (!is.na(pm$path_tiles)) {pm$path_tiles} else {file.path(path_tmp,"tiles")}
  path_merged <- if (!is.na(pm$path_merged)) {pm$path_merged} else {file.path(path_tmp,"merged")}
  path_warped <- if (is.na(pm$mask_type)) {path_out} else {file.path(path_tmp,"warped")}

  # accepted products (update together with the same variables in s2_gui())
  l1c_prods <- c("TOA")
  l2a_prods <- c("BOA","SCL","TCI")

  # if masking is required, produce also SCL
  list_prods <- if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods){
    c(pm$list_prods,"SCL")
  } else {
    pm$list_prods
  }
  # if some indices is required, compute also TOA or BOA
  if (!is.na(pm$list_indices) & !pm$index_source %in% pm$list_prods){
    list_prods <- c(list_prods, pm$index_source)
  }


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

  ### Find, download and preprocess ###

  ## 2. List required products ##
  s2_lists <- list()

  if (pm$online == TRUE) {

    # if online mode, retrieve list with s2_list() basing on parameters
    if ("l1c" %in% pm$s2_levels) {
      # list of SAFE (L1C) needed for required L1C
      s2_lists[["l1c"]] <- s2_list(spatial_extent = st_read(pm$extent),
                                   time_interval = pm$timewindow,
                                   tile = pm$s2tiles_selected,
                                   level = "L1C")
    }
    if ("l2a" %in% pm$s2_levels) {
      # list of SAFE (L1C or/and L2A) needed for required L2A
      s2_lists[["l2a"]] <- s2_list(spatial_extent = st_read(pm$extent),
                                   time_interval = pm$timewindow,
                                   tile = pm$s2tiles_selected,
                                   level = if (pm$step_atmcorr=="auto") {
                                     "auto"
                                   } else if (pm$step_atmcorr=="l2a") {
                                     "L2A"
                                   } else if (pm$step_atmcorr %in% c("scihub","no")) {
                                     "L1C"
                                   })
    }

  } else {

    # if offline mode, read the SAFE product list from folders and filter
    if ("l1c" %in% pm$s2_levels) {
      s2_lists[["l1c"]] <- list.files(pm$path_l1c, "\\.SAFE$")
    }
    if ("l2a" %in% pm$s2_levels) {
      s2_lists[["l2a"]] <- if (pm$step_atmcorr=="l2a") {
        list.files(pm$path_l2a, "\\.SAFE$")
      } else if (pm$step_atmcorr %in% c("scihub","no","auto")) { # FIXME "auto"? -> for now, managed as "scihub" (apply sen2cor; if l2a already exists, do nothing)
        list.files(pm$path_l1c, "\\.SAFE$")
      }
    }
    s2_lists <- lapply(s2_lists, function(l) {
      sapply(l, function(x) {
        tryCatch(s2_getMetadata(x, info="nameinfo")$level,
                 error = function(e) NA)
      })
    })
    s2_lists <- lapply(s2_lists, function(l) {l[!is.na(l)]})

  }
  s2_list <- unlist(s2_lists)[!duplicated(unlist(lapply(s2_lists, names)))]
  names(s2_list) <- gsub("^l[12][ac]\\.","",names(s2_list))

  # If s2_list is empty, exit
  if (length(s2_list)==0) {
    print_message(
      type = "message",
      if (pm$online==FALSE) {
        paste0("No SAFE products which match the settings were found locally; ",
               "please download them or set different spatial/temporal extents.")
      } else {
        paste0("No SAFE products matching the settings were found; ",
               "please set different spatial/temporal extents.")
      }
    )
  }


  # getting required metadata
  s2_dt <- lapply(names(s2_list), function(x) {
    unlist(s2_getMetadata(x, info="nameinfo")) %>%
      t() %>%
      as.data.frame()
  }) %>%
    rbindlist(fill=TRUE)
  s2_dt[,c("name","url"):=list(names(s2_list),s2_list)]
  s2_dt[,c("sensing_datetime","creation_datetime"):=list(as.POSIXct(sensing_datetime, format="%s"),
                                                         as.POSIXct(creation_datetime, format="%s"))]
  s2_dt <- s2_dt[id_tile %in% pm$s2tiles_selected & # FIXME works only with new products! (add retrieval of all tiles)
                   id_orbit %in% pm$s2orbits_selected &
                   mission %in% toupper(substr(pm$sel_sensor,2,3)) &
                   as.Date(sensing_datetime) > pm$timewindow[1] &
                   as.Date(sensing_datetime) < pm$timewindow[2],][
                     order(-sensing_datetime),]
  setorder(s2_dt, -sensing_datetime)
  s2_dt <- s2_dt[!duplicated(s2_dt[,list(mission,level,id_orbit,id_tile)]) &  # consider only most recent tiles
                   # id_orbit %in% pm$s2orbits_selected &       # use only selected orbits (TODO not yet implemented)
                   id_tile %in% pm$s2tiles_selected,]           # use only selected tiles
  s2_list_l1c <- s2_dt[level=="1C",url] # list of required L1C
  s2_list_l2a <- s2_dt[level=="2A",url] # list of required L2A
  names(s2_list_l1c) <- s2_dt[level=="1C",name]
  names(s2_list_l2a) <- s2_dt[level=="2A",name]


  ## 3. Download them ##
  # TODO implement ovwerite/skip
  # (now it skips, but analysing each single file)

  if (pm$online == TRUE) {
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
    }
  }

  # apply sen2cor
  s2_list_l1c_tocorrect <- if (pm$overwrite_safe==FALSE) {
    s2_list_l1c[!gsub("^S2([AB])\\_MSIL1C\\_","S2\\1\\_MSIL2A\\_",names(s2_list_l1c)) %in% names(s2_list_l2a)]
  } else {
    s2_list_l1c
  }
  if (length(s2_list_l1c_tocorrect)>0) {
    s2_list_l2a_corrected <- s2_sen2cor(names(s2_list_l1c_tocorrect),
               l1c_dir = pm$path_l1c,
               outdir = pm$path_l2a,
               n_procs = 1) # TODO implement multicore
    names(s2_list_l2a_corrected) <- basename(s2_list_l2a_corrected)
    s2_list_l2a <- c(s2_list_l2a,s2_list_l2a_corrected)
  }

  # delete SAFE, if required
  if (pm$rm_safe == "all") {
    unlink(file.path(pm$path_l1c,names(s2_list_l1c)), recursive=TRUE)
    unlink(file.path(pm$path_l2a,names(s2_list_l2a)), recursive=TRUE)
  } else if (pm$rm_safe == "l1c" & !("l1c" %in% pm$s2_levels)) {
    unlink(file.path(pm$path_l1c,names(s2_list_l1c_tocorrect)), recursive=TRUE)
  }

  # if no processing is required, stop here
  if (pm$preprocess == FALSE) {
    return(safe_fullnames)
  }


  ### Processing steps ###

  # do anything if preprocess is not required
if (pm$preprocess == TRUE) {


  ## Define output formats
  if (!is.na(pm$path_tiles)) {
    tiles_ext <- out_ext
    tiles_outformat <- pm$outformat
  } else {
    tiles_ext <- "vrt"
    tiles_outformat <- "VRT"
  }
  if (!is.na(pm$path_merged)) {
    merged_ext <- out_ext
    merged_outformat <- pm$outformat
  } else {
    merged_ext <- "vrt"
    merged_outformat <- "VRT"
  }
  if (is.na(pm$mask_type)) {
    warped_ext <- out_ext
    warped_outformat <- pm$outformat
  } else {
    warped_ext <- "vrt"
    warped_outformat <- "VRT"
  }

  ## Define output file names and lists ##
  # meaning of the suffixes _exp, _req and _new
  # (here and for all the script):
  # _exp: [full] names of the files expected to be present at the
  #       end of the processing chain (already existing or not);
  # _req: names of the files required for the next step
  #       (e.g. tiles_names_req are required to perform s2_merge())
  # _new: names of the required files not existing yet (expected
  #       to be created).
  # With overwrite=TRUE, all these vectors are equal
  # (e.g. merged_names_exp = merged_names_req = merged_names_new),
  # because all is overwritten.

  # expected names for tiles
  tiles_l1c_names_exp <- lapply(names(s2_list_l1c), function(x){
    lapply(list_prods[list_prods %in% l1c_prods], function(p){
      file.path(
        path_tiles,
        if(pm$path_subdirs==TRUE){p}else{""},
        s2_shortname(x, prod_type=p, ext=tiles_ext)
      )
    })
  }) %>% unlist()
  tiles_l2a_names_exp <- lapply(names(s2_list_l2a), function(x){
    lapply(list_prods[list_prods %in% l2a_prods], function(p){
      file.path(
        path_tiles,
        if(pm$path_subdirs==TRUE){p}else{""},
        s2_shortname(x, prod_type=p, ext=tiles_ext)
      )
    })
  }) %>% unlist()
  tiles_names_exp <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_exp},
                       if("l2a" %in% pm$s2_levels) {tiles_l2a_names_exp})

  # expected names for merged
  if (length(tiles_names_exp)==0) {
    merged_names_exp <- NULL
  } else {
    merged_names_exp <- data.table(
      fs2nc_getElements(tiles_names_exp, format="data.frame")
    )[,paste0("S2",
              mission,
              level,"_",
              strftime(sensing_date,"%Y%m%d"),"_",
              id_orbit,"__",
              prod_type,"_",
              substr(res,1,2),".",
              file_ext)]
    merged_names_exp <- merged_names_exp[!duplicated(merged_names_exp)]
    merged_names_exp <- gsub(paste0(tiles_ext,"$"),merged_ext,merged_names_exp) %>%
      file.path(path_merged,
                if(pm$path_subdirs==TRUE){fs2nc_getElements(merged_names_exp, format="data.frame")$prod_type}else{""},
                .)
  }

# index which is TRUE for SCL products, FALSE for others
names_merged_exp_scl_idx <- fs2nc_getElements(merged_names_exp,format="data.frame")$prod_type=="SCL"
# index which is TRUE for products to be atm. masked, FALSE for others
names_tomask_idx <- if ("SCL" %in% pm$list_prods) {
  !names_merged_exp_scl_idx
} else {
  names_merged_exp_scl_idx>-1
}

  # expected names for warped products
warped_names_exp <- if (length(merged_names_exp)==0) {
  NULL
} else {
  # if SCL were explicitly required, directly create them as output files (since they are never masked);
  # instead, build only virtual files
  ifelse(
    names_merged_exp_scl_idx & "SCL" %in% pm$list_prods,
    file.path(path_out,
              if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
              gsub(paste0(merged_ext,"$"),out_ext,basename(merged_names_exp))),
    ifelse(names_merged_exp_scl_idx,
           file.path(path_warped,
                     if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                     gsub(paste0(merged_ext,"$"),out_ext,basename(merged_names_exp))),
           file.path(path_warped,
                     if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                     gsub(paste0(merged_ext,"$"),warped_ext,basename(merged_names_exp))))
  )
}

# expected names for masked products
masked_names_exp <- if (!is.na(pm$mask_type)) {
  file.path(path_out,
            if(pm$path_subdirs==TRUE){basename(dirname(warped_names_exp[!names_merged_exp_scl_idx]))}else{""},
            gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names_exp[!names_merged_exp_scl_idx])))
}


  # expected names for output products
  out_names_exp <- c(warped_names_exp[names_merged_exp_scl_idx], masked_names_exp)

  # expected names for indices
  level_for_indices <- if (all(pm$index_source=="TOA")) {
    "1C"
  } else if (all(pm$index_source=="BOA")) {
    "2A"
  } else {
    c("1C","2A")
  }
  indices_names_exp <- if (length(out_names_exp)==0 | any(is.na(pm$list_indices))) {
    NULL
  } else {
    data.table(
      fs2nc_getElements(out_names_exp, format="data.frame")
    )[level %in% level_for_indices,
      paste0("S2",
             mission,
             level,"_",
             strftime(sensing_date,"%Y%m%d"),"_",
             id_orbit,"__",
             "<index>_",
             substr(res,1,2),".",
             file_ext)] %>%
      expand.grid(pm$list_indices) %>%
      apply(1,function(x){
        file.path(
          if(pm$path_subdirs==TRUE){x[2]}else{""},
          gsub("<index>",x[2],x[1])
        )
      }) %>%
      file.path(path_indices,.) %>%
      gsub(paste0(merged_ext,"$"),out_ext,.)
  }


  # list of required files and steps

  # if overwrite is set to TRUE, works with expected names;
  # otherwise, compute non-existing values

  if (pm$overwrite==TRUE) {

    out_names_req <- out_names_new <- out_names_exp
    masked_names_req <- masked_names_new <- masked_names_exp
    warped_names_req <- warped_names_new <- warped_names_reqout <- warped_names_exp
    merged_names_req <- merged_names_new <- merged_names_exp
    tiles_names_req <- tiles_names_new <- tiles_names_exp
    safe_names_l1c_req <- file.path(pm$path_l1c,names(s2_list_l1c))
    safe_names_l2a_req <- file.path(pm$path_l2a,names(s2_list_l2a))

  } else {

    indices_names_new <- if (length(indices_names_exp)==0) {
      NULL
    } else {
      indices_names_exp[!file.exists(indices_names_exp)]
    }

    # required output products
    out_names_req <- if (length(out_names_exp)==0) {
      NULL
    } else if (is.na(pm$path_out)) {
      if (length(indices_names_new)==0) {
        NULL
      } else {
        out_basenames_req <- data.table(
          fs2nc_getElements(indices_names_new, format="data.frame")
        )[,paste0("S2",
                  mission,
                  level,"_",
                  strftime(sensing_date,"%Y%m%d"),"_",
                  id_orbit,"__",
                  ifelse(level=="2A","BOA","TOA"),"_",
                  substr(res,1,2),".",
                  out_ext)]
        out_names_exp[basename(out_names_exp) %in% out_basenames_req &
                        !file.exists(out_names_exp)]
      }
    } else {
      out_names_exp
    }
    out_names_new <- if (length(out_names_req)==0) {
      NULL
    } else {
      out_names_req[!file.exists(out_names_req)]
    }

    # required masked and warped
    if (is.na(pm$mask_type)) {
      masked_names_req <- masked_names_new <- NULL
      warped_names_req <- warped_names_new <- out_names_new
    } else {
      masked_names_req <- out_names_new[fs2nc_getElements(out_names_new, format="data.frame")$prod_type!="SCL"]
      masked_names_new <- if (length(masked_names_req)==0) {
        NULL
      } else {
        masked_names_req[!file.exists(masked_names_req)]
      }
      warped_names_req <- file.path(path_warped,
                                    if(pm$path_subdirs==TRUE){basename(dirname(masked_names_new))}else{""},
                                    gsub(paste0(out_ext,"$"),warped_ext,basename(out_names_new)))
    }
    warped_names_new <- if (length(warped_names_req)==0) {
      NULL
    } else {
      warped_names_req[!file.exists(warped_names_req)]
    }

    # required merged
    merged_names_req <- if (length(merged_names_exp)==0) {
      NULL
    } else if (is.na(pm$path_merged)) {
      if (length(out_names_new)==0) {
        NULL
      } else {
        merged_basenames_req <- gsub(paste0(out_ext,"$"),merged_ext,basename(out_names_new))
        merged_names_exp[basename(merged_names_exp) %in% merged_basenames_req]
      }
    } else {
      merged_names_exp
    }
    merged_names_new <- if (length(merged_names_req)==0) {
      NULL
    } else {
      merged_names_req[!file.exists(merged_names_req)]
    }
    # output of merged_names_req
    warped_names_reqout <- warped_names_exp[merged_names_exp %in% merged_names_req]

    # required tiles
    tiles_names_req <- if (length(tiles_names_exp)==0) {
      NULL
    } else if (is.na(pm$path_tiles)) {
      if (length(merged_names_new)==0) {
        NULL
      } else {
        tiles_basenames_req <- gsub(paste0(merged_ext,"$"),tiles_ext,basename(merged_names_new)) %>%
          gsub("\\_\\_","_[A-Z0-9]{5}_",.) %>%
          paste0("^",.,"$")
        tiles_names_exp[unlist(lapply(tiles_basenames_req, grep, basename(tiles_names_exp)))]
      }
    } else {
      tiles_names_exp
    }
    tiles_names_new <- if (length(tiles_names_req)==0) {
      NULL
    } else {
      tiles_names_req
    }

    # required SAFE products
    if (length(tiles_names_req)==0) {
      safe_names_l1c_req <- safe_names_l2a_req <- NULL
    } else {
      tiles_dt_new <- data.table(fs2nc_getElements(tiles_names_new,format="data.frame"))
      safe_dt_av <- sapply(c(names(s2_list_l1c),names(s2_list_l2a)), function(x) unlist(s2_getMetadata(x, info="nameinfo"))) %>%
        t() %>% data.table()
      tiles_basenames_av <- safe_dt_av[,paste0("S",
                                               mission,
                                               level,"_",
                                               strftime(as.POSIXct(sensing_datetime, format="%s"),"%Y%m%d"),"_",
                                               id_orbit,"_",
                                               id_tile,"_",
                                               "[A-Z0-9]{3}_",
                                               "[126]0\\.",
                                               tiles_ext)]
      tiles_basenames_l1c_av <- tiles_basenames_av[safe_dt_av$level=="1C"]
      tiles_basenames_l2a_av <- tiles_basenames_av[safe_dt_av$level=="2A"]
      safe_names_l1c_req <- if (nrow(tiles_dt_new[level=="1C",])>0) {
        names(s2_list_l1c)[
          lapply(tiles_basenames_l1c_av,
                 function(x){grep(x,tiles_names_new)} %>% length() > 0) %>%
            unlist()
          ] %>%
          file.path(path_l1c,.)
      } else {
        character(0)
      }
      safe_names_l2a_req <- if (nrow(tiles_dt_new[level=="2A",])>0) {
        names(s2_list_l2a)[
          lapply(tiles_basenames_l2a_av,
                 function(x){grep(x,tiles_names_new)} %>% length() > 0) %>%
            unlist()
          ] %>%
          file.path(path_l2a,.)
      } else {
        character(0)
      }
    }

  } # end of pm$overwrite FALSE IF cycle
browser()

  ## 4. Convert in vrt ##
  if (length(c(safe_names_l1c_req,safe_names_l2a_req))>0) {

    dir.create(path_tiles, recursive=FALSE, showWarnings=FALSE)
    tiles_l1c_names_out <- tiles_l2a_names_out <- character(0)

    if("l1c" %in% pm$s2_levels) {
      list_l1c_prods <- list_prods[list_prods %in% l1c_prods]
      for (sel_prod in safe_names_l1c_req) {
        tiles_l1c_names_out <- c(
          tiles_l1c_names_out,
          s2_translate(infile = sel_prod,
                       outdir = path_tiles,
                       prod_type = list_l1c_prods,
                       format = tiles_outformat,
                       res = pm$res_s2,
                       vrt_rel_paths = TRUE,
                       subdirs = pm$path_subdirs,
                       overwrite = pm$overwrite))
      }
    }
    if("l2a" %in% pm$s2_levels) {
      list_l2a_prods <- list_prods[list_prods %in% l2a_prods]
      for (sel_prod in safe_names_l2a_req) {
        tiles_l2a_names_out <- c(
          tiles_l2a_names_out,
          s2_translate(infile = sel_prod,
                       outdir = path_tiles,
                       prod_type = list_l2a_prods,
                       format = tiles_outformat,
                       res = pm$res_s2,
                       vrt_rel_paths = TRUE,
                       subdirs = pm$path_subdirs,
                       overwrite = pm$overwrite))
      }
    }

    tiles_names_out <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_out},
                         if("l2a" %in% pm$s2_levels) {tiles_l2a_names_out})
    # TODO check tiles_names_out - tiles_names_req

  } # end of s2_translate IF cycle

## 5. Merge by orbit ##
  if (length(tiles_names_req)>0) {

    dir.create(path_merged, recursive=FALSE, showWarnings=FALSE)
    merged_names_out <- s2_merge(tiles_names_req,
                                 path_merged,
                                 subdirs=pm$path_subdirs,
                                 format=merged_outformat,
                                 overwrite=pm$overwrite)
    # TODO check merged_names_out - merged_names_req

  } # end of s2_merge IF cycle


if (length(out_names_req)>0) {

  ## 6. Clip, rescale, reproject ##
  dir.create(path_warped, recursive=FALSE, showWarnings=FALSE)
  # create mask
  s2_mask_extent <- if (pm$extent_as_mask==TRUE) {
    st_read(pm$extent)
  } else {
    st_cast(st_read(pm$extent),"LINESTRING")
  }  # TODO add support for multiple extents

    if(pm$path_subdirs==TRUE){
      sapply(unique(dirname(warped_names_reqout)),dir.create,showWarnings=FALSE)
    }

    if (any(!file.exists(warped_names_reqout)) | pm$overwrite==TRUE) {
      # index which is TRUE for SCL products, FALSE for others
      names_merged_req_scl_idx <- fs2nc_getElements(merged_names_req,format="data.frame")$prod_type=="SCL"
      fidolasen::gdal_warp(merged_names_req[!names_merged_req_scl_idx],
                           warped_names_reqout[!names_merged_req_scl_idx],
                           of = warped_outformat,
                           ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
                           mask = s2_mask_extent,
                           tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
                           t_srs = if (!is.na(pm$proj)){pm$proj} else {NULL},
                           r = pm$resampling,
                           overwrite = pm$overwrite) # TODO dstnodata value?
      fidolasen::gdal_warp(merged_names_req[names_merged_req_scl_idx],
                           warped_names_reqout[names_merged_req_scl_idx],
                           of = pm$outformat, # use physical files to speed up next steps
                           ref = if (!is.na(pm$reference_path)) {pm$reference_path} else {NULL},
                           mask = s2_mask_extent,
                           tr = if (!any(is.na(pm$res))) {pm$res} else {NULL},
                           t_srs = if (!is.na(pm$proj)) {pm$proj} else {NULL},
                           r = pm$resampling_scl,
                           overwrite = pm$overwrite)
    }

    ## 7. Apply mask ##
    # FIXME understand if this should be done before warping (if so, how to manage virtual/physical files?)
    # masked_names <- file.path(path_out,
    #                           if(pm$path_subdirs==TRUE){basename(dirname(warped_names[!names_merged_exp_scl_idx]))}else{""},
    #                           gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names[!names_merged_exp_scl_idx])))
    masked_names_out <- s2_mask(warped_names_req,
                            warped_names_exp[names_merged_exp_scl_idx],
                            mask_type=pm$mask_type,
                            outdir=path_out,
                            format=pm$outformat,
                            subdirs=pm$path_subdirs,
                            overwrite=pm$overwrite,
                            parallel=TRUE)

} # end of gdal_warp and s2_mask IF cycle

    ## 8. Compute spectral indices ##
    # dir.create(file.path(path_out,pm$list_indices), recursive=FALSE, showWarnings = FALSE)
    # if (any(!file.exists(indices_names))) {
    indices_names <- s2_calcindices(out_names_req,
                                    indices=pm$list_indices,
                                    outdir=path_indices,
                                    subdirs=TRUE,
                                    source=pm$index_source,
                                    format=pm$outformat,
                                    overwrite=pm$overwrite)


  } # end of pm$preprocess IF cycle


  ## 9. remove temporary files
  unlink(path_tmp, recursive = TRUE)


}

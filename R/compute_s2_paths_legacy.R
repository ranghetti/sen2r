#' @title Compute names of S2 file to be generated (old function)
#' @description `compute_s2_paths` is an internal function
#'  (to be used within [sen2r()])
#'  which computes the names of the required output image files
#'  (see details). 
#'  The function was split from [sen2r()] because this code
#'  is called twice (and to shorten the main function).
#'  This function is deprecated, [compute_s2_paths] is used instead.
#' @details `compute_s2_paths_legacy` was structured in the following way:
#' 1. Retrieve the file names expected to be present at the
#'    end of the processing chain (suffix `_exp`):
#'    - `tiles_names_`
#'    - `merged_names_`
#'    - `warped_names_`
#'    - `masked_names_`
#'    - `out_names_` (= `warped_names_` or `subset(warped_names_)+masked_names_`)
#'    - `indices_names_`
#' 2. Compute the file names expected to be created
#'    (suffixes `_req` and `_new`, see below)
#'    (this operation is done in reverse order).
#'    
#'     Meaning of the suffixes `_exp`, `_req` and `_new`
#'     (here and for all the script):
#'     - `_exp`: full names of the files expected to be present at the
#'         end of the processing chain (already existing or not);
#'     - `_req`: names of the files required for the next step
#'         (e.g. tiles_names_req are required to perform s2_merge())
#'     - `_new`: names of the required files not existing yet (expected
#'         to be created).
#'         
#' With `overwrite=TRUE`, all these vectors are equal
#' (e.g. `merged_names_exp = merged_names_req = merged_names_new`),
#' because all is overwritten.
#' @return A list, containing the following vectors:
#' `tiles_names_exp`, `merged_names_exp`, `warped_names_exp`, 
#' `masked_names_exp`, `out_names_exp`, `indices_names_exp`,
#' `indices_names_new`, `out_names_req`, `out_names_new`,
#' `masked_names_new`, `warped_names_req`, `warped_names_reqforrgb`, 
#' `warped_names_new`,  `merged_names_req`, `merged_names_reqforrgb`,
#' `merged_names_new`, `tiles_names_req`, `tiles_names_new`,
#' `safe_names_l1c_req`, `safe_names_l2a_req`.
#' 
#' @param pm List of input parameters.
#' @param s2_list_l1c Names and paths of input SAFE level-1C products.
#' @param s2_list_l2a Names and paths of input SAFE level-2A products.
#' @param paths Named vector of required paths. It must contain elements
#'  named `"out"`, `"indices"`, `"tiles"`, `"merged"` and `"warped"`.
#' @param list_prods Character vector with the values of the
#'  products to be processed (accepted values: "TOA", "BOA", "SCL", "TCI").
#' @param main_ext File extension corresponding to pm$outformat.
#' @param rgb_ext File extension corresponding to pm$rgb_outformat.
#' @param force_tiles (optional) Logical: passed to [safe_shortname] (default: FALSE).
#' @param check_tmp (optional) Logical: if TRUE (default), temporary files
#'  are also searched when `_exi` names are computed; 
#'  if FALSE, only non temporary files are searched.
#' @param ignorelist Vector of output files to be ignored.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table

compute_s2_paths_legacy <- function(pm, 
                             s2_list_l1c, 
                             s2_list_l2a, 
                             paths, 
                             list_prods, 
                             main_ext, rgb_ext, 
                             # index_ext,
                             # tiles_ext, 
                             # merged_ext, 
                             # warped_ext, 
                             # rgb_ext, 
                             # sr_masked_ext,
                             force_tiles = FALSE,
                             check_tmp = TRUE,
                             ignorelist) {
  
  # to avoid NOTE on check
  . <- type <- mission <- level <- id_orbit <- extent_name <- file_ext <-
    mission <- level <- sensing_date <- id_orbit <- prod_type <- res <- 
    sensing_datetime <- id_tile <- NULL
  
  # for compatibility
  index_ext <- out_ext["indices"]
  tiles_ext <- out_ext["tiles"]
  merged_ext <- out_ext["merged"]
  warped_ext <- out_ext["warped"]
  rgb_ext <- out_ext["rgb"]
  sr_masked_ext <- out_ext["masked"]
  out_ext <- out_ext["masked"]
  
  # accepted products (update together with the same variables in s2_gui() and in sen2r())
  l1c_prods <- c("TOA")
  l2a_prods <- c("BOA","SCL","TCI")
  
  l1c_prods_regex <- paste0("^((",paste(l1c_prods,collapse=")|("),"))$")
  l2a_prods_regex <- paste0("^((",paste(l2a_prods,collapse=")|("),"))$")
  # TODO load parameter file if pm is a path
  
  # load output formats
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))$drivers
  
  ## Define output file names and lists ##
  
  # expected names for tiles
  tiles_l1c_names_exp <- lapply(file.path(pm$path_l1c,names(s2_list_l1c)), function(x){
    lapply(list_prods[grepl(l1c_prods_regex, list_prods)], function(p){
      file.path(
        paths["tiles"],
        if(pm$path_subdirs==TRUE){p}else{""},
        basename(safe_shortname(x, prod_type=p, ext=tiles_ext, res=pm$res_s2, tiles=pm$s2tiles_selected, force_tiles=force_tiles, multiple_names=TRUE))
      )
    })
  }) %>% unlist()
  tiles_l1c_names_exp <- tiles_l1c_names_exp[!duplicated(tiles_l1c_names_exp)]
  tiles_l2a_names_exp <- lapply(file.path(pm$path_l2a,names(s2_list_l2a)), function(x){
    lapply(list_prods[grepl(l2a_prods_regex, list_prods)], function(p){
      sel_av_tiles <- tryCatch(
        safe_getMetadata(x,"tiles"),
        error = function(e){safe_getMetadata(x,"nameinfo")$id_tile}
      )
      sel_tiles <- sel_av_tiles[sel_av_tiles %in% pm$s2tiles_selected]
      file.path(
        paths["tiles"],
        if(pm$path_subdirs==TRUE){p}else{""},
        basename(safe_shortname(x, prod_type=p, ext=tiles_ext, res=pm$res_s2, tiles=pm$s2tiles_selected, force_tiles=force_tiles, multiple_names=TRUE))
      )
    })
  }) %>% unlist()
  tiles_l2a_names_exp <- tiles_l2a_names_exp[!duplicated(tiles_l2a_names_exp)]
  tiles_names_exp <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_exp},
                       if("l2a" %in% pm$s2_levels) {tiles_l2a_names_exp})
  
  # add existing files for tiles
  tiles_names_exi <- if (!is.na(pm$path_tiles) | check_tmp == TRUE) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["tiles"],list_prods), full.names=TRUE)
    } else {
      list.files(paths["ti"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (length(pm$s2tiles_selected)>0 & !anyNA(pm$s2tiles_selected) & length(all_meta$extent_name)>0) {
        all_meta <- all_meta[extent_name %in% pm$s2tiles_selected,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  tiles_names_exp <- unique(c(tiles_names_exp,tiles_names_exi))
  
  # expected names for merged
  if (length(tiles_names_exp)==0) {
    merged_names_exp <- NULL
  } else {
    merged_names_exp <- sen2r_getElements(tiles_names_exp)[,paste0(
      "S2",
      mission,
      level,"_",
      strftime(sensing_date,"%Y%m%d"),"_",
      id_orbit,"__",
      prod_type,"_",
      substr(res,1,2),".",
      file_ext
    )]
    merged_names_exp <- merged_names_exp[!duplicated(merged_names_exp)]
    merged_names_exp <- gsub(paste0(tiles_ext,"$"),merged_ext,merged_names_exp) %>%
      file.path(
        paths["merged"],
        if(pm$path_subdirs==TRUE){sen2r_getElements(merged_names_exp)$prod_type}else{""},
        .
      )
  }
  # add existing files for merged
  merged_names_exi <- if (!is.na(pm$path_merged) | check_tmp == TRUE) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["merged"],list_prods), full.names=TRUE)
    } else {
      list.files(paths["merged"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  merged_names_exp <- unique(c(merged_names_exp,merged_names_exi))
  
  
  # index which is TRUE for SCL products, FALSE for others
  names_merged_exp_scl_idx <- sen2r_getElements(merged_names_exp)$prod_type=="SCL"
  # index which is TRUE for products to be atm. masked, FALSE for others
  names_merged_tomask_idx_scl <- if (!"SCL" %in% pm$list_prods) {
    !names_merged_exp_scl_idx
  } else {
    rep(TRUE, length(merged_names_exp))
  }
  names_merged_tomask_idx_boa <- if (
    !"BOA" %in% pm$list_prods & 
    (anyNA(pm$list_indices) | pm$index_source != "BOA")
  ) {
    sen2r_getElements(merged_names_exp)$prod_type!="BOA"
  } else {
    rep(TRUE, length(merged_names_exp))
  }
  names_merged_tomask_idx_toa <- if (
    !"TOA" %in% pm$list_prods & 
    (anyNA(pm$list_indices) | pm$index_source != "TOA")
  ) {
    sen2r_getElements(merged_names_exp)$prod_type!="TOA"
  } else {
    rep(TRUE, length(merged_names_exp))
  }
  names_merged_tomask_idx <- names_merged_tomask_idx_scl & 
    names_merged_tomask_idx_boa & names_merged_tomask_idx_toa
  
  # expected names for warped products
  if (pm$clip_on_extent==FALSE | length(merged_names_exp)==0) {
    warped_names_exp <- NULL
  } else {
    basename_warped_names_exp <- sen2r_getElements(merged_names_exp)[,paste0(
      "S2",
      mission,
      level,"_",
      strftime(sensing_date,"%Y%m%d"),"_",
      id_orbit,"_", 
      pm$extent_name,"_",
      prod_type,"_",
      substr(res,1,2),".",
      file_ext
    )]
    # if SCL were explicitly required, directly create them as output files (since they are never masked);
    # instead, build only virtual files
    warped_names_exp <- ifelse(
      names_merged_exp_scl_idx & "SCL" %in% pm$list_prods,
      file.path(paths["out"],
                if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                gsub(paste0(merged_ext,"$"),out_ext,basename_warped_names_exp)),
      ifelse(names_merged_exp_scl_idx, # make SCL layers as temporary TIF (to save time when applying them to mask)
             file.path(paths["warped"],
                       if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                       gsub(paste0(merged_ext,"$"),out_ext,basename_warped_names_exp)),
             file.path(paths["warped"], # and other products as vrt
                       if(pm$path_subdirs==TRUE){basename(dirname(merged_names_exp))}else{""},
                       gsub(paste0(merged_ext,"$"),warped_ext,basename_warped_names_exp)))
    )
  }
  # add existing files for warped
  warped_names_exi <- if (is.na(pm$mask_type) & (!is.na(pm$path_out) | check_tmp == TRUE)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["out"],list_prods), full.names=TRUE)
    } else {
      list.files(paths["out"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (length(pm$extent_name)>0 & !anyNA(pm$extent_name) & length(all_meta$extent_name)>0) {
        all_meta <- all_meta[extent_name %in% pm$extent_name,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  warped_names_exp <- unique(c(warped_names_exp,warped_names_exi))
  
  
  # expected names for RGB
  if ((pm$clip_on_extent==TRUE & length(warped_names_exp)==0) | 
      (pm$clip_on_extent==FALSE & length(merged_names_exp)==0) | 
      anyNA(pm$list_rgb)) {
    rgb_names_exp <- NULL
  } else {
    rgb_names_exp <- sen2r_getElements(
      if (pm$clip_on_extent==TRUE) {warped_names_exp} else {merged_names_exp} 
    )[,paste0(
      "S2",
      mission,
      level,"_",
      strftime(sensing_date,"%Y%m%d"),"_",
      id_orbit,"_",
      if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
      "<rgbname>_",
      substr(res,1,2),".",
      rgb_ext
    )] %>%
      unique() %>%
      expand.grid(pm$list_rgb) %>%
      apply(1,function(x){
        file.path(
          if(pm$path_subdirs==TRUE){x[2]}else{""},
          gsub("<rgbname>",x[2],x[1])
        )
      }) %>%
      file.path(paths["rgb"],.)
  }
  # add existing files for indices
  rgb_names_exi <-  if (!is.na(pm$path_rgb) | check_tmp == TRUE) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["rgb"],pm$list_rgb), full.names=TRUE)
    } else {
      list.files(paths["rgb"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (length(pm$extent_name)>0 & !anyNA(pm$extent_name) & length(all_meta$extent_name)>0) {
        all_meta <- all_meta[extent_name %in% pm$extent_name,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  rgb_names_exp <- unique(c(rgb_names_exp,rgb_names_exi))
  
  
  # expected names for masked products
  # if clip_on_extent is required, mask warped, otherwise, mask merged
  if (is.na(pm$mask_type)) {
    masked_names_exp <- NULL
  } else {
    masked_names_exp <- if (pm$clip_on_extent==TRUE) {
      file.path(paths["out"],
                if(pm$path_subdirs==TRUE){basename(dirname(nn(warped_names_exp[names_merged_tomask_idx])))}else{""},
                gsub(paste0(warped_ext,"$"),out_ext,basename(nn(warped_names_exp[names_merged_tomask_idx]))))
    } else {
      file.path(paths["out"],
                if(pm$path_subdirs==TRUE){basename(dirname(nn(merged_names_exp[names_merged_tomask_idx])))}else{""},
                gsub(paste0(merged_ext,"$"),out_ext,basename(nn(merged_names_exp[names_merged_tomask_idx]))))
    }
    # use sr_masked_ext if necessary
    if (!pm$index_source %in% pm$list_prods) {
      masked_names_exp_sr_idx <- sen2r_getElements(masked_names_exp)$prod_type==pm$index_source
      masked_names_exp[masked_names_exp_sr_idx] <- gsub(
        paste0(out_ext,"$"), sr_masked_ext, 
        masked_names_exp[masked_names_exp_sr_idx]
      )
    }
  }
  # add existing files for masked
  masked_names_exi <- if (!is.na(pm$mask_type) & (!is.na(pm$path_out) | check_tmp == TRUE)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["out"],list_prods), full.names=TRUE)
    } else {
      list.files(paths["out"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          prod_type != "SCL" &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (length(pm$extent_name)>0 & !anyNA(pm$extent_name) & length(all_meta$extent_name)>0) {
        all_meta <- all_meta[extent_name %in% pm$extent_name,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  masked_names_exp <- unique(c(masked_names_exp,masked_names_exi))
  
  # expected names for output products
  out_names_exp <- if (!is.na(pm$mask_type)) {
    if (pm$clip_on_extent==TRUE) {
      c(warped_names_exp[names_merged_exp_scl_idx], masked_names_exp)
    } else {
      c(merged_names_exp[names_merged_exp_scl_idx], masked_names_exp)
    }
  } else {
    if (pm$clip_on_extent==TRUE) {
      warped_names_exp
    } else {
      merged_names_exp
    }
  }
  
  # expected names for indices
  if (length(out_names_exp)==0 | anyNA(pm$list_indices)) {
    indices_names_exp <- NULL
  } else {
    level_for_indices <- if (all(pm$index_source=="TOA")) {
      "1C"
    } else if (all(pm$index_source=="BOA")) {
      "2A"
    } else {
      c("1C","2A")
    }
    indices_names_exp <- sen2r_getElements(out_names_exp)[
      level %in% level_for_indices,
      paste0("S2",
             mission,
             level,"_",
             strftime(sensing_date,"%Y%m%d"),"_",
             id_orbit,"_",
             if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
             "<index>_",
             substr(res,1,2),".",
             index_ext)
      ] %>%
      expand.grid(pm$list_indices) %>%
      apply(1,function(x){
        file.path(
          if(pm$path_subdirs==TRUE){x[2]}else{""},
          gsub("<index>",x[2],x[1])
        )
      }) %>%
      file.path(paths["indices"],.) %>%
      gsub(paste0(merged_ext,"$"),out_ext,.)
  }
  # add existing files for indices
  indices_names_exi <-  if (!is.na(pm$path_indices) | check_tmp == TRUE) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(paths["indices"],pm$list_indices), full.names=TRUE)
    } else {
      list.files(paths["indices"], full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- suppressWarnings(sen2r_getElements(all_names, abort=FALSE))
      all_meta$names <- all_names
      # filter
      if (is.null(all_meta$prod_type)) {all_meta$prod_type <- character()}
      if (is.null(all_meta$mission)) {all_meta$mission <- character()}
      if (is.null(all_meta$level)) {all_meta$level <- character()}
      if (is.null(all_meta$file_ext)) {all_meta$file_ext <- character()}
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% pm$list_indices &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(all_meta$sensing_date)>0) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (length(pm$extent_name)>0 & !anyNA(pm$extent_name) & length(all_meta$id_orbit)>0) {
        all_meta <- all_meta[extent_name %in% pm$extent_name,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  indices_names_exp <- unique(c(indices_names_exp,indices_names_exi))
  
  
  # Filter names included in ignorelist
  tiles_names_exp <- tiles_names_exp[!tiles_names_exp %in% ignorelist]
  merged_names_exp <- merged_names_exp[!merged_names_exp %in% ignorelist]
  warped_names_exp <- warped_names_exp[!warped_names_exp %in% ignorelist]
  masked_names_exp <- masked_names_exp[!masked_names_exp %in% ignorelist]
  out_names_exp <- out_names_exp[!out_names_exp %in% ignorelist]
  rgb_names_exp <- rgb_names_exp[!rgb_names_exp %in% ignorelist]
  indices_names_exp <- indices_names_exp[!indices_names_exp %in% ignorelist]
  
  
  # list of required files and steps
  
  # if overwrite is set to TRUE, works with expected names;
  # otherwise, compute non-existing values
  
  if (pm$overwrite==TRUE) {
    
    indices_names_new <- indices_names_exp
    rgb_names_new <- rgb_names_exp
    out_names_req <- if (length(indices_names_exp)==0) {NULL} else {out_names_exp}
    out_names_new <- out_names_exp
    basenames_reqforrgb <- if (length(rgb_names_new)==0) {
      NULL
    } else {
      sen2r_getElements(rgb_names_new)[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_",
        if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
        ifelse(level=="2A","BOA","TOA"),"_",
        substr(res,1,2),".",
        if (pm$clip_on_extent==TRUE) {warped_ext} else {merged_ext}
      )] %>%
        unique()
    }
    warped_names_reqforrgb <- if (pm$clip_on_extent==TRUE) {
      warped_names_exp[basename(nn(warped_names_exp)) %in% basenames_reqforrgb]
    } else {NULL}
    merged_names_reqforrgb <- if (pm$clip_on_extent==FALSE) {
      merged_names_exp[basename(nn(merged_names_exp)) %in% basenames_reqforrgb]
    } else {NULL}
    masked_names_new <- masked_names_exp
    warped_names_req <- if (pm$clip_on_extent==FALSE | length(out_names_exp)==0) {
      NULL
    } else {
      warped_names_exp
    }
    warped_names_new <- warped_names_reqout <- warped_names_exp
    merged_names_req <- if (pm$clip_on_extent==TRUE & length(warped_names_exp)==0) {
      NULL
    } else if (pm$clip_on_extent==FALSE & length(out_names_exp)==0) {
      NULL
    } else {
      merged_names_exp
    }
    merged_names_new <- merged_names_exp
    tiles_names_req <- if (length(merged_names_exp)==0) {
      NULL
    } else {
      tiles_names_exp
    }
    tiles_names_new <- tiles_names_exp
    safe_names_l1c_req <- file.path(pm$path_l1c,names(s2_list_l1c))
    safe_names_l2a_req <- file.path(pm$path_l2a,names(s2_list_l2a))
    
  } else {
    
    indices_names_new <- indices_names_exp[!file.exists(nn(indices_names_exp))]
    rgb_names_new <- rgb_names_exp[!file.exists(nn(rgb_names_exp))]
    
    # required output products for indices
    out_basenames_req <- if (length(indices_names_new)==0) {
      NULL
    } else {
      sen2r_getElements(indices_names_new)[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_",
        if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
        ifelse(level=="2A","BOA","TOA"),"_",
        substr(res,1,2),".",
        if (!pm$index_source %in% pm$list_prods) {sr_masked_ext} else {out_ext}
      )]
    }
    out_names_req <- out_names_exp[basename(nn(out_names_exp)) %in% out_basenames_req]
    out_names_new <- if (is.na(pm$path_out)) {
      out_names_req
    } else {
      unique(c(
        out_names_req,
        if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods) {
          out_names_exp[sen2r_getElements(out_names_exp)$prod_type!="SCL"]
        } else {
          out_names_exp
        }
      ))
    }
    out_names_new <- out_names_new[!file.exists(nn(out_names_new))]
    if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
      # if some output needs to be created with masking, include the creation of SCL
      out_names_new <- unique(c(
        out_names_new,
        out_names_exp[sen2r_getElements(out_names_exp)$prod_type=="SCL"]
      ))
    }
    out_names_new <- out_names_new[!file.exists(nn(out_names_new))]
    
    
    # index which is TRUE for SCL products, FALSE for others
    names_merged_new_out_idx <- sen2r_getElements(out_names_new)$prod_type=="SCL"
    
    # required output products for RGB
    basenames_reqforrgb <- if (length(rgb_names_new)==0) {
      NULL
    } else {
      sen2r_getElements(rgb_names_new)[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_",
        if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
        ifelse(level=="2A","BOA","TOA"),"_",
        substr(res,1,2),".",
        if (pm$clip_on_extent==TRUE) {warped_ext} else {merged_ext}
      )] %>%
        unique()
    }
    warped_names_reqforrgb <- if (pm$clip_on_extent==TRUE) {
      warped_names_exp[basename(nn(warped_names_exp)) %in% basenames_reqforrgb]
    } else {NULL}
    merged_names_reqforrgb <- if (pm$clip_on_extent==FALSE) {
      merged_names_exp[basename(nn(merged_names_exp)) %in% basenames_reqforrgb]
    } else {NULL}
    
    # required masked and warped
    masked_names_new <- if (is.na(pm$mask_type)) {
      NULL
    } else {
      out_names_new[sen2r_getElements(out_names_new)$prod_type!="SCL"]
    }
    warped_names_req1 <- if (pm$clip_on_extent==FALSE | length(out_names_new)==0) {
      NULL
    } else if (is.na(pm$mask_type)) {
      out_names_new[!names_merged_new_out_idx] # FIXME check!
    } else {
      file.path(
        paths["warped"],
        if(pm$path_subdirs==TRUE){basename(dirname(out_names_new))}else{""},
        ifelse(
          names_merged_new_out_idx & !"SCL" %in% pm$list_prods,
          basename(out_names_new),
          gsub(paste0(out_ext,"$"),warped_ext,basename(out_names_new))
        )
      )
    }
    warped_names_req <- unique(c(warped_names_req1,warped_names_reqforrgb))
    warped_names_new <- warped_names_req[!file.exists(nn(warped_names_req))]
    
    # required merged
    merged_basenames_req <- c(
      gsub(paste0(warped_ext,"$"),merged_ext,basename(nn(warped_names_new))) %>%
        gsub(paste0("\\_",pm$extent_name,"\\_"),"__",.) %>%
        gsub(paste0(out_ext,"$"),merged_ext,.),
      gsub(paste0(out_ext,"$"),merged_ext,basename(nn(masked_names_new))) %>%
        gsub(paste0("\\_",pm$extent_name,"\\_"),"__",.)) %>%
      unique()
    merged_names_req1 <- if (pm$clip_on_extent==TRUE) {
      merged_names_exp[basename(nn(merged_names_exp)) %in% merged_basenames_req]
    } else {
      c(
        file.path(
          paths["merged"],
          if(pm$path_subdirs==TRUE){basename(dirname(nn(out_names_new)))}else{""},
          gsub(paste0(out_ext,"$"),merged_ext,basename(nn(out_names_new)))
        ),
        if (is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
          out_names_exp[sen2r_getElements(out_names_req)$prod_type=="SCL"]
        }
      )
    }
    merged_names_req <- unique(c(merged_names_req1,merged_names_reqforrgb))
    merged_names_new <- if (is.na(pm$path_merged)) {
      merged_names_req
    } else {
      unique(c(merged_names_req,merged_names_exp))
    }
    merged_names_new <- merged_names_new[!file.exists(nn(merged_names_new))]
    
    # output of merged_names_req
    warped_names_reqout <- warped_names_exp[merged_names_exp %in% merged_names_req]
    
    # required tiles
    tiles_basenames_req <- gsub(paste0(merged_ext,"$"),tiles_ext,basename(nn(merged_names_new))) %>%
      gsub("\\_\\_","_[A-Z0-9]{5}_",.) %>%
      paste0("^",.,"$")
    tiles_names_req <- tiles_names_exp[unlist(lapply(tiles_basenames_req, grep, basename(nn(tiles_names_exp))))]
    tiles_names_new <- if (is.na(pm$path_tiles)) {
      tiles_names_req
    } else {
      unique(c(tiles_names_req,tiles_names_exp))
    }
    tiles_names_new <- tiles_names_new[!file.exists(nn(tiles_names_new))]
    
    # required SAFE products
    if (length(tiles_names_new)==0) {
      safe_names_l1c_req <- safe_names_l2a_req <- NULL
    } else {
      tiles_dt_new <- sen2r_getElements(tiles_names_new)
      safe_dt_av <- lapply(c(names(s2_list_l1c),names(s2_list_l2a)), function(x) {
        unlist(safe_getMetadata(x, info=c("nameinfo"))) %>%
          t() %>%
          as.data.frame(stringsAsFactors=FALSE)
      }) %>%
        rbindlist(fill=TRUE)
      safe_dt_av$id_tile <- if (is.null(safe_dt_av$id_tile)) {
        ""
      } else {
        lapply(c(file.path(pm$path_l1c,names(s2_list_l1c)),file.path(pm$path_l2a,names(s2_list_l2a))), function(x) {
          tryCatch(safe_getMetadata(x, "tiles"), error = function(e) {NULL})
        }) %>%
          sapply(paste, collapse = " ") %>% as.character()
        # keep only required tiles
        safe_dt_av$id_tile <- sapply(safe_dt_av$id_tile, function(x) {
          strsplit(x," ")[[1]][strsplit(x," ")[[1]] %in% pm$s2tiles_selected] %>% 
            paste(collapse=" ")
        })
      }
      tiles_basenames_av <- safe_dt_av[,paste0("S",
                                               mission,
                                               level,"_",
                                               strftime(as.POSIXct(sensing_datetime, format="%s"),"%Y%m%d"),"_",
                                               id_orbit,"_",
                                               ifelse(id_tile!="",id_tile,"[A-Z0-9]{5}"),"_",
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
          file.path(pm$path_l1c,.)
      } else {
        character(0)
      }
      safe_names_l2a_req <- if (nrow(tiles_dt_new[level=="2A",])>0) {
        names(s2_list_l2a)[
          lapply(tiles_basenames_l2a_av,
                 function(x){grep(x,tiles_names_new)} %>% length() > 0) %>%
            unlist()
          ] %>%
          file.path(pm$path_l2a,.)
      } else {
        character(0)
      }
    }
    
  } # end of pm$overwrite FALSE IF cycle
  
  # End of the section of the creation of file names
  # List of the file names:
  # List of all the file names, in order of creation
  list(
    "tiles_names_exp" = tiles_names_exp,
    "merged_names_exp" = merged_names_exp,
    "warped_names_exp" = warped_names_exp,
    "masked_names_exp" = masked_names_exp,
    "out_names_exp" = out_names_exp,
    "rgb_names_exp" = rgb_names_exp,
    "indices_names_exp" = indices_names_exp,
    "indices_names_new" = indices_names_new,
    "rgb_names_new" = rgb_names_new,
    "out_names_req" = out_names_req,
    "out_names_new" = out_names_new,
    "masked_names_new" = masked_names_new,
    "warped_names_req" = warped_names_req,
    "warped_names_reqforrgb" = warped_names_reqforrgb,
    "warped_names_new" = warped_names_new,
    "warped_names_reqout" = warped_names_reqout,
    "merged_names_req" = merged_names_req,
    "merged_names_reqforrgb" = merged_names_reqforrgb,
    "merged_names_new" = merged_names_new,
    "tiles_names_req" = tiles_names_req,
    "tiles_names_new" = tiles_names_new,
    "safe_names_l1c_req" = safe_names_l1c_req,
    "safe_names_l2a_req" = safe_names_l2a_req
  )
  
}

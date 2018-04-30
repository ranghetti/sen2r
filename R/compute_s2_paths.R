#' @title Compute names of S2 file to be generated
#' @description Internal function (to be used within [sen2r()])
#'  which computes the names of the required output image files
#'  (see details). 
#'  The function was splitted from [sen2r()] because this code
#'  is called twice (and to shorten the main function).
#' @details This fuction is structured in the following way:
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
#' `masked_names_new`, `warped_names_req`, `warped_names_new`, 
#' `merged_names_req`, `merged_names_new`, `tiles_names_req`, `tiles_names_new`,
#' `safe_names_l1c_req`, `safe_names_l1c_req`, `safe_names_l2a_req`.
#' 
#' @param pm List of input parameters.
#' @param s2_list_l1c Names and paths of input SAFE level-1C products.
#' @param s2_list_l2a Names and paths of input SAFE level-2A products.
#' @param paths Named vector of required paths. It must contain elements
#'  named `"out"`, `"indices"`, `"tiles"`, `"merged"` and `"warped"`.
#' @param list_prods Character vector with the values of the
#'  products to be processed (accepted values: "TOA", "BOA", "SCL", "TCI").
#' @param out_ext Extension (character) of output products.
#' @param tiles_ext Extension (character) of tiled products.
#' @param merged_ext Extension (character) of merged products.
#' @param warped_ext Extension (character) of warped products.
#' @param sr_masked_ext Extension (character) of masked products of SR products.
#' @param force_tiles (optional) Logical: passed to [s2_shortname] (default: TRUE).
#' @param ignorelist Vector of output files to be ignored.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table


compute_s2_paths <- function(pm, 
                             s2_list_l1c, 
                             s2_list_l2a, 
                             paths, 
                             list_prods, 
                             out_ext, 
                             tiles_ext, 
                             merged_ext, 
                             warped_ext, 
                             sr_masked_ext,
                             force_tiles=FALSE,
                             ignorelist) {
  
  # accepted products (update together with the same variables in s2_gui() and in sen2r())
  l1c_prods <- c("TOA")
  l2a_prods <- c("BOA","SCL","TCI")
  
  # TODO load parameter file if pm is a path
  
  # load output formats
  gdal_formats <- fromJSON(system.file("extdata","gdal_formats.json",package="sen2r"))
  
  ## Define output file names and lists ##
  
  # expected names for tiles
  tiles_l1c_names_exp <- lapply(file.path(pm$path_l1c,names(s2_list_l1c)), function(x){
    lapply(list_prods[list_prods %in% l1c_prods], function(p){
      file.path(
        paths["tiles"],
        if(pm$path_subdirs==TRUE){p}else{""},
        basename(s2_shortname(x, prod_type=p, ext=tiles_ext, res=pm$res_s2, tiles=pm$s2tiles_selected, force_tiles=force_tiles, multiple_names=TRUE))
      )
    })
  }) %>% unlist()
  tiles_l1c_names_exp <- tiles_l1c_names_exp[!duplicated(tiles_l1c_names_exp)]
  tiles_l2a_names_exp <- lapply(file.path(pm$path_l2a,names(s2_list_l2a)), function(x){
    lapply(list_prods[list_prods %in% l2a_prods], function(p){
      sel_av_tiles <- tryCatch(
        s2_getMetadata(x,"tiles"),
        error = function(e){s2_getMetadata(x,"nameinfo")$id_tile}
      )
      sel_tiles <- sel_av_tiles[sel_av_tiles %in% pm$s2tiles_selected]
      file.path(
        paths["tiles"],
        if(pm$path_subdirs==TRUE){p}else{""},
        basename(s2_shortname(x, prod_type=p, ext=tiles_ext, res=pm$res_s2, tiles=pm$s2tiles_selected, force_tiles=force_tiles, multiple_names=TRUE))
      )
    })
  }) %>% unlist()
  tiles_l2a_names_exp <- tiles_l2a_names_exp[!duplicated(tiles_l2a_names_exp)]
  tiles_names_exp <- c(if("l1c" %in% pm$s2_levels) {tiles_l1c_names_exp},
                       if("l2a" %in% pm$s2_levels) {tiles_l2a_names_exp})
  
  # add existing files for tiles
  tiles_names_exi <- if (!is.na(pm$path_tiles)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(pm$path_tiles,list_prods), full.names=TRUE)
    } else {
      list.files(pm$path_tiles, full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- data.table(suppressWarnings(fs2nc_getElements(all_names, abort=FALSE, format="data.frame")))
      all_meta$names <- all_names
      # filter
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (!is.null(pm$timewindow) & !anyNA(pm$timewindow) & !is.null(all_meta$sensing_date)) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (!is.null(pm$s2orbits_selected) & !anyNA(pm$s2orbits_selected) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (!is.null(pm$s2tiles_selected) & !anyNA(pm$s2tiles_selected) & !is.null(all_meta$id_tile)) {
        all_meta <- all_meta[id_tile %in% pm$s2tiles_selected,]
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
      file.path(paths["merged"],
                if(pm$path_subdirs==TRUE){fs2nc_getElements(merged_names_exp, format="data.frame")$prod_type}else{""},
                .)
  }
  # add existing files for merged
  merged_names_exi <- if (!is.na(pm$path_merged)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(pm$path_merged,list_prods), full.names=TRUE)
    } else {
      list.files(pm$path_merged, full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- data.table(suppressWarnings(fs2nc_getElements(all_names, abort=FALSE, format="data.frame")))
      all_meta$names <- all_names
      # filter
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (!is.null(pm$timewindow) & !anyNA(pm$timewindow) & !is.null(all_meta$sensing_date)) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (!is.null(pm$s2orbits_selected) & !anyNA(pm$s2orbits_selected) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  merged_names_exp <- unique(c(merged_names_exp,merged_names_exi))
  
  
  # index which is TRUE for SCL products, FALSE for others
  names_merged_exp_scl_idx <- fs2nc_getElements(merged_names_exp,format="data.frame")$prod_type=="SCL"
  # index which is TRUE for products to be atm. masked, FALSE for others
  names_merged_tomask_idx <- if ("SCL" %in% pm$list_prods) {
    names_merged_exp_scl_idx>-1
  } else {
    !names_merged_exp_scl_idx
  }
  
  # expected names for warped products
  if (pm$clip_on_extent==FALSE | length(merged_names_exp)==0) {
    warped_names_exp <- NULL
  } else {
    basename_warped_names_exp <- data.table(
      fs2nc_getElements(merged_names_exp, format="data.frame")
    )[,paste0("S2",
              mission,
              level,"_",
              strftime(sensing_date,"%Y%m%d"),"_",
              id_orbit,"_", 
              pm$extent_name,"_",
              prod_type,"_",
              substr(res,1,2),".",
              file_ext)]
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
  warped_names_exi <- if (is.na(pm$mask_type)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(pm$path_out,list_prods), full.names=TRUE)
    } else {
      list.files(pm$path_out, full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- data.table(suppressWarnings(fs2nc_getElements(all_names, abort=FALSE, format="data.frame")))
      all_meta$names <- all_names
      # filter
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (!is.null(pm$timewindow) & !anyNA(pm$timewindow) & !is.null(all_meta$sensing_date)) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (!is.null(pm$s2orbits_selected) & !anyNA(pm$s2orbits_selected) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (!is.null(pm$extent_name) & !anyNA(pm$extent_name) & !is.null(all_meta$extent_name)) {
        all_meta <- all_meta[extent_name %in% pm$extent_name,]
      }
      all_meta$names
    } else {
      character(0)
    }
  }
  warped_names_exp <- unique(c(warped_names_exp,warped_names_exi))
  
  # expected names for masked products
  # if clip_on_extent is required, mask warped, otherwise, mask merged
  if (is.na(pm$mask_type)) {
    masked_names_exp <- NULL
  } else {
    masked_names_exp <- if (pm$clip_on_extent==TRUE) {
      file.path(paths["out"],
                if(pm$path_subdirs==TRUE){basename(dirname(nn(warped_names_exp[!names_merged_exp_scl_idx])))}else{""},
                gsub(paste0(warped_ext,"$"),out_ext,basename(nn(warped_names_exp[!names_merged_exp_scl_idx]))))
    } else {
      file.path(paths["out"],
                if(pm$path_subdirs==TRUE){basename(dirname(nn(merged_names_exp[!names_merged_exp_scl_idx])))}else{""},
                gsub(paste0(merged_ext,"$"),out_ext,basename(nn(merged_names_exp[!names_merged_exp_scl_idx]))))
    }
    # use sr_masked_ext if necessary
    if (!pm$index_source %in% pm$list_prods){
      masked_names_exp_sr_idx <- sapply(masked_names_exp,function(x){
        fs2nc_getElements(x)$prod_type
      })==pm$index_source
      masked_names_exp[masked_names_exp_sr_idx] <- gsub(
        paste0(out_ext,"$"), sr_masked_ext, 
        masked_names_exp[masked_names_exp_sr_idx]
      )
    }
  }
  # add existing files for masked
  masked_names_exi <- if (!is.na(pm$mask_type)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(pm$path_out,list_prods), full.names=TRUE)
    } else {
      list.files(pm$path_out, full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- data.table(suppressWarnings(fs2nc_getElements(all_names, abort=FALSE, format="data.frame")))
      all_meta$names <- all_names
      # filter
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          prod_type != "SCL" &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (!is.null(pm$timewindow) & !anyNA(pm$timewindow) & !is.null(all_meta$sensing_date)) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (!is.null(pm$s2orbits_selected) & !anyNA(pm$s2orbits_selected) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (!is.null(pm$extent_name) & !anyNA(pm$extent_name) & !is.null(all_meta$extent_name)) {
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
    indices_names_exp <- data.table(
      fs2nc_getElements(out_names_exp, format="data.frame")
    )[level %in% level_for_indices,
      paste0("S2",
             mission,
             level,"_",
             strftime(sensing_date,"%Y%m%d"),"_",
             id_orbit,"_",
             if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
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
      file.path(paths["indices"],.) %>%
      gsub(paste0(merged_ext,"$"),out_ext,.)
  }
  # add existing files for indices
  indices_names_exi <-  if (!is.na(pm$path_indices)) {
    all_names <- if (pm$path_subdirs==TRUE) {
      list.files(file.path(pm$path_indices,pm$list_indices), full.names=TRUE)
    } else {
      list.files(pm$path_indices, full.names=TRUE)
    }
    if (length(all_names)>0) {
      all_meta <- data.table(suppressWarnings(fs2nc_getElements(all_names, abort=FALSE, format="data.frame")))
      all_meta$names <- all_names
      # filter
      all_meta <- all_meta[
        type != "unrecognised" &
          prod_type %in% list_prods &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3)) &
          file_ext == gdal_formats[gdal_formats$name==pm$outformat,"ext"]
        ,]
      if (!is.null(pm$timewindow) & !anyNA(pm$timewindow) & !is.null(all_meta$sensing_date)) {
        all_meta <- all_meta[
          all_meta$sensing_date>=pm$timewindow[1] & 
            all_meta$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (!is.null(pm$s2orbits_selected) & !anyNA(pm$s2orbits_selected) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$s2orbits_selected,]
      }
      if (!is.null(pm$extent_name) & !anyNA(pm$extent_name) & !is.null(all_meta$id_orbit)) {
        all_meta <- all_meta[id_orbit %in% pm$extent_name,]
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
  indices_names_exp <- indices_names_exp[!indices_names_exp %in% ignorelist]
  
  
  # list of required files and steps
  
  # if overwrite is set to TRUE, works with expected names;
  # otherwise, compute non-existing values
  
  if (pm$overwrite==TRUE) {
    
    indices_names_new <- indices_names_exp
    out_names_req <- if (length(indices_names_exp)==0) {NULL} else {out_names_exp}
    out_names_new <- out_names_exp
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
    
    # required output products
    out_basenames_req <- if (length(indices_names_new)==0) {
      NULL
    } else {
      data.table(
        fs2nc_getElements(indices_names_new, format="data.frame")
      )[,paste0("S2",
                mission,
                level,"_",
                strftime(sensing_date,"%Y%m%d"),"_",
                id_orbit,"_",
                if (pm$clip_on_extent==TRUE) {pm$extent_name},"_",
                ifelse(level=="2A","BOA","TOA"),"_",
                substr(res,1,2),".",
                if (!pm$index_source %in% pm$list_prods) {sr_masked_ext} else {out_ext})]
    }
    out_names_req <- out_names_exp[basename(nn(out_names_exp)) %in% out_basenames_req]
    out_names_new <- if (is.na(pm$path_out)) {
      out_names_req
    } else {
      unique(c(
        out_names_req,
        if (!is.na(pm$mask_type) & !"SCL" %in% pm$list_prods) {
          out_names_exp[fs2nc_getElements(out_names_exp, format="data.frame")$prod_type!="SCL"]
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
        out_names_exp[fs2nc_getElements(out_names_exp, format="data.frame")$prod_type=="SCL"]
      ))
    }
    out_names_new <- out_names_new[!file.exists(nn(out_names_new))]
    
    # index which is TRUE for SCL products, FALSE for others
    names_merged_new_out_idx <- fs2nc_getElements(out_names_new,format="data.frame")$prod_type=="SCL"
    
    # required masked and warped
    masked_names_new <- if (is.na(pm$mask_type)) {
      NULL
    } else {
      out_names_new[fs2nc_getElements(out_names_new, format="data.frame")$prod_type!="SCL"]
    }
    warped_names_req <- if (pm$clip_on_extent==FALSE | length(out_names_new)==0) {
      NULL
    } else if (is.na(pm$mask_type)) {
      out_names_exp[!names_merged_new_out_idx] # FIXME check!
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
    warped_names_new <- warped_names_req[!file.exists(nn(warped_names_req))]
    
    # required merged
    merged_basenames_req <- c(
      gsub(paste0(warped_ext,"$"),merged_ext,basename(nn(warped_names_new))) %>%
        gsub(paste0("\\_",pm$extent_name,"\\_"),"__",.) %>%
        gsub(paste0(out_ext,"$"),merged_ext,.),
      gsub(paste0(out_ext,"$"),merged_ext,basename(nn(masked_names_new))) %>%
        gsub(paste0("\\_",pm$extent_name,"\\_"),"__",.)) %>%
      unique()
    merged_names_req <- if (pm$clip_on_extent==TRUE) {
      merged_names_exp[basename(nn(merged_names_exp)) %in% merged_basenames_req]
    } else {
      c(
        file.path(
          paths["merged"],
          if(pm$path_subdirs==TRUE){basename(dirname(nn(out_names_new)))}else{""},
          gsub(paste0(out_ext,"$"),merged_ext,basename(nn(out_names_new)))
        ),
        if (is.na(pm$mask_type) & !"SCL" %in% pm$list_prods & length(out_names_new)>0) {
          out_names_exp[fs2nc_getElements(out_names_req, format="data.frame")$prod_type=="SCL"]
        }
      )
    }
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
      tiles_dt_new <- data.table(fs2nc_getElements(tiles_names_new,format="data.frame"))
      safe_dt_av <- lapply(c(names(s2_list_l1c),names(s2_list_l2a)), function(x) {
        unlist(s2_getMetadata(x, info=c("nameinfo"))) %>%
          t() %>%
          as.data.frame(stringsAsFactors=FALSE)
      }) %>%
        rbindlist(fill=TRUE)
      safe_dt_av$id_tile <- if (is.null(safe_dt_av$id_tile)) {
        ""
      } else {
        lapply(c(file.path(pm$path_l1c,names(s2_list_l1c)),file.path(pm$path_l2a,names(s2_list_l2a))), function(x) {
          tryCatch(s2_getMetadata(x, "tiles"), error = function(e) {NULL})
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
    "indices_names_exp" = indices_names_exp,
    "indices_names_new" = indices_names_new,
    "out_names_req" = out_names_req,
    "out_names_new" = out_names_new,
    "masked_names_new" = masked_names_new,
    "warped_names_req" = warped_names_req,
    "warped_names_new" = warped_names_new,
    "warped_names_reqout" = warped_names_reqout,
    "merged_names_req" = merged_names_req,
    "merged_names_new" = merged_names_new,
    "tiles_names_req" = tiles_names_req,
    "tiles_names_new" = tiles_names_new,
    "safe_names_l1c_req" = safe_names_l1c_req,
    "safe_names_l2a_req" = safe_names_l2a_req
  )
  
}

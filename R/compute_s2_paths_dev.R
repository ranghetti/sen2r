compute_s2_paths_dev <- function(pm, 
                                 s2_list_l1c, 
                                 s2_list_l2a, 
                                 paths, 
                                 list_prods, 
                                 out_ext, 
                                 index_ext,
                                 tiles_ext, 
                                 merged_ext, 
                                 warped_ext, 
                                 rgb_ext, 
                                 sr_masked_ext,
                                 force_tiles = FALSE,
                                 check_tmp = TRUE,
                                 ignorelist) {
  

  # Preliminary settings
  list_prods <- list_prods[!is.na(nn(list_prods))]
  list_rgb <- pm$list_rgb[!is.na(nn(pm$list_rgb))]
  list_indices <- pm$list_indices[!is.na(nn(pm$list_indices))]
  
  # Remove duplicates
  list_prods <- list_prods[!duplicated(list_prods)]
  list_rgb <- list_rgb[!duplicated(list_rgb)]
  list_indices <- list_indices[!duplicated(list_indices)]
  s2_list_l1c <- s2_list_l1c[!duplicated(names(s2_list_l1c))]
  s2_list_l2a <- s2_list_l2a[!duplicated(names(s2_list_l2a))]
  
  # Steps to perform
  steps_todo <- c(
    "tiles" = TRUE,
    "merged" = TRUE,
    # "merged_scl" = "SCL" %in% list_prods,
    "warped" = pm$clip_on_extent,
    "warped_scl" = pm$clip_on_extent & "SCL" %in% list_prods,
    "rgb" = length(list_rgb) > 0,
    "masked" = !is.na(pm$mask_type),
    "indices" = length(list_indices) > 0
  )
  
  # Output explicitly required by the user (to be maintained)
  output_req <- c(
    "tiles" = !is.na(pm$path_tiles),
    "merged" = !is.na(pm$path_merged),
    "warped" = length(list_prods[list_prods != "SCL"]) > 0 & !steps_todo[["masked"]],
    "warped_scl" = "SCL" %in% pm$list_prods,
    "rgb" = steps_todo[["rgb"]],
    "masked" = length(list_prods[list_prods != "SCL"]) > 0 & steps_todo[["masked"]],
    "indices" = steps_todo[["indices"]]
  )
  
  # File extensions
  fileext <- c(
    "tiles" = tiles_ext,
    "merged" = merged_ext,
    # "merged_scl" = if (pm$clip_on_extent) merged_ext else out_ext,
    "warped" = warped_ext,
    "warped_scl" = out_ext, # do not use VRT in order to speed up mask application
    "rgb" = rgb_ext,
    "masked" = sr_masked_ext,
    "indices" = index_ext
  )
  
  # Order of requirements
  output_dep <- c(
    "tiles" = "SAFE",
    "merged" = "tiles",
    "warped" = "merged",
    "warped_scl" = "merged",
    "rgb" = if (steps_todo["warped"]) {"warped"} else {"merged"},
    "masked.nonscl" = if (steps_todo["warped"]) {"warped"} else {"merged"},
    "masked.scl" = if (steps_todo["warped"]) {"warped_scl"} else {"merged"},
    "indices" = if (steps_todo["masked"]) {"masked"} else if (steps_todo["warped"]) {"warped"} else {"merged"}
  )
  
  # Paths (additions for compatibility)
  paths["masked"] <- paths["out"]
  paths["warped_scl"] <- paths["out_scl"]
  
  # Out extent name
  ExtentName <- if (steps_todo["warped"]) {pm$extent_name} else {""}
  
  # Level for indices
  level_for_indices <- switch(pm$index_source, TOA = "1C", BOA = "2A")
  
  ## internal functions
  
  # function to remove duplicate elements of a vector
  remove_duplicates <- function(x) {x[!duplicated(x)]}
  
  # function to merge exp_paths (files required as outputs) 
  # to req_paths (files required as intermediate steps)
  merge_exp_req <- function(exp_paths, req_paths, step) {
    sapply(names(exp_paths[[step]]), function(prod) {
      c(
        exp_paths[[step]][[prod]], 
        unlist(sapply(req_paths[names(which(output_dep==step))], function(sellist) {
          sellist[[prod]]
        }))
      ) %>%
        as.vector() %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # function to filter non-existing files
  nonex_paths <- function(list_paths, overwrite = FALSE) {
    if (overwrite) {
      list_paths
    } else {
      sapply(names(list_paths), function(prod) {
        list_paths[[prod]][!file.exists(list_paths[[prod]])]
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
  }
  
  
  ## Existing files
  # Raw list
  exi_paths <- list(
    "tiles" = sapply(list_prods, function(prod) {
      list.files(
        file.path(paths["tiles"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([0-9]{2}[A-Z]{3})\\_(",prod,")\\_([126]0)\\.?(",fileext["tiles"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "merged" = sapply(list_prods[list_prods != "SCL"], function(prod) {
      list.files(
        file.path(paths["merged"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_\\_(",prod,")\\_([126]0)\\.?(",fileext["merged"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "warped" = sapply(list_prods[list_prods != "SCL"], function(prod) {
      list.files(
        file.path(paths["warped"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_(",prod,")\\_(",fileext["warped"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "warped_scl" = sapply(list_prods[list_prods == "SCL"], function(prod) {
      list.files(
        file.path(paths["warped_scl"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_(",prod,")\\_([126]0)\\.?(",fileext["warped_scl"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "rgb" = sapply(list_rgb, function(prod) {
      list.files(
        file.path(paths["rgb"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_(",prod,")\\_([126]0)\\.?(",fileext["rgb"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "masked" = sapply(list_prods[list_prods != "SCL"], function(prod) {
      list.files(
        file.path(paths["masked"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_(",prod,")\\_([126]0)\\.?(",fileext["masked"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE),
    "indices" = sapply(list_indices, function(prod) {
      list.files(
        file.path(paths["indices"], if (pm$path_subdirs) {prod}), 
        paste0("^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_(",prod,")\\_([126]0)\\.?(",fileext["indices"],")$"),
        full.names=TRUE
      )
    }, simplify = FALSE, USE.NAMES = TRUE)
  )
  # Metadata and generic filters
  exi_meta <- sapply(names(exi_paths), function(step) {
    sapply(names(exi_paths[[step]]), function(prod) {
      table <- suppressWarnings(sen2r_getElements(exi_paths[[step]][[prod]], abort = FALSE))
      table$names <- exi_paths[[step]][[prod]]
      # filter
      if (is.null(table$prod_type)) {table$prod_type <- character()}
      if (is.null(table$mission)) {table$mission <- character()}
      if (is.null(table$level)) {table$level <- character()}
      if (is.null(table$file_ext)) {table$file_ext <- character()}
      table <- table[
        type != "unrecognised" &
          mission %in% toupper(substr(pm$sel_sensor,3,3)) & 
          level %in% toupper(substr(pm$s2_levels,2,3))
        ,]
      if (length(pm$timewindow)>0 & !anyNA(pm$timewindow) & length(table$sensing_date)>0) {
        table <- table[
          table$sensing_date>=pm$timewindow[1] & 
            table$sensing_date<=pm$timewindow[2]
          ,]
      }
      if (length(pm$s2orbits_selected)>0 & !anyNA(pm$s2orbits_selected) & length(table$id_orbit)>0) {
        table <- table[id_orbit %in% pm$s2orbits_selected,]
      }
      table
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  rm(exi_paths)
  # Specific filters
  if (length(pm$s2tiles_selected)>0 & !anyNA(pm$s2tiles_selected)) {
    for (step in c("tiles")) {
      exi_meta[[step]] <- sapply(names(exi_meta[[step]]), function(prod) {
        table <- exi_meta[[step]][[prod]]
        table <- table[extent_name %in% pm$s2tiles_selected,]
        table
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
  }
  if (length(pm$extent_name)>0 & !anyNA(pm$extent_name)) {
    for (step in c("warped", "warped_scl", "masked", "rgb", "indices")) {
      exi_meta[[step]] <- sapply(names(exi_meta[[step]]), function(prod) {
        table <- exi_meta[[step]][[prod]]
        table <- table[extent_name %in% pm$extent_name,]
        table
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
  }
  # Filters on file extension
  # (already done for tiles, merged, rgb, indices)
  for (step in c("warped", "warped_scl", "masked")) {
    exi_meta[[step]] <- sapply(names(exi_meta[[step]]), function(prod) {
      table <- exi_meta[[step]][[prod]]
      table <- table[file_ext %in% fileext[step],]
      table
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  # Recreate list of paths
  exi_paths <- sapply(exi_meta, function(x) {
    sapply(x, function(y) {y$names}, simplify = TRUE, USE.NAMES = FALSE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  # Filter names included in ignorelist
  exi_paths <- sapply(exi_paths, function(x) {
    sapply(x, function(y) {y[!y %in% ignorelist]}, simplify = TRUE, USE.NAMES = FALSE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  

  ## Expected files
  exp_paths <- list()
  
  # tiles
  if (steps_todo["tiles"]) {
    exp_paths[["tiles"]] <- sapply(list_prods, function(prod){
      sapply(
        c(file.path(pm$path_l1c,names(s2_list_l1c)), file.path(pm$path_l2a,names(s2_list_l2a))),
        function(safe){
          sel_av_tiles <- tryCatch(
            safe_getMetadata(safe,"tiles"),
            error = function(e){safe_getMetadata(safe,"nameinfo")$id_tile}
          )
          file.path(
            paths["tiles"],
            if (pm$path_subdirs) {prod} else {""},
            basename(safe_shortname(
              safe, prod_type=prod, ext=fileext["tiles"], 
              res=pm$res_s2, tiles=pm$s2tiles_selected, 
              force_tiles=force_tiles, multiple_names=TRUE
            ))
          )
        },
        simplify = TRUE, USE.NAMES = FALSE
      ) %>%
        c(exi_paths$tiles[[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # merged
  if (steps_todo["merged"]) {
    exp_paths[["merged"]] <- sapply(list_prods, function(prod) {
      sen2r_getElements(exp_paths[[output_dep["merged"]]][[prod]])[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"__",
        prod_type,"_",
        substr(res,1,2),".",
        fileext["merged"]
      )] %>%
        file.path(
          paths["merged"],
          if (pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        c(exi_paths[["merged"]][[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # warped
  if (steps_todo["warped"]) {
    exp_paths[["warped"]] <- sapply(list_prods[list_prods != "SCL"], function(prod) {
      sen2r_getElements(exp_paths[[output_dep["warped"]]][[prod]])[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_", 
        ExtentName,"_",
        prod_type,"_",
        substr(res,1,2),".",
        fileext["warped"]
      )] %>%
        file.path(
          paths["warped"],
          if (pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        c(exi_paths[["warped"]][[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # SCL
  if (steps_todo["warped_scl"]) {
    exp_paths[["warped_scl"]] <- sapply(list_prods[list_prods == "SCL"], function(prod) {
      sen2r_getElements(exp_paths[[output_dep["warped_scl"]]][[prod]])[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_", 
        ExtentName,"_",
        prod_type,"_",
        substr(res,1,2),".",
        fileext["warped_scl"]
      )] %>%
        file.path(
          paths["warped_scl"],
          if (pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        c(exi_paths[["warped_scl"]][[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # RGB
  if (steps_todo["rgb"]) {
    exp_paths[["rgb"]] <- sapply(list_rgb, function(prod) {
      sen2r_getElements(unlist(exp_paths[[output_dep["rgb"]]]))[
        level == switch(substr(prod,7,7), T = "1C", B = "2A"),
        paste0(
          "S2",
          mission,
          level,"_",
          strftime(sensing_date,"%Y%m%d"),"_",
          id_orbit,"_",
          ExtentName,"_",
          "<rgbname>_",
          substr(res,1,2),".",
          rgb_ext
        )] %>%
        unique() %>%
        file.path(
          paths["rgb"],
          if(pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        gsub("<rgbname>", prod , .)  %>%
        c(exi_paths[["rgb"]][[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # masked
  if (steps_todo["masked"]) {
    exp_paths[["masked"]] <- sapply(list_prods[list_prods != "SCL"], function(prod) {
      sen2r_getElements(exp_paths[[output_dep["masked.nonscl"]]][[prod]])[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_", 
        ExtentName,"_",
        prod_type,"_",
        substr(res,1,2),".",
        fileext["masked"]
      )] %>%
        file.path(
          paths["masked"],
          if (pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        c(exi_paths$masked[[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # indices
  if (steps_todo["indices"]) {
    exp_paths[["indices"]] <- sapply(list_indices, function(prod) {
      sen2r_getElements(unlist(exp_paths[[output_dep["indices"]]]))[
        level %in% level_for_indices,
        paste0(
          "S2",
          mission,
          level,"_",
          strftime(sensing_date,"%Y%m%d"),"_",
          id_orbit,"_", 
          ExtentName,"_",
          "<index>_",
          substr(res,1,2),".",
          fileext["indices"]
        )] %>%
        file.path(
          paths["indices"],
          if (pm$path_subdirs) {prod} else {""},
          .
        ) %>%
        gsub("<index>", prod , .) %>%
        c(exi_paths$indices[[prod]]) %>%
        remove_duplicates()
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # Filter names included in ignorelist
  exp_paths <- sapply(exp_paths, function(x) {
    sapply(x, function(y) {
      y[!y %in% ignorelist]
    }, simplify = FALSE, USE.NAMES = FALSE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  

  ## New (missing) and required files
  new_paths <- req_paths <- list()
  
  # indices
  if (steps_todo["indices"]) {
    new_paths[["indices"]] <- nonex_paths(exp_paths[["indices"]], pm$overwrite)
    req_paths[["indices"]] <- list()
    req_paths[["indices"]][[pm$index_source]] <- if (length(unlist(new_paths[["indices"]])) == 0) {
      character(0)
    } else {
      sen2r_getElements(
        unlist(new_paths[["indices"]])
      )[level %in% level_for_indices,
        paste0(
          "S2",
          mission,
          level,"_",
          strftime(sensing_date,"%Y%m%d"),"_",
          id_orbit,"_", 
          ExtentName,"_",
          pm$index_source,"_",
          substr(res,1,2),".",
          fileext[output_dep["indices"]]
        )] %>%
        remove_duplicates() %>%
        file.path(
          paths[output_dep["indices"]],
          if (pm$path_subdirs) {pm$index_source} else {""},
          .
        )
    }
  }
  
  # masked
  if (steps_todo["masked"]) {
    exp_paths[["masked"]] <- merge_exp_req(exp_paths, req_paths, "masked")
    new_paths[["masked"]] <- nonex_paths(exp_paths[["masked"]], pm$overwrite)
    req_paths[["masked"]] <- sapply(list_prods[list_prods!="SCL"], function(prod) {
      if (length(new_paths[["masked"]][[prod]]) == 0) {
        character(0)
      } else {
        sen2r_getElements(new_paths[["masked"]][[prod]])[
          ,paste0(
            "S2",
            mission,
            level,"_",
            strftime(sensing_date,"%Y%m%d"),"_",
            id_orbit,"_", 
            ExtentName,"_",
            prod,"_",
            substr(res,1,2),".",
            fileext[output_dep["masked.nonscl"]]
          )] %>%
          remove_duplicates() %>%
          file.path(
            paths[output_dep["masked.nonscl"]],
            if (pm$path_subdirs) {prod} else {""},
            .
          )
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
    req_paths[["masked"]][["SCL"]] <- if (length(new_paths[["masked"]][["SCL"]]) == 0) {
      character(0)
    } else {
      sen2r_getElements(
        unlist(new_paths[["masked"]])
      )[,paste0(
        "S2",
        mission,
        level,"_",
        strftime(sensing_date,"%Y%m%d"),"_",
        id_orbit,"_", 
        ExtentName,"_",
        "SCL","_",
        substr(res,1,2),".",
        fileext[output_dep["masked.scl"]]
      )] %>%
        remove_duplicates() %>%
        file.path(
          paths[output_dep["masked.scl"]],
          if (pm$path_subdirs) {"SCL"} else {""},
          .
        )
    }
  }
  
  # rgb
  if (steps_todo["rgb"]) {
    new_paths[["rgb"]] <- nonex_paths(exp_paths[["rgb"]], pm$overwrite)
    req_paths[["rgb"]] <- sapply(list_prods[list_prods!="SCL"], function(prod) {
      if (length(new_paths[["rgb"]][[prod]]) == 0) {
        character(0)
      } else {
        sen2r_getElements(unlist(
          new_paths[["rgb"]][substr(names(new_paths[["rgb"]]),7,7) == substr(prod,1,1)]
        ))[
          level == switch(prod, TOA = "1C", BOA = "2A"),
          paste0(
            "S2",
            mission,
            level,"_",
            strftime(sensing_date,"%Y%m%d"),"_",
            id_orbit,"_", 
            ExtentName,"_",
            prod,"_",
            substr(res,1,2),".",
            fileext[output_dep["rgb"]]
          )] %>%
          remove_duplicates() %>%
          file.path(
            paths[output_dep["rgb"]],
            if (pm$path_subdirs) {prod} else {""},
            .
          )
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # warped_scl
  if (steps_todo["warped_scl"]) {
    exp_paths[["warped_scl"]] <- merge_exp_req(exp_paths, req_paths, "warped_scl")
    new_paths[["warped_scl"]] <- nonex_paths(exp_paths[["warped_scl"]], pm$overwrite)
    req_paths[["warped_scl"]] <- list(
      "SCL" = if (length(new_paths[["warped_scl"]][["SCL"]]) == 0) {
        character(0)
      } else {
        sen2r_getElements(new_paths[["warped_scl"]][["SCL"]])[
          ,paste0(
            "S2",
            mission,
            level,"_",
            strftime(sensing_date,"%Y%m%d"),"_",
            id_orbit,"_", 
            "_",
            "SCL","_",
            substr(res,1,2),".",
            fileext[output_dep["warped_scl"]]
          )] %>%
          remove_duplicates() %>%
          file.path(
            paths[output_dep["warped_scl"]],
            if (pm$path_subdirs) {"SCL"} else {""},
            .
          )
      }
    )
  }
  
  # warped
  if (steps_todo["warped"]) {
    exp_paths[["warped"]] <- merge_exp_req(exp_paths, req_paths, "warped")
    new_paths[["warped"]] <- nonex_paths(exp_paths[["warped"]], pm$overwrite)
    req_paths[["warped"]] <- sapply(list_prods[list_prods!="SCL"], function(prod) {
      if (length(new_paths[["warped"]][[prod]]) == 0) {
        character(0)
      } else {
        sen2r_getElements(new_paths[["warped"]][[prod]])[
          ,paste0(
            "S2",
            mission,
            level,"_",
            strftime(sensing_date,"%Y%m%d"),"_",
            id_orbit,"_", 
            "_",
            prod,"_",
            substr(res,1,2),".",
            fileext[output_dep["warped"]]
          )] %>%
          remove_duplicates() %>%
          file.path(
            paths[output_dep["warped"]],
            if (pm$path_subdirs) {prod} else {""},
            .
          )
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # merged
  if (steps_todo["merged"]) {
    exp_paths[["merged"]] <- merge_exp_req(exp_paths, req_paths, "merged")
    new_paths[["merged"]] <- nonex_paths(exp_paths[["merged"]], pm$overwrite)
    req_paths[["merged"]] <- sapply(list_prods, function(prod) {
      if (length(new_paths[["merged"]][[prod]]) == 0) {
        character(0)
      } else {
        sen2r_getElements(new_paths[["merged"]][[prod]])[
          ,paste0(
            "S2",
            mission,
            level,"_",
            strftime(sensing_date,"%Y%m%d"),"_",
            id_orbit,"_", 
            "[0-9]{2}[A-Z]{3}_",
            prod,"_",
            substr(res,1,2),".",
            fileext[output_dep["merged"]]
          )] %>%
          # a bit different respect to other steps:
          # since it is not possible to easily know which tiles are required by a merged product,
          # the list of exp_paths[["tiles"]] matching each merged file is computed.
          # So, only existing tiles are used.
          # Since file names are used as regexp, possibly problems could appear
          # in case some special characters are present in the file name.
          lapply(function(x){
            exp_paths[[output_dep["merged"]]][[prod]][grep(x, exp_paths[[output_dep["merged"]]][[prod]])]
          }) %>%
          unlist() %>%
          remove_duplicates()
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # tiles
  if (steps_todo["tiles"]) {
    exp_paths[["tiles"]] <- merge_exp_req(exp_paths, req_paths, "tiles")
    new_paths[["tiles"]] <- nonex_paths(exp_paths[["tiles"]], pm$overwrite)
    req_paths[["tiles"]] <- if (sum(sapply(new_paths[["tiles"]], length)) == 0) {
      list("L1C" = character(0), "L2A" = character(0))
    } else {
      safe_dt_av <- lapply(c(names(s2_list_l1c),names(s2_list_l2a)), function(x) {
        unlist(safe_getMetadata(x, info=c("nameinfo"))) %>%
          t() %>%
          as.data.frame(stringsAsFactors=FALSE)
      }) %>%
        rbindlist(fill=TRUE)
      tiles_basenames_av <- safe_dt_av[,paste0(
        "S",mission,level,"_",
        strftime(as.POSIXct(sensing_datetime, format="%s"),"%Y%m%d"),"_",
        id_orbit,"_",
        ifelse(id_tile!="",id_tile,"[A-Z0-9]{5}"),"_",
        "[A-Z0-9]{3}_",
        "[126]0\\.",
        fileext["tiles"]
      )]
      list(
        "L1C" = names(s2_list_l1c)[
          lapply(tiles_basenames_av[safe_dt_av$level=="1C"],
                 function(x){grep(x,unlist(new_paths[["tiles"]]))} %>% length() > 0) %>%
            unlist()
          ] %>%
          file.path(pm$path_l1c,.),
        "L2A" = names(s2_list_l2a)[
          lapply(tiles_basenames_av[safe_dt_av$level=="2A"],
                 function(x){grep(x,unlist(new_paths[["tiles"]]))} %>% length() > 0) %>%
            unlist()
          ] %>%
          file.path(pm$path_l2a,.)
      )
    }
  }
  
  list("exi" = exi_paths, "exp" = exp_paths, "new" = new_paths, "req" = req_paths)
  
}

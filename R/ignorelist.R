#' @title Read / write the ignore list
#' @description Internal functions to read or write the file containing
#'  information about images not to process (because cloud covered or because, 
#'  for any reason, they were not produced during previous processing runs).
#' @details Sometimes not all the output files are correctly created:
#'  the main reason is the cloud coverage higher than the maximum allowed
#'  value (argument `max_mask`), but also some other unexpected reasons could
#'  happen, i.e. because of old name SAFE products which do not include all the tiles.
#'  To prevent to try to create these files every time the function is called
#'  with the same parameter file, the TOML file `".ignorelist.txt"` is created
#'  in the directory where output files (or indices, or RGB images) are saved.
#'  With sen2r <= 1.3.3, a different strategy was used: if `param_list` is a path,
#'  these lists were saved in two hidden files ( one per file not created 
#'  because of cloud coverage, one other for all the other reasons).
#'  To try it again, delete the files/files or set `overwrite = TRUE` in `sen2r()`).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0

#' @name write_ignorelist
#' @rdname ignorelist
#' @param pm parameter list (passed by `sen2r()`).
#' @param names_cloudcovered paths of cloud covered images (passed by `sen2r()`).
#' @param dates_cloudcovered dates of cloud covered images (passed by `sen2r()`)
#'  (this is used only if `names_cloudcovered` is not specified).
#' @param names_missing paths of non produced images (passed by `sen2r()`).
#' @param param_list path of the parameter file (passed by `sen2r()`).
#' @return `write_ignorelist()` returns the path of the written TOML file
#'  (invisibly).

write_ignorelist <- function(
  pm,
  names_cloudcovered = NULL,
  dates_cloudcovered = NULL,
  names_missing = NULL,
  param_list = NULL
) {
  
  # Parameters used for processing
  max_mask <- pm$max_mask
  mask_type <- if (is.na(pm$mask_type)) {""} else {pm$mask_type}
  
  # Add previous files to be ignored
  suppressWarnings(
    previous_ignorelist <- read_ignorelist(pm = pm, param_list = param_list)
  )
  
  # Retrieve dates_cloudcovered from names_cloudcovered
  dates_cloudcovered <- if (!missing(names_cloudcovered)) {
    sort(unique(c(
      previous_ignorelist$dates_cloudcovered,
      sen2r_getElements(names_cloudcovered)$sensing_date
    )))
  } else {
    sort(unique(c(
      previous_ignorelist$dates_cloudcovered,
      dates_cloudcovered
    )))
  }
  names_missing <- sort(unique(c(
    previous_ignorelist$names_missing,
    basename(nn(names_missing))
  )))
  
  # Prepare the file (in TOML-like syntax)
  ignorelist_text <- c(
    "## --- List of images to be ignored because cloud-covered ---",
    "## --- or not produced for other reasons.                 ---",
    "",
    "# Masking parameters used during processing",
    paste0("mask_type = \"", mask_type, "\""),
    paste0("max_mask = ", max_mask),
    "",
    "# Dates resulted cloud-covered using the above-defined parameters",
    if (length(dates_cloudcovered) > 0) {c(
      "dates_cloudcovered = [",
      paste0("  ",dates_cloudcovered,", "),
      "]"
    )} else {
      "dates_cloudcovered = []"
    },
    "",
    "# Images not produced for any reason",
    "# (this entry can be deleted in case the user wants to retry processing them)",
    if (length(names_missing) > 0) {c(
      "names_missing = [",
      paste0("  \"",names_missing,"\", "),
      "]"
    )} else {
      "names_missing = []"
    },
    "",
    "# --- archive produced with sen2r (https://sen2r.ranghetti.info/) ---"
  )
  
  # Determine out_dir
  ignorelist_path <- path_ignorelist(pm)
  # Write file
  if (length(ignorelist_path) > 0) {
    writeLines(ignorelist_text, ignorelist_path)
  }
  
  invisible(ignorelist_path)
  
}


#' @name read_ignorelist
#' @rdname ignorelist
#' @return `read_ignorelist()` returns a list with two vectors:
#'  - `dates_cloudcovered` (dates of cloud covered files);
#'  - `names_missing` (base names of files to be ignored).
#' @importFrom RcppTOML parseTOML

read_ignorelist <- function(pm, param_list = NULL) {
  
  # Parameters used for processing
  max_mask <- pm$max_mask
  mask_type <- if (is.na(pm$mask_type)) {""} else {pm$mask_type}
  
  # Retrieve / generate the ignorelist path
  # retrieve it in path_out, OR in path_indices, OR in path_rgb (in this order)
  exists_in_out <- !is.na(pm$path_out) &&
    file.exists(file.path(pm$path_out, ".ignorelist.txt"))
  exists_in_indices <- !is.na(pm$path_indices) &&
    file.exists(file.path(pm$path_indices, ".ignorelist.txt"))
  exists_in_rgb <- !is.na(pm$path_rgb) &&
    file.exists(file.path(pm$path_rgb, ".ignorelist.txt"))
  ignorelist_path <- if (exists_in_out) {
    file.path(pm$path_out, ".ignorelist.txt")
  } else if (exists_in_indices) {
    file.path(pm$path_indices, ".ignorelist.txt")
  } else if (exists_in_rgb) {
    file.path(pm$path_rgb, ".ignorelist.txt")
  } else {
    character(0)
  } 
  
  ignorelist <- if (length(ignorelist_path) > 0) {
    
    # Read the existing ignorelist
    tomlist <- parseTOML(ignorelist_path)
    tomlist$max_mask <- as.integer(nn(tomlist$max_mask))
    tomlist$dates_cloudcovered <- as.Date(nn(tomlist$dates_cloudcovered))
    tomlist$names_missing <- nn(tomlist$names_missing)
    
    # Check that mask_type / max_mask did not change (warn otherwise)
    if (any(
      tomlist$mask_type != as.character(mask_type),
      tomlist$max_mask != as.character(max_mask)
    )) {
      print_message(
        type = "warning",
        "The ignorelist was saved using ",
        "\"mask_type\" = '",tomlist$mask_type,"' and ",
        "\"max_mask\" = ",tomlist$max_mask,", ",
        "while this processing is using ",
        "\"mask_type\" = '",mask_type,"' and ",
        "\"max_mask\" = ",max_mask,". ",
        "To avoid unexpected results, delete the file ",
        "\"",ignorelist_path,"\"."
      )
    }
    
    tomlist[c("dates_cloudcovered", "names_missing")]  
    
    
  } else if (length(param_list) > 0) {
    
    # Check if an old-format ignorelist exists
    oldignorelist_path <- gsub("\\.json$","_ignorelist.txt",param_list)
    oldcloudlist_path <- gsub("\\.json$","_cloudlist.txt",param_list)
    ignorelist0 <- if (file.exists(oldignorelist_path)) {
      readLines(oldignorelist_path)
    } else {
      character()
    }
    cloudlist0 <- if (file.exists(oldcloudlist_path)) {
      readLines(oldcloudlist_path)
    } else {
      character()
    }
    list(
      names_missing = sort(unique(basename(ignorelist0))),
      dates_cloudcovered = sort(unique(sen2r_getElements(cloudlist0)$sensing_date))
    )
    
  } else {
    
    # If neither exist, use an amply list
    list(
      names_missing = character(0),
      dates_cloudcovered = as.Date(character(0))
    )
    
  }
  
  # Return
  ignorelist
  
}


#' @name path_ignorelist
#' @rdname ignorelist
#' @return `path_ignorelist()` returns the path in which the TOML file
#'  should be written (basing on processing parameters).

path_ignorelist <- function (pm) {
  out_dir <- if (!is.na(pm$path_out)) {
    pm$path_out
  } else if (!is.na(pm$path_indices)) {
    pm$path_indices
  } else if (!is.na(pm$path_rgb)) {
    pm$path_rgb
  }
  if (length(out_dir) > 0) {
    normalize_path(file.path(out_dir, ".ignorelist.txt"), mustWork = FALSE)
  } else {
    NULL
  }
}


#' @name clean_ignorelist
#' @rdname ignorelist
#' @return `clean_ignorelist()` returns NULL (it is called for its side effects).

clean_ignorelist <- function (pm, param_list = NULL) {
  
  ignorelist_path <- path_ignorelist(pm) # path to be used
  
  # Search for ignorelists in different directories
  indicesignorelist_path <- normalize_path(
    file.path(pm$path_indices, ".ignorelist.txt"), 
    mustWork = FALSE
  )
  rgbignorelist_path <- normalize_path(
    file.path(pm$path_rgb, ".ignorelist.txt"), 
    mustWork = FALSE
  )
  if (!is.na(pm$path_indices) &&
      file.exists(indicesignorelist_path) &&
      indicesignorelist_path != ignorelist_path) {
    file.remove(indicesignorelist_path)
  }
  if (!is.na(pm$path_rgb) &&
      file.exists(rgbignorelist_path) &&
      rgbignorelist_path != ignorelist_path) {
    file.remove(rgbignorelist_path)
  }
  
  # Search for old format ignorelists
  if (length(param_list) > 0) {
    oldignorelist_path <- normalize_path(
      gsub("\\.json$","_ignorelist.txt",param_list), 
      mustWork = FALSE
    )
    oldcloudlist_path <- normalize_path(
      gsub("\\.json$","_cloudlist.txt",param_list), 
      mustWork = FALSE
    )
    if (file.exists(oldignorelist_path)) {
      file.remove(oldignorelist_path)
    }
    if (file.exists(oldcloudlist_path)) {
      file.remove(oldcloudlist_path)
    }
  }
  
  invisible(NULL)
  
}

#' @title Check a parameter list
#' @description Check that the parameter list (or JSON parameter file)
#'  is in the correct format, and then speficied values are coherent with 
#'  parameters.
#' @param pm List of parameters or path of a JSON parameter file.
#' @param type Type of the output (see [print_message] for details).
#' @param correct Logical: if TRUE (default), the function corrects
#'  some incoherences (e.g. timewindow of length 1 is transformed in length 2)
#'  and returns the corrected list as output; if false, only checking is 
#'  performed, and the output is NULL if no errors occur.
#' @return In case of errors, depending on `type` argument, output can be 
#'  a vector of errors (if `type = "string"`), 
#'  the first error occurred (if `type = "error"`)
#'  or a set of warnings (if `type = "warning"`). 
#'  If no errors occur, output is the corrected parameter list if 
#'  `correct = TRUE` or NULL otherwise.
#'  
#' @importFrom jsonlite fromJSON
#' @importFrom methods is
#' @importFrom stringr str_pad
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


check_param_list <- function(pm, type = "string", correct = TRUE) {
  
  # to avoid NOTE on check
  . <- NULL
  
  # check the output type
  
  # check the format of pm object
  if (is(pm, "character")) {
    if (file.exists(pm)) {
      # load json parameter file
      pm <- jsonlite::fromJSON(pm)
    } else {
      print_message(
        type = "error",
        "The file ",pm," does not exist."
      )
    }
  } else if (!is(pm, "list")) {
    print_message(
      type = "error",
      "\"",deparse(substitute(pm)),"\"",
      "must be a list or a path of a JSON parameter file."
    )
  }
  
  # TODO check the names of the content of the list
  
  # TODO check package version and parameter names
  
  # check processing_order
  if (length(nn(pm$processing_order) > 0)) {
    if (pm$preprocess == FALSE) {
      pm$processing_order <- "by_step"
      # in case no preprocessing is required, "by_step" is the only accepted value
    }
  }
  
  # check timewindow
  if (!anyNA(pm$timewindow)) {
    if (length(pm$timewindow)==1) {
      if (is(pm$timewindow, "numeric") | is(pm$timewindow, "difftime")) {
        pm$timewindow <- c(Sys.Date() - pm$timewindow, Sys.Date())
      } else {
        pm$timewindow <- rep(pm$timewindow, 2)
      }
    } else if (length(pm$timewindow)>2) {
      print_message(
        type = type,
        "Parameter 'timewindow' must be of length 1 or 2."
      )
    }
    if (is(pm$timewindow, "character")) {
      tryCatch(pm$timewindow <- as.Date(pm$timewindow), error = print)
    } else if (is(pm$timewindow, "POSIXt")) {
      pm$timewindow <- as.Date(pm$timewindow)
    }
    if (!is(pm$timewindow, "Date")) {
      print_message(
        type = type,
        "Parameter 'timewindow' must be a Date object."
      )
    }
  } else if (pm$online == TRUE) {
    # in online mode, NA value is converted to last 90 days
    pm$timewindow <- c(Sys.Date() - 90, Sys.Date())
  }
  
  # check the extent name
  if (is.na(pm$extent_name) || length(nn(pm$extent_name))==0 || pm$extent_name=="") {
    print_message(
      type = type,
      "The extent name (parameter \"extent_name\" ) can not be empty."
    )
  }
  if (grepl("[ \\.\\_]", pm$extent_name)) {
    print_message(
      type = type,
      "The extent name (parameter \"extent_name\" ) can not contain ",
      "spaces, points nor underscores."
    )
  }
  if (grepl("^[0-9]{2}[A-Z]{3}$", pm$extent_name)) {
    print_message(
      type = type,
      "The extent name (parameter \"extent_name\" ) can not cannot be ",
      "a five-length string with the same structure of a tile ID",
      "(two numeric and three uppercase character values)."
    )
  }
  
  # check maximum cloud covers
  if (all(is.na(nn(pm$max_cloud_safe)))) {
    print_message(
      type = "warning",
      "Maximum SAFE cloud coverage was not specified; ",
      "setting it to 100 (all SAFE are used)."
    )
    pm$max_cloud_safe <- 100
  }
  if (all(is.na(nn(pm$max_mask)))) {
    print_message(
      type = "warning",
      "Maximum cloud coverage was not specified; ",
      "setting it to 80 (default value)."
    )
    pm$max_mask <- 80
  }
  
  # check output resolution
  if (!anyNA(pm$res) & any(pm$res <= 0)) {
    print_message(
      type = type,
      "Output custom resolution (parameter \"res\" ) must be positive."
    )
  }
  if ((!anyNA(pm$res) & !is.null(pm$res)) & (!anyNA(pm$res_s2) & !is.null(pm$res_s2))) {
    print_message(
      type = "warning",
      "Both native and custom resolution were provided; ",
      "only custom one (\"res\") will be used."
    )
    pm$res_s2 <- NA
  }
  if (!anyNA(pm$res_s2) & any(!pm$res_s2 %in% c("10m", "20m", "60m"))) {
    print_message(
      type = type,
      "Output native resolution (parameter \"res_s2\" ) is invalid ",
      "(accepted values are '10m', '20m' and '60m')."
    )
  }
  
  # Compute S2 resolution to be used
  if (any(!pm$res_s2 %in% c("10m","20m","60m"))) {
    pm$res_s2 <- if (as.integer(mean(pm$res)) >= 60) {"60m"} else if (as.integer(mean(pm$res)) >= 20) {"20m"} else {"10m"}
  }
  
  # Duplicate res if it is a 1-length value
  if (length(pm$res) == 1) {
    pm$res <- rep(pm$res, 2)
  }
  
  
  # check RGB objects
  if (all(is.array(pm$rgb_ranges), length(dim(pm$rgb_ranges)) > 2)) {
    pm$rgb_ranges <- asplit(pm$rgb_ranges, 1)
  }
  if (is.matrix(pm$rgb_ranges)) {
    pm$rgb_ranges <- list(pm$rgb_ranges)
  }
  if (all(is.na(pm$list_rgb), length(nn(pm$rgb_ranges))==0)) { # for compatibility
    pm$rgb_ranges <- NA
  }
  if (length(pm$rgb_ranges) != length(pm$list_rgb)) {
    print_message(
      type = type,
      "\"rgb_ranges\" and \"list_rgb\" must be of the same length."
    )
    pm$rgb_ranges <- pm$list_rgb <- NA
  }
  
  # check SAFE paths
  # if one of path_l1c and path_l2a is missing, copy from the other
  # FIXME this is a workaround for parameter pm$s2_levels,
  # whose default is c("l1c","l2a") even if L1C is not requested.
  # Fix by removing it and retrieve it automatically.
  if (sum(is.na(c(pm$path_l1c, pm$path_l2a)))==1) {
    if (is.na(pm$path_l1c)) {
      pm$path_l1c <- pm$path_l2a
    } else {
      pm$path_l2a <- pm$path_l1c
    }
  }
  
  # check output paths
  # (if no products are selected, set to NA)
  if (sum(!is.na(pm$list_indices))==0) {
    pm$path_indices <- NA
  } else if (is.na(pm$path_indices) | pm$path_indices=="") {
    pm$path_indices <- pm$path_out
  }
  if (sum(!is.na(pm$list_rgb))==0) {
    pm$path_rgb <- NA
  } else if (is.na(pm$path_rgb) | pm$path_rgb=="") {
    pm$path_rgb <- pm$path_out
  }
  if (sum(!is.na(nn(pm$list_prods)))==0) {
    pm$path_out <- NA
  }
  
  # check s2orbits_selected
  if (is(pm$s2orbits_selected, "numeric")) {
    pm$s2orbits_selected <- str_pad(pm$s2orbits_selected, 3, "left", "0")
  }
  
  # check consistency among mask_type and selected products
  # (if masking is selected but no prods or indices are selected, set to NA)
  if (
    !is.na(pm$mask_type) & 
    all(is.na(nn(pm$list_indices))) & 
    all(is.na(nn(pm$list_prods[pm$list_prods!="SCL"])))
  ) {
    pm$mask_type <- NA
  } 
  
  # check bands numbers for required RGB
  # (TOA:1-12; BOA: 1-9,11-12)
  if (!is.na(pm$path_rgb) & sum(!is.na(pm$list_rgb))>0) {
    rgb_bands <- lapply(
      strsplit(gsub("^RGB([0-9a-f]{3})([BT])$","\\1",pm$list_rgb),""), 
      function(x) {strtoi(paste0("0x",x))}
    )
    rgb_sources <- gsub("^RGB([0-9a-f]{3})([BT])$","\\2OA",pm$list_rgb)
    rgb_list <- foreach(i = seq_along(pm$list_rgb), .combine=c) %do% {
      if (any(
        rgb_bands[[i]]<1 | 
        rgb_bands[[i]]>12 | 
        rgb_bands[[i]]==10 & rgb_sources[i]=="BOA"
      )) {
        print_message(
          type = "warning",
          "RGB ",pm$list_rgb[i]," can not be computed (bands out of range)."
        )
        character(0)
      } else {
        pm$list_rgb[i]
      }
    }
    pm$list_rgb <- rgb_list
  }

  
  
  
  # WIP 
  
  if (correct==TRUE) {
    return(pm)
  } else {
    return(invisible(NULL))
  }
  
  
}
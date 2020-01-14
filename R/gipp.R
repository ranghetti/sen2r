#' @title Copy L2A_GIPP.xml in sen2r
#' @description Internal function to copy L2A_GIPP.xml from default Sen2Cor 
#'  directory to sen2r. After that, user will allow editing Sen2Cor options
#'  in sen2r without affecting standalone Sen2Cor behaviour.
#' @param gipp_sen2r_path Character path of the output GIPP XML file.
#'  By default it is equal to NA (meaning the default sen2r GIPP path).
#' @param force Logical: if TRUE, the file is copied in any case (this is used
#'  by `reset_gipp()`); if FALSE (default), only if it does not yet exist.
#' @return TRUE if the file was copied, FALSE elsewhere (invisible output)
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples
#' \dontrun{
#' gipp_init()
#' }
gipp_init <- function(gipp_sen2r_path = NA, force = FALSE) {
  
  binpaths <- load_binpaths()
  if (is.na(gipp_sen2r_path)) {
    gipp_sen2r_path <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  }
  
  # If Sen2Cor configuration file is missing, copy it from its default directory
  if (any(force == TRUE, !file.exists(gipp_sen2r_path))) {
    
    # get Sen2Cor version
    sen2cor_version_raw0 <- system(paste(binpaths$sen2cor, "-h"), intern = TRUE)
    sen2cor_version_raw1 <- sen2cor_version_raw0[grep(
      "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version:",
      sen2cor_version_raw0
    )]
    sen2cor_version <- gsub(
      "^Sentinel\\-2 Level 2A Processor \\(Sen2Cor\\)\\. Version: ([2-9]+\\.[0-9]+)\\.[0-9]+,.*$",
      "\\1",
      sen2cor_version_raw1
    )
    # define L2A_GIPP.xml path
    gipp_sen2cor_path <- file.path("~/sen2cor", sen2cor_version, "cfg/L2A_GIPP.xml")
    # (this assumes Sen2Cor to be installed and configured to be used with sen2r)
    
    if (!file.exists(gipp_sen2cor_path)) {
      print_message(
        type = "error", 
        "Sen2Cor configuration file was not found in its default directory"
      )
    }
    
    # copy file (this assumes SEN2COR_HOME to be ~/sen2cor)
    file.copy(gipp_sen2cor_path, gipp_sen2r_path, overwrite = force)
    invisible(TRUE)
    
  } else {
    invisible(FALSE)
  }
  
}


#' @title Manage GIPP parameters for Sen2Cor
#' @name read_gipp
#' @rdname gipp
#' @description `read_gipp()` reads Ground Image Processing Parameters (GIPP) 
#'  from an XML file.
#' @param gipp_names Character vector with the names of the parameters 
#'  to be read.
#' @param gipp_path_in Character path of the GIPP XML file to be read.
#'  If NA (default), the default sen2r GIPP path is used.
#' @return `read_gipp()` returns a named list of GIPP with the required parameters
#'  (values not found in the XML are skipped).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export
read_gipp <- function(gipp_names, gipp_path_in = NA) {
  
  binpaths <- load_binpaths()
  
  # Copy L2A_GIPP.xml within .sen2r if missing; otherwise, check that it exists
  if (is.na(gipp_path_in)) {
    gipp_init()
    gipp_path_in <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  } else {
    gipp_path_in <- normalize_path(gipp_path_in, mustWork = TRUE)
  }
  
  gipp_xml <- readLines(gipp_path_in)
  
  # Create and populate the parameter list
  gipp <- list()
  for (sel_par in gipp_names) {
    sel_line <- grep(paste0("<",sel_par,">.+</",sel_par,">"), gipp_xml, ignore.case = TRUE)[1]
    gipp[[sel_par]] <- gsub(
      paste0("^.*<",sel_par,">(.+)</",sel_par,">.*$"), "\\1",
      gipp_xml[sel_line],
      ignore.case = TRUE
    )
  }
  gipp[!is.na(gipp)]
  
}


#' @name set_gipp
#' @rdname gipp
#' @description `set_gipp()` modifies values of a list of GIPP in an XML file.
#' @param gipp List of Ground Image Processing Parameters (GIPP),
#'  in the form `parameter_name = "value"`, where 
#'  `parameter_name` is the name of the parameter as specified in the 
#'  `L2A_GIPP.xml` file of the used Sen2Cor version (case insensitive), and
#'  `"value"` is the character value which the user wants to set 
#'  (notice that, in the case the user wants to specify the value `NONE`,
#'  both `"NONE"` and `NA` can be used, but not `NULL`, which has the effect
#'  to maintain the value specified in the XML file).
#'  Elements whose name is missing in the XML file are skipped.
#' @param gipp_path_out Character path of the output GIPP XML file.
#'  By default it is equal to `gipp_path_in` in function `set_gipp()`
#'  (edited values are overwritten) and to NA (meaning the default sen2r GIPP
#'  path) in `reset_gipp()`.
#' @return `set_gipp()` and `reset_gipp()` return NULL
#'  (functions are called for their side effects).
#' @export
#' @examples
#' \donttest{
#' # Read default values
#' read_gipp(c("dem_directory", "dem_reference"))
#' # Edit one value and save the output as temporary file
#' set_gipp(list(DEM_Directory = NA), gipp_path_out = gipp_temp <- tempfile())
#' # Read the parameters in the created temporary files
#' read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path_in = gipp_temp)
#' # Read the original (not edited) XML file
#' read_gipp(c("DEM_Directory", "DEM_Reference"))
#' # Reset to default Sen2Cor GIPP values
#' reset_gipp(gipp_path_out = gipp_temp)
#' # Read again values
#' read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path_in = gipp_temp)
#' }
set_gipp <- function(gipp, gipp_path_in = NA, gipp_path_out = gipp_path_in) {
  
  binpaths <- load_binpaths()
  
  # Copy L2A_GIPP.xml within .sen2r if missing; otherwise, check that it exists
  if (is.na(gipp_path_in)) {
    gipp_init() # copy L2A_GIPP.xml within .sen2r if missing
    gipp_path_in <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  } else {
    gipp_path_in <- normalize_path(gipp_path_in, mustWork = TRUE)
  }
  if (missing(gipp_path_out)) {
    gipp_path_out <- gipp_path_in
  }
  
  gipp_xml <- readLines(gipp_path_in)
  gipp <- gipp[!sapply(gipp, is.null)] # NULL: leave as default (so, skip)
  for (i in which(is.na(gipp))) {gipp[[i]] <- "NONE"} # NA -> "NONE"
  
  # Edit parameter values
  for (sel_par in names(gipp)) {
    sel_line <- grep(paste0("<",sel_par,">.+</",sel_par,">"), gipp_xml, ignore.case = TRUE)[1]
    gipp_xml[sel_line] <- gsub(
      paste0("(^.*<",sel_par,">).+(</",sel_par,">.*$)"),
      paste0("\\1",as.character(gipp[[sel_par]][1]),"\\2"), 
      gipp_xml[sel_line],
      ignore.case = TRUE
    )
  }
  writeLines(gipp_xml, gipp_path_out)
  invisible(NULL)
  
}


#' @name reset_gipp
#' @rdname gipp
#' @description `reset_gipp()` restores default GIPP values (using values
#'  set in the `L2A_GIPP.xml` default Sen2Cor file).
#' @export
reset_gipp <- function(gipp_path_out = NA) {
  gipp_init(gipp_sen2r_path = gipp_path_out, force = TRUE)
}
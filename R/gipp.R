#' @title Copy L2A_GIPP.xml in sen2r
#' @description Internal function to copy L2A_GIPP.xml from default Sen2Cor 
#'  directory to sen2r. After that, user will allow editing Sen2Cor options
#'  in sen2r without affecting standalone Sen2Cor behaviour.
#' @param gipp_sen2r_path Character path of the output GIPP XML file.
#'  By default it is equal to NA (meaning the default sen2r GIPP path).
#' @param force Logical: if TRUE, the file is copied in any case (this is used
#'  by [reset_gipp()]); if FALSE (default), only if it does not yet exist.
#' @param dem_warning TEMPORARY ARGUMENT Logical: if TRUE, a warning about
#'  the fact that DEM_Directory XML parameter was not overwritten is shown
#'  (default is FALSE).
#'  This argument will be removed when use_dem = TRUE will become the default.
#' @return TRUE if the file was copied, FALSE elsewhere (invisible output)
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples
#' \dontrun{
#' gipp_init()
#' }
gipp_init <- function(gipp_sen2r_path = NA, force = FALSE, dem_warning = FALSE) {
  
  binpaths <- load_binpaths()
  is_default_sen2r_gipp <- if (is.na(gipp_sen2r_path)) {
    gipp_sen2r_path <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
    TRUE
  } else {FALSE}
  
  # Check Sen2Cor to be installed
  if (is.null(binpaths$sen2cor)) {
    print_message(
      type = "error", 
      "Sen2Cor was not yet installed; ",
      "install it using function install_sen2cor() (command line) ",
      "or check_sen2r_deps() (GUI)."
    )
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
    
    # temporary WARNING about default use_dem value
    # (this will be removed / replaced when use_dem = TRUE will become the default value)
    if (dem_warning == TRUE) {
      print_message(
        type = "message",
        "Default Sen2Cor parameters were written in file ",
        normalize_path(
          file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml"), 
          mustWork = FALSE
        ), 
        ", and will be used with sen2cor() and sen2r() ",
        "unless different parameters will be specified.\n",
        "IMPORTANT NOTE: for backward compatibility, the parameter ",
        "\"DEM_Directory\" was maitained to its default value. ",
        "This does not grant homogeneity between Level-2A SAFE products ",
        "generated locally with Sen2Cor and downloaded from ESA Hub ",
        "(which make use of DEM for topographic correction).",
        "To grant it, activate topographic correction using:\n",
        " > set_gipp(use_dem = TRUE)\n",
        "In a future sen2r release, this will be the default Sen2Cor behaviour."
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
#' @description [read_gipp()] reads Ground Image Processing Parameters (GIPP) 
#'  from an XML file.
#' @param gipp_names Character vector with the names of the parameters 
#'  to be read.
#' @param gipp_path Character path of the GIPP XML file to be read 
#'  ([read_gipp()]) or written ([set_gipp()] and [reset_gipp()]).
#'  If NA (default), the default sen2r GIPP path is used.
#' @return [read_gipp()] returns a named list of GIPP with the required parameters
#'  (values not found in the XML are skipped).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export
read_gipp <- function(gipp_names, gipp_path = NA) {
  
  binpaths <- load_binpaths()
  
  # Copy L2A_GIPP.xml within .sen2r if missing; otherwise, check that it exists
  if (is.na(gipp_path)) {
    gipp_init(dem_warning = TRUE) # remove dem_warning in future!
    gipp_path <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  } else {
    gipp_path <- normalize_path(gipp_path, mustWork = TRUE)
  }
  
  gipp_xml <- readLines(gipp_path)
  
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
#' @description [set_gipp()] modifies values of a list of GIPP in an XML file.
#' @param gipp (optional) Ground Image Processing Parameters (GIPP)
#'  (see [set_gipp()] for further details).
#'  Elements whose name is missing in the XML file are skipped.
#' @param gipp_path Character path of the output GIPP XML file.
#'  If NA (default), the default sen2r GIPP path is used.
#' @param use_dem Logical, determining if a DEM should be set for being used 
#'  for topographic correction in the XML specified with argument `gipp_path`
#'  (see [sen2cor()] for further details).
#' @return [set_gipp()] and [reset_gipp()] return NULL
#'  (functions are called for their side effects).
#' @export
#' @examples
#' \donttest{
#' # Read default values
#' read_gipp(c("dem_directory", "dem_reference"))
#' # Edit one value and save the output as temporary file
#' set_gipp(list(DEM_Directory = NA), gipp_path = gipp_temp <- tempfile())
#' # Read the parameters in the created temporary files
#' read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path = gipp_temp)
#' # Read the original (not edited) XML file
#' read_gipp(c("DEM_Directory", "DEM_Reference"))
#' # Reset to default Sen2Cor GIPP values
#' reset_gipp(gipp_path = gipp_temp)
#' # Read again values
#' read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path = gipp_temp)
#' }
set_gipp <- function(
  gipp = list(), gipp_path = NA, use_dem = NA
) {
  
  binpaths <- load_binpaths()
  
  # If gipp is a character, interpret as path and use the specified XML
  gipp_sen2r_path <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  if (is.character(gipp)) {
    gipp_curr_path <- normalize_path(gipp, mustWork = TRUE)
    gipp <- list()
  } else {
    # copy L2A_GIPP.xml within .sen2r if missing
    gipp_init(dem_warning = all(
      is.na(use_dem), 
      length(grep("DEM_Directory", names(gipp), ignore.case = TRUE)) == 0
    )) # remove dem_warning in future!
    gipp_curr_path <- gipp_sen2r_path
  }
  
  # Define gipp_path if missing
  if (is.na(gipp_path)) {
    gipp_path <- gipp_sen2r_path
  }
  
  gipp_xml <- readLines(gipp_curr_path)
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
  
  # Edit DEM_Directory basing on use_dem
  if (!is.na(use_dem)) {
    
    # Set DEM_Directory  
    sel_line_dir <- grep("<DEM_Directory>.+</DEM_Directory>", gipp_xml, ignore.case = TRUE)[1]
    if (use_dem == TRUE) {
      dem_dir_in <- gsub(
        paste0("^.*<DEM_Directory>(.+)</DEM_Directory>.*$"), "\\1", 
        gipp_xml[sel_line_dir],
        ignore.case = TRUE
      )
      if (dem_dir_in == "NONE") {
        dem_dir_in <- file.path(dirname(attr(binpaths, "path")), "srtm90")
      }
      dem_dir_out <- dem_dir_in
      dir.create(dem_dir_out, showWarnings = FALSE)
    } else if (use_dem == FALSE) {
      dem_dir_out <- "NONE"
    } else {
      print_message(type = "error", "\"use_dem\" must be a logical value.")
    }
    gipp_xml[sel_line_dir] <- gsub(
      paste0("(^.*<DEM_Directory>).+(</DEM_Directory>.*$)"),
      paste0("\\1",dem_dir_out,"\\2"), 
      gipp_xml[sel_line_dir],
      ignore.case = TRUE
    )
    
    # Set DEM_Reference
    sel_line_ref <- grep("<DEM_Reference>.+</DEM_Reference>", gipp_xml, ignore.case = TRUE)[1]
    if (use_dem == TRUE) {
      dem_ref_in <- gsub(
        paste0("^.*<DEM_Reference>(.+)</DEM_Reference>.*$"), "\\1", 
        gipp_xml[sel_line_ref],
        ignore.case = TRUE
      )
      if (dem_ref_in == "NONE") {
        dem_ref_in <- "http://data_public:GDdci@data.cgiar-csi.org/srtm/tiles/GeoTIFF/"
      }
      dem_ref_out <- dem_ref_in
      dir.create(dem_ref_out, showWarnings = FALSE)
      gipp_xml[sel_line_ref] <- gsub(
        paste0("(^.*<DEM_Reference>).+(</DEM_Reference>.*$)"),
        paste0("\\1",dem_ref_out,"\\2"), 
        gipp_xml[sel_line_ref],
        ignore.case = TRUE
      )
    }  
    
  }
  
  writeLines(gipp_xml, gipp_path)
  invisible(NULL)
  
}


#' @name reset_gipp
#' @rdname gipp
#' @description [reset_gipp()] restores default GIPP values (using values
#'  set in the `L2A_GIPP.xml` default Sen2Cor file).
#' @export
reset_gipp <- function(gipp_path = NA) {
  gipp_init(
    gipp_sen2r_path = gipp_path, 
    force = TRUE, 
    dem_warning = TRUE # remove dem_warning in future!
  )
}
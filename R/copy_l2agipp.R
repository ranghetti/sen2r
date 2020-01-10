#' @title Copy L2A_GIPP.xml in sen2r
#' @description Internal function to copy L2A_GIPP.xml from default Sen2Cor 
#'  directory to sen2r. After that, user will allow editing Sen2Cor options
#'  in sen2r without affecting standalone Sen2Cor behaviour.
#' @return TRUE if the file was copied, FALSE elsewhere (invisible output)
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples
#' \dontrun{
#' copy_l2agipp()
#' }
copy_l2agipp <- function() {
  
  binpaths <- load_binpaths()
  gipp_sen2r_path <- file.path(dirname(attr(binpaths, "path")), "sen2r_L2A_GIPP.xml")
  
  # If Sen2Cor configuration file is missing, copy it from its default directory
  if (!file.exists(gipp_sen2r_path)) {
    
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
    file.copy(gipp_sen2cor_path, gipp_sen2r_path)
    invisible(TRUE)
    
  } else {
    invisible(FALSE)
  }
  
}

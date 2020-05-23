#' @title Give permission to write settings on disk
#' @description In interactive mode, ask users for permission to create a 
#'  .sen2r settings directory, in which to store files required by the packages.
#'  The function can be used also in non-interactive mode by setting 
#'  `agree = TRUE`.
#'  The function has no effect if the directory already exists.
#' @param agree Logical: if TRUE, allow creating the hidden directory;
#'  if FALSE, do not allow it; if NA (default), the permission is asked to
#'  the user in interactive mode (in non-interactive mode, the permission is
#'  denied).
#' @return Logical: if TRUE, R was authorised saving in the directory;
#'  if FALSE, it was not and a temporary directory is being used.
#' @author Lorenzo Busetto, phD (2019) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0

give_write_permission <- function(agree = NA) {
  
  settings_dir <- normalize_path("~/.sen2r", mustWork = FALSE)
  
  if (!dir.exists(settings_dir)) {
    
    if (is.na(agree)) {
      if (interactive()) {
        message(paste(
          "\nsen2r would like to create a hidden folder in your home directory, ",
          paste0("named '",settings_dir,"', "),
          "in which storing some files required by the package:",
          "- 'paths.json', containing the paths of sen2r runtime dependencies ",
          "   (GDAL, aria2, Sen2Cor);",
          "- 'apihub.txt', in which saving the user's SciHub credentials;",
          "- 's2_tiles.rds', containing the Sentinel-2 tile boundaries;",
          "- the 'log' subfolder, used to keep trace of temporary files created ",
          "   during processing operations.\n",
          "Do you authorise this?\n",
          "Yes: folder '.sen2r' will be created permanently, and the files ",
          "   will be created and updated when needed. ",
          "   You will not see this message anymore.\n",
          "No: a temporary folder will be used and will be lost when exiting ",
          "   R. You will see this message every time you will restart R.\n",
          sep = "\n"
        ))
        choice <- ""
        while (!tolower(choice) %in% c("y", "yes", "n", "no")) {
          choice <- readline(prompt = "Choice (y/n): ")
        }
        agree <- tolower(choice) %in% c("y", "yes")
      } else {
        agree <- FALSE
      }
    }
    
    if (agree == TRUE) {
      dir.create(settings_dir)
    }
    
  } else {
    agree <- TRUE
  }
  
  return(agree)
  
}

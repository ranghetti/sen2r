#' @title Ask permission to write settings on disk
#' @description Ask users for permission to create a .sen2r settings directory,
#'  in which to store the files paths.json and apihub.txt.
#' @return 'logical' if TRUE, the user authorised saving in this directory.
#' @author Lorenzo Busetto, phD (2019) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0

ask_permission <- function() {
  
  settings_dir <- normalize_path("~/.sen2r", mustWork = FALSE)
  
  if (!dir.exists(settings_dir)) {
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
    if (tolower(choice) %in% c("y", "yes")) {
      dir.create(settings_dir)
    }
  } else {
    choice <- "y"
  }
  
  return(tolower(choice) %in% c("y", "yes"))
  
}

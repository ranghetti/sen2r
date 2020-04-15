#' @title Interface to GDAL Python-based utilities
#' @description This accessory function interfaces with GDAL 
#'  utilities (sen2r must be interfaced with a runtime GDAL
#'  environment, see `check_gdal()`).
#'  Python-based utilities are always called from a runtime GDAL;
#'  C-based ones are called using `sf::gdal_utils()`.
#' @param util Character: one among `"info"`, `"translate"`, `"warp"`, 
#'  `"demprocessing"` ,`"buildvrt"` (C-based),
#'  `"calc"` and `"fillnodata"` (Python-based).
#'  Other utilities were not implemented, since they are not used by sen2r.
#' @param source path of input layer(s); for `calc` this can be more than one.
#' @param destination Path of the output layer.
#' @param options Character vector with GDAL options.
#' @param quiet Logical: if TRUE, suppress printing of output for info
#'  (this argument is ignored in case package `sys` is not installed).
#' @param formula (for `util = "calc"`) Calculation in `gdalnumeric` syntax using
#'  `+`, `-`, `/`, `*`, or any `numpy` array functions (i.e. `log10()`).
#' @param processing Character: processing options for `util = "demprocessing"`.
#' @param colorfilename Character: name of colour file for `util = "demprocessing"`
#'  (mandatory if `processing="color-relief"`).
#' @return A logical (invisible) indicating success (i.e., TRUE); 
#'  in case of failure, an error is raised and FALSE is returned (in case of 
#'  Python-based utilities).
#' @importFrom sf gdal_utils
#' @export
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @examples 
#' # Define product names
#' examplename <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif",
#'   package = "sen2r"
#' )
#' 
#' \donttest{
#' ## gdalinfo
#' out0 <- gdalUtil("info", examplename, quiet = TRUE)
#' message(out0)
#' 
#' ## gdal_translate
#' outname1 <- tempfile(fileext = ".tif")
#' gdalUtil(
#'   "translate", 
#'   examplename, outname1,
#'   options = c("-tr", "2", "2", "-r", "cubicspline", "-co", "COMPRESS=DEFLATE")
#' )
#' oldpar <- par(mfrow = c(1,2), mar = rep(0,4))
#' image(stars::read_stars(examplename), rgb = c(11,8,4))
#' image(stars::read_stars(outname1), rgb = c(11,8,4))

#' 
#' ## gdalwarp
#' outname2 <- tempfile(fileext = ".tif")
#' gdalUtil(
#'   "warp", 
#'   examplename, outname2,
#'   options = c("-t_srs", "EPSG:32633", "-co", "COMPRESS=DEFLATE")
#' )
#' oldpar <- par(mfrow = c(1,2), mar = rep(0,4))
#' image(stars::read_stars(examplename), rgb = c(11,8,4))
#' image(stars::read_stars(outname2), rgb = c(11,8,4))
#' 
#' ## gdal_calc
#' outname3 <- tempfile(fileext = ".tif")
#' ndvirefname <- system.file(
#'   "extdata/out/S2A2A_20190723_022_Barbellino_NDVI_10.tif",
#'   package = "sen2r"
#' )
#' gdalUtil(
#'   "calc", 
#'   rep(examplename,2), outname3,
#'   formula = "10000*(A.astype(float)-B)/(A+B)",
#'   options = c("--A_band", "8", "--B_band", "4", "--type", "Int16")
#' )
#' oldpar <- par(mfrow = c(1,2), mar = rep(0,4))
#' image(stars::read_stars(ndvirefname))
#' image(stars::read_stars(outname3))
#' }



gdalUtil <-function(
  util = "info",
  source,
  destination = character(0),
  options = character(0),
  quiet = FALSE,
  formula = character(0),
  processing = character(0),
  colorfilename = character(0)
) {
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Check "util"
  utils <- c(
    "info" = "gdalinfo",
    "translate" = "gdal_translate",
    "warp" = "gdalwarp", 
    "demprocessing" = "gdaldem" , 
    "buildvrt" = "gdalbuildvrt",
    "calc" = "gdal_calc", 
    "fillnodata" = "gdal_fillnodata"
  )
  if (!util %in% names(utils)) {
    print_message(
      type = "error", 
      "Currently, method '",util,"' is not implemented."
    )
  }
  
  # Check additional arguments
  if (util != "info" && missing(destination)) {
    print_message(
      type = "error",
      "Argument \"destination\" is missing, with no default."
    )
  }
  if (util == "calc" && missing(formula)) {
    print_message(
      type = "error",
      "Argument \"formula\" is required for gdal_calc."
    )
  }
  if (util == "demprocessing" && missing(processing)) {
    print_message(
      type = "error",
      "Argument \"processing\" is required for gdaldem."
    )
  }
  if (util == "demprocessing" && processing == "color-relief" && missing(colorfilename)) {
    print_message(
      type = "error",
      "Argument \"colorfilename\" is required for gdaldem with mode 'color-relief'."
    )
  }
  
  # Normalise paths
  source <- normalize_path(source, mustWork = TRUE)
  destination <- normalize_path(destination, mustWork = FALSE)
  
  # Choose the modality to proceed (sf::gdal_utils or system call)
  if (util %in% c("calc", "fillnodata")) {
    
    ## System call mode
    
    # Define arguments
    gdal_args <- if (util %in% c("fillnodata")) {
      list(
        "sys" = c(options, source, destination),
        "base" = paste0(
          paste(options, collapse = " ")," ",
          source," ",destination
        )
      )
    } else if (util %in% c("calc")) {
      list(
        "sys" = c(
          unlist(lapply(seq_along(source), function(i) {c(paste0("-",LETTERS[i]), source[i])})),
          "--outfile", destination,
          "--calc", formula,
          options
        ),
        "base" = paste0(
          paste(paste0("-",LETTERS[seq_along(source)]," \"",source,"\""), collapse = " ")," ",
          "--outfile=\"",destination,"\" ",
          "--calc=\"",formula,"\" ",
          paste(options, collapse = " ")
        )
      )
    }
    
    # use exec_wait if sys is installed, system otherwise
    gdal_out <- if (requireNamespace("sys", quietly = TRUE)) {
      sel_log_output <- if (quiet) {
        tempfile(pattern = "stdout_", fileext = ".txt")
      } else {NA}
      sys::exec_wait(
        binpaths[[utils[util]]],
        args = gdal_args$sys,
        std_out = sel_log_output
      )
    } else {
      system(
        paste(binpaths[[utils[util]]], gdal_args$base),
        intern = Sys.info()["sysname"] == "Windows"
      )
    }
    invisible(gdal_out == 0)
    
    # end of system call modality
    
  } else {
    
    ## Use gdal_utils()
    gdal_utils(
      util = util,
      source = source,
      destination = destination,
      options = options,
      quiet = quiet,
      processing = processing,
      colorfilename = colorfilename
    )
    
  }
  
}

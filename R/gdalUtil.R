#' @title Interface to GDAL Python-based utilities
#' @description This accessory function interfaces with GDAL 
#'  utilities (sen2r must be interfaced with a runtime GDAL
#'  environment, see `check_gdal()`).
#'  Python-based utilities are always called from an existing runtime GDAL
#'  environment (see `check_gdal()`);
#'  C-based ones are called using `sf::gdal_utils()`
#'  (with the exception of `gdalUtil("buildvrt", ...)`, which is called from
#'  runtime GDAL if `sf` version < 0.9-2).
#' @param util Character: one among `"info"`, `"translate"`, `"warp"`, 
#'  `"demprocessing"` ,`"buildvrt"` (C-based),
#'  `"calc"` and `"fillnodata"` (Python-based).
#'  Other utilities were not implemented, since they are not used by sen2r.
#' @param source path of input layer(s); for `calc` this can be more than one.
#' @param destination Path of the output layer.
#' @param options Character vector with GDAL options.
#' @param quiet Logical: if TRUE, suppress printing of output for info
#'  (this argument is ignored in case package `sys` is not installed).
#' @param formula (for `util = "calc"`) Calculation in gdalnumeric syntax using
#'  `+`, `-`, `/`, `*`, or any `numpy` array functions (i.e. `log10()`).
#' @param processing Character: processing options for `util = "demprocessing"`.
#' @param colorfilename Character: name of color file for `util = "demprocessing"`
#'  (mandatory if `processing="color-relief"`).
#' @return A logical (invisible) indicating success (i.e., TRUE); 
#'  in case of failure, an error is raised.
#' @importFrom sf gdal_utils
#' @export
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0


gdalUtil <-function(
  util,
  source, destination,
  options = character(0),
  quiet = FALSE,
  formula = character(0),
  processing = character(0),
  colorfilename = character(0)
) {
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Check "util"
  if (!util %in% c("info", "translate", "warp", "demprocessing" , "buildvrt",
                   "calc", "fillnodata")) {
    print_message(
      type = "error", 
      "Currently, method '",util,"' is not implemented."
    )
  }
  
  # Check additional arguments
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
  if (any(
    util %in% c("calc", "fillnodata"),
    util == "buildvrt" & packageVersion("sf") < package_version("0.9.2")
  )) {
    
    ## System call mode
    
    # Define arguments
    gdal_args <- switch(
      util,
      buildvrt = list(
        "sys" = c(options, source, destination),
        "base" = paste0(
          paste(options, collapse = " ")," ",
          destination, " ",
          paste(source, collapse = " ")
        )
      ),
      calc = list(
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
      ),
      fillnodata = list(
        "sys" = c(options, source, destination),
        "base" = paste0(
          paste(options, collapse = " ")," ",
          source," ",destination
        )
      )
    )
    
    # use exec_wait if sys is installed, system otherwise
    if (requireNamespace("sys", quietly = TRUE)) {
      sel_log_output <- if (quiet) {
        tempfile(pattern = "stdout_", fileext = ".txt")
      } else {NA}
      sel_log_error <- if (quiet) {
        tempfile(pattern = "stderr_", fileext = ".txt")
      } else {NA}
      sys::exec_wait(
        binpaths[[paste0("gdal_",util)]],
        args = gdal_args$sys,
        std_out = sel_log_output,
        std_err = sel_log_error
      )
    } else {
      system(
        paste(binpaths[[paste0("gdal_",util)]], gdal_args$base),
        intern = Sys.info()["sysname"] == "Windows"
      )
    }
    
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

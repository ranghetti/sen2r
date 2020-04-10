#' @title Interface to GDAL Python-based utilities
#' @description This accessory function interfaces with GDAL 
#'  Python-based utilities (sen2r must be interfaced with a runtime GDAL
#'  environment, see `check_gcal()`).
#' @param util Character: one among `"calc"` and `"fillnodata"` 
#'  (other utilities were not implemented, since they are not used by sen2r).
#' @param source path of input layer(s); for `calc` this can be more than one.
#' @param destination Path of the output layer.
#' @param formula (for `util = "calc"`) Calculation in gdalnumeric syntax using
#'  `+`, `-`, `/`, `*`, or any `numpy` array functions (i.e. `log10()`).
#' @param options Character vector with GDAL options.
#' @param quiet Logical: if TRUE, suppress printing of output for info
#'  (this argument is ignored in case package `sys` is not installed).
#' @return A logical (invisible) indicating success (i.e., TRUE); 
#'  in case of failure, an error is raised.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export


gdalpython_utils <-function(
  util,
  source, destination, formula = character(0),
  options = character(0),
  quiet = FALSE
) {
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Check "util"
  if (!util %in% c("calc", "fillnodata")) {
    print_message(
      type = "error", 
      "Currently, only methods 'calc' and 'fillnodata' are implemented."
    )
  }
  
  # Check formula
  if (util == "calc" & missing(formula)) {
    print_message(
      type = "error",
      "Argument \"formula\" is required for gdla_calc."
    )
  }
  
  # Define arguments
  gdalpython_args <- switch(
    util,
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
      args = gdalpython_args$sys,
      std_out = sel_log_output,
      std_err = sel_log_error
    )
  } else {
    system(
      paste(binpaths[[paste0("gdal_",util)]], gdalpython_args$base),
      intern = Sys.info()["sysname"] == "Windows"
    )
  }
  
  return(invisible(TRUE))
  
}

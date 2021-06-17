#' @title Check Google Cloud SDK installation
#' @description The function checks that Google Cloud SDK is installed
#'  and if a user account is set.
#'  Google Cloud SDK is an optional dependency, required to search and download
#'  SAFE archives from Google Cloud.
#' @param gsutil_dir (optional) Character: the path of the `gsutil` executable,
#'  or the directory in which it is installed.
#'  If not provided, `gsutil` is searched in the system path.
#' @param force (optional) Logical: if TRUE, check even if it is already
#'  configured (default is FALSE).
#' @param abort (optional) Logical: if TRUE (default), the function aborts
#'  in case no Google Cloud SDK installation is found; if FALSE, 
#'  a warning is shown and FALSE is returned.
#' @param check_creds (optional) Logical: if TRUE, check also if a user account
#'  (required to search and download products) was set.
#' @return Logical (invisible): TRUE in case Google Cloud SDK was correctly set,
#'  FALSE if it was not found, not configured (if `check_creds = TRUE`) and 
#'  `abort = FALSE` (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2021) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom jsonlite toJSON
#' @export
#' @examples
#' \dontrun{
#' check_gcloud()
#' }


check_gcloud <- function(
  gsutil_dir, 
  force = FALSE, 
  abort = TRUE, 
  check_creds = TRUE
) {
  
  # Check if gcloud was alreay configured
  binpaths <- load_binpaths()
  
  # set message method
  message_type <- ifelse(abort==TRUE, "error", "warning")
  
  if (force == TRUE || is.null(binpaths$gsutil)) {
    
    ## Check binary installation
    if (!missing(gsutil_dir)) {
      
      if (!file.exists(gsutil_dir)) {
        # Exit if Sen2Cor does not exist in the provided directory
        print_message(
          type = message_type, 
          "Google Cloud SDK was not found at the provided path."
        )
        return(invisible(FALSE))
      } else if (file.info(gsutil_dir)$isdir) {
        # Complete the path if a directory was provided
        gsutil_dir <- file.path(gsutil_dir, "gsutil") # TODO check on Windows
      }
      if (!file.exists(gsutil_dir)) {
        print_message(
          type = message_type,
          "\"gsutil\" was not found in the provided directory."
        )
        return(invisible(FALSE))
      } else {
        binpaths$gsutil <- normalize_path(gsutil_dir)
      }
      
    } else if (Sys.which("gsutil")[1] != "") {
      # Auto-fill gsutil_dir if found in the system PATH
      binpaths$gsutil <- normalize_path(Sys.which("gsutil")[1])
    } else {
      print_message(
        type = message_type,
        "Google Cloud SDK was not found in your system; press install it ",
        "following the instructions at https://cloud.google.com/sdk/docs/install ",
        "or set an existing installation using function check_gcloud() ."
      )
      return(invisible(FALSE))
    }
    
    ## Check configuration
    if (check_creds == TRUE) {
      check_creds_result <- .check_gcloud_creds(binpaths)
      if (!check_creds_result) {return(invisible(FALSE))}
    }
    
    writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
    
  }
  
  return(invisible(TRUE))
  
}


# Internal function to check if gcloud account was configured,
# returning NULL if so or an error if neither.
.check_gcloud_creds <- function(binpaths, message_type) {
  if (missing(binpaths)) {binpaths <- load_binpaths()}
  gcloud_path <- file.path(dirname(binpaths$gsutil), "gcloud") # TODO check on Windows
  gcloudauth_raw <- system(paste0(gcloud_path, " auth list"), intern = TRUE)
  # FIXME the previous instruction returns a Python message which has to be hidden
  check_result <- if (!any(grepl("credentialed accounts", tolower(gcloudauth_raw)))) {
    print_message(
      type = message_type,
      "It was not possible to check for an existing Google Cloud SDK login; ",
      "do it following the indications ",
      "in https://cloud.google.com/sdk/docs/install ",
      "or set 'check_creds = FALSE'."
    )
    FALSE
  } else if (any(grepl("no credentialed accounts", tolower(gcloudauth_raw)))) {
    print_message(
      type = message_type,
      "Google Cloud SDK was not initialised; ",
      "do it following the indications ",
      "in https://cloud.google.com/sdk/docs/install ",
      "or set 'check_creds = FALSE'."
    )
    FALSE
  } else {
    TRUE
  }
  return(invisible(check_result))
}

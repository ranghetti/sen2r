#' @title Check Google Cloud SDK installation
#' @description 
#'  Google Cloud SDK is an optional dependency, required to search and download
#'  SAFE archives from Google Cloud.
#'  
#'  `check_gcloud()` checks if Google Cloud SDK is externally 
#'  installed and if a user account is set.
#' @param gsutil_dir (optional) Character: the path of the `gsutil` executable,
#'  or the directory in which it is installed.
#'  If not provided, `gsutil` is searched in the system path.
#' @param force (optional) Logical: if TRUE, check even if it is already
#'  configured (default is FALSE).
#' @param full_scan (optional) Logical: in Linux and MacOS, if `gsutil_dir` was
#'  not manually defined, `gsutil` is searched within the system path in case this
#'  argument is left to default value FALSE; instead, if TRUE, a full search is
#'  performed. In Windows, if the folder `Google\Cloud SDK` exist in
#'  `C:\Program Files (x86)`, `C:\Program Files` or
#'  `C:\Users\<username>\AppData\Local`, then `gsutil` is searched there, otherwise in the main
#'  directory `C:\`; setting `full_scan = TRUE`, is is always searched
#'  in the whole `C:\`.
#'  This argument takes no effect if `gsutil_dir` was defined, since, in that case,
#'  a full search is always performed in `gsutil_dir`.
#' @param abort (optional) Logical: if TRUE (default), the function aborts
#'  in case no Google Cloud SDK installation is found; if FALSE,
#'  a warning is shown and FALSE is returned.
#' @param check_creds (optional) Logical: if TRUE, check also if a user account
#'  (required to search and download products) was set.
#' @return `check_gcloud()` returns TRUE (invisible) in case Google Cloud SDK 
#'  was correctly set, FALSE if it was not found, not configured 
#'  (if `check_creds = TRUE`) and `abort = FALSE` (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2021)
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
#' check_gcloud_connection()
#' }
#' \donttest{
#' is_gcloud_configured()
#' }


check_gcloud <- function(
  gsutil_dir,
  force = FALSE,
  full_scan = FALSE,
  abort = TRUE,
  check_creds = TRUE
) {

  # Check if gcloud was alreay configured
  binpaths <- load_binpaths()

  # set message method
  message_type <- ifelse(abort==TRUE, "error", "warning")

  if (force == TRUE || is.null(binpaths$gsutil)) {

    print_message(
      type="message",
      "Searching for a valid Google Cloud SDK installation",
      if (!missing(gsutil_dir)) {paste0(
        " (this could take a long time, ",
        "depending on the content of \"gsutil_path\")"
      )} else if (full_scan == TRUE) {paste0(
        " (a full scan in the whole file system was required, ",
        "this generally takes a long time)"
      )},
      "..."
    )

    # if gsutil_dir is not a directory, consider the dirname
    if (!missing(gsutil_dir) && file.exists(gsutil_dir) && !file.info(gsutil_dir)$isdir) {
      gsutil_dir <- dirname(gsutil_dir)
    }

    paths_gsutil <- if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
      if (all(full_scan == FALSE, missing(gsutil_dir))) {
        Sys.which("gsutil")
      } else {
        list.files(
          if (missing(gsutil_dir)) {"/"} else {gsutil_dir},
          "^gsutil$", recursive = TRUE, full.names = TRUE
        )
      }
    } else if (Sys.info()["sysname"] == "Windows") {
      gcloud_dirs <- file.path(
        c("C:/Program Files (x86)", "C:/Program Files", "~/../AppData/Local"),
        "Google/Cloud SDK/google-cloud-sdk/bin"
      )
      if (missing(gsutil_dir)) {
        if (all(
          full_scan == FALSE,
          any(dir.exists(gcloud_dirs))
        )) {
          list.files(
            gcloud_dirs[dir.exists(gcloud_dirs)],
            "^gsutil$",
            full.names = TRUE
          )[1]
        } else {
          list.files("C:/", "^gsutil$", recursive = TRUE, full.names = TRUE)[1]
        }
      } else {
        list.files(gsutil_dir, "^gsutil$", recursive = TRUE, full.names = TRUE)[1]
      }
    }
    paths_gsutil <- paths_gsutil[!is.na(paths_gsutil)]
    paths_gsutil <- normalize_path(paths_gsutil)

    # Exit if Sen2Cor does not exist in the provided directory
    if (length(paths_gsutil) == 0 || sum(file.exists(paths_gsutil)) == 0) {
      print_message(
        type = message_type,
        "Google Cloud SDK was not found; press install it ",
        "following the instructions at https://cloud.google.com/sdk/docs/install ",
        "or set an existing installation using function check_gcloud() ",
        "(eventually specifying the argument 'gsutil_dir' if the automatic check ",
        "would fail)."
      )
      return(invisible(FALSE))
    }
    binpaths$gsutil <- paths_gsutil[file.exists(paths_gsutil)][1]

    ## Check configuration
    if (check_creds == TRUE) {
      check_creds_result <- .check_gcloud_creds(binpaths, message_type = message_type)
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
  gcloudauth_user <- system(paste0(
    gcloud_path, 
    " info --format=\"value(config.account)\""
  ), intern = TRUE)
  check_result <- if (
    length(gcloudauth_user)==0 | 
    all(grepl("^ *$", gcloudauth_user))
  ) {
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


#' @name is_gcloud_configured
#' @rdname check_gcloud
#' @description `is_gcloud_configured()` check if Google Cloud SDK were already
#'  configured in sen2r using `check_gcloud()`.
#' @return `is_gcloud_configured()` returns TRUE if Google Cloud SDK is installed
#'  and an account is configured, FALSE if not.
#' @export

is_gcloud_configured <- function() {
  !inherits(try(check_gcloud(), silent = TRUE), "try-error")
}


#' @name check_gcloud_connection
#' @rdname check_gcloud
#' @description `check_gcloud_connection()` check if internet connection
#'  is available and Sentinel-2 bucket is accessible on Google Cloud.
#' @return `check_gcloud_connection()` returns TRUE if connection
#'  is available, FALSE otherwise.
#' @importFrom httr RETRY handle
#' @export
check_gcloud_connection <- function() {
  check_online <- try(
    RETRY(
      "GET",
      url = "https://console.cloud.google.com/storage/browser/gcp-public-data-sentinel-2",
      handle = handle("")
    )
  )
  !inherits(check_online, "try-error")
}

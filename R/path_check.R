#' @title Check a path
#' @description Accessory functions to check that a directory exists and
#'  the user have write permissions on it (to be used in a Shiny context)
#' @param path `string` full path to a folder
#' @param mustbe_empty `logical` if TRUE, accept only empty directories
#' @param mustbe_writable `logical` if TRUE, accept only directories with write 
#'  permissions
#' @rdname path_check
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @author Lorenzo Busetto, PhD (2019)
#' @keywords internal
path_check <- function(path, mustbe_empty = FALSE, mustbe_writable = TRUE) {
  
  if (all(length(path)>0, path[1]!="")) {
    if (!dir.exists(path)) {
      outtext <- "\u2718 (the directory does not exist)"
      isvalid <- FALSE
    } else if (mustbe_empty==TRUE & length(list.files(path, all.files = TRUE))>2) {
      outtext <- "\u2718 (the directory is not empty)"
      isvalid <- FALSE
    } else if (mustbe_writable == TRUE & file.access(path, mode=2)<0) {
      outtext <- "\u2718 (missing write permissions)"
      isvalid <- FALSE
    } else {
      outtext <- "\u2714"
      isvalid <- TRUE
    }
    #
  } else {
    outtext <- ""
    isvalid <- NA
  }

  out <- if (requireNamespace("shiny", quietly = TRUE)) {
    if (is.na(isvalid)) {
      shiny::renderText(outtext)
    } else if (isvalid) {
      shiny::renderUI(shiny::span(style="color:darkgreen", outtext))
    } else {
      shiny::renderUI(shiny::span(style="color:red", outtext))
    }
  } else {
    outtext
  }
  attr(out, "isvalid") <- isvalid
  return(out)
  
}

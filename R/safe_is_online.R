#' @title Check if SAFE is available for download (deprecated)
#' @description This function is deprecated and will be removed.
#' @param s2_prodlist deprecated
#' @param apihub deprecated
#' @param verbose deprecated
#' @return deprecated
#' @export

safe_is_online <- function(s2_prodlist = NULL, apihub = NA, verbose = TRUE) {
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # exit if empty
  if (length(nn(s2_prodlist)) == 0) {
    return(invisible(NULL))
  }
  
  # check input format
  s2_prodlist <- as(s2_prodlist, "safelist")
  # TODO add input checks
  
  # replace apihub with dhus
  s2_prodlist <- gsub(
    "((scihub)|(apihub))\\.copernicus\\.eu/apihub", 
    "scihub.copernicus.eu/dhus", 
    s2_prodlist
  )
  
  # check for availability
  s2_availability <- sapply(s2_prodlist, function(p) {
    TRUE
  })
  
  names(s2_availability) <- names(s2_prodlist)
  
  s2_availability
  
}

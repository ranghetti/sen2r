#' @title Check if SAFE is available for download
#' @description The function checks if the required SAFE archives are 
#'  available for download, or if they have to be ordered from the Long Term
#'  Archive.
#' @param s2_prodlist List of the products to be checked
#'  (this must be the output of [s2_list] function).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @return A logical vector of the same length and names of `s2_prodlist`,
#'  in which each element is TRUE if the corresponding SAFE archive is 
#'  available for download, FALSE if it is not or NA in case of errors with
#'  the SAFE url.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom httr GET authenticate content
#' @export
#'
#' @examples
#' \donttest{
#' single_s2 <- paste0("https://scihub.copernicus.eu/apihub/odata/v1/",
#'   "Products(\'c7142722-42bf-4f93-b8c5-59fd1792c430\')/$value")
#' names(single_s2) <- "S2A_MSIL1C_20170613T101031_N0205_R022_T32TQQ_20170613T101608.SAFE"
#' # (this is equivalent to:
#' # single_s2 <- example_s2_list[1]
#' # where example_s2_list is the output of the example of the
#' # s2_list() function)
#'
#' # Check for availability
#' safe_is_online(single_s2)
#' }

safe_is_online <- function(s2_prodlist = NULL, apihub = NA) {
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # read credentials
  creds <- read_scihub_login(apihub)
  
  # check for availability
  s2_availability <- as.logical(sapply(s2_prodlist, function(p) {
    tryCatch(
      httr::content(httr::GET(
        url = gsub("\\$value$", "Online/$value", p),
        config = httr::authenticate(creds[1], creds[2])
      ), as = "parsed", encoding = "UTF-8"), 
      error = function(e) {NA}
    )
  }))
  names(s2_availability) <- names(s2_prodlist)
  s2_availability

}
  
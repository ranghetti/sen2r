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
#' # Generate the lists of products
#' pos <- sf::st_sfc(sf::st_point(c(-57.8815,-51.6954)), crs = 4326)
#' time_window <- as.Date(c("2018-02-21", "2018-03-20"))
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' # (at the time the documentation was written, this list was containing 5
#' # archives already available online and 2 stored in the Long Term Archive)
#'
#' # Check for availability
#' safe_is_online(list_safe)
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
  
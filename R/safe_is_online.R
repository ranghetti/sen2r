#' @title Check if SAFE is available for download
#' @description The function checks if the required SAFE archives are 
#'  available for download, or if they have to be ordered from the Long Term
#'  Archive.
#' @param s2_prodlist Named character: list of the products to be checked, 
#'  in the format `safelist` (see [safelist-class]).
#'  Alternatively, it can be the path of a JSON file exported by [s2_order].
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param verbose Logical: if TRUE, provide processing messages summarising
#'  how many of the SAFE archives in `s2_prodlist` are available online.
#' @return A logical vector of the same length and names of the SAFE products
#'  passed with `s2_prodlist`,
#'  in which each element is TRUE if the corresponding SAFE archive is 
#'  available for download, FALSE if it is not or NA in case of errors with
#'  the SAFE url.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @author Lorenzo Busetto, phD (2020) \email{lbusett@@gmail.com}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom httr RETRY authenticate content
#' @importFrom jsonlite fromJSON
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
  
  # # replace apihub with dhus
  # s2_prodlist <- gsub("apihub", "dhus", s2_prodlist)
  
  # read credentials
  creds <- read_scihub_login(apihub)
  
  # check for availability
  s2_availability <- sapply(s2_prodlist, function(p) {
    if (grepl("^http.+Products\\(.+\\)/\\$value$", p)) {
    tryCatch(
      as.logical(content(RETRY(
        verb = "GET",
        url = gsub("\\$value$", "Online/$value", p),
        config = httr::authenticate(creds[1], creds[2])
      ), as = "parsed", encoding = "UTF-8")), 
      error = function(e) {
        print_message(
          type = "error",
          "Impossible to reach the SciHub server ",
          "(internet connection or SciHub may be down)." 
        )
        NA
      }
    )
    } else if (grepl("^gs://gcp-public-data-sentinel-2", p)) {TRUE} else {NA}
  })
  
  names(s2_availability) <- names(s2_prodlist)
  
  if (verbose == TRUE) {
    if (all(s2_availability)) {
      print_message(
        type = "message",
        date = FALSE,
        "All ", length(s2_availability), " products are online."
      )
    } else {
      print_message(
        type = "message",
        date = FALSE,
        length(which(s2_availability)), " out of ",  
        length(s2_availability), " products are online."
      )
    }
  }
  s2_availability
  
}

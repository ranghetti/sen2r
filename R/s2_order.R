#' @title Order S2 products.
#' @description The function orders S2 products from Long Term Archive
#'  (https://scihub.copernicus.eu/userguide/LongTermArchive).
#' @param s2_prodlist List of the products to be ordered
#'  (this must be the output of [s2_list] function).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param delay Numeric: time frame (in seconds) to leave between two 
#'  consecutive orders. Default is 5 seconds: use a higher value if you 
#'  encountered errors (i.e. not all the products were correctly ordered).
#' @return A named vector, containing the selection of `s2_prodlist` elements 
#'  which were ordered.
#'  Moreover, the vector includes two attributes:
#'  - "available" with the elements of `s2_prodlist` which were already
#'      available for download,
#'  - "notordered" with the elements of `s2_prodlist` which were not ordered
#'      for any reasons.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom httr GET authenticate 
#' @importFrom foreach foreach "%do%"
#' @export
#'
#' @examples
#' \donttest{
#' # Generate the lists of products
#' pos <- sf::st_sfc(sf::st_point(c(-57.8815,-51.6954)), crs = 4326)
#' time_window <- as.Date(c("2018-02-21", "2018-03-20"))
#' list_safe <- s2_list(spatial_extent = pos, time_interval = time_window)
#' print(list_safe)
#' # (at the time the documentation was written, this list was containing 5
#' # archives already available online and 2 stored in the Long Term Archive)
#'
#' # Order the products
#' ordered_prods <- s2_order(list_safe)
#' 
#' # Check in a second time if the product was made available
#' safe_is_online(ordered_prods)
#' }

s2_order <- function(s2_prodlist = NULL, apihub = NA, delay = 5) {
  .s2_order(
    s2_prodlist = s2_prodlist,
    apihub = apihub,
    delay = delay,
    .s2_availability = NULL
  )
}

# internal function, used internally in order not to repeat the check
# for online availability
.s2_order <- function(
  s2_prodlist = NULL,
  apihub = NA,
  delay = 5,
  .s2_availability = NULL
) {
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # read credentials
  creds <- read_scihub_login(apihub)
  
  # Split products to be downloaded from products to be ordered
  s2_availability <- if (is.null(.s2_availability)) {
    print_message(
      type = "message",
      date = TRUE,
      "Check if products are already available for download..."
    )
    safe_is_online(s2_prodlist)
  } else {
    .s2_availability
  }
  
  # If some products are already available, print a message
  if (sum(s2_availability) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      sum(s2_availability)," Sentinel-2 images are already available ",
      "and will not be ordered."
    )
  }
  
  
  ## Order products stored in Long Term Archive
  if (sum(!s2_availability) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      "Ordering ",sum(!s2_availability)," Sentinel-2 images ",
      "stored in the Long Term Archive..."
    )
  }
  ordered_products <- foreach(i = which(!s2_availability), .combine = c) %do% {
    # delay after previous order
    if (i != which(!s2_availability)[1]) {
      Sys.sleep(delay)
    }
    # order products
    make_order <- httr::GET(
      url = as.character(s2_prodlist[i]),
      config = httr::authenticate(creds[1], creds[2])
    )
    # check if the order was successful
    if (inherits(make_order, "response")) {
      make_order$status_code == 202
    } else FALSE
  }
  
  out_list <- s2_prodlist[!s2_availability][ordered_products]
  attr(out_list, "available") <- s2_prodlist[s2_availability]
  attr(out_list, "notordered") <- s2_prodlist[!s2_availability][!nn(ordered_products)]
  
  if (sum(ordered_products) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      sum(ordered_products)," of ",sum(!s2_availability)," Sentinel-2 images ",
      "were correctly ordered. ",
      "You can check at a later time if the ordered products were made available ",
      "using the command:\n\n",
      'safe_is_online(c(\n  "',
      paste(out_list, collapse = '",\n  "'),
      '"\n))\n'
    )
  }
  if (sum(!nn(ordered_products)) > 0) {
    print_message(
      type = "warning",
      date = TRUE,
      sum(!ordered_products)," of ",sum(!s2_availability)," Sentinel-2 images ",
      "were not correctly ordered. ",
      "Try using a higher value for the argument \"delay\"."
    )
  }
  
  return(out_list)

}

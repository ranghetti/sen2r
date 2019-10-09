#' @title Order S2 products.
#' @description The function orders S2 products from Long Term Archive
#'  (https://scihub.copernicus.eu/userguide/LongTermArchive).
#' @param s2_prodlist Named character: list of the products to be ordered
#'  (this must be the output of [s2_list] function).
#'  Alternatively, it can be the path of a JSON file exported by a previous
#'  execution of [s2_order], in case the user wants, for any reason, to 
#'  resubmit the order.
#' @param export_prodlist Logical or character: if TRUE (default), 
#'  the list of ordered products is saved in a JSON text file, so to be easily 
#'  retrievable at a later stage with [safe_is_online] or [s2_download];
#'  if FALSE, no output files are generated.
#'  It is also possible to pass the path of an existing folder in which the 
#'  JSON file will be saved (otherwise, a default path is used).
#' @param delay Numeric: time frame (in seconds) to leave between two 
#'  consecutive orders. Default is 5 seconds: use a higher value if you 
#'  encountered errors (i.e. not all the products were correctly ordered).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @return A named vector, containing the selection of `s2_prodlist` elements 
#'  which were ordered.
#'  Moreover, the vector includes the following attributes:
#'  - "available" with the elements of `s2_prodlist` which were already
#'      available for download,
#'  - "notordered" with the elements of `s2_prodlist` which were not ordered
#'      for any reasons,
#'  - "path" (only if argument `export_prodlist` is not FALSE) with the path of
#'      the text file in which the list of the ordered products was saved.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom httr GET authenticate 
#' @importFrom foreach foreach "%do%"
#' @importFrom jsonlite fromJSON toJSON
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
#' (order_path <- attr(ordered_prods, "path"))
#' safe_is_online(order_path)
#' }

s2_order <- function(
  s2_prodlist = NULL, 
  export_prodlist = TRUE, 
  delay = 5, 
  apihub = NA
) {
  .s2_order(
    s2_prodlist = s2_prodlist,
    export_prodlist = export_prodlist, 
    delay = delay,
    apihub = apihub,
    .s2_availability = NULL
  )
}

# internal function, used internally in order not to repeat the check
# for online availability
.s2_order <- function(
  s2_prodlist = NULL,
  export_prodlist = TRUE, 
  delay = 5,
  apihub = NA,
  .s2_availability = NULL
) {
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "export_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # check export_prodlist
  if (all(is.character(export_prodlist), length(export_prodlist) > 0)) {
    if (!dir.exists(export_prodlist)) {
      print_message(
        type = "error", 
        "Argument 'export_prodlist' must be TRUE, FALSE or the path of an existing folder."
      )
    }
  }
  
  # check delay to be numeric
  if (any(length(delay) == 0, !is.numeric(delay))) {
    print_message(
      type = "error", 
      "Argument 'delay' must be numeric"
    )
  }
  
  # import s2_prodlist if it is a path
  if (all(length(s2_prodlist) == 1, file.exists(s2_prodlist))) {
    s2_prodlist <- unlist(fromJSON(s2_prodlist))
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
  order_time <- Sys.time()
  
  # export prodlist, unless export_prodlist == FALSE
  if (any(export_prodlist != FALSE) & length(out_list) > 0) {
    prodlist_dir <- if (is.logical(export_prodlist)) {
      file.path(dirname(attr(load_binpaths(), "path")), "lta_orders")
    } else {
      export_prodlist
    }
    dir.create(prodlist_dir, showWarnings = FALSE)
    prodlist_path <- file.path(
      prodlist_dir,
      strftime(order_time, format = "lta_%Y%m%d_%H%M%S.json")
    )
    writeLines(
      toJSON(as.list(out_list), pretty = TRUE),
      prodlist_path
    )
    attr(out_list, "path") <- prodlist_path
  }
  
  if (sum(ordered_products) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      sum(ordered_products)," of ",sum(!s2_availability)," Sentinel-2 images ",
      "were correctly ordered. ",
      "You can check at a later time if the ordered products were made available ",
      "using the command:\n\n",
      if (is.null(attr(out_list, "path"))) {paste0(
        'safe_is_online(c(\n  "',paste(out_list, collapse = '",\n  "'),'"\n))'
      )} else {paste0(
        'safe_is_online("',attr(out_list, "path"),'")'
      )},
      "\n"
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

#' @title Order S2 products.
#' @description The function orders S2 products from
#'  [Long Term Archive](https://scihub.copernicus.eu/userguide/LongTermArchive).
#' @param s2_prodlist Named character: list of the products to be ordered,
#'  in the format `safelist` (see [safelist-class]).
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
#'  consecutive orders. Default is 0.5 seconds: use a higher value if you 
#'  encountered errors (i.e. not all the products were correctly ordered).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param service Character: it can be `"dhus"` or `"apihub"`, in which cases
#'  the required service is forced instead that the one present in the URLs
#'  passed through argument `s2_prodlist`.
#'  If NA (default), the service present in the URLs is maintained.
#' @param reorder Logical: If TRUE, and a json file exported by s2_order 
#'  is passed as argument to the function, try to order again also 
#'  the `"available"` and `"ordered"` S2 datasets. 
#'  Otherwise, only order the `"notordered"` ones.
#' @return A named vector, containing the subset of `s2_prodlist` elements 
#'  which were ordered.
#'  Moreover, the vector includes the following attributes:
#'  - `"available"` with the elements of `s2_prodlist` which were already
#'      available for download,
#'  - `"notordered"` with the elements of `s2_prodlist` which were not ordered
#'      for any reasons,
#'  - `"path"` (only if argument `export_prodlist` is not FALSE) with the path 
#'      of the json file in which the list of the products (ordered, available
#'      and not ordered) was saved (if `export_prodlist = TRUE`).
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @author Lorenzo Busetto, phD (2020) \email{lbusett@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom httr RETRY authenticate 
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
  delay = 0.5, 
  apihub = NA, 
  service = NA,
  reorder = TRUE
) {
  .s2_order(
    s2_prodlist = s2_prodlist,
    export_prodlist = export_prodlist, 
    delay = delay,
    apihub = apihub,
    service = service,
    reorder = reorder,
    .s2_availability = NULL
  )
}

# internal function, used internally in order not to repeat the check
# for online availability
.s2_order <- function(
  s2_prodlist = NULL,
  export_prodlist = TRUE, 
  delay = 0.5,
  apihub = NA,
  service = NA,
  reorder = TRUE,
  .s2_availability = NULL,
  .log_path = TRUE # TRUE to log all, FALSE to skip the path of the json
) {
  
  # to avoid NOTE on check
  i <- NULL
  
  # convert input NA arguments in NULL
  for (a in c("s2_prodlist", "export_prodlist", "apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # exit if empty
  if (length(nn(s2_prodlist)) == 0) {
    return(invisible(NULL))
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
  
  # check input format
  s2_prodlist <- as(s2_prodlist, "safelist")
  # TODO add input checks
  
  # check the used service
  if (!is.na(service)) {
    s2_prodlist <- gsub(
      "^https://scihub.copernicus.eu/((apihub)|(dhus))/odata",
      paste0("https://scihub.copernicus.eu/",service,"/odata"),
      s2_prodlist
    )
  }
  
  # read credentials
  creds <- read_scihub_login(apihub)
  
  # Split products to be downloaded from products to be ordered
  # 
  # First, check availability: order is NEVER attempted for already online
  # products 
  s2_availability <- if (is.null(.s2_availability)) {
    print_message(
      type = "message",
      date = TRUE,
      "Check if products are already available for download..."
    )
    
    safe_is_online(s2_prodlist, verbose = FALSE)
  } else {
    .s2_availability
  }
  
  # If some products are already available, print a message
  if (sum(s2_availability) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      sum(s2_availability)," Sentinel-2 images are already online."
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
  quota_exceeded <- FALSE # initialise variable
  status_codes <- c()
  
  if (!is.null(attr(s2_prodlist, "order_status")) & reorder == FALSE) {  
    # if the list includes the "order_status" attribute (meaning it was 
    # derived from a json file saved by s2_order), and reorder is FALSE,  
    # create a "old_order" variable contatining indices of already succesfull orders
    # that in the meantime did not change to "available", 
    # and an "to_order" variable containing indices of datasets yet to be ordered
    # (notordered and not available - could happen that another user requested the
    # dataset in the meantime)
    old_order  <- which(!s2_availability & attr(s2_prodlist, "order_status") == "ordered")
    to_order   <- which(!s2_availability & attr(s2_prodlist, "order_status") != "ordered")
    
  } else {
    to_order  <- which(!s2_availability)
    old_order <- NULL
  }
  
  # cycle along "to_order" to create a TRUE/FALSE array of succesfull orders
  ordered_products <- foreach(i = to_order, .combine = c) %do% {
    # delay after previous order
    if (i != to_order[1]) {
      Sys.sleep(delay)
    }
    # order products
    make_order <- RETRY(
      verb = "GET",
      url = as.character(s2_prodlist[i]),
      config = authenticate(creds[1], creds[2])
    )
    
    # check if the order was successful
    if (inherits(make_order, "response")) {
      # save status code
      status_codes <- c(status_codes, make_order$status_code)
      # check that user quota did not exceed
      if (any(grepl("retrieval quota exceeded", make_order$headers$`cause-message`))) {
        quota_exceeded <- TRUE
      }
      make_order$status_code == 202
    } else FALSE
  }
  
  # create a temporary array as long as s2_prodlist and "fill" it with logical 
  # depending on status: TRUE, if successfull order, FALSE for not 
  # succesfull order OR online dataset. 
  # In case we are working on resubmitting an order, and reorder is FALSE, recreate
  # the "ordered_products" array, appending results of current run with "old_order"
  
  tempordered <- rep(FALSE, length(s2_prodlist))
  if (!is.null(old_order)) {
    tempordered[sort(unique(c(to_order[ordered_products], old_order)))] <- TRUE
  } else {
    tempordered[to_order[ordered_products]] <- TRUE
  }
  ordered_products    <- tempordered
  notordered_products <- !ordered_products & !s2_availability
  
  out_list <- s2_prodlist[ordered_products]
  attr(out_list, "available")  <- s2_prodlist[s2_availability]
  attr(out_list, "notordered") <- s2_prodlist[notordered_products]
  
  # remove order_status if present
  attr(out_list, "order_status") <- NULL
  
  # export list_towrite, unless export_prodlist == FALSE
  list_towrite <- list(
    ordered = as.list(out_list), 
    available = as.list(attr(out_list, "available")), 
    notordered = as.list(attr(out_list, "notordered"))
  )
  if (any(export_prodlist != FALSE) & length(list_towrite) > 0) {
    order_time <- Sys.time()
    
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
      toJSON(as.list(list_towrite), pretty = TRUE),
      prodlist_path
    )
    attr(out_list, "path") <- prodlist_path
  }
  
  # Issue processing messages / warnings
  if (sum(ordered_products) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      sum(ordered_products)," of ",sum(!s2_availability)," Sentinel-2 images ",
      "were correctly ordered. ",
      if (.log_path == TRUE) {paste0(
        "You can check at a later time if the ordered products are available online ",
        "using the command:\n",
        if (is.null(attr(out_list, "path"))) {paste0(
          '\u00A0\u00A0safe_is_online(c(\n  "',paste(out_list, collapse = '",\n  "'),'"\n))'
        )} else {paste0(
          '\u00A0\u00A0safe_is_online("',attr(out_list, "path"),'")'
        )},
        "\n"
      )}
    )
  }
  
  if (sum(notordered_products) > 0) {
    
    print_message(
      type = "message",
      date = TRUE,
      sum(notordered_products)," of ",sum(!s2_availability)," Sentinel-2 images ",
      "were not correctly ordered ",
      "(HTML status code: ",unique(paste(status_codes[status_codes!=202]), collapse = ", "),")",
      if (quota_exceeded) {paste0(
        " because user '",creds[1],"' offline products retrieval quota exceeded. ",
        "Please retry later, otherwise use different SciHub credentials ",
        "(see ?write_scihub_login or set a specific value for argument \"apihub\")."
      )} else {
        "."#," Try using a higher value for the argument \"delay\"."
      },
      if (.log_path == TRUE) {paste0(
        " You can try ordering them at a later time ",
        "using the command:\n",
        if (is.null(attr(out_list, "path"))) {paste0(
          '\u00A0\u00A0s2_order(c(\n  "',paste(out_list, collapse = '",\n  "'),'"\n))'
        )} else {paste0(
          '\u00A0\u00A0s2_order("',attr(out_list, "path"),'")'
        )},
        "\n"
      )}
    )
  }
  
  return(out_list)
  
}

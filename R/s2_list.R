#' @title Retrieve list of available S2 products.
#' @description The function retrieves the list of available Sentinel-2
#'  products satisfying given search criteria. 
#' @param spatial_extent A valid spatial object object of class `sf`,
#'  `sfc` or `sfg`
#' @param tile `string array` Sentinel-2 Tiles to be considered string (5-length character)
#' @param orbit `string array` Sentinel-2 orbit numbers to be considered
#' @param time_interval Dates to be considered, as a temporal vector (class [POSIXct] or
#'  [Date], or string in `YYYY-mm-dd` format) of length 1 (specific day) or 2 (time interval).
#' @param time_period (optional) Character:
#'  * "full" (default) means that all
#'  the images included in the time window are considered;
#'  * "seasonal" means that only the single seasonal periods in the
#'  window are used (i.e., with a time window from 2015-06-01 to
#'  2017-08-31, the periods 2015-06-01 to 2015-08-31, 2016-06-01
#'  to 2016-08-31 and 2017-06-01 to 2017-08-31 are considered).
#' @param level Character vector with one of the following:
#'     - "auto" (default): check if level-2A is available on SciHub:
#'         if so, list it; if not, list the corresponding level-1C
#'         product
#'     - "L1C": list available level-1C products
#'     - "L2A": list available level-2A products
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param max_cloud Integer number (0-100) containing the maximum cloud
#'  level of the tiles to be listed (default: no filter).
#' @param availability Character argument, determining which products have
#'  to be returned: 
#'  - "online" : only archive names already available for download are returned;
#'  - "lta": only archive names stored in the Long Term Archive
#'      (see https://scihub.copernicus.eu/userguide/LongTermArchive)
#'      are returned;
#'  - "check": all archive names are returned, checking if they are
#'      available or not for download (see "Value" to know 
#'      how to distinguish each other);
#'  - "ignore" (default): all archive names are returned, without doing the check
#'      (running the function is faster).
#' @param output_type Character: if 'vector' (default), the function returns
#'  a vector or URLs, whose names are the SAFE names;
#'  if 'data.table', the output is a data.table with metadata.
#' @return Depending on the value of argument `output_type`:
#'  if `output_type = "vector"`, a vector of available products 
#'  (being each element an URL and its name the product name), 
#'  together with the attributes `online` (with the index of products available 
#'  for download) and `lta` (with the index of products stored in the Long Term
#'  Archive) in the case `availability == "check"`;
#'  if `output_type = "data.table"`, a data.table with product metadata
#'  (including the logical field `online` to allow distinguishing products
#'  available for download and stored in the Long Term Archive).
#' @author Lorenzo Busetto, phD (2019) \email{lbusett@@gmail.com} - Inspired by 
#'  function `getSentinel_query` of package `getSpatialData` by J. Schwalb-Willmann
#'  (https://github.com/16EAGLE/getSpatialData)
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom methods is
#' @importFrom magrittr "%>%"
#' @importFrom sf st_as_sfc st_sfc st_point st_as_text st_bbox st_coordinates
#'  st_geometry st_intersection st_geometry st_convex_hull st_transform st_cast
#'  st_union st_simplify
#' @importFrom httr GET authenticate content
#' @importFrom XML htmlTreeParse saveXML xmlRoot
#' @importFrom utils head read.table
#' @export
#'
#' @examples
#' \donttest{
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2016-05-01", "2017-07-30"))
#'
#' # Full-period list
#' example_s2_list <- s2_list(
#'   spatial_extent = pos,
#'   tile = "32TNR",
#'   time_interval = time_window,
#'   orbit = "065"
#' )
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' as.vector(sort(sapply(names(example_s2_list), function(x) {
#'   strftime(safe_getMetadata(x,"nameinfo")$sensing_datetime)
#' })))
#'
#' # Seasonal-period list
#' example_s2_list <- s2_list(
#'   spatial_extent = pos,
#'   tile = "32TNR",
#'   time_interval = time_window,
#'   time_period = "seasonal"
#' )
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' as.vector(sort(sapply(names(example_s2_list), function(x) {
#'   strftime(safe_getMetadata(x,"nameinfo")$sensing_datetime)
#' })))
#' }

s2_list <- function(spatial_extent = NULL,
                    tile = NULL,
                    orbit = NULL, # spatial parameters
                    time_interval = c(Sys.Date() - 10, Sys.Date()), 
                    time_period = "full", # temporal parameters
                    level = "auto",
                    apihub = NA,
                    max_cloud = 100,
                    availability = "ignore",
                    output_type = "vector") {
  
  if (!level %in% c("auto", "L2A", "L1C")) {
    print_message(
      type = "error",
      "`level` must be \"auto\", \"L2A\" or \"L1C\""
    )
  }
  
  if (!time_period %in% c("full", "seasonal")) {
    print_message(
      type = "error",
      "`level` must be \"full\" or \"seasonal\""
    )
  }
  
  if (!availability %in% c("ignore", "check", "online", "lta")) {
    print_message(
      type = "error",
      "`availability` must be one among \"online\", \"lta\", ",
      "\"check\" and \"ignore\""
    )
  }
  
  if (inherits(try(as.Date(time_interval)), "try-error")) {
    print_message(
      type = "error",
      "`time_interval` must be of class `Date`, `POSIXct` or `character` ",
      "cohercible to Date (YYYY-mm-dd)."
    )
  }
  
  # to avoid NOTE on check
  . <- i <- online <- NULL
  
  # convert input NA arguments in NULL
  for (a in c("spatial_extent","tile","orbit","time_interval","apihub")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a,NULL)
    }
  }
  
  # check if spatial_extent was provided
  spatial_extent_exists <- if (!exists("spatial_extent")) {
    FALSE
  } else if (is.null(spatial_extent)) {
    FALSE
  } else if (is(spatial_extent, "POLYGON")) {
    if (length(spatial_extent) == 0) {
      FALSE
    } else {
      TRUE
    }
  } else {
    TRUE
  }
  
  # verify that spatial_extent is `sf` or `sfc`, and convert to `sfc`
  if (spatial_extent_exists) {
    if (!inherits(spatial_extent, c("sf", "sfc"))){
      print_message(
        type = "error",
        "`spatial_extent` is not a `sf` or `sfc` object."
      )
    } else {
      spatial_extent <- sf::st_geometry(spatial_extent) %>%
        sf::st_transform(4326)
    }
  }
  
  # if not, retrieve it from tile
  if (all(!spatial_extent_exists, is.null(tile))) {
    print_message(
      type = "error",
      "At least one parameter among spatial_extent and tile must be specified."
    )
  }
  # load tiles borders if needed.
  if (any(!spatial_extent_exists, is.null(tile))) {
    # extract and import tiles kml
    s2tiles <- s2_tiles()
  }
  
  # take the the selected tiles as extent if needed
  # (this will result in the selection of more tiles, cause to overlapping
  # areas; it is filtered in s2_download, but it is slow: FIXME).
  # It is not possible to use tile centroids, because tile of external areas
  # of orbits could not be included).
  if (!spatial_extent_exists & !is.null(tile)) {
    spatial_extent <- suppressWarnings(
      sf::st_cast(sf::st_geometry(s2tiles[s2tiles$tile_id %in% tile,]), "POLYGON")
    )
  }
  
  spatial_extent <- suppressWarnings(sf::st_union(spatial_extent))
  spatial_extent_or <- spatial_extent
  
  # # If spatial_extent is not point, simplify polygon if needed / convert to bbox
  # NOTE: simplifiying polygons was disabled because this could result in a smaller polygon
  # (loosing products).
  # Minimum convex hull will be used, which could instead cause the selection of more
  # products than needed.
  # if (inherits(spatial_extent, "sfc_POLYGON")) {
  #   
  #   # if spatial_extent has too many vertices, simplify it
  #   dtolerance <- with(as.list(sf::st_bbox(spatial_extent)), sqrt((xmax - xmin)^2 + (ymax - ymin)^2))/500
  #   # initial dtolerance value: 0.5% maximum distance
  #   n_while = 0
  #   while (length(suppressWarnings(sf::st_cast(spatial_extent, "POINT"))) > 30) {
  #     if (n_while < 10) {
  #       spatial_extent <- suppressWarnings(sf::st_simplify(spatial_extent_or, dTolerance = dtolerance))
  #       dtolerance <- dtolerance * 2
  #       n_while <- n_while + 1
  #     } else {
  #       spatial_extent <- sf::st_as_sfc(sf::st_bbox(spatial_extent_or))
  #     }
  #   }
  #   
  # } else 
  if (!inherits(spatial_extent, "sfc_POINT")) {
    # spatial_extent <- st_as_sfc(sf::st_bbox(spatial_extent_or))
    spatial_extent <- sf::st_convex_hull(spatial_extent)
  }
  # }
  
  # checks on dates
  # TODO add checks on format
  if (length(time_interval) == 1) {
    time_interval <- rep(time_interval,2)
  }
  # split time_interval in case of seasonal download
  time_intervals <- if (time_period == "full") {
    data.frame(
      "start" = time_interval[1],
      "end" = time_interval[2],
      stringsAsFactors = FALSE
    )
  } else if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_interval[1], time_interval[2], by = "year"), "%Y-%m-%d"),
      "end" = strftime(rev(seq(time_interval[2], time_interval[1], by = "-1 year")), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(orbit)) {
    orbit <- as.integer(orbit)
    if (any(is.na(orbit))) {
      print_message(
        type = "error",
        "`orbit` must be integer or cohercible to integer."
      )
    }
  }
  
  if (!is.numeric(max_cloud)) {
    print_message(
      type = "error",
      "`max_cloud` must be integer [0,100]."
    )
  }
  
  # Check connection
  if (!check_scihub_connection()) {
    print_message(
      type = "error", 
      "Impossible to reach the SciHub server ",
      "(internet connection or SciHub may be down)." 
    )
  }
  
  # Get credentials
  creds <- read_scihub_login(apihub)
  if (!check_scihub_login(creds[1,1], creds[1,2])) {
    print_message(
      type = "error", 
      "SciHub credentials are not correct, ",
      "please check them." 
    )
  }
  
  foot <- ifelse(
    inherits(spatial_extent, "sfc_POINT"),
    paste0('footprint:%22Intersects(', paste(as.numeric(sf::st_coordinates(spatial_extent)[c(2,1)]), collapse = ",%20"),')%22'),
    paste0('footprint:%22Intersects(', sf::st_as_text(sf::st_geometry(spatial_extent)),')%22')
  )
  
  n_entries <- 1
  out_list <- list()
  
  for (t_int in seq_len(nrow(time_intervals))) {
    
    rows <- 100
    start <- 0
    end_query <- FALSE
    
    while (!end_query) {
      
      query_string <- paste0(
        'https://scihub.copernicus.eu/apihub/search?',
        'start=', start,
        '&rows=', rows,
        '&q=', foot,
        ' AND platformname:Sentinel-2',
        ' AND beginposition:[', time_intervals[t_int,1], 'T00:00:00.000Z',
        ' TO ', time_intervals[t_int,2], 'T23:59:59.000Z]',
        ' AND cloudcoverpercentage:[0 TO ', max_cloud,']'
      )
      query_string <- gsub(" ", "%20",query_string)
      query_string <- gsub("\\[", "%5b",query_string)
      query_string <- gsub("\\]", "%5d",query_string)
      
      out_query <- httr::GET(query_string, authenticate(creds[1,1], creds[1,2]))
      out_xml <- httr::content(out_query, as = "parsed", encoding = "UTF-8")
      out_xml_list <- XML::htmlTreeParse(out_xml, useInternalNodes = TRUE) %>% XML::xmlRoot()
      out_xml_list <- out_xml_list[["body"]][["feed"]]
      
      
      for (ll in which(names(out_xml_list)=="entry")) {
        
        in_entry <- XML::saveXML(out_xml_list[[ll]]) %>%
          strsplit(., "\n")
        
        if (length(which(grepl("<link href=", in_entry[[1]]))) != 0) {
          
          in_entry <- in_entry[[1]]
          
          title <- in_entry[which(grepl("<title>", in_entry))] %>%
            gsub("^.*<title>([^<]+)</title>.*$", "\\1", .)
          
          url <- in_entry[which(grepl("<link href=", in_entry))] %>%
            gsub("^.*<link href=\"([^\"]+)\"/>.*$", "\\1", .)
          
          orbitid <- in_entry[which(grepl("relativeorbitnumber", in_entry))] %>%
            gsub("^.*<int name=\"relativeorbitnumber\">([^<]+)</int>.*$", "\\1", .) %>%
            as.numeric() %>% sprintf("%03i", .)
          
          ccov <- in_entry[which(grepl("cloudcoverpercentage", in_entry))] %>%
            gsub("^.*<double name=\"cloudcoverpercentage\">([^<]+)</double>.*$", "\\1", .) %>%
            as.numeric()
          
          proclev <- in_entry[which(grepl("processinglevel", in_entry))] %>%
            gsub("^.*<str name=\"processinglevel\">([^<]+)</str>.*$", "\\1", .)
          
          sensor <- in_entry[which(grepl("platformserialidentifier", in_entry))] %>%
            gsub("^.*<str name=\"platformserialidentifier\">([^<]+)</str>.*$", "\\1", .)
          
          tileid <- in_entry[which(grepl("name=\"tileid\"", in_entry))] %>%
            gsub("^.*<str name=\"tileid\">([^<]+)</str>.*$", "\\1", .)
          
          sensdate <- in_entry[which(grepl("name=\"endposition\"", in_entry))] %>%
            gsub("^.*<date name=\"endposition\">([0-9\\-]+)T[0-9\\:\\.]+Z</date>.*$", "\\1", .) %>%
            as.Date()
          
          proctime <- in_entry[which(grepl("name=\"ingestiondate\"", in_entry))] %>%
            gsub("^.*<date name=\"ingestiondate\">([0-9\\-]+)T([0-9\\:\\.]+)Z</date>.*$", "\\1 \\2", .) %>%
            as.POSIXct()
          
          if (length(tileid) == 0 ) {
            tileid <- gsub("^.+_T([0-9]{2}[A-Z]{3})_.+$", "\\1", title)
          }
          # print(paste0(title, ".SAFE"))
          out_list[[n_entries]] <- data.frame(
            name = paste0(title, ".SAFE"),
            url = url,
            orbitid = orbitid,
            date = sensdate,
            proctime = proctime,
            ccov = ccov,
            proclev = proclev,
            sensor = sensor,
            tileid = tileid,
            stringsAsFactors = FALSE
          )
          n_entries <- n_entries + 1
        }
      }
      
      if (sum(names(out_xml_list)=="entry") != rows) {
        end_query <- TRUE
      } else {
        start <- start + rows
      }
    }
    
  }
  out_dt <- rbindlist(out_list)
  
  if (nrow(out_dt) == 0) {return(character(0))}
  
  # check online availability
  out_dt$online <- if (availability == "ignore") {
    NA
  } else {
    as.logical(safe_is_online(out_dt$url))
  }
  
  # remove "wrong" tiles and orbits if needed
  if (!is.null(tile)) {
    out_dt <- out_dt[tileid %in% tile,]
  } else {
    sel_s2tiles <- suppressMessages(suppressWarnings(
      sf::st_intersection(s2tiles, spatial_extent_or)))
    out_dt <- out_dt[tileid %in% unique(sel_s2tiles$tile_id),]
  }
  
  if (!is.null(orbit)) {
    out_dt <- out_dt[orbitid %in% sprintf("%03i", as.numeric(orbit)),]
  }
  
  if (nrow(out_dt) == 0) {return(character(0))}
  if (level == "L1C") {
    out_dt <- out_dt[proclev == "Level-1C",]
  } else if (level == "L2A") {
    out_dt <- out_dt[grepl("^Level-2Ap?$", proclev),]
  } else if (level == "auto") {
    out_dt <- out_dt[order(-proclev,-proctime),]
    out_dt <- out_dt[,head(.SD, 1), by = .(date, tileid, orbitid)]
  }
  if (nrow(out_dt) == 0) {return(character(0))}
  out_dt <- out_dt[order(date),]
  
  # filter by availability
  if (availability == "online") {
    out_dt <- out_dt[online == TRUE,]
  } else if (availability == "lta") {
    out_dt <- out_dt[online == FALSE,]
  }
  
  # return output
  if (output_type == "data.table") {
    return(out_dt)
  } else {
    out_vector <- out_dt$url
    names(out_vector) <- out_dt$name
    if (availability == "check") {
      attr(out_vector, "online") <- which(out_dt$online)
      attr(out_vector, "lta") <- which(!out_dt$online)
    }
    return(out_vector)
  }
}

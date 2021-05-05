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
#' @param server The servers where archives are searched. Currently, 
#'  only `"scihub"` is supported (Google Cloud will be implemented in future).
#'  In case of multiple values, they are used in order of priority.
#'  If `availability = "check"`, products on LTA are always left as last choice.
#' @param apihub Path of the `apihub.txt` file containing credentials
#'  of SciHub account.
#'  If NA (default), the default location inside the package will be used.
#' @param service Character: it can be `"dhus"` or `"apihub"` (default),
#'  in which cases the required service is forced instead that the one present
#'  in the URLs passed through argument `s2_prodlist`.
#' @param max_cloud Integer number (0-100) containing the maximum cloud
#'  level of the tiles to be listed (default: no filter).
#' @param availability Character argument, determining which products have
#'  to be returned: 
#'  - `"online"` : only archive names already available for download are returned;
#'  - `"lta"`: only archive names stored in the
#'      [Long Term Archive](https://scihub.copernicus.eu/userguide/LongTermArchive)
#'      are returned;
#'  - `"check"`: all archive names are returned, checking if they are
#'      available or not for download (see "Value" to know 
#'      how to distinguish each other);
#'  - `"ignore"` (default): all archive names are returned, without doing the check
#'      (running the function is faster).
#' @param output_type Deprecated (use `as.data.table` to obtain a data.table).
#' @return An object of class [safelist].
#'  The attribute `online` contains logical values: in case 
#'  `availability != "ignore"`, values are TRUE / FALSE for
#'  products available for download / stored in the Long Term Archive; 
#'  otherwise, values are set to NA.
#' @author Lorenzo Busetto, phD (2019) - Inspired by 
#'  function `getSentinel_query` of package 
#'  [`getSpatialData`](https://github.com/16EAGLE/getSpatialData) by J. Schwalb-Willmann
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom methods is
#' @importFrom sf st_as_sfc st_sfc st_point st_as_text st_bbox st_coordinates
#'  st_geometry st_intersection st_geometry st_convex_hull st_transform st_cast
#'  st_union st_centroid st_is_valid st_make_valid
#' @importFrom httr RETRY authenticate content
#' @importFrom XML htmlTreeParse saveXML xmlRoot
#' @importFrom utils head read.table
#' @export
#'
#' @examples
#' \donttest{
#' 
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2016-05-01", "2017-07-30"))
#'
#' # Full-period list
#' if (is_scihub_configured()) {
#'   example_s2_list <- s2_list(
#'     spatial_extent = pos,
#'     tile = "32TNR",
#'     time_interval = time_window,
#'     orbit = "065"
#'   )
#' } else {
#'   example_s2_list <- as(character(), "safelist")
#' }
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' safe_getMetadata(example_s2_list, "sensing_datetime")
#'
#' # Seasonal-period list
#' if (is_scihub_configured()) {
#'   example_s2_list <- s2_list(
#'     spatial_extent = pos,
#'     tile = "32TNR",
#'     time_interval = time_window,
#'     time_period = "seasonal"
#'   )
#' } else {
#'   example_s2_list <- as(character(), "safelist")
#' }
#' print(example_s2_list)
#' # Print the dates of the retrieved products
#' safe_getMetadata(example_s2_list, "sensing_datetime")
#' 
#' }

s2_list <- function(spatial_extent = NULL,
                    tile = NULL,
                    orbit = NULL, # spatial parameters
                    time_interval = c(Sys.Date() - 10, Sys.Date()), 
                    time_period = "full", # temporal parameters
                    level = "auto",
                    server = "scihub",
                    apihub = NA,
                    service = "apihub",
                    max_cloud = 100,
                    availability = "ignore",
                    output_type = "deprecated") {
  
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
  
  # check the used service
  if (!service %in% c("apihub", "dhus")) {
    print_message(
      type = "error",
      "Argument 'service' can be only \"apihub\" or \"dhus\"; ",
      "switching to \"apihub\"."
    )
    service <- "apihub"
  }
  
  if (inherits(try(as.Date(time_interval), silent = TRUE), "try-error")) {
    print_message(
      type = "error",
      "`time_interval` must be of class `Date`, `POSIXct` or `character` ",
      "cohercible to Date (YYYY-mm-dd)."
    )
  } else if (inherits(time_interval, "character")) {
    time_interval <- as.Date(time_interval)
  }
  
  if (anyNA(match(server, c("scihub", "gcloud")))) {
    print_message(
      type = "error",
      "`server` must be \"scihub\" (and/or \"gcloud\" in a future release)"
    )
  }
  
  # to avoid NOTE on check
  . <- online <- id_tile <- id_orbit <- 
    sensing_datetime <- ingestion_datetime <- centroid <- footprint <- NULL
  
  # Do not use the {s2} spherical geometry package (to avoid errors)
  # (when the interface between {sf} and {s2} will be stable, this should be removed)
  if (requireNamespace("sf", quietly = TRUE)) {
    try({
      sf_use_s2_prev <- sf::sf_use_s2(FALSE)
      on.exit(sf::sf_use_s2(sf_use_s2_prev))
    }, silent = TRUE)
  }
  
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
      spatial_extent <- st_transform(
        st_geometry(spatial_extent),
        4326
      )
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
  if (any(!spatial_extent_exists, is.null(tile), "gcloud" %in% server)) {
    # extract and import tiles kml
    s2tiles <- s2_tiles()
  }
  
  # determine required tiles if needed
  if (is.null(tile)) {
    tile <- tiles_intersects(spatial_extent, .s2tiles = s2tiles)
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
  
  spatial_extent <- suppressMessages(sf::st_union(spatial_extent))
  if (any(!st_is_valid(spatial_extent))) {
    spatial_extent <- st_make_valid(spatial_extent)
  }
  
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
      "start" = seq(time_interval[1], time_interval[2], by = "year"),
      "end" = rev(seq(time_interval[2], time_interval[1], by = "-1 year")),
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
  
  # List of output dt (one per server method)
  out_dt_list <- list()
  
  ## ESA SciHub specific methods
  if ("scihub" %in% server) {
    out_dt_list[["scihub"]] <- .s2_list_scihub(
      spatial_extent = spatial_extent, 
      time_intervals = time_intervals, 
      tile = tile, 
      orbit = orbit, 
      max_cloud = max_cloud, 
      apihub = apihub, 
      service = service,
      availability = availability,
      .s2tiles = s2tiles
    )
  }
  
  ## Google Cloud specific methods
  if ("gcloud" %in% server) {
    if (eval(parse(text = 'requireNamespace("sen2r.extras", quietly = TRUE)'))) {
      out_dt_list[["gcloud"]] <- eval(parse(text = paste0(
        "sen2r.extras::.s2_list_gcloud(",
        "  time_intervals = time_intervals,",
        "  tile = tile,",
        "  orbit = orbit,",
        "  max_cloud = max_cloud",
        ")"
      )))
    }
  }
  
  ## Merge dt (in case of multiple servers)
  # order by the order of input "server" argument (this is used in duplicated:
  # first "server" value is taken in case of douplicated availability)
  out_dt <- rbindlist(out_dt_list[server])
  if (nrow(out_dt) == 0) {return(as(setNames(character(0), character(0)), "safelist"))}
  # compute date (to ignore duplicated dates)
  out_dt[,date := as.Date(substr(as.character(out_dt$sensing_datetime), 1, 10))]
  out_names <- copy(names(out_dt))
  # fix footprint topology errors
  invalid_entries <- out_dt[,which(!st_is_valid(st_as_sfc(footprint, crs = 4326)))]
  if (length(invalid_entries) > 0) {
    out_footprint_fixed <- st_as_text(st_make_valid(st_as_sfc(out_dt[invalid_entries, footprint], crs = 4326)))
    out_dt[invalid_entries, footprint := out_footprint_fixed]
  }
  
  if (nrow(out_dt) == 0) {return(as(setNames(character(0), character(0)), "safelist"))}
  # first, order by level (L2A, then L1C) and ingestion time (newers first)
  out_dt <- out_dt[order(-level,-ingestion_datetime),]
  # second, order by availability (LTA SciHub products are always used as last choice)
  out_dt <- rbind(
    out_dt[is.na(online) | online == TRUE,],
    out_dt[online == FALSE,]
  )
  if (level == "L1C") {
    out_dt <- out_dt[level == "1C",]
  } else if (level == "L2A") {
    out_dt <- out_dt[grepl("^2Ap?$", level),]
  } # for level = "auto", do nothing because unuseful products are filtered below
  # filter (univocity)
  suppressWarnings({
    out_dt[,centroid := st_centroid(st_as_sfc(footprint, crs = 4326))]
  })
  out_dt <- out_dt[,head(.SD, 1), by = .(
    date, id_tile, id_orbit, 
    apply(round(st_coordinates(centroid), 2), 1, paste, collapse = " ")
  )]
  out_dt <- out_dt[,out_names,with=FALSE]
  if (nrow(out_dt) == 0) {return(as(setNames(character(0), character(0)), "safelist"))}
  out_dt <- out_dt[order(sensing_datetime),]
  out_dt[,date := NULL]
  
  # filter by availability
  if (availability == "online") {
    out_dt <- out_dt[online == TRUE,]
  } else if (availability == "lta") {
    out_dt <- out_dt[online == FALSE,]
  }
  
  # return output
  if (output_type =="data.table") { # deprecated
    out_dt
  } else if (output_type =="data.frame") { # deprecated
    as.data.frame(out_dt)
  } else if (output_type =="vector") { # deprecated
    as.character(as(out_dt, "safelist"))
  } else {
    as(out_dt, "safelist")
  }
}


.s2_list_scihub <- function(spatial_extent, time_intervals, tile, orbit, max_cloud, apihub, service, availability, .s2tiles) {
  
  n_entries <- 1
  out_list <- list()
  
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
  
  # S2 tiles
  if (missing(.s2tiles)) {.s2tiles <- s2_tiles()}
  
  # # If spatial_extent is not point, simplify polygon if needed / convert to bbox
  spatial_extent_or <- spatial_extent
  if (length(st_cast(spatial_extent, "POINT")) >= 50) {
    spatial_extent <- st_convex_hull(spatial_extent_or)
  }
  if (length(st_cast(spatial_extent, "POINT")) >= 50) {
    spatial_extent <- st_as_sfc(sf::st_bbox(spatial_extent_or))
    print_message(
      type = "warning",
      "Input extent contains too many nodes, so its bounding box was used ",
      "(a larger number of Sentinel-2 tiles could have been used); ",
      "consider simplifying the extent manually."
    )
  }
  
  # Prepare footprint
  foot <- ifelse(
    inherits(spatial_extent, "sfc_POINT"),
    paste0('footprint:%22Intersects(', paste(as.numeric(sf::st_coordinates(spatial_extent)[c(2,1)]), collapse = ",%20"),')%22'),
    paste0('footprint:%22Intersects(', sf::st_as_text(sf::st_geometry(spatial_extent)),')%22')
  )
  
  for (t_int in seq_len(nrow(time_intervals))) {
    
    rows <- 100
    start <- 0
    end_query <- FALSE
    
    while (!end_query) {
      
      query_string <- paste0(
        'https://',ifelse(service=='dhus','scihub','apihub'),
        '.copernicus.eu/',service,'/search?',
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
      
      times_429 <- 10 # if 429 "too many requests", retry up to 10 times
      while (times_429 > 0) {
        out_query <- RETRY(
          verb = "GET",
          url = query_string,
          config = authenticate(creds[1,1], creds[1,2])
        )
        times_429 <-if (out_query$status_code != 429) {0} else {times_429 - 1}
      }
      
      out_xml <- content(out_query, as = "parsed", encoding = "UTF-8")
      out_xml_list <- xmlRoot(htmlTreeParse(out_xml, useInternalNodes = TRUE))
      out_xml_list <- out_xml_list[["body"]][["feed"]]
      
      
      for (ll in which(names(out_xml_list)=="entry")) {
        
        in_entry <- strsplit(saveXML(out_xml_list[[ll]]), "\n")
        
        if (length(which(grepl("<link href=", in_entry[[1]]))) != 0) {
          
          in_entry <- in_entry[[1]]
          
          title <- gsub(
            "^.*<title>([^<]+)</title>.*$", "\\1", 
            in_entry[which(grepl("<title>", in_entry))]
          )
          
          url <- gsub(
            "^.*<link href=\"([^\"]+)\"/>.*$", "\\1", 
            in_entry[which(grepl("<link href=", in_entry))]
          )
          
          id_orbit <- sprintf("%03i", as.numeric(
            gsub(
              "^.*<int name=\"relativeorbitnumber\">([^<]+)</int>.*$", "\\1", 
              in_entry[which(grepl("\\\"relativeorbitnumber\\\"", in_entry))]
            )
          ))
          
          footprint <- tryCatch(
            st_as_sfc(
              gsub(
                "^.*<str name=\"footprint\">([^<]+)</str>.*$", "\\1", 
                in_entry[which(grepl("\\\"footprint\\\"", in_entry))]
              ),
              crs = 4326
            ),
            error = function(e) {st_polygon()}
          )
          
          clouds <- as.numeric(gsub(
            "^.*<double name=\"cloudcoverpercentage\">([^<]+)</double>.*$", "\\1",
            in_entry[which(grepl("\\\"cloudcoverpercentage\\\"", in_entry))]
          ))
          
          proc_level <- gsub(
            "^.*<str name=\"processinglevel\">Level\\-([^<]+)</str>.*$", "\\1", 
            in_entry[which(grepl("\\\"processinglevel\\\"", in_entry))]
          )
          
          mission <- gsub(
            "^.*<str name=\"platformserialidentifier\">Sentinel\\-([^<]+)</str>.*$", "\\1", 
            in_entry[which(grepl("\\\"platformserialidentifier\\\"", in_entry))]
          )
          
          id_tile <- gsub("^.+_T([0-9]{2}[A-Z]{3})_.+$", "\\1", title)
          
          sensing_datetime <- as.POSIXct(
            gsub(
              "^S2[AB]\\_MSIL[12][AC]\\_([0-9]{8}T[0-9]{6})\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_[0-9]{8}T[0-9]{6}$",
              "\\1", 
              title
            ),
            format = "%Y%m%dT%H%M%S", tz = "UTC"
          )
          
          creation_datetime <- as.POSIXct(
            gsub(
              "^S2[AB]\\_MSIL[12][AC]\\_[0-9]{8}T[0-9]{6}\\_N[0-9]{4}\\_R[0-9]{3}\\_T[A-Z0-9]{5}\\_([0-9]{8}T[0-9]{6})$",
              "\\1",
              title
            ),
            format = "%Y%m%dT%H%M%S", tz = "UTC"
          )
          
          ingestion_datetime <- as.POSIXct(
            gsub(
              "^.*<date name=\"ingestiondate\">([0-9\\-]+)T([0-9\\:\\.]+)Z</date>.*$", 
              "\\1 \\2", 
              in_entry[which(grepl("name=\"ingestiondate\"", in_entry))]
            ),
            tz = "UTC"
          )
          
          uuid <- gsub(
            "^.*<str name=\"uuid\">([^<]+)</str>.*$", "\\1", 
            in_entry[which(grepl("\\\"uuid\\\"", in_entry))]
          )
          
          
          # print(paste0(title, ".SAFE"))
          out_list[[n_entries]] <- data.frame(
            name = paste0(title, ".SAFE"),
            url = url,
            mission = mission,
            level = proc_level,
            id_tile = id_tile,
            id_orbit = id_orbit,
            sensing_datetime = sensing_datetime,
            ingestion_datetime = ingestion_datetime,
            clouds = clouds,
            footprint = st_as_text(footprint),
            uuid = uuid,
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
  
  if (nrow(out_dt) == 0) {return(data.table())}
  
  # remove "wrong" tiles and orbits if needed
  if (!is.null(tile)) {
    out_dt <- out_dt[id_tile %in% tile,]
  } else {
    sel_s2tiles <- suppressMessages(suppressWarnings(
      sf::st_intersection(.s2tiles, spatial_extent_or)))
    out_dt <- out_dt[id_tile %in% unique(sel_s2tiles$tile_id),]
  }
  
  if (!is.null(orbit)) {
    out_dt <- out_dt[id_orbit %in% sprintf("%03i", as.numeric(orbit)),]
  }
  
  # check online availability
  out_dt$online <- if (availability == "ignore") {
    NA
  } else {
    as.logical(safe_is_online(out_dt, verbose = FALSE, apihub = apihub))
  }
  
  out_dt
  
}

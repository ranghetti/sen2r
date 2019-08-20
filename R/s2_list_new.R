#' @title Retrieve list of available products.
#' @description The function retrieves the list of available Sentinel-2
#'  products basing on search criteria. It makes use of
#'  [s2download](https://github.com/ranghetti/s2download)
#'  python function only to retrieve the list of files, without
#'  downloading and correcting them.
#' @param spatial_extent A valid spatial object object of class `sf`,
#'  `sfc` or `sfg`
#' @param tile Single Sentinel-2 Tile string (5-length character)
#' @param orbit Single Sentinel-2 orbit number
#' @param time_interval a temporal vector (class [POSIXct] or
#'  [Date]) of length 1 (specific day) or 2 (time interval).
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
#'  of scihub account. If NA (default) the default credentials
#'  (username "user", password "user") will be used.
#' @param max_cloud Integer number (0-100) containing the maximum cloud
#'  level of the tiles to be listed (default: no filter).
#' @param output_type Character: if 'vector' (default), the function returns
#'  a vector or URLs, whose names are the SAFE names;
#'  if 'data.table', the output is a data.table with metadata.
#' @return Basing on the value of argument `output_type``,
#'  a vector of available products (being each element an URL
#'  and its name the product name), or a data.table with product metadata.
#'
#' @author Lorenzo Busetto, phD (2019) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate py_to_r r_to_py
#' @importFrom magrittr "%>%"
#' @importFrom sf st_bbox st_read st_centroid st_polygon st_transform
#'
#' @examples \dontrun{
#' pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
#' time_window <- as.Date(c("2016-05-01", "2017-07-30"))
#'
#' # Full-period list
#' example_s2_list <- s2_list_new(
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
#' example_s2_list <- s2_list_new(
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

s2_list_new <- function(spatial_extent=NULL, tile=NULL, orbit=NULL, # spatial parameters
                        time_interval=NULL, time_period = "full", # temporal parameters
                        level="auto",
                        apihub=NA,
                        max_cloud=100,
                        output_type = "vector") {
  
  # to avoid NOTE on check
  . <- i <- NULL
  
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
    if (length(spatial_extent)==0) {
      FALSE
    } else {
      TRUE
    }
  } else {
    TRUE
  }
  
  # if not, retrieve it from tile
  if (!spatial_extent_exists) {
    if (is.null(tile)) {
      print_message(
        type = "error",
        "At least one parameter among spatial_extent and tile must be specified."
      )
    } else {
      # extract and import tiles kml
      s2tiles <- s2_tiles()
      # take the the selected tiles as extent
      # (this will result in the selection of more tiles, cause to overlapping
      # areas; it is filtered in s2_download, but it is slow: FIXME).
      # It is not possible to use tile centroids, because tile of external areas
      # of orbits could not be included).
      spatial_extent <- suppressWarnings(
        s2tiles[s2tiles$tile_id %in% tile,] #%>%
        # sf::st_centroid()
      )
    }
  } else {
    # # dissolve spatial extent to multipolygon
    # spatial_extent <- st_union(spatial_extent)
    spatial_extent <- st_as_sfc(st_bbox(spatial_extent))
  }
  
  # checks on dates
  # TODO add checks on format
  if (length(time_interval)==1) {
    time_interval <- rep(time_interval,2)
  }
  # split time_interval in case of seasonal download
  time_intervals <- if (time_period == "full") {
    data.frame(
      "start" = time_interval[1],
      "end"   = time_interval[2],
      stringsAsFactors = FALSE
    )
  } else if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_interval[1], time_interval[2], by = "year"), "%Y-%m-%d"),
      "end"   = strftime(rev(seq(time_interval[2], time_interval[1], by = "-1 year")), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  }
  
  # convert orbits to integer
  if (is.null(orbit)) {
    orbit <- list(NULL)
  } else {
    orbit <- as.integer(orbit)
    if (anyNA(orbit)) {
      orbit <- list(NULL)
    }
  }
  
  # link to apihub
  if (is.null(apihub)) {
    apihub <- system.file("extdata/apihub.txt", package="sen2r")
  }
  user <- as.character(read.table(apihub)[1,1])
  pwd <- as.character(read.table(apihub)[1,2])
  if (!file.exists(apihub)) {
    print_message(
      type="error",
      "File apihub.txt with the SciHub credentials is missing."
    )
  }
  
  foot <- ifelse(
    inherits(spatial_extent, "sfc_POINT"),
    paste0('footprint:%22Intersects(',paste(as.numeric(st_coordinates(spatial_extent)[c(2,1)]), collapse = ",%20"),')%22'),
    paste0('footprint:%22Intersects(', sf::st_as_text(sf::st_geometry(spatial_extent)),')%22')
  )
  
  n_entries <- 1
  out_list <- list()
  
  for (t_int in seq_len(nrow(time_intervals))) {
    
    rows      <- 100
    start     <- 0
    end_query <- FALSE
    
    while (!end_query) {
      
      query_string <- paste0(
        'https://scihub.copernicus.eu/dhus//search?',
        'start=', start,
        '&rows=', rows,
        '&q=', foot,
        ' AND platformname:Sentinel-2 ',
        ' AND beginposition:[', time_intervals[t_int,1], 'T00:00:00.000Z',
        ' TO ', time_intervals[t_int,2], 'T23:59:59.000Z]',
        ' AND cloudcoverpercentage:[0 TO ', max_cloud,']'
      )
      query_string = gsub(" ", "%20",query_string)
      query_string = gsub("\\[", "%5b",query_string)
      query_string = gsub("\\]", "%5d",query_string)
      
      out_query <- httr::GET(query_string, httr::authenticate(user, pwd))
      out_xml   <- httr::content(out_query, as = "parsed", encoding = "UTF-8")
      tmp_list  <- xml2::as_list(out_xml)
      
      
      for (ll in seq_along(tmp_list[[1]])) {
        
        in_entry <- xml2::xml_child(out_xml,ll) %>%
          as.character(.) %>%
          strsplit(., "\n")
        
        if (length(which(grepl("<link href=", in_entry[[1]]))) != 0) {
          
          in_entry <- in_entry[[1]]
          
          title <- in_entry[which(grepl("<title>", in_entry))] %>%
            gsub("^.*<title>([^<]+)</title>.*$", "\\1", .)
          
          url <- in_entry[which(grepl("<link href=", in_entry))] %>%
            gsub("^.*<link href=\"([^\"]+)\"/>.*$", "\\1", .)
          
          orbit <- in_entry[which(grepl("relativeorbitnumber", in_entry))] %>%
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
          
          if (length(tileid) == 0 ) {
            tileid <- gsub("^.+_T([0-9]{2}[A-Z]{3})_.+$", "\\1", title)
          }
          # print(paste0(title, ".SAFE"))
          out_list[[n_entries]] <- data.frame(
            name             = paste0(title, ".SAFE"),
            url              = url,
            orbit            = orbit,
            date             = sensdate,
            ccov             = ccov,
            proclev          = proclev,
            sensor           = sensor,
            tileid           = tileid,
            stringsAsFactors = FALSE
          )
          n_entries <- n_entries + 1
        }
      }
      
      if (length(tmp_list[[1]]) - 14 != rows) {
        end_query <- TRUE
      } else {
        start <- start + rows
      }
    }
    
  }
  out_dt <- rbindlist(out_list)
  
  # remove "wrong" orbits if needed
  if (!is.null(tile)) {
    out_dt <- out_dt[tileid %in% tile,]
  }
  
  if (!is.null(orbit)) {
    out_dt <- out_dt[orbit %in% sprintf("%03i", as.numeric(orbit)),]
  }
  
  if (nrow(out_dt) == 0) {return(character(0))}
  if (level == "L1C") {
    out_dt <- out_dt[proclev == "Level-1C",]
  } else if (level == "L2A") {
    out_dt <- out_dt[grepl("^Level-2Ap?$", proclev),]
  } else if (level == "auto") {
    out_dt <- out_dt[order(-proclev),]
    out_dt <- out_dt[,head(.SD, 1),  by = .(date, orbit)]
  }
  if (nrow(out_dt) == 0) {return(character(0))}
  out_dt <- out_dt[order(date),]
  
  # FIXME remove this part of code when s2_download will be rewritten
  out_dt$url <- gsub("/\\$value", "/\\\\$value", out_dt$url) %>%
    gsub("/dhus/", "/apihub/", .)
  
  if (output_type == "data.table") {
    return(out_dt)
  } else {
    out_vector <- out_dt$url
    names(out_vector) <- out_dt$name
    return(out_vector)
  }
  
}

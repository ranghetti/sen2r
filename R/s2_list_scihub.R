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

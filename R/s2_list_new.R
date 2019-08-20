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
#' @param ignore_ingestion_time (optional) Logical: if TRUE (default),
#'  the research is performed basing only on the sensing date and time
#'  (the time in which the image was acquired), ignoring the ingestion date
#'  and time (the time the image was ingested).
#'  If FALSE, products with the ingestion time specified with `time_interval`
#'  are first of all filtered, and them the research is performed on the sensing
#'  time among them.
#'  `ignore_ingestion_time = TRUE` ensures to perform a complete research, but
#'  it is slower; setting it to `FALSE` speeds up, although some products could
#'  be ignored (but generally the ingestion date is the same of the sensing date).
#' @param apihub Path of the "apihub.txt" file containing credentials
#'  of scihub account. If NA (default) the default credentials
#'  (username "user", password "user") will be used.
#' @param max_cloud Integer number (0-100) containing the maximum cloud
#'  level of the tiles to be listed (default: no filter).
#' @return A vector of available products (being each element an URL,
#'  and its name the product name).
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom reticulate py_to_r r_to_py
#' @importFrom magrittr "%>%"
#' @importFrom sf st_bbox st_read st_centroid st_polygon st_transform
#'
#' @examples \dontrun{
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

s2_list_new <- function(spatial_extent=NULL, tile=NULL, orbit=NULL, # spatial parameters
                        time_interval=NULL, time_period = "full", # temporal parameters
                        level="auto",
                        ignore_ingestion_time = TRUE,
                        apihub=NA,
                        max_cloud=100) {

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
      s2tiles_kmz <- system.file("extdata","vector","s2_tiles.kmz",package="sen2r")
      s2tiles_kml <- gsub("\\.kmz$",".kml",s2tiles_kmz)
      if (!file.exists(s2tiles_kml)) {
        unzip(zipfile = s2tiles_kmz,
              files   = basename(s2tiles_kml),
              exdir   = dirname(s2tiles_kml),
              unzip   = "internal")
      }
      s2tiles <- st_read(s2tiles_kml, stringsAsFactors=FALSE, quiet=TRUE)
      # take the the selected tiles as extent
      # (this will result in the selection of more tiles, cause to overlapping
      # areas; it is filtered in s2_download, but it is slow: FIXME).
      # It is not possible to use tile centroids, because tile of external areas
      # of orbits could not be included).
      spatial_extent <- suppressWarnings(
        s2tiles[s2tiles$Name %in% tile,] #%>%
        # sf::st_centroid()
      )
    }
  }

  # checks on inputs
  spatext <- st_bbox(st_transform(spatial_extent, 4326))

  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (spatext["xmin"]==spatext["xmax"] || spatext["ymin"]==spatext["ymax"]) {
    lon <- mean(spatext["xmin"], spatext["xmax"])
    lat <- mean(spatext["ymin"], spatext["ymax"])
    lonmin <- lonmax <- latmin <- latmax <- NULL
  } else {
    lonmin <- spatext["xmin"]; lonmax <- spatext["xmax"]
    latmin <- spatext["ymin"]; latmax <- spatext["ymax"]
    lon <- lat <- NULL
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

  # import s2download
  s2download <- import_s2download(convert=FALSE)

  # read the path of wget
  binpaths <- load_binpaths("wget")

  # link to apihub
  if (is.null(apihub)) {
    apihub <- file.path(s2download$inst_path,"apihub.txt")
    user   <- as.character(read.table(apihub)[1,1])
    pwd    <- as.character(read.table(apihub)[1,2])
  }
  if (!file.exists(apihub)) {
    print_message(
      type="error",
      "File apihub.txt with the SciHub credentials is missing."
    ) # TODO build it
  }

  # set corr_type
  corr_type <- switch(
    level,
    auto = "auto",
    L1C  = "no",
    L2A  = "scihub",
    "auto")

  foot <- ifelse(inherits(spatial_extent, "sfc_POINT"),
                 paste0('footprint:%22Intersects(',paste(as.numeric(st_coordinates(spatial_extent)[c(2,1)]), collapse = ",%20"),')%22'),
                 paste0('footprint:%22Intersects(', sf::st_as_text(sf::st_geometry(spatial_extent)),')%22')
  )

  n_entries <- 1
  out_list  <- list()

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
        ' TO ', time_intervals[t_int,2], 'T00:00:00.000Z]',
        ' AND cloudcoverpercentage:[0 TO ', max_cloud,']'
      )
      # browser()
      query_string = gsub(" ", "%20",query_string)
      query_string = gsub("\\[", "%5b",query_string)
      query_string = gsub("\\]", "%5d",query_string)

      out_query <- httr::GET(query_string, httr::authenticate(user, pwd))
      out_xml   <- httr::content(out_query, as = "parsed")
      tmp_list  <- xml2::as_list(out_xml)


      for (ll in seq_along(tmp_list[[1]])) {

        in_entry <- xml2::xml_child(out_xml,ll) %>%
          as.character(.) %>%
          strsplit(., "\n")

        if (length(which(grepl("<link href=", in_entry[[1]]))) != 0) {

          in_entry <- in_entry[[1]]

          title <- in_entry[which(grepl("<title>", in_entry))]
          title <- gsub("<title>", "", title)
          title <- trimws(gsub("</title>", "", title))

          url <- in_entry[which(grepl("<link href=", in_entry))]
          url <- gsub("<link href=\"", "", url)
          url <- trimws(gsub("\"/>", "", url))

          cur_orbit <- in_entry[which(grepl("relativeorbitnumber", in_entry))]
          cur_orbit <- gsub("<int name=\"relativeorbitnumber\">", "", cur_orbit)
          cur_orbit <- trimws(gsub("</int>", "", cur_orbit))
          orbit <- sprintf("%03i", as.numeric(orbit))

          ccov <- in_entry[which(grepl("cloudcoverpercentage", in_entry))]
          ccov <- gsub("<double name=\"cloudcoverpercentage\">", "", ccov)
          ccov <- as.numeric(trimws(gsub("</double>", "", ccov)))

          proclev <- in_entry[which(grepl("processinglevel", in_entry))]
          proclev <- gsub("<str name=\"processinglevel\">", "", proclev)
          proclev <- trimws(gsub("</str>", "", proclev))

          sensor <- in_entry[which(grepl("platformserialidentifier", in_entry))]
          sensor <- gsub("<str name=\"platformserialidentifier\">", "", sensor)
          sensor <- trimws(gsub("</str>", "", sensor))

          tileid <- in_entry[which(grepl("name=\"tileid\"", in_entry))]
          tileid <- gsub("<str name=\"tileid\">", "", tileid)
          tileid <- trimws(gsub("</str>", "", tileid))

          sensdate <- in_entry[which(grepl("name=\"endposition\"", in_entry))]
          sensdate <- gsub("<date name=\"endposition\">", "", sensdate)
          sensdate <- as.Date(trimws(gsub("</date>", "", sensdate)))

          if (length(tileid) == 0 ) {
            tileid <- substring(title,40,44)
          }
          # print(paste0(title, ".SAFE"))
          out_list[[n_entries]] <- data.frame(name             = paste0(title, ".SAFE"),
                                              url              = url,
                                              orbit            = cur_orbit,
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
  out_list <- do.call("rbind", out_list)

  # remove "wrong" orbits if needed
  if (!is.null(tile)) {
    out_list <- out_list[out_list$tileid %in% tile,]
  }

  if (!is.null(orbit)) {
    out_list <- out_list[out_list$orbit %in% as.numeric(orbit),]
  }

  if (level == "L1C") {
    out_list <- out_list[out_list$proclev == "Level-1C",]
  } else {
    if (level == "L2A") {
      out_list <- out_list[out_list$proclev == "Level-2Ap",]
    } else {
      out_list <- data.table::data.table(out_list)
      out_list <- out_list[order(-proclev)]
      out_list <- out_list[,head(.SD, 1),  by = .(date, orbit)]
      out_list <- out_list[order(date)]
    }
  }

  return(out_list)
  # browser()
  #
  #     if (length(names(inxml)) != 0 && names(inxml)[1] == "title") {
  #       for (nn in seq_along(inxml)) {
  #         url  <- inxml$link
  #         name <- inxml$title
  #       }
  #     }
  #     safename <- inxml["title"]
  #     url      <- inxml["link"]
  #     cloud    <- inxml["link"]
  #
  #   }
  #
  #
  #   https://scihub.copernicus.eu/dhus//search?start=0&rows=100&q=
  #     (footprint:%22Intersects(POLYGON((9.29168701%2049.9388475,9.36584472%2049.58400677,10.145874%2049.44312875,10.6814575%2049.75642885,9.75860595%2050.2032753,9.29168701%2049.9388475)))
  # %22%20AND%20platformname:Sentinel-2%20AND%20beginposition:%5b2017-08-01T00:00:00.000Z%20TO%202017-08-30T00:00:00.000Z%5d)
  # orbit = 65
  #
  #
  # # run the research of the list of products
  # av_prod_tuple <- lapply(orbit, function(o) {
  #   lapply(seq_len(nrow(time_intervals)), function(i) {
  #     # py_capture_output was added not only to allow threating the python output
  #     # as R message, but also because, on Windows, the following error was returned
  #     # if the function was launched outside py_capture_output:
  #     # Error in py_call_impl(callable, dots$args, dots$keywords) :
  #     #   IOError: [Errno 9] Bad file descriptor
  #     # from a python console, the error did not appear (only inside reticulate).
  #     py_output <- reticulate::py_capture_output(
  #       py_return <- s2download$s2_download(
  #         lat=lat, lon=lon, latmin=latmin, latmax=latmax, lonmin=lonmin, lonmax=lonmax,
  #         start_date=time_intervals[i,1], end_date=time_intervals[i,2],
  #         start_ingest_date=if (ignore_ingestion_time==FALSE) {time_intervals[i,1]} else {r_to_py(NULL)},
  #         end_ingest_date=if (ignore_ingestion_time==FALSE) {time_intervals[i,2]} else {r_to_py(NULL)},
  #         # start_ingest_date=time_intervals[i,1],
  #         # end_ingest_date=time_intervals[i,2],
  #         tile=r_to_py(tile),
  #         orbit=r_to_py(o),
  #         apihub=apihub,
  #         max_cloud=max_cloud,
  #         list_only=TRUE,
  #         max_records=0, # TODO this is possible after an addition in Sentinel-download python script:
  #         # cycle on product requests (one per 100 products) is interrupted after
  #         # the first request of length 0.
  #         corr_type=corr_type,
  #         downloader_path=dirname(binpaths$wget)
  #       )
  #     )
  #     # message(py_output) # do not show the output of the products found
  #     # as formatted by Hagolle
  #     py_return
  #   })
  # })
  #
  # av_prod_list <- lapply(av_prod_tuple, lapply, function(x) {py_to_r(x)[[1]]})
  # if (is.character(try(unlist(av_prod_list)))) {
  #   av_prod_list <- unlist(av_prod_list)
  #   names(av_prod_list) <- unlist(lapply(av_prod_tuple, lapply, function(x) {py_to_r(x)[[2]]}))
  # } else {
  #   av_prod_list <- character(0)
  # }
  #
  # # filter on tiles
  # # (filtering within python code does not take effect with list_only=TRUE)
  # # The filter is applied only on compactname products
  # # (using sen2r(), a complete filter on tiles is applied after downloading the product;
  # # however, s2_download() would correctly download only required tiles)
  #
  # if (!is.null(tile) & length(av_prod_list)>0) {
  #   av_prod_tiles <- lapply(names(av_prod_list), function(x) {
  #     safe_getMetadata(x, info="nameinfo")$id_tile %>%
  #       ifelse(is.null(.), NA, .)
  #   }) %>%
  #     unlist()
  #   av_prod_list <- av_prod_list[av_prod_tiles %in% tile | is.na(av_prod_tiles)]
  # }
  #
  # return(av_prod_list)

  # TODO add all the checks!

}

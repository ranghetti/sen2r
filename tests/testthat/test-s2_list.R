message("\n---- Test s2_list() and safe_getMetadata(info = 'nameinfo') ----")
testthat::skip_on_cran()
# testthat::skip_on_travis()
testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")

testthat::test_that(
  "Tests on s2_list - Error if internet is down", {
    testthat::expect_error(
      httptest::without_internet({
        s2_list(
          tile = "32TNR",
          time_interval = as.Date("2017-05-01"),
          orbit = "065",
          apihub = tests_apihub_path
        )
      }),
      regexp = gsub(
        " ", "[ \n]",
        "[Ii]nternet connection or SciHub may be down"
      )
    )
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, no pos", {
    s2_list_test <- s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      apihub = tests_apihub_path
    )
    testthat::expect_is(s2_list_test, "safelist")
    s2_dt_test <- as(s2_list_test, "data.table")
    testthat::expect_is(s2_dt_test, "data.table")
    testthat::expect_equal(nrow(s2_dt_test), 3)
    testthat::expect_true(min(as.Date(s2_dt_test$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(max(as.Date(s2_dt_test$sensing_datetime)) <= as.Date("2017-08-01"))
    testthat::expect_equal(unique(s2_dt_test$id_tile), "32TNR")
    testthat::expect_equal(unique(s2_dt_test$id_orbit), "065")
    testthat::expect_equal(mean(s2_dt_test$clouds), 38.3432, tolerance = 1e-6)
    testthat::expect_equal(unique(s2_dt_test$online), NA)
    testthat::expect_equal(
      grepl("^https://apihub\\.copernicus\\.eu",s2_dt_test$url),
      rep(TRUE, 3)
    )
    
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_dt_test[order(sensing_datetime),]$name, info = "nameinfo") # data.table
    testthat::expect_is(safe_metadata, "data.table")
    testthat::expect_equal(unique(safe_metadata$prod_type), "product")
    testthat::expect_equal(unique(safe_metadata$version), "compact")
    testthat::expect_equal(unique(safe_metadata$mission), "2A")
    testthat::expect_equal(unique(safe_metadata$level), "2A")
    testthat::expect_equal(as.Date(safe_metadata$sensing_datetime), as.Date(paste0("2017-05-",c("07","17","27"))))
    testthat::expect_equal(unique(safe_metadata$id_orbit), "065")
    testthat::expect_equal(unique(safe_metadata$id_tile), "32TNR")
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, no orbits, nopos, check availability, apihub service", {
    s2_list_test <- s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      availability = "check",
      apihub = tests_apihub_path,
      service = "apihub"
    )
    testthat::expect_is(s2_list_test, "safelist")
    s2_sf_test <- as(s2_list_test, "sf")
    testthat::expect_is(s2_sf_test, "sf")
    testthat::expect_equal(sf::st_crs(s2_sf_test)$epsg, 4326)
    testthat::expect_equal(nrow(s2_sf_test), 6)
    testthat::expect_is(
      safe_getMetadata(names(s2_sf_test$name), info = "nameinfo", format = "list"), 
      "list"
    )

    testthat::expect_true(grepl("https://apihub.copernicus.eu/apihub/", s2_list_test[1]))
    testthat::expect_true(min(as.Date(s2_sf_test$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(min(as.Date(s2_sf_test$sensing_datetime)) <= as.Date("2017-05-31"))
    testthat::expect_equal(unique(s2_sf_test$id_tile), "32TNR")
    testthat::expect_true(all(s2_sf_test$id_orbit %in% c("022","065")))
    testthat::expect_equal(mean(s2_sf_test$clouds), 58.36295, tolerance = 1e-6)
    testthat::expect_true(all(s2_sf_test$online %in% c(TRUE, FALSE)))
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Multiple tiles, multiple orbits, pos, check availability, dhus service", {
    s2_list_test <- as.data.table(s2_list(
      tile = c("32TNR", "32TMR"),
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      availability = "check",
      apihub = tests_apihub_path,
      service = "dhus"
    ))
    testthat::expect_true(grepl("https://scihub.copernicus.eu/dhus/", s2_list_test$url[1]))
    testthat::expect_equal(nrow(s2_list_test), 11)
    testthat::expect_equal(unique(s2_list_test$id_tile), c("32TNR", "32TMR"))
    testthat::expect_true(min(as.Date(s2_list_test$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(max(as.Date(s2_list_test$sensing_datetime)) <= as.Date("2017-08-01"))
    testthat::expect_equal(unique(s2_list_test$id_orbit), c("022", "065", "108"))
    testthat::expect_equal(mean(s2_list_test$clouds), 51.683, tolerance = 1e-6)
    testthat::expect_true(all(unique(s2_list_test$online) %in% c(TRUE, FALSE)))
    testthat::expect_equal(
      grepl("^https://scihub\\.copernicus\\.eu",s2_list_test$url),
      rep(TRUE, 11)
    )
    
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(
      as.character(s2_list_test[order(sensing_datetime),]$name), 
      info = "nameinfo", format = "vector"
    )
    testthat::expect_is(safe_metadata, "list")
    testthat::expect_equal(length(safe_metadata), 11)
    testthat::expect_equal(unique(safe_metadata$prod_type), "product")
    testthat::expect_equal(unique(safe_metadata$version), "compact")
    testthat::expect_equal(unique(safe_metadata$mission), "2A")
    testthat::expect_equal(unique(safe_metadata$level), c("1C","2A"))
    testthat::expect_equal(unique(safe_metadata$id_orbit), c("022", "065", "108"))
    testthat::expect_equal(unique(safe_metadata$id_tile), c("32TNR", "32TMR"))
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - single orbit, point pos, no tile, only available online", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      availability ="online",
      apihub = tests_apihub_path
    )
    testthat::expect_lte(length(s2_list_test), 3)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, pos, tile, only from LTA", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      availability = "lta",
      apihub = tests_apihub_path
    )
    testthat::expect_lte(length(s2_list_test), 3)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Cloudiness", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-31")),
      orbit = "065",
      max_cloud = 50,
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 1)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    time_window <- as.Date(c("2016-05-01", "2016-05-31"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = time_window,
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - multipoint", {
    pp <- data.frame(x = c(6, 9.95),
                     y = c(45.81, 45.95))
    pos <- sf::st_set_crs(sf::st_as_sf(pp, coords = c("x","y")), 4326)
    time_window <- as.Date(c("2016-05-01", "2016-05-31"))
    s2_list_test <- as(
      s2_list(
        spatial_extent = pos, 
        time_interval = time_window, 
        apihub = tests_apihub_path
      ), 
      "data.frame"
    )
    testthat::expect_equal(length(s2_list_test$id_orbit), 12)
    
    # reproject
    pos <- sf::st_transform(pos, 32632)
    s2_list_test <- as(
      s2_list(
        spatial_extent = pos, 
        time_interval = time_window,
        apihub = tests_apihub_path
      ),
      "data.frame"
    )
    testthat::expect_equal(length(s2_list_test$id_orbit), 12)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - polygon, multiple tiles, multiple orbits", {
    pos <- sf::st_as_sfc(sf::st_bbox(
      c("xmin" = 7, "ymin" = 44, "xmax" = 13, "ymax" = 47),
      "crs" = sf::st_crs(4326)
    ))
    time_window <- as.Date(c("2016-05-01", "2016-05-10"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window,
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 61)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - point, single tile, large time window", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    time_window <- as.Date(c("2016-05-01", "2019-05-10"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window,
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 182)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit - no images", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-01")),
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 0)
    testthat::expect_equal(
      nrow(safe_getMetadata(names(s2_list_test), info = "nameinfo")), 0
    )
    testthat::expect_length(
      safe_getMetadata(names(s2_list_test), info = "vector"), 0
    )
    testthat::expect_length(
      safe_getMetadata(names(s2_list_test), info = "list"), 0
    )
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit - seasonal", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2017-08-01")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 22)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - seasonal - single year", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - seasonal - single year", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "L1C",
      time_interval = as.Date(c("2017-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on s2_list - process level", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- as.data.table(s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "auto",
      time_interval = as.Date(c("2016-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path
    ))
    testthat::expect_equal(length(s2_list_test$level), 11)
    testthat::expect_equal(unique(s2_list_test$level), c("1C" , "2Ap"))
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_list_test[order(sensing_datetime),]$name, info = "nameinfo")
    testthat::expect_is(safe_metadata, "data.table")
    testthat::expect_equal(dim(safe_metadata), c(11,11))
    testthat::expect_equal(unique(safe_metadata$level), c("1C","2A"))
    
    s2_list_test <- as.data.table(s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "L1C",
      time_interval = as.Date(c("2016-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path
    ))
    testthat::expect_equal(length(s2_list_test$level), 11)
    testthat::expect_equal(unique(s2_list_test$level), c("1C"))
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_list_test[order(sensing_datetime),]$name, info = "nameinfo")
    testthat::expect_is(safe_metadata, "data.table")
    testthat::expect_equal(dim(safe_metadata), c(11,11))
    testthat::expect_equal(unique(safe_metadata$level), "1C")
  }
)

testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Parameter errors", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    
    #wrong extent
    testthat::expect_error(
      s2_list(
        spatial_extent = "pos",
        time_interval = as.Date(c("2017-05-01", "2017-06-30")),
        apihub = tests_apihub_path
      ),
      regexp = gsub(
        " ", "[ \n]",
        "`spatial_extent` is not a `sf` or `sfc` object"
      )
    )
    
    # wrong dates
    testthat::expect_error(
      s2_list(
        spatial_extent = pos,
        time_interval = c("2017-05-XX", "2017-06-30"),
        apihub = tests_apihub_path
      ),
      regexp = gsub(
        " ", "[ \n]",
        "`time_interval` must be of class .+ cohercible to Date"
      )
    )
    
    # wrong availability
    testthat::expect_error(
      s2_list(
        spatial_extent = pos,
        time_interval = c("2017-05-01", "2017-06-30"),
        availability = "offline",
        apihub = tests_apihub_path
      ),
      regexp = gsub(
        " ", "[ \n]",
        '`availability` must be one among "online", "lta", "check" and "ignore"'
      )
    )
  }
)


message("\n---- Test s2_list(..., server = 'gcloud') ----")

# Run tests only if gcloud is installed and configured
testthat::skip_if_not(suppressWarnings(check_gcloud(abort = FALSE)))

# Check the gcloud check
testthat::test_that(
  "Check GCloud installation", {
    testthat::expect_equal(
      check_gcloud(force = TRUE),
      TRUE
    )
    testthat::expect_equal(
      check_gcloud(load_binpaths()$gsutil, force = TRUE),
      TRUE
    )
    testthat::expect_equal(
      check_gcloud(dirname(load_binpaths()$gsutil), force = TRUE),
      TRUE
    )
    testthat::expect_error(
      check_gcloud("/wrong/path", force = TRUE),
      regexp = gsub(
        " ", "[ \n]",
        "Google Cloud SDK was not found at the provided path"
      )
    )
    testthat::expect_warning(
      check_warning <- check_gcloud("/wrong/path", force = TRUE, abort = FALSE),
      regexp = gsub(
        " ", "[ \n]",
        "Google Cloud SDK was not found at the provided path"
      )
    )
    testthat::expect_equal(check_warning, FALSE)
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, single tile, single orbit, no pos, only L1C, separate servers", {
    s2_list_test_scihub <- sen2r::s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      level = "L1C",
      apihub = tests_apihub_path,
      server = "scihub"
    )
    s2_list_test_gcloud <- sen2r::s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      level = "L1C",
      apihub = tests_apihub_path,
      server = "gcloud"
    )
    testthat::expect_is(s2_list_test_gcloud, "safelist")
    s2_dt_test_scihub <- as(s2_list_test_gcloud, "data.table")
    s2_dt_test_gcloud <- as(s2_list_test_gcloud, "data.table")
    testthat::expect_equal(nrow(s2_dt_test_gcloud), 3)
    testthat::expect_equal(nrow(s2_dt_test_gcloud), nrow(s2_dt_test_scihub))
    testthat::expect_true(min(as.Date(s2_dt_test_gcloud$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(max(as.Date(s2_dt_test_gcloud$sensing_datetime)) <= as.Date("2017-08-01"))
    testthat::expect_equal(unique(s2_dt_test_gcloud$id_tile), "32TNR")
    testthat::expect_equal(unique(s2_dt_test_gcloud$id_orbit), "065")
    testthat::expect_equal(s2_dt_test_gcloud$name, s2_dt_test_scihub$name)
    testthat::expect_equal(
      grepl("^gs://gcp-public-data-sentinel-2/(L2/)?tiles/32/T/NR/",s2_dt_test_gcloud$url),
      rep(TRUE, 3)
    )
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, multiple tiles, multiple orbits, pos, all servers", {
    # s2_dt_test_scihub <- as.data.table(sen2r::s2_list(
    #   tile = c("32TNR", "32TMR"),
    #   time_interval = as.Date(c("2017-05-01", "2017-05-31")),
    #   availability = "check",
    #   apihub = tests_apihub_path,
    #   server = "scihub"
    # ))
    s2_dt_test_check <- as.data.table(sen2r::s2_list(
      tile = c("32TNR", "32TMR"),
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      availability = "check",
      apihub = tests_apihub_path,
      server = c("scihub", "gcloud")
    ))
    s2_dt_test_nocheck <- as.data.table(sen2r::s2_list(
      tile = c("32TNR", "32TMR"),
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      availability = "ignore",
      apihub = tests_apihub_path,
      server = c("scihub", "gcloud")
    ))
    testthat::expect_equal(nrow(s2_dt_test_check), 11)
    testthat::expect_equal(nrow(s2_dt_test_check), nrow(s2_dt_test_nocheck))
    testthat::expect_true(min(as.Date(s2_dt_test_check$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(max(as.Date(s2_dt_test_check$sensing_datetime)) <= as.Date("2017-08-01"))
    testthat::expect_equal(sort(unique(s2_dt_test_check$id_tile)), c("32TMR", "32TNR"))
    testthat::expect_equal(sort(unique(s2_dt_test_check$id_orbit)), c("022", "065", "108"))
    testthat::expect_equal(
      grepl(paste0(
        "^((gs://gcp-public-data-sentinel-2/(L2/)?tiles/32/T/[MN]R/)|",
        "(https://((sci)|(api))hub\\.copernicus\\.eu))"
      ),s2_dt_test_check$url),
      rep(TRUE, 11)
    )
    testthat::expect_equal(
      grepl(paste0(
        "^((gs://gcp-public-data-sentinel-2/(L2/)?tiles/32/T/[MN]R/)|",
        "(https://((sci)|(api))hub\\.copernicus\\.eu))"
      ),s2_dt_test_nocheck$url),
      rep(TRUE, 11)
    )
    testthat::expect_gte(
      sum(s2_dt_test_check$level=="1C"),
      sum(s2_dt_test_nocheck$level=="1C")
    )
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, single orbit, point pos, no tile, only available online", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- sen2r::s2_list(
      spatial_extent = pos,
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      availability ="online",
      apihub = tests_apihub_path,
      server = c("scihub", "gcloud")
    )
    testthat::expect_equal(length(s2_list_test), 3)
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, cloudiness", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- sen2r::s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-31")),
      orbit = "065",
      max_cloud = 50,
      apihub = tests_apihub_path,
      server = "gcloud"
    )
    testthat::expect_equal(length(s2_list_test), 1)
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, single tile, multi orbit - no images", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- sen2r::s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-01")),
      apihub = tests_apihub_path,
      server = "gcloud"
    )
    testthat::expect_equal(length(s2_list_test), 0)
    testthat::expect_equal(
      nrow(safe_getMetadata(names(s2_list_test), info = "nameinfo")), 0
    )
    testthat::expect_length(
      safe_getMetadata(names(s2_list_test), info = "vector"), 0
    )
    testthat::expect_length(
      safe_getMetadata(names(s2_list_test), info = "list"), 0
    )
  }
)

testthat::test_that(
  "Tests on s2_list - GCloud, single tile, multi orbit - seasonal", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- sen2r::s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2017-08-01")),
      time_period = "seasonal",
      orbit = "065",
      apihub = tests_apihub_path,
      server = "gcloud"
    )
    testthat::expect_equal(length(s2_list_test), 21)
  }
)

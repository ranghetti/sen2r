context("Test s2_list() and safe_getMetadata(info = 'nameinfo')")
testthat::skip_on_cran()
testthat::skip_on_travis()

testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, no pos", {
    s2_list_test <- s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      output_type = "data.table"
    )
    testthat::expect_is(s2_list_test, "data.table")
    testthat::expect_equal(nrow(s2_list_test), 3)
    testthat::expect_true(min(as.Date(s2_list_test$sensing_datetime)) >= as.Date("2017-05-01"))
    testthat::expect_true(max(as.Date(s2_list_test$sensing_datetime)) <= as.Date("2017-08-01"))
    testthat::expect_equal(unique(s2_list_test$id_tile), "32TNR")
    testthat::expect_equal(unique(s2_list_test$id_orbit), "065")
    testthat::expect_equal(mean(s2_list_test$clouds), 38.3432, tolerance = 1e-6)
    testthat::expect_equal(unique(s2_list_test$online), NA)
    testthat::expect_equal(
      grepl("^https://scihub\\.copernicus\\.eu",s2_list_test$url),
      rep(TRUE, 3)
    )
    
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_list_test[order(sensing_datetime),]$name, info = "nameinfo") # data.table
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

testthat::test_that(
  "Tests on s2_list - Single tile, no orbits, nopos, check availability", {
    s2_list_test <- s2_list(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      output_type = "vector",
      availability = "check"
    )
    testthat::expect_is(s2_list_test, "safelist")
    testthat::expect_equal(length(s2_list_test), 6)
    testthat::expect_is(
      safe_getMetadata(names(s2_list_test), info = "nameinfo", format = "list"), 
      "list"
    )

    testthat::expect_true(min(as.Date(attr(s2_list_test, "sensing_datetime"))) >= as.Date("2017-05-01"))
    testthat::expect_true(min(as.Date(attr(s2_list_test, "sensing_datetime"))) <= as.Date("2017-05-31"))
    testthat::expect_equal(unique(attr(s2_list_test, "id_tile")), "32TNR")
    testthat::expect_true(all(attr(s2_list_test, "id_orbit") %in% c("022","065")))
    testthat::expect_equal(mean(attr(s2_list_test, "clouds")), 58.36295, tolerance = 1e-6)
    testthat::expect_true(all(attr(s2_list_test, "online") %in% c(TRUE, FALSE)))
  }
)

testthat::test_that(
  "Tests on s2_list - Multiple tiles, multiple orbits, pos, check availability", {
    s2_list_test <- as.data.table(s2_list(
      tile = c("32TNR", "32TMR"),
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      availability = "check"
    ))
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

testthat::test_that(
  "Tests on s2_list - single orbit, point pos, no tile, only available online", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      availability ="online"
    )
    testthat::expect_lte(length(s2_list_test), 3)
  }
)

testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, pos, tile, only from LTA", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-05-31")),
      orbit = "065",
      availability = "lta"
    )
    testthat::expect_lte(length(s2_list_test), 3)
  }
)

testthat::test_that(
  "Tests on s2_list - Cloudiness", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-31")),
      orbit = "065",
      max_cloud = 50
    )
    testthat::expect_equal(length(s2_list_test), 1)
  }
)

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    time_window <- as.Date(c("2016-05-01", "2016-05-31"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = time_window
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::test_that(
  "Tests on s2_list - multipoint", {
    pp <- data.frame(x = c(6, 9.95),
                     y = c(45.81, 45.95))
    pos <- sf::st_as_sf(pp, coords = c("x","y")) %>% sf::st_set_crs(4326)
    time_window <- as.Date(c("2016-05-01", "2016-05-31"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window,
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$id_orbit), 12)
    
    # reproject
    pos <- sf::st_transform(pos, 32632)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window,
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$id_orbit), 12)
  }
)

testthat::test_that(
  "Tests on s2_list - polygon, multiple tiles, multiple orbits", {
    pos <- sf::st_as_sfc(sf::st_bbox(
      c("xmin" = 7, "ymin" = 44, "xmax" = 13, "ymax" = 47),
      "crs" = sf::st_crs(4326)
    ))
    time_window <- as.Date(c("2016-05-01", "2016-05-10"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window
    )
    testthat::expect_equal(length(s2_list_test), 69)
  }
)

testthat::test_that(
  "Tests on s2_list - point, single tile, large time window", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    time_window <- as.Date(c("2016-05-01", "2019-05-10"))
    s2_list_test <- s2_list(
      spatial_extent = pos,
      time_interval = time_window
    )
    testthat::expect_equal(length(s2_list_test), 182)
  }
)

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit - no images", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-01"))
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
  "Tests on s2_list - Single tile, multi orbit - seasonal", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2017-08-01")),
      time_period = "seasonal",
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 22)
  }
)

testthat::test_that(
  "Tests on s2_list - seasonal - single year", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::test_that(
  "Tests on s2_list - seasonal - single year", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "L1C",
      time_interval = as.Date(c("2017-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 6)
  }
)

testthat::test_that(
  "Tests on s2_list - process level", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "auto",
      time_interval = as.Date(c("2016-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065", output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$level), 11)
    testthat::expect_equal(unique(s2_list_test$level), c("1C" , "2Ap"))
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_list_test[order(sensing_datetime),]$name, info = "nameinfo")
    testthat::expect_is(safe_metadata, "data.table")
    testthat::expect_equal(dim(safe_metadata), c(11,11))
    testthat::expect_equal(unique(safe_metadata$level), c("1C","2A"))
    
    s2_list_test <- s2_list(
      spatial_extent = pos,
      tile = "32TNR",
      level = "L1C",
      time_interval = as.Date(c("2016-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065", output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$level), 11)
    testthat::expect_equal(unique(s2_list_test$level), c("1C"))
    # test safe_getMetadata
    safe_metadata <- safe_getMetadata(s2_list_test[order(sensing_datetime),]$name, info = "nameinfo")
    testthat::expect_is(safe_metadata, "data.table")
    testthat::expect_equal(dim(safe_metadata), c(11,11))
    testthat::expect_equal(unique(safe_metadata$level), "1C")
  }
)

testthat::test_that(
  "Parameter errors", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    
    #wrong extent
    testthat::expect_error(
      s2_list(
        spatial_extent = "pos",
        time_interval = as.Date(c("2017-05-01", "2017-06-30"))
      ),
      regexp = "`spatial_extent` is not a `sf` or `sfc` object"
    )
    
    # wrong dates
    testthat::expect_error(
      s2_list(
        spatial_extent = pos,
        time_interval = c("2017-05-XX", "2017-06-30")
      ),
      regexp = "`time_interval` must be of class .+ cohercible to Date"
    )
    
    # wrong availability
    testthat::expect_error(
      s2_list(
        spatial_extent = pos,
        time_interval = c("2017-05-01", "2017-06-30"),
        availability = "offline"
      ),
      regexp = '`availability` must be one among "online", "lta", "check" and "ignore"'
    )
  }
)

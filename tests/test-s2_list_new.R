context("Test s2_list")
skip_on_cran()
testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, no pos", {
    s2_list_test <- s2_list_new(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-08-01")),
      orbit = "065",
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$orbit), 13)
    testthat::expect_equal(unique(s2_list_test$orbit), "065")
  })

testthat::test_that(
  "Tests on s2_list - Single tile, no orbits, nopos", {
    s2_list_test <- s2_list_new(
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-08-01"))
    )
    testthat::expect_equal(length(s2_list_test), 25)
  })

testthat::test_that(
  "Tests on s2_list - Multiple tiles, no orbits, nopos", {
    s2_list_test <- s2_list_new(
      tile = c("32TNR", "32TMR"),
      time_interval = as.Date(c("2017-05-01", "2017-08-01")),
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$tile), 36)
    testthat::expect_equal(unique(s2_list_test$tile), c("32TNR", "32TMR"))
  })

testthat::test_that(
  "Tests on s2_list - single orbit, point pos, no tile", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      time_interval = as.Date(c("2017-05-01", "2017-08-01")),
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 13)
  })

testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, pos, tile", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-08-01")),
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 13)
  })

testthat::test_that(
  "Tests on s2_list - Cloudiness", {
    s2_list_test <- s2_list_new(
      spatial_extent        = pos,
      tile                  = "32TNR",
      time_interval         = as.Date(c("2016-05-01", "2016-08-01")),
      orbit                 = "065",
      max_cloud             = 50
    )
    testthat::expect_equal(length(s2_list_test), 4)
  })


testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    time_window <- as.Date(c("2016-05-01", "2016-08-01"))
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = time_window
    )
    testthat::expect_equal(length(s2_list_test), 17)
  })

testthat::test_that(
  "Tests on s2_list - multipoint", {
    pp <- data.frame(x = c(6, 9.95),
          y = c(45.81, 45.95))
    pos <-  sf::st_as_sf(pp, coords = c("x","y")) %>% st_set_crs(4326)
    time_window <- as.Date(c("2016-05-01", "2016-08-01"))
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      time_interval = time_window,
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$orbitid), 34)

    # reproject
    pos <- sf::st_transform(pos, 32632)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      time_interval = time_window,
      output_type = "data.table"
    )
    testthat::expect_equal(length(s2_list_test$orbitid), 34)
  })

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit - no images", {
    pos <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2016-05-01"))
    )
    testthat::expect_equal(length(s2_list_test), 0)
  })

testthat::test_that(
  "Tests on s2_list - Single tile, multi orbit - seasonal", {
    pos          <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2016-05-01", "2017-08-01")),
      time_period = "seasonal",
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 22)
  })

testthat::test_that(
  "Tests on s2_list - seasonal - single year", {
    pos          <- sf::st_sfc(sf::st_point(c(9.85,45.81)), crs = 4326)
    s2_list_test <- s2_list_new(
      spatial_extent = pos,
      tile = "32TNR",
      time_interval = as.Date(c("2017-05-01", "2017-06-30")),
      time_period = "seasonal",
      orbit = "065"
    )
    testthat::expect_equal(length(s2_list_test), 6)
  })


context("Test s2_dop()")
testthat::skip_on_cran()
# testthat::skip_on_travis()

testthat::test_that(
  "All the passages in a cycle of 10 days over all the orbits", {
    dop_out <- s2_dop()
    testthat::expect_is(dop_out, "data.table")
    testthat::expect_equal(names(dop_out), c("date", "mission", "orbit"))
    testthat::expect_gte(min(dop_out$date), Sys.Date())
    testthat::expect_lte(max(dop_out$date), Sys.Date()+9)
    testthat::expect_true(all(dop_out$orbit %in% str_pad2(1:143, 3, "left", "0")))
  }
)

testthat::test_that(
  "The passages in the current month over two orbits", {
    dop_out <- s2_dop(c("022", "065"), "this month")
    testthat::expect_is(dop_out, "data.table")
    testthat::expect_equal(unique(strftime(dop_out$date, "%m")), strftime(Sys.Date(), "%m"))
    testthat::expect_true(all(unique(dop_out$orbit) %in% c("022", "065")))
  }
)

testthat::test_that(
  "The dates in which Sentinel-2A will pass in next six weeks over one orbit", {
    dop_out <- s2_dop("022", "6 weeks", mission = "2A")$date
    testthat::expect_is(dop_out, "Date")
    testthat::expect_gte(min(dop_out), Sys.Date())
    testthat::expect_lte(max(dop_out), Sys.Date()+6*7-1)
  }
)

testthat::test_that(
  "The date in which Sentinel-2A would be passed in the last 10 days over one orbit", {
    dop_out <- s2_dop("022", "-10 days", mission = "2A")$date
    testthat::expect_is(dop_out, "Date")
    testthat::expect_equal(length(dop_out), 1)
    testthat::expect_gte(dop_out, Sys.Date()-9)
    testthat::expect_lte(dop_out, Sys.Date())
  }
)

testthat::test_that(
  "All the orbits covered today", {
    dop_out <- s2_dop(timewindow = Sys.Date(), mission = "2B")$orbit
    testthat::expect_is(dop_out, "character")
    testthat::expect_true(all(dop_out %in% str_pad2(1:143, 3, "left", "0")))
  }
)

testthat::test_that(
  "The passages in a fixed time window for one orbit", {
    dop_out <- s2_dop(65, as.Date(c("2018-08-01", "2018-08-31")))
    testthat::expect_is(dop_out, "data.table")
    testthat::expect_gte(min(dop_out$date), as.Date("2018-08-01"))
    testthat::expect_lte(max(dop_out$date), as.Date("2018-08-31"))
    testthat::expect_true(all(dop_out$orbit %in% str_pad2(1:143, 3, "left", "0")))
    testthat::expect_true(all(dop_out$mission %in% c("2A", "2B")))
  }
)

testthat::test_that(
  "A research with no passages found", {
    dop_out <- s2_dop(22, "2018-08-16", mission = "2A")
    testthat::expect_is(dop_out, "data.table")
    testthat::expect_equal(nrow(dop_out), 0)
    testthat::expect_equal(names(dop_out), c("date", "mission", "orbit"))
  }
)

testthat::test_that(
  "Expect error with wrong orbit", {
    testthat::expect_error(
      dop_out <- s2_dop(140:150),
      regexp = "[Pp]arameter 's2_orbits' contains invalid Sentinel-2 orbits"
    )
    testthat::expect_error(
      dop_out <- s2_dop("32TNR"),
      regexp = "[Pp]arameter 's2_orbits' contains invalid Sentinel-2 orbits"
    )
    testthat::expect_error(
      dop_out <- s2_dop(Sys.Date()),
      regexp = "[Pp]arameter 's2_orbits' contains invalid Sentinel-2 orbits"
    )
  }
)

testthat::test_that(
  "Expect error with wrong time window", {
    testthat::expect_error(
      dop_out <- s2_dop(timewindow = "yesterday"),
      regexp = "[Pp]arameter 'timewindow' is not a recognised string"
    )
  }
)

testthat::test_that(
  "Expect error with wrong sensor", {
    testthat::expect_error(
      dop_out <- s2_dop(mission = c("2A", "2B", "2C")),
      regexp = "[Pp]arameter 'mission' cannot contain values different from"
    )
  }
)

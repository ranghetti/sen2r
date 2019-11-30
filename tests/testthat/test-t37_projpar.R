context("Test projpar() / projname()")
testthat::skip_on_cran()
testthat::skip_on_travis()

testthat::test_that(
  "Test longlat", {
    crs_totest <- st_crs(4326)
    testthat::expect_true(projname(crs_totest) %in% c("WGS 84", "unknown"))
    testthat::expect_true(projpar(crs_totest, "geogcs") %in% c("WGS 84", "unknown"))
    testthat::expect_equivalent(projpar(crs_totest, "unit"), "degree")
    testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984")
    testthat::expect_equivalent(projpar(crs_totest, "spheroid"), "WGS 84")
  }
)

testthat::test_that(
  "Test UTM32", {
    crs_totest <- st_crs2("32N")
    testthat::expect_true(projname(crs_totest) %in% c("UTM Zone 32, Northern Hemisphere", "unknown"))
    testthat::expect_true(projpar(crs_totest, "projcs") %in% c("UTM Zone 32, Northern Hemisphere", "unknown"))
    testthat::expect_true(projpar(crs_totest, "geogcs") %in% c("WGS 84", "unknown"))
    testthat::expect_true(projpar(crs_totest, "unit") %in% c("Meter", "metre"))
    testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984")
    testthat::expect_equivalent(projpar(crs_totest, "spheroid"), "WGS 84")
  }
)


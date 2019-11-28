context("Test projpar() / projname()")
testthat::skip_on_cran()
testthat::skip_on_travis()

testthat::test_that(
  "Test longlat", {
    crs_totest <- st_crs(4326)
    testthat::expect_equivalent(projname(crs_totest), "WGS 84")
    testthat::expect_equivalent(projpar(crs_totest, "geogcs"), "WGS 84")
    testthat::expect_equivalent(projpar(crs_totest, "unit"), "degree")
    testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984")
    testthat::expect_equivalent(projpar(crs_totest, "spheroid"), "WGS 84")
  }
)

testthat::test_that(
  "Test UTM32", {
    crs_totest <- st_crs2("32N")
    testthat::expect_equivalent(projname(crs_totest), "UTM Zone 32, Northern Hemisphere")
    testthat::expect_equivalent(projpar(crs_totest, "projcs"), "UTM Zone 32, Northern Hemisphere")
    testthat::expect_equivalent(projpar(crs_totest, "geogcs"), "WGS 84")
    testthat::expect_equivalent(projpar(crs_totest, "unit"), "Meter")
    testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984")
    testthat::expect_equivalent(projpar(crs_totest, "spheroid"), "WGS 84")
  }
)


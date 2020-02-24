context("Test projpar() / projname()")
testthat::skip_on_cran()
testthat::skip_on_travis()

testthat::test_that(
  "Test longlat", {
    crs_totest <- st_crs(4326)
    testthat::expect_true(projname(crs_totest) %in% c("WGS 84", "unknown"))
    testthat::expect_warning(
      testthat::expect_true(projpar(crs_totest, "geogcs") %in% c("WGS 84", "unknown")),
      gsub(" ", "[ \n]", "is now an alias of par = \"name\"")
    )
    testthat::expect_equivalent(projpar(crs_totest, "unit"), "degree")
    testthat::expect_error(
      testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984"),
      gsub(" ", "[ \n]", "is no longer accepted")
    )
  }
)

testthat::test_that(
  "Test UTM32", {
    crs_totest <- st_crs2("32N")
    testthat::expect_true(
      projname(crs_totest) %in% 
        c("UTM Zone 32, Northern Hemisphere", "WGS 84 / UTM zone 32N", "unknown")
    )
    testthat::expect_warning(
      testthat::expect_true(
        projpar(crs_totest, "geogcs") %in%
          c("UTM Zone 32, Northern Hemisphere", "WGS 84 / UTM zone 32N", "unknown")
      ),
      gsub(" ", "[ \n]", "is now an alias of par = \"name\"")
    )
    testthat::expect_true(projpar(crs_totest, "unit") %in% c("Meter", "metre"))
    testthat::expect_error(
      testthat::expect_equivalent(projpar(crs_totest, "datum"), "WGS_1984"),
      gsub(" ", "[ \n]", "is no longer accepted")
    )
  }
)


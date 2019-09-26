context("Test st_crs2")
# testthat::skip_on_cran()
# testthat::skip_on_travis()
# 

testthat::test_that(
    "Tests on st_crs2", {
        testthat::expect_equal(st_crs2("+init=epsg:32609")[["epsg"]], 32609)
        testthat::expect_equal(st_crs2(32609)[["proj4string"]], "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
        testthat::expect_equal(st_crs2(9)[["proj4string"]], "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
        testthat::expect_equal(st_crs2("9N")[["proj4string"]], "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
        testthat::expect_equal(st_crs2("9S")[["proj4string"]], "+proj=utm +zone=9 +south +datum=WGS84 +units=m +no_defs")        	
        testthat::expect_error(st_crs2("wrong"))
    })

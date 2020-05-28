context("Test mountpoint()")
testthat::skip_on_cran()
# testthat::skip_on_travis()

if (Sys.info()["sysname"] != "Windows") {
  testthat::test_that(
    "Mountpoint home", {
      mp_1 <- mountpoint(".")
      testthat::expect_is(mp_1, "character")
      testthat::expect_true(dir.exists(mp_1))
      testthat::expect_equal(names(attributes(mp_1)), "protocol")
    }
  )
  testthat::test_that(
    "Mountpoint package home", {
      mp_2 <- mountpoint(system.file(package = "sen2r"))
      testthat::expect_is(mp_2, "character")
      testthat::expect_true(dir.exists(mp_2))
      testthat::expect_equal(names(attributes(mp_2)), "protocol")
    }
  )
}

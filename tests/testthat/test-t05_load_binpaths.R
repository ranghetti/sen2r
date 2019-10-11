context("Load empty binaries")
# testthat::skip_on_cran()
# testthat::skip_on_travis()

settings_dir <- normalize_path("~/.sen2r", mustWork = FALSE)
if (dir.exists(settings_dir)) {
  file.rename(settings_dir, gsub("sen2r$", "sen2r~", settings_dir))
  restore_settings <- TRUE
} else {
  restore_settings <- FALSE
}
# if the test was manually runned, reload sen2r before proceeding

test_that("Load empty binpaths", {
  binpaths_0 <- load_binpaths()
  expect_is(binpaths_0, "list")
  expect_length(binpaths_0, 0)
  expect_equal(basename(attr(binpaths_0, "path")), "paths.json")
})

test_that("Try loading Sen2Cor", {
  testthat::expect_warning(
    binpaths_1 <- load_binpaths("sen2cor"),
    "Sen2Cor was not found in your system; you can install it using the function"
  )
  expect_equal(binpaths_0, binpaths_1)
})


context("Load aria2 and GDAL")
testthat::skip_on_cran()
# testthat::skip_on_travis()

test_that("Load GDAL", {
  binpaths_2 <- load_binpaths("gdal")
  expect_is(binpaths_2, "list")
  expect_length(binpaths_2, 9)
  expect_equal(binpaths_2$gdalinfo, normalize_path(Sys.which("gdalinfo")))
  expect_equal(basename(attr(binpaths_2, "path")), "paths.json")
})

test_that("Load aria2", {
  binpaths_3 <- load_binpaths("aria2")
  expect_is(binpaths_3, "list")
  expect_length(binpaths_3, 10)
  expect_equal(binpaths_3$aria2, normalize_path(Sys.which("aria2c")))
  expect_equal(basename(attr(binpaths_2, "path")), "paths.json")
})

if (restore_settings) {
  file.rename(gsub("sen2r$", "sen2r~", settings_dir), settings_dir)
}

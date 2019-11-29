context("Load binaries")
testthat::skip_on_cran()
testthat::skip_on_travis()

settings_dir <- normalize_path("~/.sen2r", mustWork = FALSE)
if (dir.exists(settings_dir)) {
  settings_dir <- strftime(
    Sys.time(), 
    normalize_path("~/.sen2r_test_%Y%m%d%H%M%S", mustWork = FALSE)
  )
  restore_settings <- TRUE
} else {
  restore_settings <- FALSE
}
# if the test was manually runned, reload sen2r before proceeding

test_that("Load empty binpaths", {
  binpaths_file <- file.path(
    if (dir.exists("~/.sen2r")) {"~/.sen2r"} else {tempdir()},
    "paths.json"
  )
  if (file.exists(binpaths_file)) {file.remove(binpaths_file)}
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
  expect_is(binpaths_1, "list")
  expect_length(binpaths_1, 0)
  expect_equal(basename(attr(binpaths_1, "path")), "paths.json")
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("Load GDAL", {
    binpaths_2 <- load_binpaths(c("gdal","python"))
    expect_is(binpaths_2, "list")
    expect_length(binpaths_2, 11)
    expect_equal(binpaths_2$gdalinfo, normalize_path(Sys.which("gdalinfo")))
    expect_equal(basename(attr(binpaths_2, "path")), "paths.json")
  })
  test_that("Load aria2", {
    binpaths_3 <- load_binpaths("aria2")
    expect_is(binpaths_3, "list")
    expect_length(binpaths_3, 12)
    expect_equal(binpaths_3$aria2, normalize_path(Sys.which("aria2c")))
    expect_equal(basename(attr(binpaths_3, "path")), "paths.json")
  })
}

if (restore_settings) {
  unlink(settings_dir, recursive = TRUE)
}

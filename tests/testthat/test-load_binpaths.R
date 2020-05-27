context("Load binaries")
testthat::skip_on_cran()
testthat::skip_on_travis()

binpaths_file <- normalize_path(file.path(
  if (dir.exists("~/.sen2r")) {"~/.sen2r"} else {tempdir()},
  "paths.json"
), mustWork = FALSE)
binpaths_file_backup <- tempfile()
if (file.exists(binpaths_file)) {
  file.copy(binpaths_file, binpaths_file_backup)
  file.remove(binpaths_file)
  restore_settings <- TRUE
} else {
  restore_settings <- FALSE
}

test_that("Load empty binpaths", {
  binpaths_0 <- load_binpaths()
  expect_is(binpaths_0, "list")
  expect_length(binpaths_0, 0)
  expect_equal(basename(attr(binpaths_0, "path")), "paths.json")
})

test_that("Try loading Sen2Cor", {
  testthat::expect_warning(
    binpaths_1 <- load_binpaths("sen2cor"),
    "Sen2Cor was not found in your system;"
  )
  expect_is(binpaths_1, "list")
  expect_length(binpaths_1, 0)
  expect_equal(basename(attr(binpaths_1, "path")), "paths.json")
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("Load GDAL", {
    binpaths_2 <- load_binpaths(c("gdal"))
    expect_is(binpaths_2, "list")
    expect_length(binpaths_2, 3)
    expect_equal(binpaths_2$gdal_calc, normalize_path(Sys.which("gdal_calc.py")))
    expect_equal(basename(attr(binpaths_2, "path")), "paths.json")
  })
  test_that("Load aria2", {
    binpaths_3 <- load_binpaths("aria2")
    expect_is(binpaths_3, "list")
    expect_length(binpaths_3, 4)
    expect_equal(binpaths_3$aria2, normalize_path(Sys.which("aria2c")))
    expect_equal(basename(attr(binpaths_3, "path")), "paths.json")
  })
}

if (restore_settings) {
  file.copy(binpaths_file_backup, binpaths_file)
}

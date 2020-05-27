context("Test abs2rel()")
# testthat::skip_on_cran()
# testthat::skip_on_travis()

# if (Sys.info()["sysname"] != "Windows") {
#   ref_path <- "/usr/lib/R/library/base"
#   in_path_1 <-"/usr/lib/R/library/datasets"
#   in_path_2 <- "/usr/lib/R/library/base/CITATION"
#   in_path_3 <- "/home/lranghetti/R/x86_64-pc-linux-gnu-library/3.6/sf"
#   in_path_4 <- "/usr/lib/R/library/sbas"
# } else {
#   ref_path <- "C:/PROGRA~1/R/R-35~1.3/library/base"
#   in_path_1 <- "C:/PROGRA~1/R/R-35~1.3/library/datasets"
#   in_path_2 <- "C:/PROGRA~1/R/R-35~1.3/library/base/CITATION"
#   in_path_3 <- "C:/Users/Public/Documents/R/win-library/3.5/sf"
#   in_path_4 <- "C:/PROGRA~1/R/R-35~1.3/library/sbas"
# }
# the reference path
ref_path <- system.file(package = "base")
# a path with a common parent with ref_path
in_path_1 <- system.file(package = "datasets")
# a path included in ref_path
in_path_2 <- file.path(ref_path, "CITATION")
# a path external to ref_path (in Linux)
in_path_3 <- system.file(package = "sf")
# an unexisting path
in_path_4 <- gsub("base","sbas",ref_path)

testthat::test_that(
  "Test a path with a common parent with ref_path", {
    test_path <- abs2rel(ref_path, in_path_1)
    testthat::expect_gt(nchar(test_path), 0)
    testthat::expect_true(grepl("^\\.\\.\\/", test_path))
    unlink(ref_path, recursive = TRUE)
    unlink(in_path_1, recursive = TRUE)
  }
)

testthat::test_that(
  "Test a path included in ref_path", {
    test_path <- abs2rel(in_path_2, ref_path, mustWork = TRUE)
    testthat::expect_gt(nchar(test_path), 0)
    testthat::expect_true(grepl("^\\.\\/", test_path))
  }
)

if (Sys.info()["sysname"] != "Windows") {
  testthat::test_that(
    "Test a path external to ref_path (in Linux)", {
      testthat::expect_warning(
        test_path <- abs2rel(normalize_path("~"), ref_path),
        regexp = gsub(
          " ", "[ \n]",
          "do not have a common parent directory"
        )
      )
      testthat::expect_gt(nchar(test_path), 0)
      testthat::expect_true(grepl("^/", test_path))
    }
  )
}

testthat::test_that(
  "Test an unexisting path", {
    testthat::expect_warning(testthat::expect_error(
      test_path <- abs2rel(in_path_4, ref_path, mustWork = TRUE)
    ))
    testthat::expect_warning(test_path <- abs2rel(in_path_4, ref_path))
  }
)

testthat::test_that(
  "Test identical paths", {
    testthat::expect_warning(
      test_path <- abs2rel(ref_path, ref_path),
      regexp = "point[ \n]to[ \n]the[ \n]same[ \n]path"
    )
  }
)

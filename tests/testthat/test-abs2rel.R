context("Test abs2rel()")

# the reference path
ref_path <- system.file(package="sen2r")
ref_path <- gsub("/inst/?$", "", ref_path)
# a path with a common parent with ref_path
in_path_1 <- system.file(package="sf")
# a path included in ref_path
in_path_2 <- system.file("R/abs2rel.R", package="sen2r")
# a path external to ref_path (in Linux)
in_path_3 <- system.file(package="base")
# an unexisting path
in_path_4 <- gsub("sen2r","r2sen",ref_path)

testthat::test_that(
  "Test a path with a common parent with ref_path", {
    test_path <- abs2rel(in_path_1, ref_path)
    testthat::expect_gt(nchar(test_path), 0)
    testthat::expect_true(grepl("^\\.\\.\\/", test_path))
  }
)

testthat::test_that(
  "Test a path included in ref_path", {
    test_path <- abs2rel(in_path_2, ref_path)
    testthat::expect_gt(nchar(test_path), 0)
    testthat::expect_true(grepl("^\\.\\/", test_path))
  }
)

testthat::test_that(
  "Test a path external to ref_path (in Linux)", {
    testthat::expect_warning(
      test_path <- abs2rel(in_path_3, ref_path),
      regexp = "have not a common parent directory"
    )
    testthat::expect_gt(nchar(test_path), 0)
    testthat::expect_true(grepl("^/", test_path))
  }
)

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
      regexp = "point to the same path"
    )
  }
)

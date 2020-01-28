# context("Test create_s2_dop()")
# testthat::skip_on_cran()
# testthat::skip_on_travis()
# 
# # NOTE: one test require a high amount of time (depending on connection speed),
# # so it is disabled by default. To perform it, replace 'test_download = FALSE' with 'TRUE'.
# test_download = TRUE
# 
# testthat::test_that(
#   "Test that a new DOP DB is not built if another exists", {
#     out_time <- system.time(create_s2_dop())
#     testthat::expect_lt(out_time["elapsed"], 10)
#   }
# )
# 
# if (test_download) {
#   
#   testthat::test_that(
#     "Test the construction of a new DOP DB", {
#       
#       newjson_path <- file.path(tempdir(), "dop.json")
#       out_time <- system.time(
#         create_s2_dop(json_path = newjson_path, force = TRUE)
#       )
#       testthat::expect_gt(out_time["elapsed"], 10)
#       defjson <- jsonlite::fromJSON(system.file("extdata/settings/doybase.json",package="sen2r"))
#       newjson <- jsonlite::fromJSON(newjson_path)
#       testthat::expect_is(newjson, "list")
#       testthat::expect_is(newjson$dop, "data.frame")
#       testthat::expect_equal(dim(newjson$dop), dim(defjson$dop))
#       testthat::expect_equal(nrow(newjson$dop), 143)
#       testthat::expect_equal(names(newjson$dop), c("orbit", "doybase"))
#       
#       testthat::expect_equal(package_version(newjson$pkg_version), packageVersion("sen2r"))
#       testthat::expect_equal(as.Date(newjson$creation_date), Sys.Date())
#       
#     }
#   )
#   
# }
context("Test abs2rel()")
testthat::skip_on_cran()
testthat::skip_on_travis()

# the reference path
ref_path <- system.file(package = "base")
# ref_path <- gsub("/inst/?$", "", ref_path)
# a path with a common parent with ref_path
in_path_1 <- system.file(package = "datasets")
# a path included in ref_path
in_path_2 <- file.path(ref_path, "CITATION")
# a path external to ref_path (in Linux)
in_path_3 <- system.file(package = "base")
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
        test_path <- abs2rel("~", ref_path),
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

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
#       defjson <- jsonlite::fromJSON(system.file("share/doybase.json",package="sen2r"))
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

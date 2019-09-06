# context("Test create_indices_db()")
# testthat::skip_on_cran()
# testthat::skip_on_travis()
# 
# example_dir <- system.file("extdata/example_files", package = "sen2r")
# dir.create(example_dir, showWarnings = FALSE)
# safe_dir <- file.path(example_dir, "safe")
# dir.create(safe_dir, showWarnings = FALSE)
# dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
# dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)
# 
# 
# testthat::test_that(
#   "Test that a new indices DB is not built if another exists", {
#     out_time <- system.time(create_indices_db())
#     testthat::expect_lt(out_time["elapsed"], 10)
#   }
# )
# 
# 
# testthat::test_that(
#   "Test the construction of a new indices DB", {
#     
#     newjson_path <- file.path(tempdir(), "indices.json")
#     out_time <- system.time(
#       create_indices_db(json_path = newjson_path, force = TRUE)
#     )
#     testthat::expect_gt(out_time["elapsed"], 10)
#     defjson <- jsonlite::fromJSON(system.file("extdata/indices.json",package="sen2r"))
#     newjson <- jsonlite::fromJSON(newjson_path)
#     testthat::expect_is(newjson, "list")
#     testthat::expect_is(newjson$indices, "data.frame")
#     testthat::expect_equal(dim(newjson$indices), dim(defjson$indices))
#     testthat::expect_equal(nrow(newjson$indices), 224)
#     testthat::expect_equal(names(newjson$indices), c(
#       "n_index", "longname", "name", "link", "s2_formula", 
#       "checked", "s2_formula_mathml", "a", "b", "c", "d"
#     ))
#     
#     testthat::expect_equal(package_version(newjson$pkg_version), packageVersion("sen2r"))
#     testthat::expect_equal(as.Date(newjson$creation_date), Sys.Date())
#     
#   }
# )

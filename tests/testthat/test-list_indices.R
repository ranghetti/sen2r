cat("\n---- Test list indices ----")
# testthat::skip_on_cran()
# testthat::skip_on_travis()

testthat::test_that(
  "Test the generation of a data.frame ", {
    df1 <- list_indices(c("name","longname"))
    testthat::expect_is(df1, "data.frame")
    testthat::expect_length(df1, 2)
  }
)

testthat::test_that(
  "Test the generation of a vector ", {
    v1 <- list_indices("s2_formula", "^MSAVI2$")
    testthat::expect_is(v1, "character")
    testthat::expect_length(v1, 1)
  }
)

testthat::test_that(
  "Test list indices to be updated ", {
    v2 <- list_indices("name", all = TRUE)
    testthat::expect_is(v2, "character")
    v2_version <- attr(v2, "pkg_version")
    sen2r_version <- packageVersion("sen2r")
    # Only first two elements are checked
    # (updating the indices list is so required only before a minor update,
    # not a patch update)
    testthat::expect_equal(v2_version$major, sen2r_version$major)
    testthat::expect_equal(v2_version$minor, sen2r_version$minor)
    # testthat::expect_equal(v2_version$patchlevel, sen2r_version$patchlevel)
  }
)

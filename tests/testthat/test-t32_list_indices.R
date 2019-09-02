context("Test list indices")
testthat::skip_on_cran()
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
    testthat::expect_equal(attr(v2, "pkg_version"), packageVersion("sen2r"))
  }
)

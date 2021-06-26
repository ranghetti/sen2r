cat("\n---- Check GCloud connection - specific functions ----")
testthat::skip_on_cran()
# testthat::skip_on_ci()

test_that("check internet connection on internal function", {
  testthat::skip_if_not(check_gcloud_connection(), "Google Cloud server is not reachable")
  expect_equal(check_gcloud_connection(), TRUE)
  expect_equal(
    httptest::without_internet(check_gcloud_connection()),
    FALSE
  )
})


testthat::test_that(
  "Check GCloud installation", {
    testthat::skip_if_not(is_gcloud_configured(), "Google account is not set")
    testthat::expect_equal(
      check_gcloud(force = TRUE),
      TRUE
    )
    testthat::expect_equal(
      check_gcloud(load_binpaths()$gsutil, force = TRUE),
      TRUE
    )
    testthat::expect_equal(
      check_gcloud(dirname(load_binpaths()$gsutil), force = TRUE),
      TRUE
    )
    testthat::expect_error(
      check_gcloud("/wrong/path", force = TRUE),
      regexp = gsub(
        " ", "[ \n]",
        "Google Cloud SDK was not found"
      )
    )
    testthat::expect_warning(
      check_warning <- check_gcloud("/wrong/path", force = TRUE, abort = FALSE),
      regexp = gsub(
        " ", "[ \n]",
        "Google Cloud SDK was not found"
      )
    )
    testthat::expect_equal(check_warning, FALSE)
  }
)

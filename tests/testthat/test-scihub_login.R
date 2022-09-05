message("\n---- Check SciHub connection - specific function ----")
testthat::skip_on_cran()
# testthat::skip_on_ci()
skip_full_tests()

test_that("check internet connection on internal function", {
  testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
  testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
  expect_equal(check_scihub_connection(), TRUE)
  expect_equal(
    httptest::without_internet(check_scihub_connection()),
    FALSE
  )
})


message("\n---- Read / write SciHub login ----")
testthat::skip_on_cran()
# testthat::skip_on_ci()

test_that("check reading / writing credentials", {
  testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
  testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
  testthat::expect_error(
    write_scihub_login("user2", "user2"),
    regexp = gsub(
      " ", "[ \n]",
      "[Tt]he provided credentials are not valid"
    )
  )
  testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
  creds <- read_scihub_login(apihub_path = tests_apihub_path)
  testthat::expect_equal(creds[1,], c(tests_apihub[1], tests_apihub[2]))
})


message("\n---- Check SciHub login ----")
testthat::skip_on_cran()
# testthat::skip_on_ci()

test_that("check_scihub_login works as expected", {
  testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
  testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
  expect_false(check_scihub_login("BastianoCoimbraDeLaCoronilla", "yAcevedo!"))
  testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
  expect_true(check_scihub_login(tests_apihub[1], tests_apihub[2]))
})

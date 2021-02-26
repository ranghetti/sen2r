context("check connection - specific function")
testthat::skip_on_cran()
testthat::skip_on_ci()
testthat::skip_if_not(check_scihub_connection(), "SciHub server is not reachable")

test_that("check internet connection on internal function", {
  expect_equal(check_scihub_connection(), TRUE)
  expect_equal(
    httptest::without_internet(check_scihub_connection()),
    FALSE
  )
})


context("read / write SciHub login")
testthat::skip_on_cran()
testthat::skip_on_ci()

test_that("check reading / writing credentials", {
  testthat::expect_error(
    write_scihub_login("user2", "user2"),
    regexp = gsub(
      " ", "[ \n]",
      "[Tt]he provided credentials are not valid"
    )
  )
  creds <- read_scihub_login(apihub_path = tests_apihub_path)
  testthat::expect_equal(creds[1,], c(tests_apihub[1], tests_apihub[2]))
})


context("check SciHub login")
testthat::skip_on_cran()
testthat::skip_on_ci()

test_that("check_scihub_login works as expected", {
  expect_false(check_scihub_login("BastianoCoimbraDeLaCoronilla", "yAcevedo!"))
  expect_true(check_scihub_login(tests_apihub[1], tests_apihub[2]))
})

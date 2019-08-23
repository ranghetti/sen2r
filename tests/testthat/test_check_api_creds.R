context("check_api_creds")

test_that("check_api_creds works as expected", {

  expect_false(check_api_creds("aaaa", "hiovdhaoivheados"))
  expect_true(check_api_creds("user", "user"))
  
})

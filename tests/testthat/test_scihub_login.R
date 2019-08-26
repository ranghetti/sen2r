context("check_scihub_login")

test_that("check_scihub_login works as expected", {

  expect_false(check_scihub_login("aaaa", "hiovdhaoivheados"))
  expect_true(check_scihub_login("user", "user"))
  
})

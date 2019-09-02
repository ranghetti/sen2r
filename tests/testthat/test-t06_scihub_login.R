context("check connection")

test_that("check internet connection on internal function", {
  expect_equal(check_scihub_connection(), TRUE)
  expect_equal(
    httptest::without_internet(check_scihub_connection()),
    FALSE
  )
})

test_that("check internet connection and fail if missing", {
  testthat::expect_error(
    httptest::without_internet(sen2r(
      gui = FALSE,
      online = TRUE,
      s2_levels = "l2a",
      step_atmcorr = "auto",
      extent = NA,
      s2tiles_selected = "32TNR",
      timewindow = as.Date("2017-07-03"),
      list_prods = "SCL",
      mask_type = NA,
      path_l1c = file.path(tempdir(), "L1C"),
      path_l2a = file.path(tempdir(), "L2A"),
      path_out = tempdir(), 
      overwrite = TRUE,
      thumbnails = FALSE
    )), 
    regexp = "[Ii]nternet connection or SciHub may be down"
  )
})


context("read / write SciHub login")

test_that("check reading / writing credentials", {
  write_scihub_login("user", "user")
  testthat::expect_error(
    write_scihub_login("user2", "user2"),
    regexp = "[Tt]he provided credentials are not valid"
  )
  write_scihub_login("user", "user", apihub_path = apitmp <- tempfile())
  creds <- read_scihub_login(apihub_path = apitmp)
  testthat::expect_equal(creds[1,], c("user", "user"))
})


context("check SciHub login")

test_that("check_scihub_login works as expected", {
  
  expect_false(check_scihub_login("aaaa", "hiovdhaoivheados"))
  expect_true(check_scihub_login("user", "user"))
  
})

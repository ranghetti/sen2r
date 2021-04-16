message("\n---- check connection from main sen2r() function ----")
testthat::skip_on_cran()
testthat::skip_on_ci() # FIXME restore

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
      path_l1c = tempfile(pattern = "L1C"),
      path_l2a = tempfile(pattern = "L2A"),
      path_out = tempfile(pattern = "out"), 
      overwrite = TRUE,
      thumbnails = FALSE,
      apihub = tests_apihub_path
    )), 
    regexp = gsub(
      " ", "[ \n]",
      "[Ii]nternet connection or SciHub may be down"
    )
  )
})

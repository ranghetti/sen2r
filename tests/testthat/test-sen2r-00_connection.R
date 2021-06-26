message("\n---- check connection from main sen2r() function ----")
testthat::skip_on_cran()
# testthat::skip_on_ci() # TODO try to remove

test_that("check internet connection and fail if missing", {
  testthat::expect_error(
    httptest::without_internet(sen2r(
      gui = FALSE,
      online = TRUE,
      server = "gcloud",
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
      "[Ii]nternet connection may be down"
    )
  )
})



message("\n---- Test downloading from main sen2r() function ----")
testthat::skip_on_cran()


testthat::test_that(
  "Tests producing SAFE archives (SciHub download)", {
   
    testthat::skip_on_ci() # TODO try to remove
    testthat::skip_if_not(is_gcloud_configured(), "Google account is not set")
    testthat::skip_if_not(check_gcloud_connection(), "Google Cloud server is not reachable")
    testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
    testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
    testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
    
    exp_outpaths_1 <- file.path(
      safe_dir, 
      c("S2B_MSIL1C_20200801T100559_N0209_R022_T32TNR_20200801T130136.SAFE",
        "S2B_MSIL1C_20200801T100559_N0209_R022_T32TNS_20200801T130136.SAFE")
    )
    out1 <- sen2r(
      gui = FALSE,
      online = TRUE,
      server = c("scihub","gcloud"),
      preprocess = FALSE,
      s2_levels = "l1c",
      extent = NA,
      timewindow = as.Date("2020-08-01"),
      s2tiles_selected = c("32TNS","32TNR"),
      path_l1c = safe_dir,
      apihub = tests_apihub_path
    )
    expect_true(all(file.exists(exp_outpaths_1)))
    
  }
)


testthat::test_that(
  "Tests producing SAFE archives (GCloud download)", {
    
    # testthat::skip_on_ci() # TODO try to remove
    testthat::skip_if_not(is_gcloud_configured(), "Google account is not set")
    testthat::skip_if_not(check_gcloud_connection(), "Google Cloud server is not reachable")
    
    exp_outpaths_2 <- file.path(
      safe_dir, 
      c("S2B_MSIL1C_20200801T100559_N0209_R022_T32TNR_20200801T130136.SAFE",
        "S2B_MSIL1C_20200801T100559_N0209_R022_T32TNS_20200801T130136.SAFE",
        "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNR_20200801T135302.SAFE",
        "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNS_20200801T135302.SAFE")
    )
    out2 <- sen2r(
      gui = FALSE,
      online = TRUE,
      server = "gcloud",
      preprocess = FALSE,
      s2_levels = c("l1c","l2a"),
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = NA,
      timewindow = as.Date("2020-08-01"),
      s2tiles_selected = c("32TNS","32TNR"),
      path_l1c = safe_dir,
      path_l2a = safe_dir
    )
    expect_true(all(file.exists(exp_outpaths_2)))
    
  }
)

message("\n---- Test ignorelist ----")
testthat::skip_on_cran()
# testthat::skip_on_ci() # FIXME restore
testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on safelist read/write", {
    
    outdir_17 <- tempfile(pattern = "out_test17_")
    dir.create(dirname(outdir_17), showWarnings = FALSE)
    out_17 <- sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/barbellino.geojson", package = "sen2r"),
      extent_name = "Barbellino",
      timewindow = as.Date("2020-08-01"),
      list_indices = c("NDVI","MSAVI2"),
      list_rgb = c("RGB432B"),
      mask_type = "land",
      max_mask = 10,
      path_l2a = safe_dir,
      path_out = outdir_17,
      thumbnails = FALSE,
      apihub = tests_apihub_path
    )
    
    ignorelist_path <- file.path(outdir_17, ".ignorelist.txt")
    expect_true(file.exists(ignorelist_path))
    ignorelist <- parseTOML(ignorelist_path)
    expect_length(ignorelist, 4)
    expect_equal(
      sort(names(ignorelist)), 
      c("dates_cloudcovered", "mask_type", "max_mask", "names_missing")
    )
    expect_equal(ignorelist$max_mask, 10)
    expect_equal(ignorelist$mask_type, "land")
    expect_equal(ignorelist$dates_cloudcovered, as.Date("2020-08-01"))
    
    # Relaunch the same processing to test that dates are skipped
    # (and to test read_ignorepath)
    out_17_2 <- sen2r(attr(out_17, "procpath"))
    expect_equal(length(out_17_2), 0)
    
  }
)

context("Test ignorelist")
testthat::skip_on_cran()
# testthat::skip_on_travis() # because required SAFE do not exists

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)

testthat::test_that(
  "Tests on safelist read/write", {
    
    outdir_17 <- tempfile(pattern = "out_test17_")
    dir.create(dirname(outdir_17), showWarnings = FALSE)
    # exp_outpath_5 <- file.path(outdir_5, "BOA", "S2A2A_20190723_022_Scalve_BOA_10.tif")
    # unlink(exp_outpath_17)
    out_17 <- sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/barbellino.geojson", package = "sen2r"),
      extent_name = "Barbellino",
      timewindow = as.Date("2019-07-23"),
      list_indices = c("NDVI","MSAVI2"),
      list_rgb = c("RGB432B"),
      mask_type = "land",
      max_mask = 10,
      path_l2a = safe_dir,
      path_out = outdir_17,
      thumbnails = FALSE
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
    expect_equal(ignorelist$dates_cloudcovered, as.Date("2019-07-23"))
    
    # Relaunch the same processing to test that dates are skipped
    # (and to test read_ignorepath)
    out_17_2 <- sen2r(attr(out_17, "procpath"))
    expect_equal(length(out_17_2), 0)
    
  }
)

context("Test mask - main function")
testthat::skip_on_cran()
# testthat::skip_on_travis() # because required SAFE do not exists

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)

testthat::test_that(
  "Tests on base mask on BOA", {
    
    outdir_5 <- tempfile(pattern = "out_test5_")
    dir.create(dirname(outdir_5), showWarnings = FALSE)
    exp_outpath_5 <- file.path(outdir_5, "BOA", "S2A2A_20190723_022_Scalve_BOA_10.dat")
    unlink(exp_outpath_5)
    unlink(gsub("dat$", "hdr", exp_outpath_5))
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2019-07-23"),
      list_prods = c("BOA", "WVP", "AOT", "CLD", "SNW"),
      mask_type = "cloud_high_proba",
      outformat = "ENVI",
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = outdir_5,
      thumbnails = FALSE
    )
    expect_true(file.exists(exp_outpath_5))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_5, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=1911, "y"=1479))
    testthat::expect_equal(exp_meta_r$res, c("x"=10, "y"=10))
    testthat::expect_equal(exp_meta_r$nbands, 11)
    testthat::expect_equal(
      as.numeric(exp_meta_r$bbox), 
      c(578590, 5086740, 597700, 5101530)
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "ENVI")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_5)
    testthat::expect_true(round(mean(exp_stars[[1]][,,1], na.rm=TRUE)) %in% c(939, 994))
    testthat::expect_equal(sum(is.na(exp_stars[[1]][,,1])), 1816353)
    rm(exp_stars)
    
  }
)

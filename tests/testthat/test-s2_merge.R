context("Test s2_merge and translate when stitching 2 tiles with no clipping")
testthat::skip_on_cran()
testthat::skip_on_travis()

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)


testthat::test_that(
  "Tests on s2_list - Single tile, single orbit, no pos", {
    
    outdir_1 <- file.path(tempdir(), "out_test1")
    dir.create(dirname(outdir_1), showWarnings = FALSE)
    exp_outpath_1 <- file.path(outdir_1, "SCL", "S2A2A_20170703_022__SCL_10.tif")
    unlink(exp_outpath_1)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = NA,
      timewindow = as.Date("2017-07-03"),
      list_prods = "SCL",
      mask_type = NA,
      clip_on_extent = FALSE,
      path_l2a = file.path(safe_dir, "L2A"),
      path_out = outdir_1, 
      overwrite = TRUE,
      thumbnails = FALSE
    )
    expect_true(file.exists(exp_outpath_1))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_1, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=5490, "y"=10491))
    testthat::expect_equal(exp_meta_r$res, c("x"=20, "y"=20))
    testthat::expect_equal(
      exp_meta_r$bbox, 
      sf::st_bbox(
        c("xmin" = 499980, "ymin" = 4990200, "xmax" = 609780, "ymax" = 5200020), 
        crs = sf::st_crs(32632)
      )
    )
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_1)
    testthat::expect_equal(exp_meta_s$type, "merged")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2017-07-03"))
    testthat::expect_equal(exp_meta_s$prod_type, "SCL")
    testthat::expect_equal(exp_meta_s$extent_name, "")
    
    # test on raster values
    r <- raster::raster(exp_outpath_1)
    testthat::expect_equal(raster::cellStats(r, "mean"), 4.729521, tolerance = 1e-06)
    
  }
)

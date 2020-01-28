context("Test s2_merge and translate when stitching 2 tiles with no clipping")
testthat::skip_on_cran() # because using runtime GDAL
testthat::skip_on_travis() # because required SAFE do not exists

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)


testthat::test_that(
  "Tests on merge all found tiles in offline mode", {
    
    outdir_1 <- file.path(tempdir(), "out_test1")
    dir.create(dirname(outdir_1), showWarnings = FALSE)
    exp_outpath_1 <- file.path(outdir_1, "SCL", "S2A2A_20170703_022__SCL_10.tif")
    unlink(exp_outpath_1)
    out1 <- sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = NA,
      timewindow = as.Date("2017-07-03"),
      list_prods = "SCL",
      mask_type = NA,
      clip_on_extent = FALSE,
      path_l2a = safe_dir,
      path_out = outdir_1, 
      overwrite = TRUE,
      thumbnails = FALSE
    )
    expect_true(file.exists(exp_outpath_1))
    
    # check sen2r output format
    testthat::expect_is(out1, "character")
    testthat::expect_equivalent(out1, exp_outpath_1)
    testthat::expect_equal(
      names(attributes(out1)), 
      c("procpath", "cloudcovered", "missing", "status")
    )
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_1, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=5490, "y"=10491))
    testthat::expect_equal(exp_meta_r$res, c("x"=20, "y"=20))
    testthat::expect_equal(
      as.numeric(exp_meta_r$bbox), 
      c(499980, 4990200, 609780, 5200020)
    )
    testthat::expect_equal(exp_meta_r$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_1)
    testthat::expect_equal(exp_meta_s$type, "merged")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2017-07-03"))
    testthat::expect_equal(exp_meta_s$prod_type, "SCL")
    testthat::expect_equal(exp_meta_s$extent_name, "")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_1)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 4.729521, tolerance = 1e-03)
    rm(exp_stars)
    
  }
)


testthat::test_that(
  "Expect error with no extent and tiles specified in online mode", {
    
    outdir_1c <- file.path(tempdir(), "out_test1c")
    dir.create(dirname(outdir_1c), showWarnings = FALSE)
    testthat::expect_error(
      sen2r(
        gui = FALSE,
        online = TRUE,
        step_atmcorr = "l2a", # to avoid checks on Sen2Cor
        extent = NA,
        timewindow = as.Date("2017-07-03"),
        list_prods = "BOA",
        clip_on_extent = FALSE,
        path_l2a = safe_dir,
        path_out = outdir_1c, 
        overwrite = TRUE,
        thumbnails = FALSE
      ),
      regexp = gsub(
        " ", "[ \n]",
        "[Aa]t least one parameter among 'extent' and 's2tiles_selected' must be provided"
      )
    )
    
  }
)

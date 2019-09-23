context("Test mask - main function")
testthat::skip_on_cran()
testthat::skip_on_travis()

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)

testthat::test_that(
  "Tests on base mask on BOA", {
    
    outdir_5 <- file.path(tempdir(), "out_test5")
    dir.create(dirname(outdir_5), showWarnings = FALSE)
    exp_outpath_5 <- file.path(outdir_5, "BOA", "S2A2A_20170703_022_Scalve_BOA_10.tif")
    unlink(exp_outpath_5)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "BOA",
      mask_type = "cloud_high_proba",
      path_l2a = file.path(safe_dir, "L2A"),
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
      exp_meta_r$bbox, 
      sf::st_bbox(
        c("xmin" = 578590, "ymin" = 5086740, "xmax" = 597700, "ymax" = 5101530),
        crs = sf::st_crs(32632)
      )
    )
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff")
    
    # test on raster values
    r <- raster::raster(exp_outpath_5)
    testthat::expect_equal(raster::cellStats(r, "mean"), 884.4695, tolerance = 1e-3)
    testthat::expect_equal(raster::cellStats(r, "countNA"), 1986322)

  }
)


context("Test mask - s2_mask()")
# testthat::skip_on_cran()
# testthat::skip_on_travis()
ref_dir <- system.file("extdata/example_files/out_ref", package = "sen2r")

testthat::test_that(
  "Tests on custom mask on TOA with smoothing and buffering", {
    
    outdir_6 <- file.path(tempdir(), "out_test6")
    dir.create(dirname(outdir_6), showWarnings = FALSE)
    exp_outpath_6 <- file.path(outdir_6, "TOA", "S2A1C_20170703_022_Barbellino_TOA_10.tif")
    unlink(exp_outpath_6)
    s2_mask(
      infiles = file.path(ref_dir, "S2A1C_20170703_022_Barbellino_TOA_10.tif"),
      maskfiles = file.path(ref_dir, "S2A2A_20170703_022_Barbellino_SCL_10.tif"),
      mask_type = "clear_sky",
      outdir = outdir_6,
      subdirs = TRUE
    )
    expect_true(file.exists(exp_outpath_6))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_6, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=24, "y"=42))
    testthat::expect_equal(exp_meta_r$res, c("x"=10, "y"=10))
    testthat::expect_equal(exp_meta_r$nbands, 12)
    testthat::expect_equal(
      exp_meta_r$bbox, 
      sf::st_bbox(
        c("xmin" = 580560, "ymin" = 5101700, "xmax" = 580800, "ymax" = 5102120),
        crs = sf::st_crs(32632)
      )
    )
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff")
    
    # test on raster values
    r <- raster::raster(exp_outpath_6)
    testthat::expect_equal(raster::cellStats(r, "mean"), 1433.497, tolerance = 1e-3)
    testthat::expect_equal(raster::cellStats(r, "countNA"), 388)
    
  }
)


testthat::test_that(
  "Tests on clip BOA on extent - do not create if cloudiness > threshold", {
    
    outdir_7 <- file.path(tempdir(), "out_test7")
    dir.create(dirname(outdir_7), showWarnings = FALSE)
    exp_outpath_7 <- file.path(outdir_7, "BOA", "S2A2A_20170703_022_Barbellino_BOA_10.tif")
    unlink(exp_outpath_7)
    s2_mask(
      infiles = file.path(ref_dir, "S2A2A_20170703_022_Barbellino_BOA_10.tif"),
      maskfiles = file.path(ref_dir, "S2A2A_20170703_022_Barbellino_SCL_10.tif"),
      mask_type = "clear_sky",
      max_mask = 20,
      outdir = outdir_7,
      subdirs = TRUE
    )
    testthat::expect_false(file.exists(exp_outpath_7))

    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_7, format = "list")
    testthat::expect_equal(names(exp_meta_r[[1]]), c("path", "valid"))
    testthat::expect_equal(exp_meta_r[[1]]$valid, FALSE)
    
  }
)

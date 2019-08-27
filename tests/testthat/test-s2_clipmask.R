### Test 5: clip and mask
context("Test clip and mask")
testthat::skip_on_cran()
testthat::skip_on_travis()
example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
out_dir <- file.path(tempdir(), "out_test2")
dir.create(out_dir, showWarnings = FALSE)
testthat::test_that(
    "Tests on clip BOA on extent", {
        sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "l2a",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2017-07-03"),
            list_prods = "BOA",
            list_rgb = c("RGB432B"),
            mask_type = "cloud_medium_proba",
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir
        )
        exp_fileout <- file.path(out_dir, "BOA/S2A2A_20170703_022_Scalve_BOA_10.vrt")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::brick(exp_fileout)
        testthat::expect_equal(raster::nlayers(r), 11)
        testthat::expect_equal(dim(r), c(1479,1911,11))
        testthat::expect_equal(raster::cellStats(r[[3]], "mean"), 798.848, tolerance = 1e-06)
        unlink(exp_fileout)
    })

testthat::test_that(
    "Tests on clip BOA on extent, custom mask with smooth/buffer", {
        sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "l2a",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2017-07-03"),
            list_prods = "BOA",
            mask_type = "scl_0_8_9_11",
            mask_smooth = 20, 
            mask_buffer = 10,
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir
        )
        exp_fileout <- file.path(out_dir, "BOA/S2A2A_20170703_022_Scalve_BOA_10.vrt")
        testthat::expect_true(file.exists(exp_fileout))
        # r <- raster::brick(exp_fileout)
        # testthat::expect_equal(raster::nlayers(r), 11)
        # testthat::expect_equal(dim(r), c(1479,1911,11))
        # testthat::expect_equal(raster::cellStats(r[[3]], "mean"), 798.848, tolerance = 1e-06)
        unlink(exp_fileout)
    })


testthat::test_that(
    "Tests on clip BOA on extent - do not create if cloudiness > threshold", {
        testthat::expect_warning(sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "l2a",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2017-07-03"),
            list_prods = "BOA",
            list_rgb = c("RGB432B"),
            mask_type = "cloud_medium_proba",
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir, 
            max_mask = 30
        ))
        exp_fileout <- file.path(out_dir, "BOA/S2A2A_20170703_022_Scalve_BOA_10.vrt")
        testthat::expect_false(file.exists(exp_fileout))
        unlink(exp_fileout)
        exp_fileout <- file.path(out_dir, "RGB432B/S2A2A_20170703_022_Scalve_RGB432B_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        unlink(exp_fileout)
    })

testthat::test_that(
    "Tests on clip TOA on extent, and compute RGBs", {
        system.time(sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l1c",
            step_atmcorr = "no",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2016-10-06"),
            list_prods = "TOA",
            list_rgb = c("RGB432T", "RGB843T"),
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir, 
            thumbnails = FALSE
        ))
        exp_fileout <- file.path(out_dir, "TOA/S2A1C_20161006_022_Scalve_TOA_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        testthat::expect_equal(dim(r), c(1479,1911,1))
        testthat::expect_equal(raster::cellStats(r, "mean"), 3245.048, tolerance = 1e-06)
        unlink(exp_fileout)
        exp_fileout <- file.path(out_dir, "RGB843T/S2A1C_20161006_022_Scalve_RGB843T_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        testthat::expect_equal(dim(r), c(1479,1911,1))
        testthat::expect_equal(raster::cellStats(r, "mean"), 126.6718, tolerance = 1e-06)
        unlink(exp_fileout)
    })

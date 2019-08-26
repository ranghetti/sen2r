### Test 5: clip and mask
context("Test clip and mask")
testthat::skip_on_cran()
testthat::skip_on_travis()
example_dir <- system.file("extdata","testdata/example_files", 
                           package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)
dir.create(file.path(example_dir, "out"), showWarnings = FALSE)
out_dir <- file.path(example_dir, "out", "out_test2")
testthat::test_that(
    "Tests on clip BOA on extent", {
        system.time(sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "l2a",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2017-07-03"),
            list_prods = "BOA",
            mask_type = "cloud_medium_proba",
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir
        ))
    })
exp_fileout <- file.path(out_dir, "BOA/S2A2A_20170703_022_Scalve_BOA_10.vrt")
testthat::expect_true(file.exists(exp_fileout))
r <- raster::brick(exp_fileout)
testthat::expect_equal(raster::nlayers(r), 11)
testthat::expect_equal(dim(r), c(1479,1911,11))
testthat::expect_equal(raster::cellStats(r[[3]], "mean"), 798.848, tolerance = 1e-06)

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)
dir.create(file.path(example_dir, "out"), showWarnings = FALSE)

context("Test s2_merge and translate when stitching 2 tiles with no clipping")
testthat::skip_on_cran()
testthat::skip_on_travis()
testthat::test_that(
    "Tests on s2_list - Single tile, single orbit, no pos", {
        out_dir <- file.path(tempdir(), "out_test1")
        dir.create(dirname(out_dir), showWarnings = FALSE)
        exp_fileout <- file.path(out_dir, "SCL", "S2A2A_20170703_022_sen2r_SCL_10.tif")
        unlink(exp_fileout)
        sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "auto",
            extent = NA,
            timewindow = as.Date("2017-07-03"),
            list_prods = "SCL",
            mask_type = NA,
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_out = out_dir, 
            overwrite = TRUE,
            thumbnails = FALSE
        )
        expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        expect_equal(raster::cellStats(r, "mean"), 4.729521, tolerance = 1e-06)
    })

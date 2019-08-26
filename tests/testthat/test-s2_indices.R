context("Test compute spectral indices - on clip")
testthat::skip_on_cran()
testthat::skip_on_travis()
example_dir <- system.file("extdata/example_files", 
                           package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)
dir.create(file.path(example_dir, "out"), showWarnings = FALSE)
out_dir <- file.path(tempdir(), "out_test3")
dir.create(out_dir, showWarnings = FALSE)
testthat::test_that(
    "Tests on indices computation, on BOA, with clip ", {
        system.time(sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l2a",
            step_atmcorr = "l2a",
            # list_prods = "BOA",
            extent = file.path(example_dir, "scalve.kml"),
            extent_name = "Scalve",
            extent_as_mask = TRUE,
            timewindow = as.Date("2017-07-03"),
            list_indices = c("NDVI","MSAVI2"),
            mask_type = NA,
            path_out = out_dir,
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_indices = out_dir,
            parallel = FALSE, 
            thumbnails = FALSE
        ))
        exp_fileout <- file.path(out_dir, "MSAVI2/S2A2A_20170703_022_Scalve_MSAVI2_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        testthat::expect_equal(dim(r), c(1479,1911,1))
        testthat::expect_equal(raster::cellStats(r, "mean"), 2845.063, tolerance = 1e-06)
        
        exp_fileout <- file.path(out_dir, "NDVI/S2A2A_20170703_022_Scalve_NDVI_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        testthat::expect_equal(dim(r), c(1479,1911,1))
        testthat::expect_equal(raster::cellStats(r, "mean"), 4166.105, tolerance = 1e-06)
    })

testthat::test_that(
    "Tests on indices computation, on TOA, no clip ", {
        system.time(sen2r(
            gui = FALSE,
            online = FALSE,
            s2_levels = "l1c",
            step_atmcorr = "no",
            extent_as_mask = TRUE,
            timewindow = as.Date("2016-10-06"),
            s2tiles_selected = "32TNR",
            list_indices = c("NDVI"),
            mask_type = NA,
            path_out = out_dir,
            path_l1c = file.path(safe_dir, "L1C"),
            path_l2a = file.path(safe_dir, "L2A"),
            path_indices = out_dir,
            parallel = FALSE, 
            thumbnails = FALSE, 
            index_source = "TOA", 
            overwrite = FALSE
        ))
        
        exp_fileout <- file.path(out_dir, "NDVI/S2A1C_20161006_022_sen2r_NDVI_10.tif")
        testthat::expect_true(file.exists(exp_fileout))
        r <- raster::raster(exp_fileout)
        testthat::expect_equal(dim(r), c(10980,10980,1))
        testthat::expect_equal(raster::cellStats(r, "mean"), 2126.659, tolerance = 1e-06)
    })

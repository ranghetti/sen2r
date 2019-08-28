context("Test compute spectral indices")
testthat::skip_on_cran()
testthat::skip_on_travis()

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)


testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    outdir_11 <- file.path(tempdir(), "out_test11")
    dir.create(dirname(outdir_11), showWarnings = FALSE)
    exp_outpath_11 <- file.path(
      outdir_11, c("NDVI","MSAVI2"), 
      c("S2A2A_20170703_022_Scalve_NDVI_10.tif","S2A2A_20170703_022_Scalve_MSAVI2_10.tif")
    )
    unlink(exp_outpath_11)
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_indices = c("NDVI","MSAVI2"),
      mask_type = NA,
      path_out = outdir_11,
      path_l2a = file.path(safe_dir, "L2A"),
      parallel = FALSE
    )
    testthat::expect_true(all(file.exists(exp_outpath_11)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_11) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=rep(1911,2), "size.y"=rep(1479))
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=rep(10,2), "res.y"=rep(10,2))
    )
    testthat::expect_equal(exp_meta_r$nbands, c(1,1))
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, rep("Int16",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_11)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2017-07-03"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("NDVI", "MSAVI2"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    r <- raster::stack(exp_outpath_11)
    testthat::expect_equal(raster::cellStats(r[[1]], "mean"), 4166.105, tolerance = 1e-06)
    testthat::expect_equal(raster::cellStats(r[[2]], "mean"), 2845.063, tolerance = 1e-06)
    testthat::expect_equal(raster::cellStats(r[[1]], "countNA"), 1417518)
    testthat::expect_equal(raster::cellStats(r[[2]], "countNA"), 1417518)
    
    # test thumbnails
    exp_outpath_t_11 <- file.path(
      dirname(exp_outpath_11), "thumbnails", 
      gsub("tif$", "jpg", basename(exp_outpath_11))
    )
    expect_true(all(file.exists(
      exp_outpath_t_11,
      paste0(exp_outpath_t_11, ".aux.xml")
    )))
    exp_meta_r_t <- raster_metadata(exp_outpath_t_11) # default format: data.table
    testthat::expect_equal(
      exp_meta_r_t[,c("size.x", "size.y")], 
      exp_meta_r[,c("size.x", "size.y")] * 1024 / exp_meta_r$size.x, 
      tolerance = 1e-3
    )
    testthat::expect_equal(
      exp_meta_r_t[,c("res.x", "res.y")], 
      exp_meta_r[,c("res.x", "res.y")] / 1024 * exp_meta_r$size.x, # dim. > 1024: resize to 1024
      tolerance = 1e-3
    )
    testthat::expect_equal(exp_meta_r_t$nbands, c(3,3))
    testthat::expect_equal(
      exp_meta_r_t[,c("xmin", "xmax", "ymin", "ymax")], 
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")]
    )
    testthat::expect_equal(exp_meta_r_t$proj, exp_meta_r$proj)
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",2))
    
  }
)


testthat::test_that(
  "Tests on indices computation, on required TOA, type Float, with clip and mask ", {
    
    exp_outpath_12 <- file.path(
      outdir_11, c("TOA","EVI"), 
      c("S2A1C_20170703_022_Scalve_TOA_10.tif","S2A1C_20170703_022_Scalve_EVI_10.tif")
    )
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "TOA",
      list_indices = "EVI",
      index_source = "TOA",
      index_datatype = "Float32",
      mask_type = "cloud_and_shadow",
      path_out = outdir_11,
      path_l1c = file.path(safe_dir, "L1C"),
      path_l2a = file.path(safe_dir, "L2A"),
      parallel = FALSE,
      overwrite = TRUE
    )
    testthat::expect_true(all(file.exists(exp_outpath_12)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_12) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=rep(1911,2), "size.y"=rep(1479))
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=rep(10,2), "res.y"=rep(10,2))
    )
    testthat::expect_equal(exp_meta_r$nbands, c(12,1))
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, c("UInt16","Float32"))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_12)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2017-07-03"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("TOA", "EVI"))
    testthat::expect_equal(exp_meta_s$level, c("1C", "1C"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    r1 <- raster::stack(exp_outpath_12[1])
    r2 <- raster::raster(exp_outpath_12[2])
    testthat::expect_equal(raster::cellStats(r1[[3]], "mean"), 974.1352, tolerance = 1e-06)
    testthat::expect_equal(raster::cellStats(r2, "mean"), 0.5943244, tolerance = 1e-06)
    testthat::expect_equal(raster::cellStats(r1[[3]], "countNA"), 2086841)
    testthat::expect_equal(raster::cellStats(r2, "countNA"), 2086841)
    
  }
)


# do test on existing TOA

# do test using s2_calcindices function
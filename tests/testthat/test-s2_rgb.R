context("Test compute RGB images - main s2_rgb()")

testthat::test_that(
  "Tests on function s2_rgb()", {
    
    outdir_17 <- tempfile(pattern = "out_test17_")
    dir.create(outdir_17, showWarnings = FALSE)
    exp_outpath_17 <- file.path(
      outdir_17, c("RGBb84B","RGB954B"), 
      paste0("S2A2A_20190723_022_Barbellino_",c("RGBb84B","RGB954B"),"_10.tif")
    )
    unlink(exp_outpath_17)
    s2_rgb(
      infiles = file.path(ref_dir, "S2A2A_20190723_022_Barbellino_BOA_10.tif"),
      rgb_bands = list(c(11,8,4),c(9,5,4)),
      scaleRange = list(c(0,7500), matrix(c(rep(0,3),8500,6000,4000),ncol=2)),
      outdir = outdir_17,
      compress = 90,
      parallel = FALSE
    )
    testthat::expect_true(all(file.exists(exp_outpath_17)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_17) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=rep(24,2), "size.y"=42)
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=rep(10,2), "res.y"=10)
    )
    testthat::expect_equal(exp_meta_r$nbands, c(3,3))
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 580560, "xmax" = 580800, "ymin" = 5101700, "ymax" = 5102120) 
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj[2]), 32632)
    testthat::expect_equal(exp_meta_r$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_17)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 56.30718, tolerance = 1e-3)
    testthat::expect_equal(mean(exp_stars[[2]], na.rm=TRUE), 56.71991, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 169, tolerance = 1e-3) #zeros
    rm(exp_stars)
    
  }
)

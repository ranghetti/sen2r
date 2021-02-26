message("\n---- Test compute spectral indices - s2_calcindices() ----")

outdir_12 <- tempfile(pattern = "out_test12_")
testthat::test_that(
  "Tests on indices computation, on required TOA, type Float, with clip and mask ", {
    
    dir.create(outdir_12, showWarnings = FALSE)
    exp_outpath_12 <- file.path(outdir_12, "S2A1C_20190723_022_Barbellino_EVI_10.tif")
    unlink(exp_outpath_12)
    s2_calcindices(
      infiles = file.path(ref_dir, "S2A1C_20190723_022_Barbellino_TOA_10.tif"),
      indices = "EVI",
      source = "TOA",
      outdir = outdir_12,
      dataType = "Float32"
    )
    testthat::expect_true(all(file.exists(exp_outpath_12)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(
      exp_outpath_12, 
      c("size", "res", "bbox", "proj", "type", "outformat")
    )
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=24, "size.y"=42)
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=10, "res.y"=10)
    )
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 580560, "xmax" = 580800, "ymin" = 5101700, "ymax" = 5102120) 
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "Float32")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_12)
    testthat::expect_equal(exp_meta_s$type, "clipped")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2019-07-23"))
    testthat::expect_equal(exp_meta_s$prod_type, "EVI")
    testthat::expect_equal(exp_meta_s$level, "1C")
    testthat::expect_equal(exp_meta_s$extent_name, "Barbellino")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_12)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 0.3392678, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0)
    rm(exp_stars)
    
  }
)


testthat::test_that(
  "Tests on indices computation, on existing TOA, type Byte", {
    
    testthat::expect_true(dir.exists(outdir_12))
    exp_outpath_13 <- file.path(
      outdir_12,
      c("S2A1C_20190723_022_Barbellino_EVI_10.tif", "S2A1C_20190723_022_Barbellino_NDRE_10.tif")
    )
    unlink(exp_outpath_13[2])
    s2_calcindices(
      infiles = file.path(ref_dir, "S2A1C_20190723_022_Barbellino_TOA_10.tif"),
      indices = c("EVI", "NDRE"),
      source = "TOA",
      outdir = outdir_12,
      dataType = "Byte",
      subdirs = FALSE
    )
    testthat::expect_true(all(file.exists(exp_outpath_13)))
    
    # test that existing file were not re-created
    exp_info_r <- file.info(exp_outpath_13)
    testthat::expect_true(exp_info_r[1,"ctime"] < exp_info_r[2,"ctime"])
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(
      exp_outpath_13[2], 
      c("size", "res", "bbox", "proj", "type", "outformat")
    )
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=24, "size.y"=42)
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=10, "res.y"=10)
    )
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 580560, "xmax" = 580800, "ymin" = 5101700, "ymax" = 5102120) 
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "Byte")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_13[2])
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 126.8006, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0)
    rm(exp_stars)
    
  }
)


message("\n---- Test compute spectral indices - s2_calcindices(), GDAL method ----")
testthat::skip_on_cran() # because using runtime GDAL
# testthat::skip_on_travis()
testthat::test_that(
  "Tests on indices computation with function s2_calcindices(), gdal method", {
    
    outdir_15 <- tempfile(pattern = "out_test15_")
    dir.create(outdir_15, showWarnings = FALSE)
    exp_outpath_15 <- file.path(outdir_15, "OSAVI", "S2A1C_20190723_022_Barbellino_OSAVI_10.tif")
    unlink(exp_outpath_15)
    s2_calcindices(
      infiles = file.path(ref_dir, "S2A1C_20190723_022_Barbellino_TOA_10.tif"),
      indices = "OSAVI", 
      outdir = dirname(dirname(exp_outpath_15)),
      subdirs = TRUE,
      source = "TOA",
      dataType = "Int32",
      proc_mode = "gdal_calc",
      scaleFactor = 1E6
    )
    testthat::expect_true(file.exists(exp_outpath_15))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(
      exp_outpath_15, 
      c("size", "res", "bbox", "proj", "type", "outformat"),
      format = "list"
    )[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=24, "y"=42))
    testthat::expect_equal(exp_meta_r$res, c("x"=10, "y"=10))
    testthat::expect_equal(
      as.numeric(exp_meta_r$bbox), 
      c(580560, 5101700, 580800, 5102120)
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "Int32")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_15)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 282040.7, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0)
    rm(exp_stars)
    
  }
)

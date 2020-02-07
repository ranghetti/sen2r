context("Test compute spectral indices - main function")
testthat::skip_on_cran() # because using runtime GDAL
testthat::skip_on_travis() # because required SAFE do not exists

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)

testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    outdir_11 <- tempfile(pattern = "out_test11_")
    dir.create(dirname(outdir_11), showWarnings = FALSE)
    exp_outpath_11 <- file.path(
      outdir_11, c("NDVI","MSAVI2"), 
      c("S2A2A_20190723_022_Scalve_NDVI_10.tif","S2A2A_20190723_022_Scalve_MSAVI2_10.tif")
    )
    unlink(exp_outpath_11)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2019-07-23"),
      list_indices = c("NDVI","MSAVI2"),
      mask_type = NA,
      path_out = outdir_11,
      path_l2a = safe_dir,
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
    testthat::expect_equal(st_crs2(exp_meta_r$proj[2])$epsg, 32632)
    testthat::expect_equal(exp_meta_r$type, rep("Int16",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_11)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2019-07-23"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("NDVI", "MSAVI2"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_11)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 4836.26, tolerance = 1e-3)
    testthat::expect_equal(mean(exp_stars[[2]], na.rm=TRUE), 3216.844, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 1417518)
    testthat::expect_equal(sum(is.na(exp_stars[[2]])), 1417518)
    rm(exp_stars)
    
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
    testthat::expect_equal(
      lapply(exp_meta_r_t$proj, function(x){st_crs2(x)$epsg}),
      lapply(exp_meta_r$proj, function(x){st_crs2(x)$epsg})
    )
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",2))
    
  }
)


context("Test compute spectral indices - s2_calcindices()")
# testthat::skip_on_cran()
# testthat::skip_on_travis()
ref_dir <- system.file("extdata/out", package = "sen2r")

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
    testthat::expect_equal(st_crs2(exp_meta_r$proj)$epsg, 32632)
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
    testthat::expect_equal(st_crs2(exp_meta_r$proj)$epsg, 32632)
    testthat::expect_equal(exp_meta_r$type, "Byte")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_13[2])
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 126.8006, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0)
    rm(exp_stars)
    
  }
)


context("Test compute spectral indices - s2_calcindices(), GDAL method")
testthat::skip_on_cran() # because using runtime GDAL
# testthat::skip_on_travis()
testthat::test_that(
  "Tests on indices computation with function s2_calcidices(), gdal method", {
    
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
    testthat::expect_equal(exp_meta_r$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_r$type, "Int32")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_15)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 282040.7, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0)
    rm(exp_stars)
    
  }
)

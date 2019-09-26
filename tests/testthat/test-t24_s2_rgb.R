context("Test compute RGB images - main function")
testthat::skip_on_cran()
testthat::skip_on_travis()

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
ref_dir <- system.file("extdata/example_files/out_ref", package = "sen2r")

testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    outdir_16 <- file.path(tempdir(), "out_test16")
    dir.create(dirname(outdir_16), showWarnings = FALSE)
    exp_outpath_16 <- file.path(
      outdir_16, c("BOA","RGB432T","RGBb84B","RGB843B"), 
      paste0("S2A",c("2A","1C","2A","2A"),"_20170703_022_Scalve_",c("BOA","RGB432T","RGBb84B","RGB843B"),"_10.tif")
    )
    unlink(exp_outpath_16)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "BOA",
      list_rgb = c("RGB432T","RGBb84B","RGB843B"),
      mask_type = NA,
      path_out = outdir_16,
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      parallel = FALSE
    )
    testthat::expect_true(all(file.exists(exp_outpath_16)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_16) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=rep(1911,4), "size.y"=1479)
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=rep(10,4), "res.y"=10)
    )
    testthat::expect_equal(exp_meta_r$nbands, c(11,3,3,3))
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, c("UInt16",rep("Byte",3)))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",4)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_16)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",4))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2017-07-03"),4))
    testthat::expect_equal(exp_meta_s$prod_type, c("BOA","RGB432T","RGBb84B","RGB843B"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",4))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_16[2])
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 162.4022, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 4218143, tolerance = 1e-3)
    rm(exp_stars)
    
    # test thumbnails
    exp_outpath_t_16 <- file.path(
      dirname(exp_outpath_16), "thumbnails", 
      gsub("tif$", "jpg", basename(exp_outpath_16))
    )
    expect_true(all(file.exists(
      exp_outpath_t_16,
      paste0(exp_outpath_t_16, ".aux.xml")
    )))
    exp_meta_r_t <- raster_metadata(exp_outpath_t_16) # default format: data.table
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
    testthat::expect_equal(exp_meta_r_t$nbands, c(3,3,3,3))
    testthat::expect_equal(
      exp_meta_r_t[,c("xmin", "xmax", "ymin", "ymax")], 
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")]
    )
    testthat::expect_equal(exp_meta_r_t$proj, exp_meta_r$proj)
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",4))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",4))
    
  }
)


context("Test compute RGB images - main s2_rgb()")
testthat::skip_on_cran() # since it uses runtime GDAL
testthat::skip_on_travis()

testthat::test_that(
  "Tests on function s2_rgb()", {
    
    outdir_17 <- file.path(tempdir(), "out_test17")
    dir.create(outdir_17, showWarnings = FALSE)
    exp_outpath_17 <- file.path(
      outdir_17, c("RGBb84B","RGB954B"), 
      paste0("S2A2A_20170703_022_Barbellino_",c("RGBb84B","RGB954B"),"_10.tif")
    )
    unlink(exp_outpath_17)
    s2_rgb(
      infiles = file.path(ref_dir, "S2A2A_20170703_022_Barbellino_BOA_10.tif"),
      rgb_bands = list(c(11,8,4),c(9,5,4)),
      scaleRange = list(c(0,7500), matrix(c(rep(0,3),8500,6000,4000),ncol=2)),
      outdir = outdir_17,
      compress = 50,
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
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_17)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 66.7342, tolerance = 1e-3)
    testthat::expect_equal(mean(exp_stars[[2]], na.rm=TRUE), 71.76323, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 18, tolerance = 1e-3) #zeros
    rm(exp_stars)

  }
)

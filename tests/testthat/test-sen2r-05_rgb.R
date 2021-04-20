message("\n---- Test compute RGB images - main function ----")
testthat::skip_on_cran()
# testthat::skip_on_ci()
testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    outdir_16 <- tempfile(pattern = "out_test16_")
    dir.create(dirname(outdir_16), showWarnings = FALSE)
    exp_outpath_16 <- file.path(
      outdir_16, c("BOA","RGB432T","RGBb84B","RGB843B"), 
      paste0("S2B",c("2A","1C","2A","2A"),"_20200801_022_Scalve_",c("BOA","RGB432T","RGBb84B","RGB843B"),"_10.tif")
    )
    unlink(exp_outpath_16)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2020-08-01"),
      list_prods = "BOA",
      list_rgb = c("RGB432T","RGBb84B","RGB843B"),
      mask_type = NA,
      path_out = outdir_16,
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      parallel = FALSE,
      apihub = tests_apihub_path
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
    expect_equal_crs(st_crs2(exp_meta_r$proj[2]), 32632)
    testthat::expect_equal(exp_meta_r$type, c("UInt16",rep("Byte",3)))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",4)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_16)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",4))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2020-08-01"),4))
    testthat::expect_equal(exp_meta_s$prod_type, c("BOA","RGB432T","RGBb84B","RGB843B"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",4))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_16[2])
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 80.17252, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 4218002, tolerance = 1e-3)
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
    lapply(seq_len(nrow(exp_meta_r_t)), function(i) {
      expect_equal_crs(st_crs2(exp_meta_r_t$proj[i]), st_crs2(exp_meta_r$proj[i]))
    })
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",4))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",4))
    
  }
)

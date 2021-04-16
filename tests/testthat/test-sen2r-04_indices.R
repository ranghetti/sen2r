message("\n---- Test compute spectral indices - main function ----")
testthat::skip_on_cran()
# testthat::skip_on_ci() # FIXME restore
testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    outdir_11 <- tempfile(pattern = "out_test11_")
    dir.create(dirname(outdir_11), showWarnings = FALSE)
    exp_outpath_11 <- file.path(
      outdir_11, c("NDVI","MSAVI2"), 
      c("S2B2A_20200801_022_Scalve_NDVI_10.tif","S2B2A_20200801_022_Scalve_MSAVI2_10.tif")
    )
    unlink(exp_outpath_11)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2020-08-01"),
      list_indices = c("NDVI","MSAVI2"),
      mask_type = NA,
      path_out = outdir_11,
      path_l2a = safe_dir,
      parallel = FALSE,
      apihub = tests_apihub_path
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
    expect_equal_crs(st_crs2(exp_meta_r$proj[2]), 32632)
    testthat::expect_equal(exp_meta_r$type, rep("Int16",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_11)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2020-08-01"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("NDVI", "MSAVI2"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_11)
    testthat::expect_equal(round(mean(exp_stars[[1]], na.rm=TRUE)), 7364)
    testthat::expect_equal(round(mean(exp_stars[[2]], na.rm=TRUE)), 4877)
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
    lapply(seq_len(nrow(exp_meta_r_t)), function(i) {
      expect_equal_crs(st_crs2(exp_meta_r_t$proj[i]), st_crs2(exp_meta_r$proj[i]))
    })
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",2))
    
  }
)

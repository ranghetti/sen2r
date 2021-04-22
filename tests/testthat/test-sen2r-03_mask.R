message("\n---- Test mask - main function ----")
testthat::skip_on_cran()
# testthat::skip_on_ci() # FIXME restore

testthat::test_that(
  "Tests on base mask on BOA", {
    
    testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
    testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
    
    outdir_5 <- tempfile(pattern = "out_test5_")
    dir.create(dirname(outdir_5), showWarnings = FALSE)
    exp_outpath_5 <- file.path(
      outdir_5, c("BOA", "WVP", "AOT", "SCL", "CLD"),
      c("S2B2A_20200801_022_Scalve_BOA_10.dat",
        "S2B2A_20200801_022_Scalve_WVP_10.dat",
        "S2B2A_20200801_022_Scalve_AOT_10.dat",
        "S2B2A_20200801_022_Scalve_SCL_10.dat",
        "S2B2A_20200801_022_Scalve_CLD_10.dat")
    )
    unlink(exp_outpath_5)
    unlink(gsub("dat$", "hdr", exp_outpath_5))
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2020-08-01"),
      list_prods = c("BOA", "SCL", "WVP", "AOT", "CLD"),
      mask_type = "cloud_high_proba",
      outformat = "ENVI",
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = outdir_5,
      thumbnails = FALSE,
      apihub = tests_apihub_path
    )
    expect_true(all(file.exists(exp_outpath_5)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_5)
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=c(1911,1911,1911,956,956), "size.y"=c(1479,1479,1479,740,740))
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=c(10,10,10,20,20), "res.y"=c(10,10,10,20,20))
    )
    testthat::expect_equal(exp_meta_r$nbands, c(11,1,1,1,1))
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table(
        "xmin" = c(578590,578590,578590,578580,578580), 
        "xmax" = c(597700,597700,597700,597700,597700),
        "ymin" = c(5086740,5086740,5086740,5086740,5086740),
        "ymax" = c(5101530,5101530,5101530,5101540,5101540)
      )
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj[1]), 32632)
    testthat::expect_equal(
      exp_meta_r$type,
      c("UInt16","UInt16","UInt16","Byte","Byte")
    )
    testthat::expect_equal(exp_meta_r$outformat, rep("ENVI",5))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_5[1])
    testthat::expect_true(round(mean(exp_stars[[1]][,,1], na.rm=TRUE)) %in% c(337))
    testthat::expect_true(sum(is.na(exp_stars[[1]][,,1])) %in% c(1430023))
    rm(exp_stars)
    exp_stars <- stars::read_stars(exp_outpath_5[2])
    testthat::expect_true(round(mean(exp_stars[[1]], na.rm=TRUE)) %in% c(1392))
    testthat::expect_true(sum(is.na(exp_stars[[1]])) %in% c(1430023))
    rm(exp_stars)
    exp_stars <- stars::read_stars(exp_outpath_5[3])
    testthat::expect_true(round(mean(exp_stars[[1]], na.rm=TRUE)) %in% c(117))
    testthat::expect_true(sum(is.na(exp_stars[[1]])) %in% c(1417518))
    rm(exp_stars)
    exp_stars <- stars::read_stars(exp_outpath_5[5])
    testthat::expect_true(round(mean(exp_stars[[1]], na.rm=TRUE)) %in% c(1))
    testthat::expect_true(sum(is.na(exp_stars[[1]])) %in% c(374223,355218))
    rm(exp_stars)
    
  }
)

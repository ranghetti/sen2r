cat("\n---- Test warping (clip, reproject, resize) ----")
testthat::skip_on_cran()
# testthat::skip_on_ci() # TODO try to remove

# Required SAFE
s2_l1c_list <- c(
  "S2B_MSIL1C_20200801T100559_N0209_R022_T32TNR_20200801T130136.SAFE",
  "S2B_MSIL1C_20200801T100559_N0209_R022_T32TNS_20200801T130136.SAFE"
)
s2_l2a_list <- c(
  "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNR_20200801T135302.SAFE",
  "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNS_20200801T135302.SAFE"
)

outdir_2 <- tempfile(pattern = "out_test2_")
exp_outpath_2 <- file.path(
  outdir_2, c("BOA", "WVP"),
  c("S2B2A_20200801_022_Scalve_BOA_10.tif", "S2B2A_20200801_022_Scalve_WVP_10.tif")
)
testthat::test_that(
  "Tests on clip and mask BOA on extent", {
    
    # Check sample inputs
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l2a_list[1],
      "GRANULE/L2A_T32TNR_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNR_20200801T100559_B08_10m.jp2"
    )))
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l2a_list[2],
      "GRANULE/L2A_T32TNS_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNS_20200801T100559_B08_10m.jp2"
    )))
    
    dir.create(dirname(outdir_2), showWarnings = FALSE)
    unlink(exp_outpath_2)
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2020-08-01"),
      list_prods = c("BOA","WVP"),
      mask_type = NA,
      path_l2a = safe_dir,
      path_out = outdir_2
    )
    expect_true(all(file.exists(exp_outpath_2)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_2, format = "data.table")
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res.x", "res.y", "size.x", "size.y", "nbands", 
      "xmin", "ymin", "xmax", "ymax", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r[,c("size.x", "size.y")], data.table("size.x"=rep(1911,2), "size.y"=rep(1479,2)))
    testthat::expect_equal(exp_meta_r[,c("res.x", "res.y")], data.table("res.x"=rep(10,2), "res.y"=rep(10,2)))
    testthat::expect_equal(exp_meta_r$nbands, c(11,1))
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = rep(578590,2), "xmax" = rep(597700,2), "ymin" = rep(5086740,2), "ymax" = rep(5101530,2)) 
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj[1]), 32632)
    testthat::expect_equal(exp_meta_r$type, c("UInt16","UInt16"))
    testthat::expect_equal(exp_meta_r$outformat, c("GTiff","GTiff")) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_2)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2020-08-01"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("BOA","WVP"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_2[1])
    testthat::expect_true(round(mean(exp_stars[[1]][,,3], na.rm=TRUE)) %in% c(734))
    testthat::expect_true(sum(is.na(exp_stars[[1]][,,3])) %in% c(0))                                 # FIXMEEEEEEEEEEEE controlla la questione dei footprint sui prodotti online da gcloud
    rm(exp_stars)
    
    # test thumbnails
    exp_outpath_t_2 <- file.path(
      dirname(exp_outpath_2), "thumbnails", 
      gsub("tif$", "jpg", basename(exp_outpath_2))
    )
    expect_true(all(file.exists(
      exp_outpath_t_2,
      paste0(exp_outpath_t_2, ".aux.xml")
    )))
    exp_meta_r_t <- raster_metadata(exp_outpath_t_2) # default format: data.table
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
      data.table(exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")])
    )
    expect_equal_crs(st_crs2(exp_meta_r_t$proj[1]), st_crs2(exp_meta_r$proj[1]))
    testthat::expect_equal(exp_meta_r_t$type, c("Byte","Byte"))
    testthat::expect_equal(exp_meta_r_t$outformat, c("JPEG","JPEG"))
    
  }
)


outdir_3 <- tempfile(pattern = "out_test3_")
exp_outpath_3 <- file.path(outdir_3, "S2B1C_20200801_022_Scalve_TOA_20.dat")
testthat::test_that(
  "Tests on clip TOA on extent, reproject and resize and save as ENVI", {
    
    # Check sample inputs
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l1c_list[1],
      "GRANULE/L1C_T32TNR_A017780_20200801T101400/IMG_DATA",
      "T32TNR_20200801T100559_B08.jp2"
    )))
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l1c_list[2],
      "GRANULE/L1C_T32TNS_A017780_20200801T101400/IMG_DATA",
      "T32TNS_20200801T100559_B08.jp2"
    )))
    
    dir.create(dirname(outdir_3), showWarnings = FALSE)
    testthat::expect_warning(
      sen2r(
        gui = FALSE,
        online = FALSE,
        step_atmcorr = "l2a", # to avoid checks on Sen2Cor
        extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
        extent_name = "Scalve",
        extent_as_mask = FALSE,
        timewindow = as.Date("2020-08-01"),
        list_prods = "TOA",
        mask_type = NA,
        proj = 32633,
        res = c(25, 25), res_s2 = NA,
        resampling = "average",
        outformat = "ENVI",
        path_l1c = safe_dir,
        path_out = outdir_3,
        path_subdirs = FALSE,
        overwrite = TRUE
      ),
      regexp = gsub(
        " ", "[ \n]",
        "[Bb]oth native and custom resolution were provided"
      )
    )
    expect_true(all(file.exists(c(
      exp_outpath_3,
      gsub("dat$", "hdr", exp_outpath_3),
      paste0(exp_outpath_3,".aux.xml")
    ))))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_3, format = "data.frame")
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res.x", "res.y", "size.x", "size.y", "nbands", 
      "xmin", "ymin", "xmax", "ymax", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r[,c("size.x", "size.y")], data.frame("size.x"=776, "size.y"=584))
    testthat::expect_equal(exp_meta_r[,c("res.x", "res.y")], data.frame("res.x"=25, "res.y"=25))
    testthat::expect_equal(exp_meta_r$nbands, 12)
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.frame("xmin" = 113900, "xmax" = 133300, "ymin" = 5097850, "ymax" = 5112450)
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32633)
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "ENVI")
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_3)
    testthat::expect_equal(exp_meta_s$type, "clipped")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2020-08-01"))
    testthat::expect_equal(exp_meta_s$prod_type, "TOA")
    testthat::expect_equal(exp_meta_s$extent_name, "Scalve")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_3)
    testthat::expect_equal(round(mean(exp_stars[[1]][,,3], na.rm=TRUE)), 885)
    testthat::expect_equal(sum(is.na(exp_stars[[1]][,,3])), 0)
    rm(exp_stars)
    
    # test thumbnails
    exp_outpath_t_3 <- file.path(
      dirname(exp_outpath_3), "thumbnails", 
      gsub("dat$", "jpg", basename(exp_outpath_3))
    )
    expect_true(all(file.exists(
      exp_outpath_t_3,
      paste0(exp_outpath_t_3, ".aux.xml")
    )))
    exp_meta_r_t <- raster_metadata(exp_outpath_t_3, format = "data.frame")
    testthat::expect_equal(
      exp_meta_r_t[,c("size.x", "size.y")], # size < 1024: keep original size
      exp_meta_r[,c("size.x", "size.y")]
    )
    testthat::expect_equal(
      exp_meta_r_t[,c("res.x", "res.y")], 
      exp_meta_r[,c("res.x", "res.y")]
    )
    testthat::expect_equal(exp_meta_r_t$nbands, 3)
    testthat::expect_equal(
      exp_meta_r_t[,c("xmin", "xmax", "ymin", "ymax")], 
      data.frame(exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")])
    )
    expect_equal_crs(st_crs2(exp_meta_r_t$proj), st_crs2(exp_meta_r$proj))
    testthat::expect_equal(exp_meta_r_t$type, "Byte")
    testthat::expect_equal(exp_meta_r_t$outformat, "JPEG")
    
  }
)


outdir_4 <- tempfile(pattern = "out_test4_")
exp_outpath_4 <- file.path(
  outdir_4, c("SCL", "CLD", "SNW"),
  c("S2B2A_20200801_022_Scalve_SCL_10.vrt", 
    "S2B2A_20200801_022_Scalve_CLD_10.vrt", 
    "S2B2A_20200801_022_Scalve_SNW_10.vrt")
)
testthat::test_that(
  "Tests on clip SCL on extent, reproject with a reference raster and save as VRT", {
    
    # Check sample inputs
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l2a_list[1],
      "GRANULE/L2A_T32TNR_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNR_20200801T100559_B08_10m.jp2"
    )))
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, s2_l2a_list[2],
      "GRANULE/L2A_T32TNS_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNS_20200801T100559_B08_10m.jp2"
    )))
    
    testthat::skip_if_not(dir.exists(outdir_3))
    testthat::skip_if_not(file.exists(exp_outpath_3))
    dir.create(dirname(outdir_4), showWarnings = FALSE)
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = FALSE,
      timewindow = as.Date("2020-08-01"),
      list_prods = c("SCL","CLD","SNW"),
      mask_type = NA,
      reference_path = exp_outpath_3,
      resampling_scl = "mode",
      outformat = "VRT",
      path_l2a = safe_dir,
      path_out = outdir_4,
      tmpdir = outdir_4, rmtmp = FALSE,
      overwrite = TRUE
    )
    expect_true(all(file.exists(exp_outpath_4)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_4, format = "list")[[1]]
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res", "size", "nbands", "bbox", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r$size, c("x"=776, "y"=584))
    testthat::expect_equal(exp_meta_r$res, c("x"=25, "y"=25))
    testthat::expect_equal(exp_meta_r$nbands, 1)
    testthat::expect_equal(
      as.numeric(exp_meta_r$bbox), 
      c(113909, 5097856, 133284, 5112431),
      tolerance = 1e-3
    )
    expect_equal_crs(exp_meta_r$proj, 32633)
    testthat::expect_equal(exp_meta_r$type, "Byte")
    testthat::expect_equal(exp_meta_r$outformat, "VRT")
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_4)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",3))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2020-08-01"),3))
    testthat::expect_equal(exp_meta_s$prod_type, c("SCL","CLD","SNW"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",3))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_4)
    testthat::expect_equal(max(exp_stars[[1]], na.rm=TRUE), 11, tolerance = 1e-03)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0, tolerance = 1e-03)
    testthat::expect_lte(max(exp_stars[[2]], na.rm=TRUE), 100)
    testthat::expect_gte(max(exp_stars[[2]], na.rm=TRUE), 0)
    testthat::expect_equal(sum(is.na(exp_stars[[2]])), 0, tolerance = 1e-03)
    testthat::expect_lte(max(exp_stars[[3]], na.rm=TRUE), 100)
    testthat::expect_gte(max(exp_stars[[3]], na.rm=TRUE), 0)
    testthat::expect_equal(sum(is.na(exp_stars[[2]])), 0, tolerance = 1e-03)
    rm(exp_stars)
    
    # test thumbnails
    exp_outpath_t_4 <- file.path(
      dirname(exp_outpath_4), "thumbnails", 
      c(gsub("vrt$", "png", basename(exp_outpath_4[1])),
        gsub("vrt$", "jpg", basename(exp_outpath_4[2:3])))
    )
    expect_true(all(file.exists(
      exp_outpath_t_4,
      paste0(exp_outpath_t_4, ".aux.xml")
    )))
    exp_meta_r_t <- raster_metadata(exp_outpath_t_4, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r_t$size, exp_meta_r$size)
    testthat::expect_equal(exp_meta_r_t$res, exp_meta_r$res)
    testthat::expect_equal(exp_meta_r_t$nbands, 3)
    testthat::expect_equal(as.numeric(exp_meta_r_t$bbox), as.numeric(exp_meta_r$bbox))
    expect_equal_crs(st_crs2(exp_meta_r_t$proj), st_crs2(exp_meta_r$proj))
    testthat::expect_equal(exp_meta_r_t$type, "Byte")
    testthat::expect_equal(exp_meta_r_t$outformat, "PNG")
    
  }
)

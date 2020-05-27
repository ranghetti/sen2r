context("Test warping (clip, reproject, resize)")
testthat::skip_on_cran() # because using runtime GDAL
testthat::skip_on_travis() # because required SAFE do not exists

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)


outdir_2 <- tempfile(pattern = "out_test2_")
exp_outpath_2 <- file.path(outdir_2, "BOA", "S2A2A_20190723_022_Scalve_BOA_10.tif")
testthat::test_that(
  "Tests on clip and mask BOA on extent", {
    
    dir.create(dirname(outdir_2), showWarnings = FALSE)
    unlink(exp_outpath_2)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2019-07-23"),
      list_prods = "BOA",
      mask_type = NA,
      path_l2a = safe_dir,
      path_out = outdir_2
    )
    expect_true(file.exists(exp_outpath_2))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_2, format = "data.table")
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res.x", "res.y", "size.x", "size.y", "nbands", 
      "xmin", "ymin", "xmax", "ymax", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r[,c("size.x", "size.y")], data.table("size.x"=1911, "size.y"=1479))
    testthat::expect_equal(exp_meta_r[,c("res.x", "res.y")], data.table("res.x"=10, "res.y"=10))
    testthat::expect_equal(exp_meta_r$nbands, 11)
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_2)
    testthat::expect_equal(exp_meta_s$type, "clipped")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2019-07-23"))
    testthat::expect_equal(exp_meta_s$prod_type, "BOA")
    testthat::expect_equal(exp_meta_s$extent_name, "Scalve")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_2)
    testthat::expect_equal(mean(exp_stars[[1]][,,3], na.rm=TRUE), 2651.254, tolerance = 1e-03)
    testthat::expect_equal(sum(is.na(exp_stars[[1]][,,3])), 1417518, tolerance = 1e-03)
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
    testthat::expect_equal(exp_meta_r_t$nbands, 3)
    testthat::expect_equal(
      exp_meta_r_t[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table(exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")])
    )
    expect_equal_crs(st_crs2(exp_meta_r_t$proj), st_crs2(exp_meta_r$proj))
    testthat::expect_equal(exp_meta_r_t$type, "Byte")
    testthat::expect_equal(exp_meta_r_t$outformat, "JPEG")
    
  }
)


outdir_3 <- tempfile(pattern = "out_test3_")
exp_outpath_3 <- file.path(outdir_3, "S2A1C_20190723_022_Scalve_TOA_20.dat")
testthat::test_that(
  "Tests on clip TOA on extent, reproject and resize and save as ENVI", {
    
    dir.create(dirname(outdir_3), showWarnings = FALSE)
    testthat::expect_warning(
      sen2r(
        gui = FALSE,
        online = FALSE,
        step_atmcorr = "l2a", # to avoid checks on Sen2Cor
        extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
        extent_name = "Scalve",
        extent_as_mask = FALSE,
        timewindow = as.Date("2019-07-23"),
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
    testthat::expect_equal(exp_meta_r[,c("size.x", "size.y")], data.frame("size.x"=775, "size.y"=583))
    testthat::expect_equal(exp_meta_r[,c("res.x", "res.y")], data.frame("res.x"=25, "res.y"=25))
    testthat::expect_equal(exp_meta_r$nbands, 12)
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.frame("xmin" = 113909, "xmax" = 133284, "ymin" = 5097856, "ymax" = 5112431),
      tolerance = 1e-3
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32633)
    testthat::expect_equal(exp_meta_r$type, "UInt16")
    testthat::expect_equal(exp_meta_r$outformat, "ENVI")
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_3)
    testthat::expect_equal(exp_meta_s$type, "clipped")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2019-07-23"))
    testthat::expect_equal(exp_meta_s$prod_type, "TOA")
    testthat::expect_equal(exp_meta_s$extent_name, "Scalve")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_3)
    testthat::expect_equal(mean(exp_stars[[1]][,,3], na.rm=TRUE), 2127.312, tolerance = 1e-03)
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
exp_outpath_4 <- file.path(outdir_4, "SCL/S2A2A_20190723_022_Scalve_SCL_10.vrt")
testthat::test_that(
  "Tests on clip SCL on extent, reproject with a reference raster and save as VRT", {
    
    testthat::expect_true(dir.exists(outdir_3))
    testthat::expect_true(file.exists(exp_outpath_3))
    dir.create(dirname(outdir_4), showWarnings = FALSE)
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/scalve.kml", package = "sen2r"),
      extent_name = "Scalve",
      extent_as_mask = FALSE,
      timewindow = as.Date("2019-07-23"),
      list_prods = "SCL",
      mask_type = NA,
      reference_path = exp_outpath_3,
      resampling_scl = "mode",
      outformat = "VRT",
      path_l2a = safe_dir,
      path_out = outdir_4,
      tmpdir = outdir_4, rmtmp = FALSE,
      overwrite = TRUE
    )
    expect_true(file.exists(exp_outpath_4))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_4, format = "list")[[1]]
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res", "size", "nbands", "bbox", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r$size, c("x"=775, "y"=583))
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
    testthat::expect_equal(exp_meta_s$type, "clipped")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2019-07-23"))
    testthat::expect_equal(exp_meta_s$prod_type, "SCL")
    testthat::expect_equal(exp_meta_s$extent_name, "Scalve")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_4)
    testthat::expect_equal(max(exp_stars[[1]], na.rm=TRUE), 11, tolerance = 1e-03)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 0, tolerance = 1e-03)
    rm(exp_stars)
    
    # test thumbnails
    exp_outpath_t_4 <- file.path(
      dirname(exp_outpath_4), "thumbnails", 
      gsub("vrt$", "png", basename(exp_outpath_4))
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

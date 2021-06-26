message("\n---- Test gdalwarp_grid() ----")

ex_sel <- system.file(
  "extdata/out/S2A2A_20190723_022_Barbellino_RGB432B_10.tif",
  package = "sen2r"
)
ex_ref <- system.file(
  "extdata/out/S2A2A_20190723_022_Barbellino_SCL_10.tif",
  package = "sen2r"
)
testthat::test_that(
  "Test on reshaping with gdalwarp_grid()", {
    
    exp_outpath_4b <- tempfile(fileext = "_BOA_out.tif")
    testthat::expect_true(all(file.exists(ex_sel,ex_ref)))
    
    unlink(exp_outpath_4b)
    sen2r:::gdalwarp_grid(srcfiles = ex_sel, dstfiles = exp_outpath_4b, ref = ex_ref)
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_4b, format = "data.frame")
    testthat::expect_equal(names(exp_meta_r), c(
      "path", "valid", "res.x", "res.y", "size.x", "size.y", "nbands", 
      "xmin", "ymin", "xmax", "ymax", "proj", "unit", "outformat", "type"
    ))
    testthat::expect_equal(exp_meta_r[,c("size.x", "size.y")], data.frame("size.x"=12, "size.y"=21))
    testthat::expect_equal(exp_meta_r[,c("res.x", "res.y")], data.frame("res.x"=20, "res.y"=20))
    testthat::expect_equal(exp_meta_r$nbands, 3)
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.frame("xmin" = 580560, "xmax" = 580800, "ymin" = 5101700, "ymax" = 5102120)
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$type, "Byte")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff")
    
    # expect error on sen2r metadata with unstandard name
    exp_meta_s <- testthat::expect_error(
      sen2r_getElements(exp_outpath_4b),
      regexp = "not[ \n]recognised"
    )
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_4b)
    testthat::expect_equal(mean(exp_stars[[1]][,,3], na.rm=TRUE), 77.17063, tolerance = 1e-03)
    testthat::expect_equal(sum(is.na(exp_stars[[1]][,,3])), 0, tolerance = 1e-03)
    rm(exp_stars)
    
  }
)

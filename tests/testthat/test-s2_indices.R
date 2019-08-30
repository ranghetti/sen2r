context("Test create_indices_db()")

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)


testthat::test_that(
  "Test that a new indices DB is not built if another exists", {
    out_time <- system.time(create_indices_db())
    testthat::expect_lt(out_time["elapsed"], 10)
  }
)


testthat::test_that(
  "Test the construction of a new indices DB", {
    
    newjson_path <- file.path(tempdir(), "indices.json")
    out_time <- system.time(
      create_indices_db(json_path = newjson_path, force = TRUE)
    )
    testthat::expect_gt(out_time["elapsed"], 10)
    defjson <- jsonlite::fromJSON(system.file("extdata/indices.json",package="sen2r"))
    newjson <- jsonlite::fromJSON(newjson_path)
    testthat::expect_is(newjson, "list")
    testthat::expect_is(newjson$indices, "data.frame")
    testthat::expect_equal(dim(newjson$indices), dim(defjson$indices))
    testthat::expect_equal(nrow(newjson$indices), 224)
    testthat::expect_equal(names(newjson$indices), c(
      "n_index", "longname", "name", "link", "s2_formula", 
      "checked", "s2_formula_mathml", "a", "b", "c", "d"
    ))
    
    testthat::expect_equal(package_version(newjson$pkg_version), packageVersion("sen2r"))
    testthat::expect_equal(as.Date(newjson$creation_date), Sys.Date())
    
  }
)


context("Test compute spectral indices")
testthat::skip_on_cran()
testthat::skip_on_travis()

outdir_11 <- file.path(tempdir(), "out_test11")
dir.create(dirname(outdir_11), showWarnings = FALSE)
testthat::test_that(
  "Tests on indices computation, on unrequired BOA, with clip ", {
    
    exp_outpath_11 <- file.path(
      outdir_11, c("NDVI","MSAVI2"), 
      c("S2A2A_20170703_022_Scalve_NDVI_10.tif","S2A2A_20170703_022_Scalve_MSAVI2_10.tif")
    )
    unlink(exp_outpath_11)
    sen2r(
      gui = FALSE,
      online = TRUE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_indices = c("NDVI","MSAVI2"),
      mask_type = NA,
      path_out = outdir_11,
      path_l2a = file.path(safe_dir, "L2A"),
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
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, rep("Int16",2))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_11)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2017-07-03"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("NDVI", "MSAVI2"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_11)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 4093.080, tolerance = 1e-3)
    testthat::expect_equal(mean(exp_stars[[2]], na.rm=TRUE), 2757.542, tolerance = 1e-3)
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
    testthat::expect_equal(exp_meta_r_t$proj, exp_meta_r$proj)
    testthat::expect_equal(exp_meta_r_t$type, rep("Byte",2))
    testthat::expect_equal(exp_meta_r_t$outformat, rep("JPEG",2))
    
  }
)


testthat::test_that(
  "Tests on indices computation, on required TOA, type Float, with clip and mask ", {
    
    testthat::expect_true(dir.exists(outdir_11))
    exp_outpath_12 <- file.path(
      outdir_11, c("TOA","EVI"), 
      c("S2A1C_20170703_022_Scalve_TOA_10.tif","S2A1C_20170703_022_Scalve_EVI_10.tif")
    )
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "TOA",
      list_indices = "EVI",
      index_source = "TOA",
      index_datatype = "Float32",
      mask_type = "cloud_and_shadow",
      path_out = outdir_11,
      path_l1c = file.path(safe_dir, "L1C"),
      path_l2a = file.path(safe_dir, "L2A"),
      parallel = FALSE,
      overwrite = TRUE
    )
    testthat::expect_true(all(file.exists(exp_outpath_12)))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_12) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=rep(1911,2), "size.y"=rep(1479))
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=rep(10,2), "res.y"=rep(10,2))
    )
    testthat::expect_equal(exp_meta_r$nbands, c(12,1))
    testthat::expect_equal(
      exp_meta_r[1,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj[2]), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, c("UInt16","Float32"))
    testthat::expect_equal(exp_meta_r$outformat, rep("GTiff",2)) # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_12)
    testthat::expect_equal(exp_meta_s$type, rep("clipped",2))
    testthat::expect_equal(exp_meta_s$sensing_date, rep(as.Date("2017-07-03"),2))
    testthat::expect_equal(exp_meta_s$prod_type, c("TOA", "EVI"))
    testthat::expect_equal(exp_meta_s$level, c("1C", "1C"))
    testthat::expect_equal(exp_meta_s$extent_name, rep("Scalve",2))
    
    # test on raster values
    r1 <- raster::stack(exp_outpath_12[1])
    r2 <- raster::raster(exp_outpath_12[2])
    # test on raster values
    exp_stars1 <- stars::read_stars(exp_outpath_12[1])
    exp_stars2 <- stars::read_stars(exp_outpath_12[2])
    testthat::expect_equal(mean(exp_stars1[[1]][,,3], na.rm=TRUE), 974.1352, tolerance = 1e-3)
    testthat::expect_equal(mean(exp_stars2[[1]], na.rm=TRUE), 0.5943244, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars1[[1]])), 2086841*12, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars2[[1]])), 2086841, tolerance = 1e-3)
    rm(exp_stars1)
    rm(exp_stars2)
    
  }
)


testthat::test_that(
  "Tests on indices computation, on existing TOA, type Byte", {
    
    testthat::expect_true(dir.exists(outdir_11))
    exp_outpath_13 <- file.path(
      outdir_11, c("TOA","NDRE"), 
      c("S2A1C_20170703_022_Scalve_TOA_10.tif","S2A1C_20170703_022_Scalve_NDRE_10.tif")
    )
    unlink(exp_outpath_13[2])
    sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "TOA",
      list_indices = "NDRE",
      index_source = "TOA",
      index_datatype = "Byte",
      mask_type = "cloud_and_shadow",
      path_out = outdir_11,
      path_l1c = file.path(safe_dir, "L1C"),
      path_l2a = file.path(safe_dir, "L2A"),
      parallel = FALSE,
      overwrite = FALSE
    )
    testthat::expect_true(all(file.exists(exp_outpath_13)))
    
    # test that existing file were not re-created
    exp_info_r <- file.info(exp_outpath_13)
    testthat::expect_true(exp_info_r[1,"ctime"] < exp_info_r[2,"ctime"])

    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_13[2]) # default format: data.table
    testthat::expect_equal(
      exp_meta_r[,c("size.x", "size.y")], 
      data.table("size.x"=1911, "size.y"=1479)
    )
    testthat::expect_equal(
      exp_meta_r[,c("res.x", "res.y")], 
      data.table("res.x"=10, "res.y"=10)
    )
    testthat::expect_equal(exp_meta_r$nbands, 1)
    testthat::expect_equal(
      exp_meta_r[,c("xmin", "xmax", "ymin", "ymax")], 
      data.table("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530) 
    )
    testthat::expect_equal(sf::st_crs(exp_meta_r$proj), sf::st_crs(32632))
    testthat::expect_equal(exp_meta_r$type, "Byte")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_13[2])
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 145.1642, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 2086841, tolerance = 1e-3)
    rm(exp_stars)
    
  }
)


testthat::test_that(
  "Tests on indices computation with function s2_calcidices(), raster method", {
    
    testthat::expect_true(dir.exists(outdir_11))
    exp_outpath_12_1 <- file.path(outdir_11, "TOA", "S2A1C_20170703_022_Scalve_TOA_10.tif")
    testthat::expect_true(file.exists(exp_outpath_12_1))
    exp_outpath_14 <- file.path(outdir_11, "S2A1C_20170703_022_Scalve_MCARI_10.dat")
    s2_calcindices(
      exp_outpath_12_1, "MCARI", 
      outdir = dirname(dirname(exp_outpath_12_1)),
      source = "TOA",
      scaleFactor = 100,
      overwrite = TRUE,
      format = "ENVI"
    )
    expect_true(all(file.exists(c(
      exp_outpath_14,
      gsub("dat$", "hdr", exp_outpath_14),
      paste0(exp_outpath_14,".aux.xml")
    ))))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_14, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=1911, "y"=1479))
    testthat::expect_equal(exp_meta_r$res, c("x"=10, "y"=10))
    testthat::expect_equal(exp_meta_r$nbands, 1)
    testthat::expect_equal(exp_meta_r$bbox, sf::st_bbox(
        c("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530),
        crs = sf::st_crs(32632)
    ))
    testthat::expect_equal(exp_meta_r$type, "Int16")
    testthat::expect_equal(exp_meta_r$outformat, "ENVI")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_14)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 7.098357, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 2086841, tolerance = 1e-3)
    rm(exp_stars)
    
  }
)


testthat::test_that(
  "Tests on indices computation with function s2_calcidices(), gdal method", {
    
    testthat::expect_true(dir.exists(outdir_11))
    exp_outpath_12_1 <- file.path(outdir_11, "TOA", "S2A1C_20170703_022_Scalve_TOA_10.tif")
    testthat::expect_true(file.exists(exp_outpath_12_1))
    exp_outpath_15 <- file.path(outdir_11, "OSAVI", "S2A1C_20170703_022_Scalve_OSAVI_10.tif")
    unlink(exp_outpath_15)
    s2_calcindices(
      exp_outpath_12_1, "OSAVI", 
      outdir = dirname(dirname(exp_outpath_12_1)),
      subdirs = TRUE,
      source = "TOA",
      dataType = "Int32",
      proc_mode = "gdal_calc",
      scaleFactor = 1E6
    )
    testthat::expect_true(file.exists(exp_outpath_15))
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_15, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=1911, "y"=1479))
    testthat::expect_equal(exp_meta_r$res, c("x"=10, "y"=10))
    testthat::expect_equal(exp_meta_r$nbands, 1)
    testthat::expect_equal(exp_meta_r$bbox, sf::st_bbox(
      c("xmin" = 578590, "xmax" = 597700, "ymin" = 5086740, "ymax" = 5101530),
      crs = sf::st_crs(32632)
    ))
    testthat::expect_equal(exp_meta_r$type, "Int32")
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_15)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 488113.3, tolerance = 1e-3)
    testthat::expect_equal(sum(is.na(exp_stars[[1]])), 2086841, tolerance = 1e-3)
    rm(exp_stars)
    
  }
)

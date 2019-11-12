context("Test s2_download() and safe_getMetadata()")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the download is disabled by default if SAFE archives are already present.
# To perform the test also on download, replace 'test_download = FALSE' with 'TRUE'.
test_download = TRUE

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)


testthat::test_that(
  "Tests on s2_download - Built-in downloader", {
    
    # s2_l2a_list <- s2_list(
    #   tile = c("32TNR", "32TNS"),
    #   time_interval = "2017-07-03",
    #   level = "auto"
    # )
    s2_l2a_list <- c(
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/apihub/odata/v1/Products('50f281e9-26ed-4d52-b394-18a4040c88b7')/$value",
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/apihub/odata/v1/Products('b4b6b897-a03a-4f3c-b78d-974066574aed')/$value"
    )
    suppressWarnings(s2_download(
      s2_l2a_list,
      downloader = "builtin",
      outdir = safe_dir,
      overwrite = test_download
    )) # suppressWarnings used to manage possible warnings for skept Md5sum checks
    exp_outsafe_1 <- file.path(safe_dir, names(s2_l2a_list))
    testthat::expect_true(all(file.exists(exp_outsafe_1)))
    
    # test raster metadata
    exp_meta_ex <- raster_metadata(file.path(
      exp_outsafe_1[grepl("32TNR", exp_outsafe_1)], 
      "GRANULE/L2A_T32TNR_A010601_20170703T101041",
      "IMG_DATA/R10m/L2A_T32TNR_20170703T101021_B02_10m.jp2"
    ), format = "list")[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x" = 10980, "y" = 10980))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 10,    "y" = 10))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox), 
      c(499980, 4990200, 609780, 5100000)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test SAFE metadata
    if (Sys.info()["sysname"] != "Windows") {
      safe_metadata1 <- safe_getMetadata(exp_outsafe_1)
      testthat::expect_is(safe_metadata1, "data.table")
      testthat::expect_equal(safe_metadata1$prod_type, rep("product",2))
      testthat::expect_equal(safe_metadata1$version, rep("compact",2))
      testthat::expect_equal(
        safe_metadata1$xml_main, 
        file.path(exp_outsafe_1,"MTD_MSIL2A.xml")
      )
      testthat::expect_equal(
        dirname(safe_metadata1$xml_granules), 
        file.path(exp_outsafe_1,paste0("GRANULE/L2A_T",c("32TNS","32TNR"),"_A010601_20170703T101041"))
      )
      testthat::expect_equal(safe_metadata1$mission, rep("2A",2))
      testthat::expect_equal(safe_metadata1$level, rep("2A",2))
      testthat::expect_equal(as.Date(safe_metadata1$sensing_datetime), rep(as.Date("2017-07-03"),2))
      testthat::expect_equal(safe_metadata1$id_orbit, rep("022",2))
      testthat::expect_equal(safe_metadata1$id_tile, c("32TNS","32TNR"))
      testthat::expect_equal(safe_metadata1$utm, rep(32,2))
      testthat::expect_equal(safe_metadata1$direction, rep("DESCENDING",2))
      testthat::expect_equal(safe_metadata1$orbit_n, rep("22",2))
      testthat::expect_equal(safe_metadata1$nodata_value, rep("0",2))
      testthat::expect_equal(safe_metadata1$saturated_value, rep("65535",2))
    }
    
    safe_metadata2 <- safe_getMetadata(exp_outsafe_1[2], info=c("tiles", "level", "id_tile"), format = "list")
    testthat::expect_is(safe_metadata2, "list")
    testthat::expect_equal(length(safe_metadata2), 4)
    testthat::expect_equal(safe_metadata2$name, basename(exp_outsafe_1[2]))
    testthat::expect_equal(safe_metadata2$tiles, "32TNR")
    testthat::expect_equal(safe_metadata2$level, "2A")
    testthat::expect_equal(safe_metadata2$id_tile, safe_metadata2$tiles)
    
  }
)


testthat::test_that(
  "Tests on s2_download - aria2 downloader", {
    
    # s2_l1c_list <- s2_list(
    #   tile = c("32TNR", "32TNS"),
    #   time_interval = "2017-07-03",
    #   level = "L1C"
    # )
    s2_l1c_list <- c(
      "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/apihub/odata/v1/Products('432572ed-450b-408f-99b5-23877bd229da')/$value",
      "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/apihub/odata/v1/Products('5f590bcb-ee55-4a20-8e75-bde99f5b93d4')/$value"
    )
    suppressWarnings(s2_download(
      s2_l1c_list,
      downloader = "aria2",
      outdir = safe_dir,
      overwrite = test_download
    )) # suppressWarnings used to manage possible warnings for skept Md5sum checks
    exp_outsafe_2 <- file.path(safe_dir, names(s2_l1c_list))
    testthat::expect_true(all(file.exists(exp_outsafe_2)))
    
    # test raster metadata
    exp_meta_ex <- raster_metadata(file.path(
      exp_outsafe_2[grepl("32TNR", exp_outsafe_2)], 
      "GRANULE/L1C_T32TNR_A010601_20170703T101041", 
      "IMG_DATA/T32TNR_20170703T101021_B01.jp2"
    ), format = "list")[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x" = 1830, "y" = 1830))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 60,   "y" = 60))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox), 
      c(499980, 4990200, 609780, 5100000)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test SAFE metadata
    if (Sys.info()["sysname"] != "Windows") {
      testthat::expect_error(safe_getMetadata(basename(exp_outsafe_2[1])))
      safe_metadata <- safe_getMetadata(exp_outsafe_2, format = "vector")
      testthat::expect_is(safe_metadata, "list")
      testthat::expect_equal(unique(sapply(safe_metadata, length)), 2)
      testthat::expect_equal(unique(safe_metadata$prod_type), "product")
      testthat::expect_equal(unique(safe_metadata$version), "compact")
      testthat::expect_equal(
        as.vector(safe_metadata$xml_main),
        file.path(exp_outsafe_2,"MTD_MSIL1C.xml")
      )
      testthat::expect_equal(
        dirname(as.vector(unlist(safe_metadata$xml_granules))), 
        file.path(exp_outsafe_2,paste0("GRANULE/L1C_T",c("32TNR","32TNS"),"_A010601_20170703T101041"))
      )
      testthat::expect_equal(unique(safe_metadata$mission), "2A")
      testthat::expect_equal(unique(safe_metadata$level), "1C")
      testthat::expect_equal(unique(safe_metadata$id_orbit), "022")
      testthat::expect_equal(unique(safe_metadata$id_tile), c("32TNR", "32TNS"))
      testthat::expect_equal(unique(safe_metadata$utm), 32)
      testthat::expect_equal(unique(safe_metadata$direction), "DESCENDING")
      testthat::expect_equal(unique(safe_metadata$orbit_n), "22")
      testthat::expect_equal(unique(safe_metadata$nodata_value), "0")
      testthat::expect_equal(unique(safe_metadata$saturated_value), "65535")
    }
    
    # test rm_invalid_safe() should not have effect
    testthat::expect_true(file.exists(exp_outsafe_2[1]))
    rm_invalid_safe(exp_outsafe_2[1])
    testthat::expect_true(file.exists(exp_outsafe_2[1]))
    
  }
)

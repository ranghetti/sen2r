context("Test s2_download")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the download is disabled by default if SAFE archives are already present.
# To perform the test also on download, replace 'overwrite = FALSE' with 'TRUE'.

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)


testthat::test_that(
  "Tests on s2_download - Built-in downloader", {
    # s2_l2a_list <- s2_list(
    #   tile = c("32TNR", "32TNS"),
    #   time_interval = "2017-07-03",
    #   level = "auto"
    # )
    s2_l2a_list <- c(
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('50f281e9-26ed-4d52-b394-18a4040c88b7')/$value",
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('b4b6b897-a03a-4f3c-b78d-974066574aed')/$value"
    )
    s2_download(
      s2_l2a_list,
      downloader = "builtin",
      outdir = file.path(safe_dir, "L2A"),
      overwrite = FALSE
    )
    exp_outsafe_1 <- file.path(safe_dir, "L2A", names(s2_l2a_list))
    testthat::expect_true(all(file.exists(exp_outsafe_1)))
    exp_meta_ex <- raster_metadata(file.path(
      exp_outsafe_1[grepl("32TNR", exp_outsafe_1)], 
      "GRANULE/L2A_T32TNR_A010601_20170703T101041",
      "IMG_DATA/R10m/L2A_T32TNR_20170703T101021_B02_10m.jp2"
    ), format = "list")[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x"=10980, "y"=10980))
    testthat::expect_equal(exp_meta_ex$res, c("x"=10, "y"=10))
    testthat::expect_equal(
      exp_meta_ex$bbox, 
      sf::st_bbox(
        c("xmin" = 499980, "ymin" = 4990200, "xmax" = 609780, "ymax" = 5100000), 
        crs = sf::st_crs(32632)
      )
    )
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
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
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('432572ed-450b-408f-99b5-23877bd229da')/$value",
      "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('5f590bcb-ee55-4a20-8e75-bde99f5b93d4')/$value"
    )
    s2_download(
      s2_l1c_list,
      downloader = "aria2",
      outdir = file.path(safe_dir, "L1C"),
      overwrite = FALSE
    )
    exp_outsafe_2 <- file.path(safe_dir, "L1C", names(s2_l1c_list))
    testthat::expect_true(all(file.exists(exp_outsafe_2)))
    exp_meta_ex <- raster_metadata(file.path(
      exp_outsafe_2[grepl("32TNR", exp_outsafe_2)], 
      "GRANULE/L1C_T32TNR_A010601_20170703T101041", 
      "IMG_DATA/T32TNR_20170703T101021_B01.jp2"
    ), format = "list")[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x"=1830, "y"=1830))
    testthat::expect_equal(exp_meta_ex$res, c("x"=60, "y"=60))
    testthat::expect_equal(
      exp_meta_ex$bbox, 
      sf::st_bbox(
        c("xmin" = 499980, "ymin" = 4990200, "xmax" = 609780, "ymax" = 5100000), 
        crs = sf::st_crs(32632)
      )
    )
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
  }
)

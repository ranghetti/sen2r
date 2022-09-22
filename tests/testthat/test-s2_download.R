message("\n---- Test s2_download() from GCloud and safe_getMetadata() ----")
testthat::skip_on_cran()
skip_full_tests()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the download is disabled by default if SAFE archives are already present.
# To perform the test also on download, replace 'test_download = FALSE' with 'TRUE'.
test_download = FALSE

# NOTE 2: causing to the reduction of the retention time in SciHub, tests on SciHub
# difficult to be updated in order to grant SAFE availability.
# For this reason, GCloud is used by default to verify output structure,
# while SciHub is tested on a recent, random product.

s2_sel_list <- c(
  "S2A_MSIL1C_20190723T101031_N0208_R022_T32TNR_20190723T121220.SAFE" =
    "gs://gcp-public-data-sentinel-2/tiles/32/T/NR/S2A_MSIL1C_20190723T101031_N0208_R022_T32TNR_20190723T121220.SAFE/",
  "S2A_MSIL2A_20190723T101031_N0213_R022_T32TNR_20190723T125722.SAFE" =
    "gs://gcp-public-data-sentinel-2/L2/tiles/32/T/NR/S2A_MSIL2A_20190723T101031_N0213_R022_T32TNR_20190723T125722.SAFE/"
)

testthat::test_that(
  "Tests on s2_download - Error if internet is down", {
    testthat::expect_error(
      httptest::without_internet(suppressWarnings({
        s2_download(
          s2_sel_list,
          downloader = "builtin",
          outdir = safe_dir,
          service = "dhus",
          overwrite = test_download
        )
      })),
      regexp = gsub(
        " ", "[ \n]",
        "[Ii]nternet connection or SciHub may be down"
      )
    )
  }
)


testthat::test_that(
  "Tests on s2_download - Gcloud downloader", {
    
    testthat::skip_if_not(is_gcloud_configured(), "Google account is not set")
    testthat::skip_if_not(check_gcloud_connection(), "Google Cloud server is not reachable")
    
    suppressWarnings(s2_downloaded <- s2_download(
      s2_sel_list,
      outdir = safe_dir,
      overwrite = test_download
    )) # suppressWarnings used to manage possible warnings for skept Md5sum checks
    exp_outsafe_1 <- file.path(safe_dir, names(s2_downloaded))
    testthat::expect_true(all(file.exists(exp_outsafe_1)))
    testthat::expect_equal(length(s2_downloaded), length(s2_sel_list))
    testthat::expect_true(grepl("gs://gcp-public-data-sentinel-2/tiles/32/T/NR/", s2_downloaded[1]))
    testthat::expect_true(grepl("gs://gcp-public-data-sentinel-2/L2/tiles/32/T/NR/", s2_downloaded[2]))
    
    # test raster metadata on L1C
    exp_meta_ex <- sen2r::raster_metadata(
      list.files(
        file.path(
          exp_outsafe_1[grepl("L1C", exp_outsafe_1)],
          "GRANULE/L1C_T32TNR_A021326_20190723T101347/IMG_DATA"
        ),
        "T32TNR_20190723T101031_B02", full.names = TRUE
      ), format = "list"
    )[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x" = 10980, "y" = 10980))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 10,    "y" = 10))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox),
      c(499980, 4990200, 609780, 5100000)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test raster metadata on L2A
    exp_meta_ex <- sen2r::raster_metadata(
      list.files(
        file.path(
          exp_outsafe_1[grepl("L2A", exp_outsafe_1)],
          "GRANULE/L2A_T32TNR_A021326_20190723T101347/IMG_DATA/R20m"
        ),
        "T32TNR_20190723T101031_B8A", full.names = TRUE
      ), format = "list"
    )[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x" = 5490, "y" = 5490))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 20,    "y" = 20))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox),
      c(499980, 4990200, 609780, 5100000)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test SAFE metadata
    if (Sys.info()["sysname"] == "Linux") {
      safe_metadata1 <- safe_getMetadata(exp_outsafe_1)
      testthat::expect_is(safe_metadata1, "data.table")
      testthat::expect_equal(safe_metadata1$prod_type, rep("product",2))
      testthat::expect_equal(safe_metadata1$version, rep("compact",2))
      testthat::expect_equal(
        safe_metadata1$xml_main, 
        paste0(exp_outsafe_1,"/MTD_MSIL",c("1C","2A"),".xml")
      )
      testthat::expect_equal(
        dirname(safe_metadata1$xml_granules), 
        file.path(exp_outsafe_1,paste0("GRANULE/L",c("1C","2A"),"_T32TNR_A021326_20190723T101347"))
      )
      testthat::expect_equal(safe_metadata1$mission, rep("2A",2))
      testthat::expect_equal(safe_metadata1$level, c("1C","2A"))
      testthat::expect_equal(as.Date(safe_metadata1$sensing_datetime), rep(as.Date("2019-07-23"),2))
      testthat::expect_equal(safe_metadata1$id_orbit, rep("022",2))
      testthat::expect_equal(safe_metadata1$id_tile, rep("32TNR",2))
      testthat::expect_equal(safe_metadata1$utm, rep("32N",2))
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


message("\n---- Test s2_download() from SciHub ----")
dir.create(safe_dir_temp <- tempfile("safe_"))

testthat::test_that(
  "Tests on s2_download - Built-in downloader, dhus service", {
    
    testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
    testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
    testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
    
    s2_l2a_list <- s2_list(
      tile = "32TNT", 
      orbit = 22, 
      service = "dhus",
      time_interval = c(Sys.Date()-11, Sys.Date()-2),
      level = "L2A",
      apihub = tests_apihub_path
    )
    
    testthat::skip_if(length(s2_l2a_list) == 0)
    if (!all(safe_is_online(s2_l2a_list))) {
      s2_order(s2_l2a_list)
      testthat::skip("s2_l2a_list contains offline products")
    }
    
    suppressWarnings(s2_l2a_downloaded <- s2_download(
      s2_l2a_list[1],
      downloader = "builtin",
      outdir = safe_dir,
      apihub = tests_apihub_path,
      service = "dhus",
      overwrite = test_download
    )) # suppressWarnings used to manage possible warnings for skept Md5sum checks
    exp_outsafe_1 <- file.path(safe_dir, names(s2_l2a_downloaded))
    testthat::expect_true(file.exists(exp_outsafe_1))
    testthat::expect_equal(length(s2_l2a_downloaded), 1)
    testthat::expect_true(grepl("https://scihub.copernicus.eu/dhus/", s2_l2a_downloaded))
    
    # test raster metadata
    granule_name <- list.files(
      file.path(exp_outsafe_1, "GRANULE"),
      "^L[12][AC]\\_T[A-Z0-9]{5}\\_A[0-9]{6}\\_[0-9]{8}T[0-9]{6}$"
    )
    exp_meta_ex <- raster_metadata(
      list.files(
        file.path(
          exp_outsafe_1,
          "GRANULE", granule_name, "IMG_DATA/R10m"
        ),
        "T32TNT_([0-9]{8}T[0-9]{6})_B02_10m", full.names = TRUE
      ), format = "list"
    )[[1]]
    testthat::expect_equal(exp_meta_ex$size, c("x" = 10980, "y" = 10980))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 10,    "y" = 10))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox), 
      c(499980, 5190240, 609780, 5300040)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test SAFE metadata
    if (Sys.info()["sysname"] == "Linux") {
      safe_metadata1 <- safe_getMetadata(exp_outsafe_1)
      testthat::expect_is(safe_metadata1, "data.table")
      testthat::expect_equal(safe_metadata1$prod_type, "product")
      testthat::expect_equal(safe_metadata1$version, "compact")
      testthat::expect_equal(
        safe_metadata1$xml_main, 
        file.path(exp_outsafe_1,"MTD_MSIL2A.xml")
      )
      testthat::expect_match(safe_metadata1$mission, "2[AB]")
      testthat::expect_equal(safe_metadata1$level, "2A")
      testthat::expect_equal(safe_metadata1$id_orbit, "022")
      testthat::expect_equal(safe_metadata1$id_tile, "32TNT")
      testthat::expect_equal(safe_metadata1$utm, "32N")
      testthat::expect_equal(safe_metadata1$direction, "DESCENDING")
      testthat::expect_equal(safe_metadata1$orbit_n, "22")
      testthat::expect_equal(safe_metadata1$nodata_value, "0")
      testthat::expect_equal(safe_metadata1$saturated_value, "65535")
    }

  }
)


# if (Sys.info()["sysname"] != "Linux") {
#   testthat::skip_on_ci() # aria2 not installed on Windows and macOS CI
# }

testthat::test_that(
  "Tests on s2_download - check aria2 installation", {
    
    if (Sys.info()["sysname"] == "Windows") {
      aria2_path <- install_aria2(dirname(attr(load_binpaths(), "path")), force = TRUE)
      # testthat::expect_equal(aria2_path, normalize_path("~/.sen2r/aria2c.exe"))
      testthat::expect_equal(aria2_path, load_binpaths()$aria2)
    } else {
      testthat::expect_warning(
        install_aria2(dirname(attr(load_binpaths(), "path")), force = TRUE), 
        "[Tt]his function is only for Windows"
      )
      if (Sys.which("aria2c") != "") {
        testthat::expect_equivalent(load_binpaths("aria2")$aria2, normalize_path(Sys.which("aria2c")))
      }
    }
  }
  
)


testthat::test_that(
  "Tests on s2_download - aria2 downloader", {
    
    testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
    testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
    testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")
    if (Sys.info()["sysname"] != "Linux") {
      testthat::skip_on_ci() # because sometimes the following error appears:
      # Download of file
      # S2B_MSIL1C_20210617T100559_N0300_R022_T32TNT_20210617T121634.SAFE was
      # incomplete (Md5sum check failed). Please retry to launch the download."
    }
    
    s2_l1c_list <- s2_list(
      tile = "32TNT", 
      orbit = 22, 
      service = "apihub",
      time_interval = c(Sys.Date()-11, Sys.Date()-2),
      level = "L1C",
      apihub = tests_apihub_path
    )
    
    testthat::skip_if(length(s2_l1c_list) == 0)
    if (!all(safe_is_online(s2_l1c_list))) {
      s2_order(s2_l1c_list)
      testthat::skip("s2_l1c_list contains offline products")
    }
    
    suppressWarnings(s2_l1c_downloaded <- s2_download(
      rev(s2_l1c_list)[1],
      downloader = "aria2",
      outdir = safe_dir,
      apihub = tests_apihub_path,
      overwrite = test_download
    )) # suppressWarnings used to manage possible warnings for skept Md5sum checks
    exp_outsafe_2 <- file.path(safe_dir, names(s2_l1c_downloaded))
    testthat::expect_true(file.exists(exp_outsafe_2))
    testthat::expect_equal(length(s2_l1c_downloaded), 1)
    
    # test raster metadata
    granule_name <- list.files(
      file.path(exp_outsafe_2, "GRANULE"),
      "^L[12][AC]\\_T[A-Z0-9]{5}\\_A[0-9]{6}\\_[0-9]{8}T[0-9]{6}$"
    )
    exp_meta_ex <- raster_metadata(
      list.files(
        file.path(
          exp_outsafe_2,
          "GRANULE", granule_name, "IMG_DATA"
        ),
        "T32TNT_([0-9]{8}T[0-9]{6})_B01", full.names = TRUE
      ), format = "list"
    )[[1]]
    
    testthat::expect_equal(exp_meta_ex$size, c("x" = 1830, "y" = 1830))
    testthat::expect_equal(exp_meta_ex$res, c("x"  = 60,   "y" = 60))
    testthat::expect_equal(
      as.numeric(exp_meta_ex$bbox), 
      c(499980, 5190240, 609780, 5300040)
    )
    testthat::expect_equal(exp_meta_ex$proj$epsg, 32632)
    testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
    
    # test SAFE metadata
    if (Sys.info()["sysname"] == "Linux") {
      testthat::expect_error(safe_getMetadata(basename(exp_outsafe_2)))
      safe_metadata <- safe_getMetadata(exp_outsafe_2, format = "vector")
      testthat::expect_is(safe_metadata, "list")
      testthat::expect_equivalent(safe_metadata$prod_type, "product")
      testthat::expect_equivalent(safe_metadata$version, "compact")
      testthat::expect_equal(
        as.vector(safe_metadata$xml_main),
        file.path(exp_outsafe_2,"MTD_MSIL1C.xml")
      )
      testthat::expect_match(safe_metadata$mission, "2[AB]")
      testthat::expect_equivalent(safe_metadata$level, "1C")
      testthat::expect_equivalent(safe_metadata$id_orbit, "022")
      testthat::expect_equivalent(safe_metadata$id_tile, "32TNT")
      testthat::expect_equivalent(safe_metadata$utm, "32N")
      testthat::expect_equivalent(safe_metadata$direction, "DESCENDING")
      testthat::expect_equivalent(safe_metadata$orbit_n, "22")
      testthat::expect_equivalent(safe_metadata$nodata_value, "0")
      testthat::expect_equivalent(safe_metadata$saturated_value, "65535")
    }
    
    # test rm_invalid_safe() should not have effect
    testthat::expect_true(file.exists(exp_outsafe_2))
    rm_invalid_safe(exp_outsafe_2)
    testthat::expect_true(file.exists(exp_outsafe_2))

  }
)

unlink(safe_dir_temp, recursive = TRUE)

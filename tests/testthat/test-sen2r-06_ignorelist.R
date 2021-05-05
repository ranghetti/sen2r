message("\n---- Test ignorelist ----")
testthat::skip_on_cran()
testthat::skip_on_ci() # TODO try to remove
testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")

# Ensure required SAFE to be downloaded
s2_l2a_list <- c(
  "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNR_20200801T135302.SAFE" =
    "https://apihub.copernicus.eu/apihub/odata/v1/Products('e502d496-631f-4557-b14f-d98195fdc8c1')/$value",
  "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNS_20200801T135302.SAFE" =
    "https://apihub.copernicus.eu/apihub/odata/v1/Products('4aac5270-bbdf-4743-9f9f-532fdbfea2fd')/$value"
)
suppressWarnings(s2_l2a_downloaded <- s2_download(
  s2_l2a_list,
  downloader = "builtin",
  outdir = safe_dir,
  apihub = tests_apihub_path,
  overwrite = FALSE
))

testthat::test_that(
  "Tests on safelist read/write", {
    
    # Check sample inputs
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, names(s2_l2a_list[1]),
      "GRANULE/L2A_T32TNR_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNR_20200801T100559_B08_10m.jp2"
    )))
    testthat::skip_if_not(file.exists(file.path(
      safe_dir, names(s2_l2a_list[2]),
      "GRANULE/L2A_T32TNS_A017780_20200801T101400/IMG_DATA/R10m",
      "T32TNS_20200801T100559_B08_10m.jp2"
    )))
    
    outdir_17 <- tempfile(pattern = "out_test17_")
    dir.create(dirname(outdir_17), showWarnings = FALSE)
    out_17 <- sen2r(
      gui = FALSE,
      online = FALSE,
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = system.file("extdata/vector/barbellino.geojson", package = "sen2r"),
      extent_name = "Barbellino",
      timewindow = as.Date("2020-08-01"),
      list_indices = c("NDVI","MSAVI2"),
      list_rgb = c("RGB432B"),
      mask_type = "land",
      max_mask = 10,
      path_l2a = safe_dir,
      path_out = outdir_17,
      thumbnails = FALSE,
      apihub = tests_apihub_path
    )
    
    ignorelist_path <- file.path(outdir_17, ".ignorelist.txt")
    expect_true(file.exists(ignorelist_path))
    ignorelist <- parseTOML(ignorelist_path)
    expect_length(ignorelist, 4)
    expect_equal(
      sort(names(ignorelist)), 
      c("dates_cloudcovered", "mask_type", "max_mask", "names_missing")
    )
    expect_equal(ignorelist$max_mask, 10)
    expect_equal(ignorelist$mask_type, "land")
    expect_equal(ignorelist$dates_cloudcovered, as.Date("2020-08-01"))
    
    # Relaunch the same processing to test that dates are skipped
    # (and to test read_ignorepath)
    out_17_2 <- sen2r(attr(out_17, "procpath"))
    expect_equal(length(out_17_2), 0)
    
  }
)

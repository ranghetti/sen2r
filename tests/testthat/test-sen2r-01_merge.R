message("\n---- Test s2_merge and translate when stitching 2 tiles with no clipping ----")
testthat::skip_on_cran()
# testthat::skip_on_travis() # because required SAFE do not exists
testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

# # Ensure required SAFE to be downloaded
# s2_l2a_list <- c(
#   "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNR_20200801T135302.SAFE" =
#     "https://scihub.copernicus.eu/apihub/odata/v1/Products('e502d496-631f-4557-b14f-d98195fdc8c1')/$value",
#   "S2B_MSIL2A_20200801T100559_N0214_R022_T32TNS_20200801T135302.SAFE" =
#     "https://scihub.copernicus.eu/apihub/odata/v1/Products('4aac5270-bbdf-4743-9f9f-532fdbfea2fd')/$value"
# )
# suppressWarnings(s2_l2a_downloaded <- s2_download(
#   s2_l2a_list,
#   outdir = safe_dir,
#   apihub = tests_apihub_path,
#   overwrite = FALSE
# ))


testthat::test_that(
  "Tests on merge all found tiles in offline mode", {

    # # Check sample inputs
    # expect_true(file.exists(file.path(
    #   safe_dir, names(s2_l2a_list[1]), 
    #   "GRANULE/L2A_T32TNR_A017780_20200801T101400/IMG_DATA/R10m",
    #   "T32TNR_20200801T100559_B08_10m.jp2"
    # )))
    # expect_true(file.exists(file.path(
    #   safe_dir, names(s2_l2a_list[2]), 
    #   "GRANULE/L2A_T32TNS_A017780_20200801T101400/IMG_DATA/R10m",
    #   "T32TNS_20200801T100559_B08_10m.jp2"
    # )))
    
    outdir_1 <- tempfile(pattern = "out_test1_")
    dir.create(dirname(outdir_1), showWarnings = FALSE)
    exp_outpath_1 <- file.path(outdir_1, "SCL", "S2B2A_20200801_022__SCL_10.tif")
    unlink(exp_outpath_1)
    out1 <- sen2r(
      gui = FALSE,
      online = TRUE, # FIXME restore FALSE
      step_atmcorr = "l2a", # to avoid checks on Sen2Cor
      extent = NA,
      timewindow = as.Date("2020-08-01"),
      s2tiles_selected = c("32TNS","32TNR"),
      list_prods = "SCL",
      mask_type = NA,
      clip_on_extent = FALSE,
      path_l2a = safe_dir,
      path_out = outdir_1, 
      overwrite = TRUE,
      thumbnails = FALSE,
      apihub = tests_apihub_path
    )
    expect_true(file.exists(exp_outpath_1))
    
    # check sen2r output format
    testthat::expect_is(out1, "character")
    testthat::expect_equivalent(
      normalize_path(out1), normalize_path(exp_outpath_1)
    )
    testthat::expect_equal(
      names(attributes(out1)), 
      c("procpath", "clouddates", "missing", "status")
    )
    
    # test on raster metadata
    exp_meta_r <- raster_metadata(exp_outpath_1, format = "list")[[1]]
    testthat::expect_equal(exp_meta_r$size, c("x"=5490, "y"=10491))
    testthat::expect_equal(exp_meta_r$res, c("x"=20, "y"=20))
    testthat::expect_equal(
      as.numeric(exp_meta_r$bbox), 
      c(499980, 4990200, 609780, 5200020)
    )
    expect_equal_crs(st_crs2(exp_meta_r$proj), 32632)
    testthat::expect_equal(exp_meta_r$outformat, "GTiff") # default value
    
    # tests on sen2r metadata
    exp_meta_s <- sen2r_getElements(exp_outpath_1)
    testthat::expect_equal(exp_meta_s$type, "merged")
    testthat::expect_equal(exp_meta_s$sensing_date, as.Date("2020-08-01"))
    testthat::expect_equal(exp_meta_s$prod_type, "SCL")
    testthat::expect_equal(exp_meta_s$extent_name, "")
    
    # test on raster values
    exp_stars <- stars::read_stars(exp_outpath_1)
    testthat::expect_equal(mean(exp_stars[[1]], na.rm=TRUE), 4.285448, tolerance = 1e-03)
    rm(exp_stars)
    
    unlink(outdir_1, recursive = TRUE)
    
  }
)


testthat::test_that(
  "Expect error with no extent and tiles specified in online mode", {
    
    outdir_1c <- tempfile(pattern = "out_test1c_")
    dir.create(dirname(outdir_1c), showWarnings = FALSE)
    testthat::expect_error(
      sen2r(
        gui = FALSE,
        online = TRUE,
        step_atmcorr = "l2a", # to avoid checks on Sen2Cor
        extent = NA,
        timewindow = as.Date("2020-08-01"),
        list_prods = "BOA",
        clip_on_extent = FALSE,
        path_l2a = safe_dir,
        path_out = outdir_1c, 
        overwrite = TRUE,
        thumbnails = FALSE,
        apihub = tests_apihub_path
      ),
      regexp = gsub(
        " ", "[ \n]",
        "[Aa]t least one parameter among 'extent' and 's2tiles_selected' must be provided"
      )
    )
    
  }
)

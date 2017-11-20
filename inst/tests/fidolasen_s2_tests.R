# Tests to perform on fidolasen_s2() processing chains.

# Please notice that these tests are not though to cover all the possible
# processing types possible with this package.


context("Fidolasen S2 examples")
testthat::test_that(
  "Tests on fidolasen_s2", {
    
    library(testthat)
    library(magrittr)
    
    skip_on_cran()
    skip_on_travis()
    
    ### Test 0: download required tiles
    context("Test 0: raw download")
    example_dir <- system.file("extdata","example_files", package="fidolasen")
    safe_dir <- file.path(example_dir, "safe")
    dir.create(safe_dir, showWarnings = FALSE)
    s2tiles_test1 <- c(
      "S2A_OPER_PRD_MSIL1C_PDMC_20161206T102010_R022_V20161205T101402_20161205T101402.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('37e6ef05-3598-4491-ae74-10a0685bcdc2')/\\$value",
      "S2A_OPER_PRD_MSIL1C_PDMC_20161206T101815_R022_V20161205T101402_20161205T101402.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('926ba88a-1e9a-4801-abb4-5b6e01b0bbc1')/\\$value",
      # "S2A_MSIL1C_20161215T101422_N0204_R022_T32TNR_20161215T101510.SAFE" = 
      #   "https://scihub.copernicus.eu/dhus/odata/v1/Products('d27e081e-3493-44ea-8ac3-3b3a7d2b7eea')/\\$value",
      # "S2A_MSIL1C_20161215T101422_N0204_R022_T32TNS_20161215T101510.SAFE" =
      #   "https://scihub.copernicus.eu/dhus/odata/v1/Products('fbd47d0a-4379-4173-9932-3bb59bb6c66e')/\\$value",
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('b4b6b897-a03a-4f3c-b78d-974066574aed')/\\$value",
      "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE" =
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('50f281e9-26ed-4d52-b394-18a4040c88b7')/\\$value"
    ) # tiles 32TNR and 32TNS of orbit 22, period 2016-12-05 - 2016-12-15
    s2_download(
      s2tiles_test1, 
      outdir = safe_dir
    )
    
    
    ### Test 1: translate and merge
    context("Test 1: translate and merge two tiles")
    out_dir <- file.path(example_dir, "out", "out_test1")
    dir.create(dirname(out_dir), showWarnings = FALSE)
    dir.create(out_dir, showWarnings = FALSE)
    fidolasen_s2(
      gui = FALSE,
      online = FALSE,
      s2_levels = "l1c",
      step_atmcorr = "no",
      extent = NA,
      timewindow = as.Date("2016-12-05"),
      list_prods = "TOA",
      mask_type = NA,
      path_l1c = safe_dir,
      path_out = out_dir
    )
    
    ### Test 2: clip and mask
    context("Test 2: clip and mask")
    out_dir <- file.path(example_dir, "out", "out_test2")
    dir.create(out_dir, showWarnings = FALSE)
    fidolasen_s2(
      gui = FALSE,
      online = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "no",
      extent = file.path(example_dir, "scalve.kml"),
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "BOA",
      mask_type = "cloud_medium_proba",
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = out_dir
    )
    
    ### Test 3: create spectral indices
    context("Test 3: spectral indices")
    out_dir <- file.path(example_dir, "out", "out_test3")
    dir.create(out_dir, showWarnings = FALSE)
    fidolasen_s2(
      gui = FALSE,
      online = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "no",
      extent = file.path(example_dir, "scalve.kml"),
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_indices = c("NDVI","MSAVI","MCARI","NDRE"),
      mask_type = NA,
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = out_dir,
      path_indices = out_dir
    )
    
    ### Test 4: download (only windows)
    context("Test 4: download SAFE tiles")
    fidolasen_s2(
      gui = FALSE,
      online = TRUE,
      preprocess = FALSE,
      s2_levels = "l1c",
      step_atmcorr = "no",
      s2tiles_selected = c("32TNR","32TNS"),
      s2orbits_selected = "022",
      timewindow = as.Date(c("2016-12-05","2016-12-15")),
      path_l1c = safe_dir,
      path_l2a = safe_dir
    )
    
    ### Test 5: sen2cor
    context("Test 5: apply sen2cor")
    fidolasen_s2(
      gui = FALSE,
      online = FALSE,
      preprocess = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "scihub",
      s2tiles_selected = c("32TNR","32TNS"),
      s2orbits_selected = "022",
      timewindow = as.Date(c("2016-12-05","2016-12-15")),
      path_l1c = safe_dir,
      path_l2a = safe_dir
    )
    
    
  }
)

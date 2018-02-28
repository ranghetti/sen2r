# Tests to perform on sto() processing chains.

# Please notice that these tests are not though to cover all the possible
# processing types possible with this package.


context("SALTO S2 examples")
testthat::test_that(
  "Tests on sto", {
    
    library(testthat)
    library(magrittr)
    
    skip_on_cran()
    skip_on_travis()
    
    ### Test 1: download
    context("Test 1: download SAFE tiles (zip)")
    example_dir <- system.file("extdata","example_files", package="salto")
    safe_dir <- file.path(example_dir, "safe")
    system.time(sto(
      gui = FALSE,
      online = TRUE,
      preprocess = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "l2a",
      s2tiles_selected = c("32TNR","32TNS"),
      s2orbits_selected = "022",
      timewindow = as.Date(c("2017-07-03")),
      path_l1c = safe_dir,
      path_l2a = safe_dir
    ))
    
    ### Test 2: download (old & compact name)
    context("Test 2: download SAFE tiles (zip & files)")
    system.time(sto(
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
    ))
    
    ### Test 3: sen2cor
    context("Test 3: apply sen2cor")
    system.time(sto(
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
    ))
    
    ### Test 4: translate and merge
    context("Test 4: translate and merge two tiles")
    out_dir <- file.path(example_dir, "out", "out_test1")
    dir.create(dirname(out_dir), showWarnings = FALSE)
    system.time(sto(
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
    ))
    
    ### Test 5: clip and mask
    context("Test 5: clip and mask")
    out_dir <- file.path(example_dir, "out", "out_test2")
    system.time(sto(
      gui = FALSE,
      online = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "l2a",
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_prods = "BOA",
      mask_type = "cloud_medium_proba",
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = out_dir
    ))
    
    ### Test 6: create spectral indices
    context("Test 6: spectral indices")
    out_dir <- file.path(example_dir, "out", "out_test3")
    system.time(sto(
      gui = FALSE,
      online = FALSE,
      s2_levels = "l2a",
      step_atmcorr = "l2a",
      extent = file.path(example_dir, "scalve.kml"),
      extent_name = "Scalve",
      extent_as_mask = TRUE,
      timewindow = as.Date("2017-07-03"),
      list_indices = c("NDVI","MSAVI","MCARI","NDRE"),
      mask_type = NA,
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = out_dir,
      path_indices = out_dir
    ))
    
  }
)

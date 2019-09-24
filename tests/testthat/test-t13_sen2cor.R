context("Test Sen2Cor")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time,
# so running Sen2Cor is disabled by default if SAFE archives are already present.
# To perform the test also on Sen2Cor, replace 'test_sen2cor = FALSE' with 'TRUE'.
test_sen2cor = TRUE

example_dir <- system.file("extdata/example_files", package = "sen2r")
dir.create(example_dir, showWarnings = FALSE)
safe_dir <- file.path(example_dir, "safe")
dir.create(safe_dir, showWarnings = FALSE)
dir.create(file.path(safe_dir, "L2A"), showWarnings = FALSE)
dir.create(file.path(safe_dir, "L1C"), showWarnings = FALSE)
s2_l1c_prods <- file.path(safe_dir, "L1C", c(
  "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE",
  "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE"
))
s2_l2a_prods <- file.path(safe_dir, "L2A", c(
  "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE",
  "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE"
))


testthat::test_that(
  "Tests that Sen2Cor does not run if an existing corresponding L2A product exists", {
    
    testthat::expect_true(dir.exists(s2_l1c_prods[1])) # test-s2_download.R
    testthat::expect_true(dir.exists(s2_l2a_prods[1])) # test-s2_download.R
    run_time <- system.time(
      sen2cor_out <- sen2cor(
        basename(s2_l1c_prods[1]), 
        l1c_dir = file.path(safe_dir, "L1C"),
        outdir = file.path(safe_dir, "L2A")
      )
    )
    testthat::expect_equal(sen2cor_out, s2_l2a_prods[1])
    testthat::expect_lt(run_time["elapsed"], 60)
    
  }
)


if (test_sen2cor) {
  
  testthat::test_that(
    "Tests a single Sen2Cor run", {
      
      testthat::expect_true(dir.exists(s2_l1c_prods[2])) # test-s2_download.R
      unlink(s2_l2a_prods[2], recursive = TRUE)
      run_time <- system.time(
        sen2cor_out <- sen2cor(
          s2_l1c_prods[2], 
          outdir = file.path(safe_dir, "L2A")
        )
      )
      testthat::expect_true(dir.exists(sen2cor_out))
      testthat::expect_gt(run_time["elapsed"], 60)
      
      # test raster metadata
      exp_meta_ex <- raster_metadata(file.path(
        sen2cor_out, 
        "GRANULE/L2A_T32TNS_A010601_20170703T101041",
        "IMG_DATA/R10m/T32TNS_20170703T101021_B02_10m.jp2"
      ), format = "list")[[1]]
      testthat::expect_equal(exp_meta_ex$size, c("x"=10980, "y"=10980))
      testthat::expect_equal(exp_meta_ex$res, c("x"=10, "y"=10))
      testthat::expect_equal(
        exp_meta_ex$bbox, 
        sf::st_bbox(
          c("xmin" = 499980, "ymin" = 5090220, "xmax" = 609780, "ymax" = 5200020), 
          crs = sf::st_crs(32632)
        )
      )
      testthat::expect_equal(exp_meta_ex$outformat, "JP2OpenJPEG")
      
      # test SAFE metadata
      safe_metadata <- safe_getMetadata(sen2cor_out)
      testthat::expect_is(safe_metadata, "list")
      testthat::expect_equal(safe_metadata$prod_type, "product")
      testthat::expect_equal(safe_metadata$version, "compact")
      testthat::expect_equal(
        safe_metadata$xml_main, 
        file.path(sen2cor_out,"MTD_MSIL2A.xml")
      )
      testthat::expect_equal(
        dirname(safe_metadata$xml_granules), 
        file.path(sen2cor_out,"GRANULE/L2A_T32TNS_A010601_20170703T101041")
      )
      testthat::expect_equal(safe_metadata$mission, "2A")
      testthat::expect_equal(safe_metadata$level, "2A")
      testthat::expect_equal(as.Date(safe_metadata$sensing_datetime), as.Date("2017-07-03"))
      testthat::expect_equal(safe_metadata$id_orbit, "022")
      testthat::expect_equal(safe_metadata$id_tile, "32TNS")
      testthat::expect_equal(safe_metadata$tiles, "32TNS")
      testthat::expect_equal(safe_metadata$utm, 32)
      testthat::expect_equal(safe_metadata$direction, "DESCENDING")
      testthat::expect_equal(safe_metadata$orbit_n, "22")

    }
  )

  
  testthat::test_that(
    "Tests a multicore Sen2Cor run", {
      
      testthat::expect_true(all(dir.exists(s2_l1c_prods)))
      # unlink(s2_l2a_prods, recursive = TRUE)
      run_time <- system.time(
        sen2cor_out <- sen2cor(
          s2_l1c_prods, 
          outdir = file.path(safe_dir, "L2A"),
          parallel = TRUE,
          overwrite = TRUE
        )
      )
      testthat::expect_true(all(dir.exists(sen2cor_out)))
      testthat::expect_gt(run_time["elapsed"], 60)
      
      # TODO copy checks from test above  
      
    }
  )
}



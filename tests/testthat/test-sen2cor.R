context("Test Sen2Cor installation")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the installation can be disabled if Sen2Cor is already installed.
# This supposes Sen2Cor 2.5.5 to be installed (errors will occur in case 2.8.0 is present).
# To perform the download of Sen2Cor, replace 'test_install_sen2cor = FALSE' with 'TRUE'.
test_install_sen2cor = TRUE

sen2cor_inst_dir <- file.path(dirname(attr(load_binpaths(), "path")), "sen2cor")

testthat::test_that(
  "Test that a mismatching Sen2Cor version causes error", {
    testthat::expect_error(
      install_sen2cor(sen2cor_inst_dir, version = "2.3.3"),
      regexp = gsub(
        " ", "[ \n]",
        "[Oo]nly Sen2Cor versions.+ are currently supported"
      )
    )
  }
)

if (test_install_sen2cor) {
  
  testthat::test_that(
    "Test Sen2Cor 2.8.0 installation", {
      
      unlink(sen2cor_inst_dir, recursive = TRUE)
      
      testthat::expect_message(
        install_sen2cor(
          sen2cor_inst_dir,
          version = "2.8.0"
        ),
        regexp = gsub(
          " ", "[ \n]",
          "(IMPORTANT NOTE: for backward compatibility)|(Sen2Cor is already installed)"
        )
      )
      
      testthat::expect_true(dir.exists(dirname(load_binpaths()$sen2cor)))
      testver <- system2(load_binpaths()$sen2cor, "-h", stdout = TRUE)
      testthat::expect_true(grepl("2\\.8\\.0", testver[grep("Version", testver)]))
      
      if (Sys.info()["sysname"] == "Windows") {
        testthat::expect_true(any(grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))))
        sen2cor_inst_dir <- file.path(
          sen2cor_inst_dir,
          list.files(sen2cor_inst_dir)[grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))][1]
        )
        testthat::expect_true(file.exists(file.path(sen2cor_inst_dir, "L2A_Process.bat")))
      } else {
        testthat::expect_false(any(grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))))
        testthat::expect_true(file.exists(file.path(sen2cor_inst_dir, "bin/L2A_Process")))
      }
      
      # Check L2A_GIPP.xml
      xml_sen2cor_path <- normalize_path("~/sen2cor/2.8/cfg/L2A_GIPP.xml")
      testthat::expect_true(file.exists(xml_sen2cor_path))
      
    }
  )
  
}

testthat::test_that(
  "Test Sen2Cor 2.5.5 installation", {
    
    if (any(test_install_sen2cor, length(nn(load_binpaths()$sen2cor)) == 0)) {
      unlink(sen2cor_inst_dir, recursive = TRUE)
    }
    testthat::expect_message(
      install_sen2cor(
        sen2cor_inst_dir,
        version = "2.5.5"
      ),
      regexp = gsub(
        " ", "[ \n]",
        "(IMPORTANT NOTE: for backward compatibility)|(Sen2Cor is already installed)"
      )
    )
    
    testthat::expect_true(dir.exists(dirname(load_binpaths()$sen2cor)))
    testver <- system2(load_binpaths()$sen2cor, "-h", stdout = TRUE)
    testthat::expect_true(grepl("2\\.5\\.5", testver[grep("Version", testver)]))
    
    if (Sys.info()["sysname"] == "Windows") {
      testthat::expect_true(any(grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))))
      sen2cor_inst_dir <- file.path(
        sen2cor_inst_dir,
        list.files(sen2cor_inst_dir)[grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))][1]
      )
      testthat::expect_true(file.exists(file.path(sen2cor_inst_dir, "L2A_Process.bat")))
    } else {
      testthat::expect_false(any(grepl("^Sen2Cor\\-", list.files(sen2cor_inst_dir))))
      testthat::expect_true(file.exists(file.path(sen2cor_inst_dir, "bin/L2A_Process")))
    }
    
    # Check L2A_GIPP.xml
    xml_sen2cor_path <- normalize_path("~/sen2cor/2.5/cfg/L2A_GIPP.xml")
    testthat::expect_true(file.exists(xml_sen2cor_path))
    
  }
)

testthat::test_that(
  "Test that a Sen2Cor reinstallation do not occur if it is already installed", {
    testthat::expect_message(
      install_sen2cor(sen2cor_inst_dir),
      regexp = "Sen2Cor[ \n]is[ \n]already[ \n]installed"
    )
  }
)

testthat::test_that(
  "Test GIPP options", {
    
    # Check option previously set with test_sen2cor(use_dem = TRUE)
    xml_sen2r_path <- normalize_path("~/.sen2r/sen2r_L2A_GIPP.xml")
    testthat::expect_true(file.exists(xml_sen2r_path))
    xml_sen2r_raw <- readLines(xml_sen2r_path)
    testthat::expect_true(grepl(
      "<DEM_Directory>NONE</DEM_Directory>",
      xml_sen2r_raw[grepl("<DEM_Directory>", xml_sen2r_raw)]
    ))
    
    # Read default values
    gipp_1 <- read_gipp(c("dem_directory", "dem_reference"))
    testthat::expect_is(gipp_1, "list")
    testthat::expect_length(gipp_1, 2)
    testthat::expect_true(grepl("NONE", gipp_1$dem_directory))
    
    # Edit one value and save the output as temporary file
    set_gipp(list(DEM_Directory = "/invalid/directory"), gipp_path = gipp_temp <- tempfile())
    testthat::expect_true(file.exists(gipp_temp))
    xml_temp_raw <- readLines(gipp_temp)
    testthat::expect_true(grepl(
      "<DEM_Directory>/invalid/directory</DEM_Directory>",
      xml_temp_raw[grepl("<DEM_Directory>", xml_temp_raw)]
    ))
    
    # Read the parameters in the created temporary files
    gipp_2 <- read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path = gipp_temp)
    testthat::expect_is(gipp_2, "list")
    testthat::expect_length(gipp_2, 2)
    testthat::expect_true(grepl("/invalid/directory", gipp_2$DEM_Directory))
    
    # Edit one value using use_dem = FALSE
    set_gipp(use_dem = FALSE, gipp_path = gipp_temp <- tempfile())
    xml_temp_raw <- readLines(gipp_temp)
    testthat::expect_true(grepl(
      "<DEM_Directory>NONE</DEM_Directory>",
      xml_temp_raw[grepl("<DEM_Directory>", xml_temp_raw)]
    ))
    
    # Edit one value using use_dem = TRUE
    set_gipp(use_dem = TRUE, gipp_path = gipp_temp <- tempfile())
    xml_temp_raw <- readLines(gipp_temp)
    testthat::expect_true(grepl(
      "<DEM_Directory>.*srtm90</DEM_Directory>",
      xml_temp_raw[grepl("<DEM_Directory>", xml_temp_raw)]
    ))
    
    # Reset to default Sen2Cor GIPP values
    testthat::expect_message(
      gipp_init(gipp_sen2r_path = gipp_temp, force = TRUE, dem_warning = TRUE),
      "IMPORTANT NOTE: for backward compatibility"
    )
    # Read again values
    gipp_3 <- read_gipp(c("DEM_Directory", "DEM_Reference"), gipp_path = gipp_temp)
    testthat::expect_is(gipp_3, "list")
    testthat::expect_length(gipp_3, 2)
    testthat::expect_true(grepl("NONE", gipp_3$DEM_Directory))
    
    testthat::expect_true(file.remove(gipp_temp))
    
  }
)


context("Test Sen2Cor")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time,
# so running Sen2Cor is disabled by default if SAFE archives are already present.
# To perform the test also on Sen2Cor, replace 'test_sen2cor = FALSE' with 'TRUE'.
test_sen2cor = FALSE

safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
safe_out_dir <- tempfile(pattern = "safe_")
dir.create(safe_dir, showWarnings = FALSE)
s2_l1c_prods <- file.path(safe_dir, c(
  "S2A_MSIL1C_20190723T101031_N0208_R022_T32TNR_20190723T121220.SAFE",
  "S2A_MSIL1C_20190723T101031_N0208_R022_T32TNS_20190723T121220.SAFE"
))


testthat::test_that(
  "Tests that Sen2Cor does not run if an existing corresponding L2A product exists", {

    testthat::expect_true(dir.exists(s2_l1c_prods[1])) # test-s2_download.R
    s2_l2a_prod1 <- list.files(
      safe_dir,
      "^S2A_MSIL2A_20190723T101031_N...._R022_T32TNR_[0-9T]{15}\\.SAFE$"
    )
    if (length(s2_l2a_prod1) == 0) {
      s2_l2a_prod1 <- names(s2_download(c(
        "S2A_MSIL2A_20190723T101031_N0213_R022_T32TNR_20190723T125722.SAFE" =
          "https://scihub.copernicus.eu/apihub/odata/v1/Products('52b43ef1-9a43-4726-a833-9fabeb1a3329')/$value"
      ), outdir = safe_dir
      ))
    }
    testthat::expect_true(dir.exists(file.path(safe_dir,s2_l2a_prod1[1])))
    run_time <- system.time(
      sen2cor_out <- sen2cor(
        basename(s2_l1c_prods[1]),
        l1c_dir = safe_dir,
        outdir = safe_dir
      )
    )
    testthat::expect_true(grepl(
      "^S2A_MSIL2A_20190723T101031_N...._R022_T32TNR_[0-9T]{15}\\.SAFE$",
      basename(sen2cor_out)
    ))
    testthat::expect_lt(run_time["elapsed"], 60)
  }
)


if (test_sen2cor) {

  testthat::test_that(
    "Tests a single Sen2Cor run", {

      testthat::expect_true(dir.exists(s2_l1c_prods[2])) # test-s2_download.R
      s2_l2a_prod2 <- list.files(
        safe_dir,
        "^S2A_MSIL2A_20190723T101031_N...._R022_T32TNS_[0-9T]{15}\\.SAFE$",
        full.names = TRUE
      )
      if (length(s2_l2a_prod2) > 0) {
        unlink(s2_l2a_prod2, recursive = TRUE)
      }
      run_time <- system.time(
        sen2cor_out <- sen2cor(
          s2_l1c_prods[2],
          use_dem = TRUE,
          gipp = list(
            DEM_Directory = demdir <- tempfile(pattern="srtm90_"),
            Generate_DEM_Output = TRUE
          ),
          outdir = safe_out_dir
        )
      )
      testthat::expect_true(dir.exists(sen2cor_out))
      testthat::expect_gt(run_time["elapsed"], 60)

      # test that DEM was downloaded
      testthat::expect_length(list.files(demdir, "^srtm_3[89]_03\\.tif$"), 2)

      # test that dem was generated
      aux_meta_ex <- raster_metadata(file.path(
        sen2cor_out,
        "GRANULE/L2A_T32TNS_A021326_20190723T101347",
        "AUX_DATA/T32TNS_20190723T101031_DEM_10m.jp2"
      ), format = "list")[[1]]
      testthat::expect_equal(aux_meta_ex$size, c("x"=10980, "y"=10980))
      testthat::expect_equal(aux_meta_ex$res, c("x"=10, "y"=10))
      testthat::expect_equal(
        aux_meta_ex$bbox,
        sf::st_bbox(
          c("xmin" = 499980, "ymin" = 5090220, "xmax" = 609780, "ymax" = 5200020),
          crs = sf::st_crs(32632)
        )
      )
      testthat::expect_equal(aux_meta_ex$outformat, "JP2OpenJPEG")

      # test raster metadata
      exp_meta_ex <- raster_metadata(file.path(
        sen2cor_out,
        "GRANULE/L2A_T32TNS_A021326_20190723T101347",
        "IMG_DATA/R10m/T32TNS_20190723T101031_B02_10m.jp2"
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
      safe_metadata <- safe_getMetadata(sen2cor_out, format = "list")
      testthat::expect_is(safe_metadata, "list")
      testthat::expect_equal(safe_metadata$prod_type, "product")
      testthat::expect_equal(safe_metadata$version, "compact")
      testthat::expect_equal(
        safe_metadata$xml_main,
        file.path(sen2cor_out,"MTD_MSIL2A.xml")
      )
      testthat::expect_equal(
        dirname(safe_metadata$xml_granules),
        file.path(sen2cor_out,"GRANULE/L2A_T32TNS_A021326_20190723T101347")
      )
      testthat::expect_equal(safe_metadata$mission, "2A")
      testthat::expect_equal(safe_metadata$level, "2A")
      testthat::expect_equal(as.Date(safe_metadata$sensing_datetime), as.Date("2019-07-23"))
      testthat::expect_equal(safe_metadata$id_orbit, "022")
      testthat::expect_equal(safe_metadata$id_tile, "32TNS")
      testthat::expect_equal(safe_metadata$tiles, "32TNS")
      testthat::expect_equal(safe_metadata$utm, "32N")
      testthat::expect_equal(safe_metadata$direction, "DESCENDING")
      testthat::expect_equal(safe_metadata$orbit_n, "22")

    }
  )


  testthat::test_that(
    "Tests a multicore Sen2Cor run", {

      testthat::expect_true(all(dir.exists(s2_l1c_prods)))
      run_time <- system.time(
        sen2cor_out <- sen2cor(
          s2_l1c_prods,
          outdir = safe_out_dir,
          use_dem = TRUE,
          parallel = TRUE,
          overwrite = TRUE
        )
      )
      testthat::expect_true(all(dir.exists(sen2cor_out)))
      testthat::expect_gt(run_time["elapsed"], 60)

      # test that dem was not generated
      testthat::expect_true(all(!file.exists(paste0(
        sen2cor_out,
        "/GRANULE/L2A_T32TN",c("R","S"),"_A021326_20190723T101347/",
        "AUX_DATA/T32TN",c("R","S"),"_20190723T101031_DEM_10m.jp2"
      ))))

      # test raster metadata
      sen2cor_out <- sort(sen2cor_out)
      exp_meta_ex <- raster_metadata(file.path(
        sen2cor_out[2],
        "GRANULE/L2A_T32TNS_A021326_20190723T101347",
        "IMG_DATA/R10m/T32TNS_20190723T101031_B02_10m.jp2"
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
      testthat::expect_is(safe_metadata, "data.table")
      testthat::expect_equal(safe_metadata$prod_type, rep("product",2))
      testthat::expect_equal(safe_metadata$version, rep("compact",2))
      testthat::expect_equal(
        safe_metadata$xml_main,
        file.path(sen2cor_out,"MTD_MSIL2A.xml")
      )
      testthat::expect_equal(
        dirname(safe_metadata$xml_granules),
        file.path(sen2cor_out,paste0("GRANULE/L2A_T32TN",c("R","S"),"_A021326_20190723T101347"))
      )
      testthat::expect_equal(safe_metadata$mission, rep("2A",2))
      testthat::expect_equal(safe_metadata$level, rep("2A",2))
      testthat::expect_equal(as.Date(safe_metadata$sensing_datetime), rep(as.Date("2019-07-23"),2))
      testthat::expect_equal(safe_metadata$id_orbit, rep("022",2))
      testthat::expect_equal(safe_metadata$id_tile, c("32TNR","32TNS"))
      testthat::expect_equal(safe_metadata$tiles, c("32TNR","32TNS"))
      testthat::expect_equal(safe_metadata$utm, rep("32N",2))
      testthat::expect_equal(safe_metadata$direction, rep("DESCENDING",2))
      testthat::expect_equal(safe_metadata$orbit_n, rep("22",2))

    }
  )

  unlink(safe_out_dir, recursive = TRUE)

}

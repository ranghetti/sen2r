context("Test Sen2Cor installation")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the installation is disabled by default if Sen2Cor is already installed.
# To perform the download of Sen2Cor, replace 'test_download = FALSE' with 'TRUE'.
test_download = FALSE

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

testthat::test_that(
  "Test Sen2Cor installation", {
    
    sen2cor_def_version <- package_version("2.8.0")
    
    if (any(test_download, length(nn(load_binpaths()$sen2cor)) == 0)) {
      unlink(sen2cor_inst_dir, recursive = TRUE)
    }
    testthat::expect_message(
      install_sen2cor(
        sen2cor_inst_dir, 
        version = sen2cor_def_version, 
        use_dem = TRUE
      ),
      regexp = gsub(
        " ", "[ \n]",
        "IMPORTANT NOTE: for backward compatibility"
      )
    )
    
    testthat::expect_true(dir.exists(dirname(load_binpaths()$sen2cor)))
    testver <- system2(load_binpaths()$sen2cor, "-h", stdout = TRUE)
    testthat::expect_true(grepl("2\\.[85]\\.[05]", testver[grep("Version", testver)]))
    
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
    xml_sen2cor_path <- paste0(
      "~/sen2cor/",
      sen2cor_def_version$major,".",sen2cor_def_version$minor,
      "/cfg/L2A_GIPP.xml"
    )
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
    xml_sen2r_path <- "~/.sen2r/sen2r_L2A_GIPP.xml"
    testthat::expect_true(file.exists(xml_sen2r_path))
    xml_sen2r_raw <- readLines(xml_sen2r_path)
    testthat::expect_true(grepl(
      "<DEM_Directory>.*srtm90</DEM_Directory>",
      xml_sen2r_raw[grepl("<DEM_Directory>", xml_sen2r_raw)]
    ))
    
    # Read default values
    gipp_1 <- read_gipp(c("dem_directory", "dem_reference"))
    testthat::expect_is(gipp_1, "list")
    testthat::expect_length(gipp_1, 2)
    testthat::expect_true(grepl("srtm90", gipp_1$dem_directory))
    
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
      reset_gipp(gipp_path = gipp_temp),
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

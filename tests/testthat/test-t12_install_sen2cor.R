context("Test Sen2Cor installation")
testthat::skip_on_cran()
testthat::skip_on_travis()

# NOTE: these tests require a high amount of time (depending on connection speed),
# so the installation is disabled by default if Sen2Cor is already installed.
# To perform the download of Sen2Cor, replace 'test_download = FALSE' with 'TRUE'.
test_download = FALSE

testthat::test_that(
  "Test that a mismatching Sen2Cor version causes error", {
    testthat::expect_error(
      install_sen2cor(version = "2.3.3"),
      regexp = "[Oo]nly Sen2Cor versions .+ are currently supported"
    )
  }
)

testthat::test_that(
  "Test Sen2Cor installation", {
    
    testthat::expect_error(
      install_sen2cor(version = "2.3.3"),
      regexp = "[Oo]nly Sen2Cor versions .+ are currently supported"
    )
    sen2cor_def_version <- package_version("2.8.0")
    sen2cor_inst_dir <- file.path(dirname(attr(load_binpaths(), "path")), "sen2cor")
    
    if (test_download | length(sen2cor_inst_dir) == 0) {
      unlink(sen2cor_inst_dir, recursive = TRUE)
      install_sen2cor(sen2cor_inst_dir)
    }
    
    testthat::expect_true(dir.exists(dirname(sen2r:::load_binpaths()$sen2cor)))
    testver <- system2(sen2r:::load_binpaths()$sen2cor, "-h", stdout = TRUE)
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
      testthat::expect_true(file.exists(paste0(
        "~/sen2cor/",
        sen2cor_def_version$major,".",sen2cor_def_version$minor,
        "/cfg/L2A_GIPP.xml"
      )))
    }
    
  }
)

testthat::test_that(
  "Test that a Sen2Cor reinstallation do not occur if it is already installed", {
    testthat::expect_message(
      install_sen2cor(),
      regexp = "Sen2Cor is already installed"
    )
  }
)

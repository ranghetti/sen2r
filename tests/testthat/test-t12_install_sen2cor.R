context("Test Sen2Cor installation")
testthat::skip_on_cran()
testthat::skip_on_travis()

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
    sen2cor_inst_dir <- file.path(
      system.file(package="sen2r"),
      paste0("sen2cor_",gsub("\\.","-",sen2cor_def_version))
    )
    unlink(sen2cor_inst_dir, recursive=TRUE)
    
    install_sen2cor()
    
    testthat::expect_true(dir.exists(sen2cor_inst_dir))
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

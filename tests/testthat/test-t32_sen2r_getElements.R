context("Test sen2r_getElements")
# testthat::skip_on_cran()
# testthat::skip_on_travis()

fs2nc_examplename <- "/path/of/the/product/S2A1C_20170603_022_32TQQ_TOA_20.tif"

testthat::test_that(
    "Tests on sen2r_getElements", {
        # Return metadata
        meta <- sen2r_getElements(fs2nc_examplename)
        testthat::expect_is(meta, "data.table")
        testthat::expect_equal(meta, data.table(type = "tile", mission = "A",
                                                level = "1C", 
                                                sensing_date = structure(17320, class = "Date"), 
                                                id_orbit = "022",
                                                extent_name = "32TQQ",
                                                prod_type = "TOA", 
                                                res = "20m", 
                                                file_ext = "tif"))
        
        meta <- sen2r_getElements(fs2nc_examplename, format = "data.frame")
        testthat::expect_is(meta, "data.frame")
        
        meta <- sen2r_getElements(fs2nc_examplename, format = "list")
        testthat::expect_is(meta, "list")
    })

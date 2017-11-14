# Tests to perform on fidolasen_s2() processing chains.

# Please notice that these tests are not though to cover all the possible
# processing types possible with this package.


context("Fidolasen S2 examples")
testthat::test_that(
  "Tests on fidolasen_s2", {
    
    library(testthat)
    
    skip_on_cran()
    skip_on_travis()
    
    ### Test 0: download required tiles
    context("Test 0: download")
    example_dir <- system.file("extdata","example_files", package="fidolasen")
    s2tiles_test1 <- c(
      "S2A_OPER_PRD_MSIL1C_PDMC_20161206T102010_R022_V20161205T101402_20161205T101402.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('37e6ef05-3598-4491-ae74-10a0685bcdc2')/\\$value",
      "S2A_OPER_PRD_MSIL1C_PDMC_20161206T101815_R022_V20161205T101402_20161205T101402.SAFE" = 
        "https://scihub.copernicus.eu/dhus/odata/v1/Products('926ba88a-1e9a-4801-abb4-5b6e01b0bbc1')/\\$value"
    )
    s2_download(s2tiles_test1, outdir=example_dir)
    
    
    ### Test 1:
    context("Test 1: [descr]")
    

  }
)

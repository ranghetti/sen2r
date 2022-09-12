message("\n---- Test s2_order() adn safe_is_online() ----")
testthat::skip_on_cran()
# testthat::skip_on_travis()
skip_full_tests()
testthat::skip_if_not(is_scihub_configured(), "SciHub credentials are not set")
testthat::skip_if_not(check_scihub_connection(service = "apihub"), "API Hub server is not reachable")
testthat::skip_if_not(check_scihub_connection(service = "dhus"), "SciHub dhus server is not reachable")

testthat::test_that(
  "Test ordering without internet", {
    s2_list_0 <- c(
      "S2A_MSIL2A_20200501T102031_N0214_R065_T32TNR_20200501T134143.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('3191642d-4818-4e23-ba54-0c0b231e13cf')/$value"
    )
    testthat::expect_error(
      httptest::without_internet({
          safe_is_online(s2_list_0, apihub = tests_apihub_path)
      }),
      regexp = gsub(
        " ", "[ \n]",
        "[Ii]nternet connection or SciHub may be down"
      )
    )
    testthat::expect_error(
      httptest::without_internet({
        s2_order(s2_list_0, apihub = tests_apihub_path)
      }),
      regexp = gsub(
        " ", "[ \n]",
        "[Ii]nternet connection or SciHub may be down"
      )
    )
  }
)

testthat::skip_if_not(check_scihub_connection(), "SciHub server is not reachable")


# NOTE: these tests are thought to test two situations, one for available products,
# another for products on LTA. However, situations could change (e.g. a product
# on LTA can be ordered and made available between two tests), so we only test
# the correct execution of the functions.
# Some tests are commented: these ones can be manually run in order to understand
# if products are online or on LTA.

testthat::test_that(
  "Test ordering already existing products", {
    # s2_list_1 <- s2_list(
    #   tile = "32TNR",
    #   time_interval = as.Date(c("2020-05-01", "2020-05-15")),
    #   orbit = "065"
    # )
    s2_list_1 <- c(
      "S2A_MSIL2A_20200501T102031_N0214_R065_T32TNR_20200501T134143.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('3191642d-4818-4e23-ba54-0c0b231e13cf')/$value",
      "S2B_MSIL2A_20200506T101559_N0214_R065_T32TNR_20200506T145433.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('c5f023bb-259b-464a-bcf3-44340e0d8595')/$value",
      "S2A_MSIL2A_20200511T102031_N0214_R065_T32TNR_20200511T130304.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('de28287d-4546-493b-8516-4cb27bb21a02')/$value"
    )
    
    # test safe_is_online()
    s2_isonline_1 <- safe_is_online(s2_list_1, apihub = tests_apihub_path)
    testthat::expect_length(s2_isonline_1, length(s2_list_1))
    testthat::expect_equal(names(s2_isonline_1), names(s2_list_1))
    testthat::expect_is(s2_isonline_1, "logical")
    testthat::expect_true(all(unique(s2_isonline_1) %in% c(TRUE, FALSE)))
    # testthat::expect_equal(as.vector(s2_isonline_1), rep(TRUE, length(s2_list_1)))
    
    # test s2_order()
    testthat::expect_message(
      s2_order_1 <- s2_order(s2_list_1, apihub = tests_apihub_path),
      "Check if products are already available"
    )
    # testthat::expect_message(
    #   s2_order_1 <- s2_order(s2_list_1, apihub = tests_apihub_path),
    #   paste0(length(s2_list_1)," Sentinel\\-2 images are already available and will not be ordered.")
    # )
    testthat::expect_true(all(names(attributes(s2_order_1)) %in% c("names","available","notordered","path")))
    testthat::expect_length(
      c(s2_order_1, attr(s2_order_1, "available"), attr(s2_order_1, "notordered")),
      length(s2_list_1)
    )
    testthat::expect_is(s2_order_1, "character")
    testthat::expect_is(attr(s2_order_1, "available"), "character")
    testthat::expect_is(attr(s2_order_1, "notordered"), "character")
    # testthat::expect_equal(names(attributes(s2_order_1)), c("names","available","notordered"))
    # testthat::expect_length(s2_order_1, 0)
    # testthat::expect_length(attr(s2_order_1, "available"), length(s2_list_1))
    # testthat::expect_length(attr(s2_order_1, "notordered"), 0)
    # testthat::expect_equal(names(attr(s2_order_1, "available")), names(s2_list_1))
  }
)

testthat::test_that(
  "Test ordering products from LTA", {
    # s2_list_2 <- s2_list(
    #   tile = "21FVC",
    #   time_interval = as.Date(c("2018-02-21", "2018-02-28")),
    #   orbit = 81
    # )
    s2_list_2 <- c(
      "S2A_MSIL1C_20180222T134641_N0206_R081_T21FVC_20180222T215312.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('4c5db929-b4cf-4709-9426-313a137c5b02')/$value",
      "S2B_MSIL1C_20180227T134629_N0206_R081_T21FVC_20180227T200327.SAFE" = 
        "https://apihub.copernicus.eu/apihub/odata/v1/Products('fa8cba66-7b8b-45aa-9038-9521d7658bad')/$value"
    )
    
    # test safe_is_online()
    s2_isonline_2 <- safe_is_online(s2_list_2, apihub = tests_apihub_path)
    testthat::expect_length(s2_isonline_2, length(s2_list_2))
    testthat::expect_equal(names(s2_isonline_2), names(s2_list_2))
    testthat::expect_is(s2_isonline_2, "logical")
    testthat::expect_true(all(unique(s2_isonline_2) %in% c(TRUE, FALSE)))
    # testthat::expect_equal(as.vector(s2_isonline_2), rep(FALSE, length(s2_list_2)))
    
    # test s2_order()
    testthat::expect_message(
      s2_order_2 <- s2_order(s2_list_2, export_prodlist = tempdir(), apihub = tests_apihub_path),
      "Check if products are already available"
    )
    
    # Using the "reorder" argument on s2_order works - we test by running 
    # "the same" command repeatetly, hoping that the statuses do not change
    # in a couple of seconds
    s2_order_3 <- s2_order(attr(s2_order_2, "path"), apihub = tests_apihub_path, reorder = FALSE)
    s2_order_4 <- s2_order(attr(s2_order_2, "path"), apihub = tests_apihub_path, reorder = TRUE)
    testthat::expect_equal(
      attr(s2_order_2, "available"), 
      attr(s2_order_3, "available"),
      attr(s2_order_4, "available")
    )
    testthat::expect_equal(
      attr(s2_order_2, "ordered"), 
      attr(s2_order_3, "ordered"),
      attr(s2_order_4, "ordered")
    )
    testthat::expect_equal(
      attr(s2_order_2, "notordered"), 
      attr(s2_order_3, "notordered"),
      attr(s2_order_4, "notordered")
    )
    # testthat::expect_message(
    #   s2_order_2 <- s2_order(s2_list_2, apihub = tests_apihub_path),
    #   paste0(
    #     length(s2_list_2)," of ",length(s2_list_2)," Sentinel\\-2 images were ",
    #     "correctly ordered\\. You can check at a later time if the ordered ",
    #     "products were made available using the command:"
    #   )
    # )
    testthat::expect_length(
      c(s2_order_2, attr(s2_order_2, "available"), attr(s2_order_2, "notordered")),
      length(s2_list_2)
    )
    testthat::expect_true(all(names(attributes(s2_order_2)) %in% c("names","available","notordered","path")))
    testthat::expect_is(s2_order_2, "character")
    testthat::expect_is(attr(s2_order_2, "available"), "character")
    testthat::expect_is(attr(s2_order_2, "notordered"), "character")
    if (!is.null(attr(s2_order_2, "path"))) {
      file_content <- jsonlite::fromJSON(attr(s2_order_2, "path"))
      testthat::expect_is(file_content, "list")
      # starting from v. 1.2.2, length is equal to 3 because a named list is saved
      testthat::expect_length(file_content, 3) 
      testthat::expect_equal(sum(lengths(file_content)), length(s2_list_2))
      # Compare all 3 "components" of the results (ordered, notordered and available)
      testthat::expect_equivalent(
        unlist(as.character(file_content$available)), 
        attr(s2_order_2, "available")
      )
      testthat::expect_equivalent(
        unlist(as.character(file_content$ordered)), 
        s2_order_2
      )
      testthat::expect_equivalent(
        unlist(as.character(file_content$notordered)),
        attr(s2_order_2, "notordered")
      )
    }
    # testthat::expect_equal(names(attributes(s2_order_2)), c("names","available","notordered", "path"))
    # testthat::expect_length(s2_order_2, length(s2_list_2))
    # testthat::expect_length(attr(s2_order_2, "available"), 0)
    # testthat::expect_length(attr(s2_order_2, "notordered"), 0)
    # testthat::expect_equal(names(s2_order_2), names(s2_list_2))
  }
)

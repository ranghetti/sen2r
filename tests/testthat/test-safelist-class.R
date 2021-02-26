message("\n---- Test s2 formats ----")
testthat::skip_on_cran()
# testthat::skip_on_travis()

s2_safelist_0 <- s2_list(
  tile = "32TNR",
  time_interval = as.Date(c("2017-05-01", "2017-05-31")),
  orbit = "065",
  apihub = tests_apihub_path
)

testthat::test_that(
  "Check s2 formats coherence", {
    testthat::expect_is(s2_safelist_0, "safelist")

    testthat::expect_equal(length(s2_safelist_0), 3)
    testthat::expect_equal(
      sort(names(attributes(s2_safelist_0))), 
      c("class","clouds","footprint","id_orbit","id_tile","ingestion_datetime",
        "level","mission","names","online","sensing_datetime","uuid")
    )
  }
)

testthat::test_that(
  "Check conversion to character", {
    s2_char_1 <- as(s2_safelist_0, "character")

    testthat::expect_equal(length(s2_char_1), length(s2_safelist_0))
    testthat::expect_equal(s2_char_1, s2_safelist_0[1:length(s2_safelist_0)])
    testthat::expect_equal(names(s2_char_1), names(s2_safelist_0))

  }
)

testthat::test_that(
  "Check conversion to data.frame / data.table", {
    s2_df_1 <- as(s2_safelist_0, "data.frame")
    s2_dt_1 <- as(s2_safelist_0, "data.table")
    
    testthat::expect_equal(as.character(s2_df_1$name), names(s2_safelist_0))
    testthat::expect_equal(nrow(s2_df_1), length(s2_safelist_0))
    testthat::expect_equal(as.data.table(s2_df_1), s2_dt_1)
    testthat::expect_equal(as.vector(s2_safelist_0), as.character(s2_df_1$url))
    
  }
)

testthat::test_that(
  "Check conversion to sf", {
    s2_sf_1 <- as(s2_safelist_0, "sf")
    
    testthat::expect_is(s2_sf_1, "sf")
    testthat::expect_equal(as.character(s2_sf_1$name), names(s2_safelist_0))
    testthat::expect_equal(nrow(s2_sf_1), length(s2_safelist_0))

  }
)

testthat::test_that(
  "Check reading from a new JSON order file (v. > 1.2.1.9007)", {
    writeLines(
      toJSON(
        list(
          "ordered" = NULL,
          "available" = as.list(s2_safelist_0),
          "notordered" = NULL
        ), pretty = TRUE),
      prodlist_path_2 <- tempfile(fileext = ".json")
    )
    s2_safelist_2 <- as(prodlist_path_2, "safelist")

    testthat::expect_equal(as.character(s2_safelist_2), as.character(s2_safelist_0))

  }
)

testthat::test_that(
  "Check reading from an old JSON order file (v. < 1.2.1.9007)", {
    writeLines(
      toJSON(as.list(s2_safelist_0), pretty = TRUE),
      prodlist_path_3 <- tempfile(fileext = ".json")
    )
    s2_safelist_3 <- as(prodlist_path_3, "safelist")
    
    testthat::expect_equal(as.character(s2_safelist_3), as.character(s2_safelist_0))
    
  }
)

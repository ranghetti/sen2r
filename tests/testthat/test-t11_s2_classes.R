context("Test s2 formats")
testthat::skip_on_cran()
testthat::skip_on_travis()

s2_s2list_0 <- s2_list(
  tile = "32TNR",
  time_interval = as.Date(c("2017-05-01", "2017-05-31")),
  orbit = "065"
)
s2_s2dt_0 <- s2_list(
  tile = "32TNR",
  time_interval = as.Date(c("2017-05-01", "2017-05-31")),
  orbit = "065",
  output_type = "data.table"
)

testthat::test_that(
  "Check s2 formats coherence", {
    testthat::expect_is(s2_s2dt_0, "s2dt")
    testthat::expect_is(s2_s2list_0, "s2list")
    
    testthat::expect_equal(nrow(s2_s2dt_0), length(s2_s2list_0))
    testthat::expect_equal(
      sort(names(s2_s2dt_0)), 
      c("ccov","date","name","online","orbitid","proclev","proctime","sensor","tileid","url")
    )
    testthat::expect_equal(
      sort(names(attributes(s2_s2list_0))), 
      c("ccov","class","date","names","online","orbitid","proclev","proctime","sensor","tileid")
    )
  }
)

testthat::test_that(
  "Check conversion to character", {
    s2_char_1 <- as(s2_s2dt_0, "character")
    s2_char_2 <- as.character(s2_s2list_0)
    
    testthat::expect_equal(length(s2_char_1), nrow(s2_s2dt_0))
    testthat::expect_equal(length(s2_char_2), nrow(s2_s2dt_0))
    testthat::expect_equal(s2_char_1, s2_char_2)
    testthat::expect_equal(names(s2_char_1), s2_s2dt_0$name)
    testthat::expect_equal(as.character(s2_char_1), s2_s2dt_0$url)
    
  }
)

testthat::test_that(
  "Check conversion to data.frame / data.table", {
    s2_df_1 <- as(s2_s2dt_0, "data.frame")
    s2_df_2 <- as.data.table(s2_s2list_0)
    
    testthat::expect_equal(nrow(s2_df_1), nrow(s2_s2dt_0))
    testthat::expect_equal(nrow(s2_df_2), nrow(s2_s2dt_0))
    testthat::expect_equal(as.data.table(s2_df_1), s2_df_2)
    testthat::expect_equal(s2_df_1$name, names(s2_s2list_0))
    testthat::expect_equal(s2_df_2$name, names(s2_s2list_0))
    testthat::expect_equal(as.vector(s2_s2list_0), s2_df_1$url)
    testthat::expect_equal(as.vector(s2_s2list_0), s2_df_2$url)
    
  }
)

testthat::test_that(
  "Check conversion from/to s2dt/list", {
    s2_s2list_1 <- as(s2_s2dt_0, "s2list")
    s2_s2dt_1 <- as(s2_s2list_0, "s2dt")
    
    testthat::expect_equal(s2_s2list_1, s2_s2list_0)
    testthat::expect_equal(s2_s2dt_1, s2_s2dt_0)
    
  }
)

testthat::test_that(
  "Check reading from a JSON order file", {
    writeLines(
      toJSON(as.list(s2_s2list_0), pretty = TRUE),
      prodlist_path <- tempfile(fileext = ".json")
    )
    s2_s2list_2 <- as.s2list(prodlist_path)
    s2_s2dt_2 <- as.s2dt(prodlist_path)
    
    testthat::expect_equal(as.character(s2_s2list_2), as.character(s2_s2list_0))
    testthat::expect_equal(s2_s2dt_2[,list(name,url)], s2_s2dt_0[,list(name,url)])
    testthat::expect_length(s2_s2dt_2, 2) # only name and url
    
  }
)

context("Test st_crs2")

testthat::test_that(
  "st_crs2, input EPSG", {
    testthat::expect_equal(st_crs2(9)[["epsg"]], 32609)
    testthat::expect_equal(st_crs2("EPSG:32609")[["epsg"]], 32609)
    testthat::expect_equal(st_crs2("9N")[["epsg"]], 32609)
    testthat::expect_equal(st_crs2("9S")[["epsg"]], 32709)
  }
)

testthat::test_that(
  "st_crs2, input spatial file path", {
    raster_path <- system.file(
      "extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif", 
      package="sen2r"
    )
    vector_path <- system.file(
      "extdata/vector/barbellino.geojson", 
      package="sen2r"
    )
    testthat::expect_equal(st_crs2(raster_path)[["epsg"]], 32632)
    testthat::expect_equal(st_crs2(vector_path)[["epsg"]], 32632)
    testthat::expect_equal(st_crs2(stars::read_stars(raster_path))[["epsg"]], 32632)
    testthat::expect_equal(st_crs2(raster::raster(raster_path))[["epsg"]], 32632)
    testthat::expect_equal(st_crs2(sf::read_sf(vector_path))[["epsg"]], 32632)
    testthat::expect_equal(st_crs2(as(sf::read_sf(vector_path), "Spatial"))[["epsg"]], 32632)
  }
)

testthat::test_that(
  "st_crs2, generics and errors", {
    testthat::expect_error(st_crs2("wrong"), "invalid crs\\: wrong")
    testthat::expect_equal(st_crs2(NULL), sf::st_crs(NA))
    testthat::expect_equal(st_crs2(NA), sf::st_crs(NA))
    testthat::expect_equal(st_crs2(), sf::st_crs(NA))
  }
)


testthat::skip_on_cran()
testthat::skip_on_travis()

testthat::test_that(
  "st_crs2, input WKT", {
    wkt_32n <- st_as_text_2(st_crs(32609))
    writeLines(wkt_32n, wkt_32n_path <- tempfile())
    testthat::expect_equal(st_crs2(wkt_32n)[["epsg"]], 32609)
    testthat::expect_equal(st_crs2(wkt_32n_path)[["epsg"]], 32609)
  }
)

testthat::test_that(
  "st_crs2, input PROJ.4", {
    testthat::expect_equal(st_crs2("+init=epsg:32609")[["epsg"]], 32609)
    gdal_version <- package_version(gsub(
      "^.*GDAL ([0-9\\.]+)[^0-9].*$", "\\1",
      system(paste0(load_binpaths("gdal")$gdalinfo," --version"), intern = TRUE)
    )) # checking GDAL >=3 instead than PROJ >= 6 for simplicity
    if (gdal_version >= 3) {
      testthat::expect_warning(
        st_crs2("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs"),
        "Using PROJ\\.4 strings is deprecated with PROJ >\\= 6"
      )
    } else {
      testthat::expect_equal(
        st_crs2("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")[["epsg"]],
        32609
      )
    }
  }
)

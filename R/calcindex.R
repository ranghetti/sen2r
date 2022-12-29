#' @name calcindex_raster
#' @rdname calcindex
#' @title Compute index using `raster` or `stars` features
#' @description Internal functions used to compute spectral indices and
#'  thumbnails through `raster` or `stars` features.
#' @param x Input file path, or `RasterLayer` (in `calcindex_raster()`) or
#'  `stars` (in `calcindex_stars()`).
#' @param sel_formula Formula used to compute output raster
#'  (specific formats are used - documentation will be improved).
#' @param out_file Output file path.
#' @param NAflag (optional)
#' @param sel_format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default "GTiff".
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param datatype (optional) Numeric datatype of the output rasters
#'  (see `s2_calcindices()`).
#' @param bigtiff (optional) Logical: if TRUE, the creation of a BigTIFF is
#'  forced (default is FALSE).
#'  This option is used only in the case a GTiff format was chosen.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param minrows (optional) parameter passed to `blockSize()`.
#' @return NULL (the function is called for its side effects)
#' @importFrom raster blockSize brick getValues writeStart writeStop writeValues
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @author Luigi Ranghetti, phD (2020)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal

calcindex_raster <- function(
  x,
  sel_formula,
  out_file,
  NAflag = -32768,
  sel_format = "GTiff",
  compress = "LZW",
  datatype = "Int32",
  bigtiff = FALSE,
  overwrite = FALSE,
  minrows = NULL
) {

  out <- if (any(
    inherits(x, "RasterBrick"),
    inherits(x, "RasterStack")
  )) {x[[1]]} else if (inherits(x, "RasterLayer")) {x} else {brick(x)}
  x <- if (any(
    inherits(x, "RasterLayer"),
    inherits(x, "RasterStack"),
    inherits(x, "RasterBrick")
  )) {x} else {brick(x)}
  suppress_warnings(
    out <- writeStart(
      out, out_file,
      NAflag = NAflag,
      datatype = convert_datatype(datatype),
      format = ifelse(sel_format=="VRT", "GTiff", sel_format),
      if (sel_format %in% c("GTiff","VRT")) {
        options = c(
          paste0("COMPRESS=",compress),
          "TILED=YES",
          if (bigtiff==TRUE) {"BIGTIFF=YES"}
        )
      },
      overwrite = overwrite
    ),
    "NOT UPDATED FOR PROJ >\\= 6"
  )

  # x <- brick(infiles)
  if (is.null(minrows)) {
    bs <- blockSize(out, n = 1)
  } else {
    bs <- blockSize(out, minrows = minrows)
  }
  if (all(inherits(stdout(), "terminal"), interactive())) {
    pb <- txtProgressBar(0, bs$n, style = 3)
  }
  for (i in seq_len(bs$n)) {
    # message("Processing chunk ", i, " of ", bs$n)
    v <- getValues(x, row = bs$row[i], nrows = bs$nrows[i])
    if (grepl("^Float", datatype)) {
      if (!is.numeric(v)) {
        v <- apply(v, 2, as.numeric)
      }
      v_out <- eval(parse(text = sel_formula))
    } else {
      v_out <- round(eval(parse(text = sel_formula)))
    }
    # m <- getValues(y, row = bs$row[i], nrows = bs$nrows[i])
    out <- writeValues(out, v_out, bs$row[i])
    if (all(inherits(stdout(), "terminal"), interactive())) {
      setTxtProgressBar(pb, i)
    }
  }
  if (all(inherits(stdout(), "terminal"), interactive())) {
    message("")
  }
  out <- writeStop(out)
  NULL
}


#' @name calcindex_stars
#' @rdname calcindex
#' @importFrom stars read_stars write_stars

calcindex_stars <- function(
  x,
  sel_formula,
  out_file,
  NAflag = -32768,
  sel_format = "GTiff",
  compress = "LZW",
  datatype = "Int16",
  bigtiff = FALSE,
  overwrite = FALSE
) {
  x_in <- if (inherits(x, "stars")) {x} else {read_stars(x, proxy = TRUE)}
  # x_out <- st_apply(x_in, c("x", "y"), function(v) {
  #   eval(parse(text = sel_formula))
  # })
  x_out <- eval(parse(text = paste0("st_apply(x_in, c('x', 'y'), function(v) {",sel_formula,"})") ))
  suppress_warnings(
    write_stars(
      x_out, out_file,
      NA_value = NAflag,
      type = datatype,
      format = ifelse(sel_format=="VRT", "GTiff", sel_format),
      options = c(
        paste0("COMPRESS=",compress),
        "TILED=YES",
        if (bigtiff==TRUE) {"BIGTIFF=YES"}
      ),
      overwrite = overwrite,
      # chunk_size was set like using raster (half of the stars default)
      chunk_size = c(dim(x_out)[1], floor(2.5e+07/2/dim(x_out)[1]))
    ),
    "NOT UPDATED FOR PROJ >\\= 6"
  )
  NULL
}

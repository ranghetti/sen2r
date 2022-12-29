#' @title Add/remove suffixes for split tiles
#' @description `add_tile_suffix()` adds specific suffixes to tile IDs
#'  in order to distinguish tiled filenames referring to different original
#'  SAFE products.
#' @details In some sporadic cases, a tiled Sentinel-2 image is split in two 
#'  SAFE products (see e.g. products
#'  [`S2A_MSIL1C_20200408T101021_N0209_R022_T32TNL_20200408T153254`](https://storage.cloud.google.com/gcp-public-data-sentinel-2/tiles/32/T/NL/S2A_MSIL1C_20200408T101021_N0209_R022_T32TNL_20200408T153254.SAFE/GRANULE/L1C_T32TNL_A025044_20200408T101022/QI_DATA/T32TNL_20200408T101021_PVI.jp2) and
#'  [`S2A_MSIL1C_20200408T101021_N0209_R022_T32TNL_20200408T171107`](https://storage.cloud.google.com/gcp-public-data-sentinel-2/tiles/32/T/NL/S2A_MSIL1C_20200408T101021_N0209_R022_T32TNL_20200408T171107.SAFE/GRANULE/L1C_T32TNL_A025044_20200408T101923/QI_DATA/T32TNL_20200408T101021_PVI.jp2)).
#'  This split, probably a consequence of the division of the whole orbit image, 
#'  creates ambiguity in the association among SAFE images and `sen2r` products,
#'  since the sen2r naming convention is not sufficient to manage them as separate
#'  products.
#'  So, in the definition of the filenames of intermediate tiled products
#'  (output of `s2_translate()`) it is necessary to add a suffix to be able to
#'  manage them separately and then merge them in `s2_merge()`.
#'  A lowercase letter ("a" and "b", but potentially "a" to "z") is used.
#'  Functions `add_tile_suffix()` and `remove_tile_suffix()` are used in the
#'  `sen2r()` main code as a workaround.
#' @param paths Paths of the input tiled products
#' @param suffix Character (1-length): if provided, the specified suffix is
#'  appended to the tile ID of each path;
#'  if not provided (default), a sequential suffix is appended only to the tile 
#'  ID of the duplicated paths.
#' @return The input paths with/without the tile suffix.
#'
#' @author Luigi Ranghetti, phD (2020)
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal
#' @examples
#' safe_names <- c(
#'   "S2A_MSIL2A_20200408T101021_N0214_R022_T32TNK_20200408T175711.SAFE",
#'   "S2A_MSIL2A_20200408T101021_N0214_R022_T32TNL_20200408T175711.SAFE",
#'   "S2A_MSIL2A_20200408T101021_N0214_R022_T32TNL_20200408T161405.SAFE"
#' )
#' prod_names <- safe_shortname(safe_names, ext = ".tif", allow_duplicated = TRUE)
#' ( prod_names_univoc <- sen2r:::add_tile_suffix(prod_names) )
#' ( prod_names_custom <- sen2r:::add_tile_suffix(prod_names, "a") )
#' sen2r:::remove_tile_suffix(prod_names_univoc)

add_tile_suffix <- function(paths, suffix) {
  if (missing(suffix)) {
    # Default behaviour: add default suffixes to duplicated paths
    for (sel_upath in names(table(paths))[table(paths)>1]) {
      n_paths <- sum(paths==sel_upath)
      paths[paths==sel_upath] <- sapply(seq_len(n_paths), function(i) {
        gsub(
          "(S2[AB][12][AC]\\_[0-9]{8}\\_[0-9]{3})\\_([0-9]{2}[A-Z]{3})\\_([^\\_\\.]+\\_[126]0\\.?[^\\_]*$)",
          paste0("\\1_\\2",letters[i],"_\\3"),
          sel_upath
        )
      })
    }
    paths
  } else {
    # Custom behaviour: add specific suffix to all the paths
    if (is.na(suffix)) {suffix <- ""}
    gsub(
      "(S2[AB][12][AC]\\_[0-9]{8}\\_[0-9]{3})\\_([0-9]{2}[A-Z]{3})\\_([^\\_\\.]+\\_[126]0\\.?[^\\_]*$)",
      paste0("\\1_\\2",suffix,"_\\3"),
      paths
    )
  }
}


#' @name remove_tile_suffix
#' @rdname add_tile_suffix
#' @description `remove_tile_suffix()` removes existing suffixes from tile IDs.

remove_tile_suffix <- function(paths) {
  accepted_suffixes <- "[a-z]?" # Define here the accepted suffixes
  sapply(paths, function(p) {
    gsub(
      paste0(
        "(S2[AB][12][AC]\\_[0-9]{8}\\_[0-9]{3})\\_([0-9]{2}[A-Z]{3})",
        accepted_suffixes,
        "\\_([^\\_\\.]+\\_[126]0\\.?[^\\_]*$)"
      ),
      "\\1_\\2_\\3",
      p
    )
  }, simplify = TRUE, USE.NAMES = FALSE)
}


#' @name extract_tile_suffix
#' @rdname add_tile_suffix
#' @description `extract_tile_suffix()` extracts suffixes from input paths.

extract_tile_suffix <- function(paths) {
  accepted_suffixes <- "[a-z]?" # Define here the accepted suffixes
  sapply(paths, function(p) {
    gsub(
      paste0(
        "^.*S2[AB][12][AC]\\_[0-9]{8}\\_[0-9]{3}\\_[0-9]{2}[A-Z]{3}",
        "(",accepted_suffixes,")",
        "\\_[^\\_\\.]+\\_[126]0\\.?[^\\_]*$"
      ),
      "\\1",
      p
    )
  }, simplify = TRUE, USE.NAMES = FALSE)
}

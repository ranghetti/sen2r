#' @title Rename products using a shorten convention
#' @description This function renames a Sentinel-2 product in order to
#'  obtain shorten names. See the details for the structure of the
#'  adopted schema. The function applies only to product names (not
#'  to single granule names), since it is thought to be applied to
#'  entire products.
#' @details [ESA Sentinel-2 naming convention](
#'  https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/naming-convention)
#'  is particularly long-winded; moreover, the convention changed after
#'  December 6th 2016, causing products to follow two different schemes.
#'
#'  The convention here adopted follows this schema:
#'
#'  `S2mll_yyyymmdd_rrr_ttttt`
#'
#'  where:
#'  * `S2mll` shows the mission ID (`S2A` or `S2B`) and the product level
#'      (`1C` or `2A`);
#'  * `yyyymmdd` is the sensing date (e.g. `20170603``) ; the hour is skipped,
#'      since a single sensor can not pass two times in a day on the same tile);
#'  * `rrr` is the relative orbit number (e.g. `022`);
#'  * `ttttt` is the tile number (e.g. `32TQQ`).
#'
#'  Products which contains only one tile (date \geq 2016-12-06 or
#'  < 2016-12-06 but containing only one tile) follows entirely this convention;
#'  for products with more than one tile (or that can not be checked), the field
#'  `ttttt` is replaced by:
#'  * `uuxxx` if the UTM zone is unique (where `uu` is the UTM zone, and `xxx`
#'      is a random numeric sequence), e.g. `32876`;
#'  * `xxxxx` if the UTM zone is not unique or it it can not be checked (being
#'      `xxxxx` a random literal A-Z sequence).
#'
#'  Since the tile number is composed by two numeric and three literal character,
#'  these two conventions (one composed by five numeric and the other by five
#'  literal characters) ensure that these three cases can be distinguished.

#'
#' @param input_name 'character' input Sentinel-2 product name: it can be both
#'  a complete path or a simple file name. If the file exists, the product
#'  is scanned to retrieve the tiles list and UTM zones; otherwise the
#'  basename is simply translated in he short naming convention.
#' @return 'character' output product name
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate import import_builtins py_to_r use_python

s2_shortname <- function(prod_name) {

}

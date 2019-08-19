#' @title Rename products using a shorten convention
#' @description This function renames a Sentinel-2 product in order to
#'  obtain shorten names. See the details for the structure of the
#'  adopted schema (named "sen2r naming convention").
#'  The function applies only to product names (not to single granule
#'  names), since it is thought to be applied to entire products.
#' @details [ESA Sentinel-2 naming convention](
#'  https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/naming-convention)
#'  is particularly long-winded; moreover, the convention changed after
#'  December 6th 2016, causing products to follow two different schemes.
#'
#'  The convention here adopted, named "sen2r naming convention",
#'  follows this schema:
#'
#'  `S2mll_yyyymmdd_rrr_ttttt_ppp_rr.fff`
#'
#'  where:
#'  * `S2mll` (length: 5) shows the mission ID (`S2A` or `S2B`) and the product level
#'      (`1C` or `2A`);
#'  * `yyyymmdd` (length: 8) is the sensing date (e.g. `20170603` for 2017-06-03) ; the
#'      hour is skipped, since a single sensor can not pass two times in a
#'      day on the same tile);
#'  * `rrr` (length: 3) is the relative orbit number (e.g. `022`);
#'  * `ttttt` (length: 5) is the tile number (e.g. `32TQQ`);
#'  * `ppp` (length: 3) is the output product, being one of these:
#'      _for level 1C:_
#'      - `TOA`: 13-bands Top-Of-Atmosphere Reflectance;
#'      _for level 2A:_
#'      - `BOA`: 13-bands Bottom-Of-Atmosphere Reflectance;
#'      - `TCI`: True Colour Image (3-band RGB 8-bit image);
#'      - `AOT`: Aerosol Optical Thickness;
#'      - `WVP`: Water Vapour;
#'      - `SCL`: Scene Classification Map;
#'      - `CLD`: Quality Indicators for cloud probabilities;
#'      - `SNW`: Quality Indicators for snow probabilities;
#'      - `VIS`: TODO Visibility (used for AOT);
#'  * `rr` (length: 2) is the original minimum spatial resolution in metres
#'      (10, 20 or 60);
#'  * `fff` (length: variable, generally 3) is the file extension.
#'
#' @param prod_name Input Sentinel-2 product name: it can be both
#'  a complete path or a simple file name. If the file exists, the product
#'  is scanned to retrieve the tiles list and UTM zones; otherwise the
#'  basename is simply translated in he short naming convention.
#' @param prod_type (optional) Output product (default: `TOA` for L1C, `BOA`
#'  for L2A); see the details for the list of accepted products.
#' @param ext (optional) Extension of the output filename (default: none).
#' @param res (optional) Spatial resolution (one between '10m', '20m' or '60m');
#'  default is '10m'. Notice that, choosing '10m' or '20m', bands with lower
#'  resolution will be rescaled to `res`. Band 08 is used with `res = '10m'`,
#'  band 08A with `res = '20m'` and `res = '60m'`.
#' @param tiles (optional) Character vector with the desired output tile IDs 
#'  (if specified IDs are not present in the input SAFE product, they are not
#'  produced). Default (NA) is to consider all the found tiles.
#' @param force_tiles (optional) Logical: if FALSE (default), only already 
#'  existing tiles (specified using the argument `tiles`) are used;
#'  if TRUE, all specified tiles are considered. 
#'  It takes effect only if `tiles` is not NA.
#' @param full.name Logical value: if TRUE (default), all the input path
#'  is maintained (if existing); if FALSE, only basename is returned.
#' @param set.seed (internal parameter) Logical value: if TRUE, the
#'  generation of random tile elements is univocal (this is useful not to
#'  assign different names to the same element); if FALSE, the generation
#'  is completely random (this is useful not to assign the same name to
#'  products with the same names (e.g. product with old name downloaded in
#'  different moments which contains different subsets of tiles).
#'  Default value is TRUE if the file exists locally, FALSE if not.
#' @param multiple_names Logical: if TRUE, multiple names are returned in case
#'  the SAFE product contains more than one tile; if FALSE (default), a random
#'  tile element is used.
#' @param abort Logical parameter: if TRUE, the function aborts in case
#'  `prod_type` is not recognised; if FALSE (default), a warning is shown.
#' @return Output product name
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom digest digest

#' @examples
#' # Define product names
#' s2_compactname_example <- file.path(
#'   "/non/existing/path",
#'   "S2A_MSIL1C_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE")
#' s2_oldname_example <-
#'   "S2A_OPER_PRD_MSIL1C_PDMC_20160809T051732_R022_V20150704T101337_20150704T101337.SAFE"
#' s2_existing_example <- "/replace/with/the/main/path/of/existing/product/with/oldname/convention"
#'
#' # With compact names, it works also without scanning the file
#' safe_shortname(s2_compactname_example, ext="tif")
#'
#' # With old names, without scanning the file the id_tile is not retrieved,
#' # so the tile filed is replaced with a random sequence
#' unlist(lapply(rep(s2_oldname_example,5), safe_shortname, full.name=FALSE))
#'
#' \dontrun{
#' # If the file exists locally, the tile is retrieved from the content
#' # (if unique; if not, a random sequence is equally used, but it is
#' # always the same for the same product)
#' safe_shortname(s2_existing_example, full.name=FALSE)
#' }


safe_shortname <- function(prod_name, prod_type=NULL, ext=NULL, res="10m", 
                         tiles=NA, force_tiles=FALSE, full.name=TRUE, 
                         set.seed=NA, multiple_names=FALSE, abort=FALSE) {
  
  # elements used by the function
  needed_metadata <- c("tiles","utm","mission","level","sensing_datetime","id_orbit","id_tile")
  # elements mandatory to convert filename
  mandatory_metadata <- c("mission","level","sensing_datetime","id_orbit")
  
  s2_metadata <- tryCatch({
    safe_getMetadata(prod_name, info=needed_metadata)
  }, error = function(e) {
    safe_getMetadata(prod_name, info="nameinfo")[needed_metadata]
  })
  
  if (is.na(set.seed)) {
    set.seed <- tryCatch({
      safe_getMetadata(prod_name, info=needed_metadata)
      TRUE
    }, error = function(e) {FALSE})
  }

  # check that necessary elements do not miss
  if (any(sapply(s2_metadata[mandatory_metadata],is.null))) {
    print_message(
      type="error",
      "Some of the required elements ('",
      paste(mandatory_metadata[sapply(s2_metadata[mandatory_metadata],is.null)], collapse="' '"),
      "') can not be retreved from the input filename.")
  }
  
  # Check "tiles" argument
  if (is.null(tiles)) {tiles <- NA}
  if (anyNA(tiles)) {tiles <- NA} else if (all(tiles=="")) {tiles <- NA}
  
  # Define tiles ID to use
  # if tile_id (= tile as retrieved from product name) exists, 
  # consider it; otherwise, use tiles (existing tile list)
  prod_tiles <- if (!is.null(s2_metadata$id_tile)) {
    s2_metadata$id_tile
  } else {
    s2_metadata$tiles
  }
  sel_tiles <- if (!anyNA(tiles)) {
    # if tiles argument was specified, consider it
    if (force_tiles==TRUE) {
      tiles
    } else {
      prod_tiles[prod_tiles %in% tiles]
    }
  } else {
    prod_tiles
  }
  
  # if sel_tiles is missing or multiple tiles must be reduced to one,
  # compute a random tile ID
  if (is.null(sel_tiles) | length(sel_tiles)>1 & multiple_names==FALSE) {
    
    # set seed if requested
    if (set.seed) {
      base_str <- paste(unlist(s2_metadata[needed_metadata]),collapse="")
      sel_seed <- strtoi(substr(digest(base_str, algo='xxhash32'),2,8), base=16)
      set.seed(sel_seed)
    }
    
    if (length(s2_metadata$utm)==1) {
      # otherwise, use convention 1 if UTM zone is unique, 2 if not
      sel_tiles <- paste0(s2_metadata$utm,
                                    str_pad2(sample(1000,1)-1,3,"left","0"))
    } else {
      sel_tiles <- paste0(paste(LETTERS[sample(26,2,replace=TRUE)],collapse=""),
                                    str_pad2(sample(1000,1)-1,3,"left","0"))
    }
    
  }
  
  # select default product type if missing
  if (is.null(prod_type)) {
    if (s2_metadata$level=="1C") {
      prod_type <- "TOA"
    } else if (s2_metadata$level=="2A") {
      prod_type <- "BOA"
    }
  }
  
  # check prod_type
  prod_type_accepted_values <- list(
    "1C" = c("TOA"),
    "2A" = c("BOA","TCI","AOT","WVP","SCL","CLD","SNW","VIS"))
  message_type <- ifelse(abort==TRUE, "error", "warning")
  if (!prod_type %in% c(prod_type_accepted_values[[s2_metadata$level]],"xxx")) {
    print_message(
      type=message_type,
      "\"prod_type\" value is not recognised (for level ",s2_metadata$level,
      " accepted values are '",
      paste(prod_type_accepted_values[[s2_metadata$level]], collapse="', '"),
      "'; use 'xxx' for a generic string in order not to show this warning).")
  }
  
  # check res
  if (!res %in% c("10m","20m","60m")) {
    print_message(
      type=message_type,
      "\"res\" value is not recognised (accepted values are '10m', '20m' ",
      "and '60m').")
  }
  
  # compose name
  short_name <- with(
    s2_metadata,
    paste0("S",mission,level,"_",
           strftime(s2_metadata$sensing_datetime,"%Y%m%d"),"_",
           id_orbit,"_",sel_tiles,"_",prod_type,"_",substr(res,1,2))
  )
  
  # check name length
  if (any(nchar(short_name)!=31)) {
    print_message(
      type="warning",
      "Length of shortname is wrong (probably \"prod_type\" or \"res\" ",
      "has wrong length).")
  }
  
  # add dirname and extension
  out_name <- if (dirname(prod_name)!="." & full.name) {
    file.path(dirname(prod_name), short_name)
  } else {
    short_name
  }
  if (!is.null(ext)) {
    out_name <- paste0(out_name,".",gsub("^\\.","",ext))
  }
  
  return(out_name)
  
}

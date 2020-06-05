#' @title Rename products using a shorten convention
#' @description This function renames a Sentinel-2 product in order to
#'  obtain shorten names. See the details for the structure of the
#'  adopted schema (named "sen2r naming convention").
#'  The function applies only to compact product names (not to single granule
#'  names), since it is thought to be applied to entire products.
#'  Old long names are no more supported.
#' @details [ESA Sentinel-2 naming convention](
#'  https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/naming-convention)
#'  is particularly long-winded.
#'  So, the convention here adopted, named "sen2r naming convention",
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
#' @param prod_name Input Sentinel-2 product name (it is not required 
#'  that the file exists).
#' @param prod_type (optional) Output product (default: `TOA` for L1C, `BOA`
#'  for L2A); see the details for the list of accepted products.
#' @param ext (optional) Extension of the output filename (default: none).
#' @param res (optional) Spatial resolution (one between '10m', '20m' or '60m');
#'  default is '10m'. Notice that, choosing '10m' or '20m', bands with lower
#'  resolution will be rescaled to `res`. Band 08 is used with `res = '10m'`,
#'  band 08A with `res = '20m'` and `res = '60m'`.
#' @param tiles Deprecated (no more used).
#' @param force_tiles Deprecated (no more used).
#' @param full.name Logical value: if TRUE (default), all the input path
#'  is maintained (if existing); if FALSE, only basename is returned.
#' @param set.seed Deprecated (no more used).
#' @param multiple_names Deprecated (no more used).
#' @param abort Logical parameter: if TRUE, the function aborts in case
#'  `prod_type` is not recognised; if FALSE (default), a warning is shown.
#' @return Output product name
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @export

#' @examples
#' safe_shortname("S2A_MSIL1C_20170603T101031_N0205_R022_T32TQQ_20170603T101026.SAFE", ext="tif")


safe_shortname <- function(
  prod_name, 
  prod_type = NULL, 
  ext = NULL, 
  res = "10m", 
  tiles = NULL, # deprecated
  force_tiles = NULL, # deprecated
  full.name = TRUE, 
  set.seed = NULL, # deprecated
  multiple_names = NULL,
  abort = FALSE
) {
  
  prodtype <- level <- mission <- level <- id_orbit <- id_tile <- prodtype <- 
    validname <- NULL # to avoid NOTE on check
  
  # elements used by the function
  needed_metadata <- c("validname","mission","level","sensing_datetime","id_orbit","id_tile")
  
  s2_metadata <- safe_getMetadata(
    prod_name, info = needed_metadata, 
    format = "data.table", simplify = TRUE, abort = abort
  )
  
  # select default product type if missing
  s2_metadata$prodtype <- if (is.null(prod_type)) {
    ifelse(
      s2_metadata$level == "1C", "TOA", 
      ifelse(s2_metadata$level == "2A", "BOA", NA)
    )
  } else {
    prod_type
  }
  
  # check prod_type
  prod_type_accepted_values <- list(
    "1C" = c("TOA","xxx"),
    "2A" = c("BOA","TCI","AOT","WVP","SCL","CLD","SNW","xxx"))
  message_type <- ifelse(abort==TRUE, "error", "warning")
  
  if (length(prod_type) > 1) {
    print_message(
      type="warning",
      "\"prod_type\" must be of length 1 (only first element will be used)."
    )
    prod_type <- prod_type[1]
  }
  
  if (any(sapply(seq_len(nrow(s2_metadata)), function(i) {
    !s2_metadata[i,prodtype] %in% prod_type_accepted_values[[s2_metadata[i,level]]]
  }))) {
    print_message(
      type=message_type,
      "\"prod_type\" value is not recognised; ",
      "use 'xxx' for a generic string in order not to show this warning)."
    )
  }
  
  # check res
  if (!res %in% c("10m","20m","60m")) {
    print_message(
      type=message_type,
      "\"res\" value is not recognised (accepted values are '10m', '20m' ",
      "and '60m').")
  }
  
  # compose name
  short_name <- s2_metadata[,paste0(
    "S",mission,level,"_",
    strftime(s2_metadata$sensing_datetime,"%Y%m%d"),"_",
    id_orbit,"_",id_tile,"_",prodtype,"_",substr(res,1,2)
  )]
  short_name[s2_metadata[,!validname]] <- NA
  
  # check name length
  if (any(s2_metadata[,validname] & nchar(short_name)!=31)) {
    print_message(
      type="warning",
      "Length of shortname is wrong (probably \"prod_type\" or \"res\" ",
      "has wrong length).")
  }
  
  # add dirname and extension
  out_name <- ifelse(
    dirname(prod_name)!="." & full.name, 
    file.path(dirname(prod_name), short_name), 
    short_name
  )
  if (!is.null(ext)) {
    out_name[s2_metadata[,validname]] <- 
      paste0(out_name[s2_metadata[,validname]],".",gsub("^\\.","",ext))
  }
  
  return(out_name)
  
}

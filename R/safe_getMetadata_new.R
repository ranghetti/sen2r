#' @title Get information from S2 file name or metadata
#' @description The function `safe_getMetadata()` scans a Sentinel2 product
#'  (main path or granule xml file) to retrieve information about the product.
#'  
#'  The accessory function `rm_invalid_safe()` remove a SAFE archive in the case
#'  it is not recognised by `safe_getMetadata()`.
#'  
#'  The accessory function `safe_isvalid()` scan the SAFE name to understand
#'  if it is a valid SAFE.
#' @param s2 Sentinel-2 products, which can be:
#'  - a list of products in the format `safelist` (see [safelist-class]);
#'  - a vector of SAFE paths;
#'  - a vector of paths of xml product files with metadata.
#'  If the product does not exist locally, the function can run only with
#'  option `info = "nameinfo"` (see below).
#' @param info (optional) A character vector with the list of the metadata
#'  which should be provided.
#'  Accepted values are:
#'  * `"all"` (default): all the retrievable metadata are provided;
#'  * `"fileinfo"`: only the metadata obtained by scanning the file name
#'      and product structure (without opening it with GDAL) are provided.
#'  * `"nameinfo"`: only the metadata obtained by scanning the file name
#'      are provided (it is faster and there is no need to have downloaded
#'      yet the file).
#'  * a vector of single specific information (one or more from the
#'      followings):
#'      - `"name"` (SAFE name - this is always returned);
#'      - `"validname"` (TRUE or FALSE);
#'      - `"exists"` (TRUE or FALSE);
#'      - `"prod_type"` ('singlegranule' or 'product');
#'      - `"version"` ('old' or 'compact');
#'      - `"tiles"` (vector with the tiles ID available in the product);
#'      - `"utm"` (vector with the UTM zones used in the product);
#'      - `"xml_main"` (name of the main XML file with metadata);
#'      - `"xml_granules"` (names of the XML with granule metadata);
#'      - `"level"` ('1C' or '2A');
#'      - `"creation_datetime"`, `"id_tile"`, `"mission"`, `"centre"`,
#'          `"file_class"`, `"id_orbit"`, `"orbit_number"`,
#'          `"sensing_datetime"`, `"id_baseline"`: metadata specific of
#'          the product type and version (they are returned only if
#'          obtainable for the specified input);
#'      - `"clouds"`, `"direction"`, `"orbit_n"`, `"preview_url"`,
#'          `"proc_baseline"`, `"level"`, `"sensing_datetime"`,
#'          `"nodata_value"`, `"saturated_value"`:
#'          information retrieved from the metadata stored in the XML file;
#'      - `"jp2list"` (data.frame with the list of the JP2 band files - 
#'          asking for this info will cause `format` to be coherced to `"list"`).
#'          
#'      Notice that the required info are returned only if available;
#'      i.e., if some info requiring existing files are asked by the user, but
#'      input SAFE do not exist, only info retrievable by the SAFE name are 
#'      returned.
#' @param format Output format, being one of the followings:
#'  * `"data.table"` (default) and `"data.frame"`: a table with one row per `s2` 
#'      input and one column per required `info`;
#'  * `"list"`: a list (one element per `s2` input) in which each element is
#'      a list of the required `info`;
#'  * `"vector"`: a list (one element per `info`) in which each element is 
#'      a named vector (with `s2` length and names) with the required `info`.
#'  * `"default"` (default): `"vector"` if `info` is of length 1;
#'      `"data.table"` otherwise.
#' @param simplify Logical parameter, which applies in case `s2` is of length 1:
#'  in this case, if TRUE (default) and `format` is `"list"` or `"vector"`,
#'  a single `info` list or vector is returned;
#'  if FALSE, a list of length 1 (containing the list or vector of the required
#'  `s2` product) is returned.
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case some inputs are not recognised, or if some files do not exists
#'  (in case some `info` elements require the files to be present);
#'  if FALSE, a warning is shown.
#' @param allow_oldnames Logical parameter: if TRUE, old (long) name products
#'  are managed (metadata are returned, and they are considered valid; 
#'  if FALSE (default), they are considered as non-supported files.
#'  Note that, from sen2r version 1.1.0, oldname products are no more supported
#'  withing processing chains.
#' @return `safe_getMetadata()` returns a data.table, a data.frame or a list
#'  (depending on argument  `format`) with the output metadata;
#' 
#'  `rm_invalid_safe()` returns a named vector (with the length of `s2`) with
#'  TRUE if the `s2` product was removed, FALSE elsewhere.
#'
#'  `safe_isvalid()` returns a named vector (with the length of `s2`) with
#'  TRUE if the product is a valid SAFE, FALSE if not.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export
#' @import data.table
#' @importFrom reticulate py_to_r
#' @importFrom methods is as
#'
#' @examples
#' # Define product name
#' s2_examplenames <- c(
#'   "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE",
#'   "S2A_MSIL2A_20170703T101021_N0205_R022_T32TNR_20170703T101041.SAFE"
#' )
#'
#' # Return only the information retrievable from the file names (files are not scanned)
#' safe_getMetadata(s2_examplenames, info="nameinfo")
#'
#' # Return some specific information without scanning files
#' safe_getMetadata(s2_examplenames, info=c("level", "id_tile"))
#'
#' # Return a single information without scanning files
#' # (in this case, the default output is a vector instead than a data.table)
#' safe_getMetadata(s2_examplenames, info="level")
#'
#' # Check if the products are valid
#' safe_isvalid(s2_examplenames)
#' 
#' # Check if the product names are valid SAFE names
#' safe_isvalid(s2_examplenames, check_file = FALSE)
#' safe_isvalid("invalid_safe_name.SAFE"), check_file = FALSE)
#' 
#' \dontrun{
#' # Download a sample SAFE archive (this can take a while)
#' s2_exampleurl <- paste0("https://scihub.copernicus.eu/apihub/odata/v1/",
#'   "Products(\'5f590bcb-ee55-4a20-8e75-bde99f5b93d4\')/$value")
#' names(s2_exampleurl) <- "S2A_MSIL1C_20170703T101021_N0205_R022_T32TNS_20170703T101041.SAFE"
#' s2_download(s2_exampleurl, outdir=tempdir())
#' s2_examplepath <- file.path(tempdir(), names(s2_exampleurl))
#'
#' # Return all the available information
#' safe_getMetadata(s2_examplepath)
#'
#' # Return some specific information
#' safe_getMetadata(s2_examplepath, info=c("tiles", "level", "id_tile"))
#'
#' # Return a single information
#' safe_getMetadata(s2_examplepath, info="orbit_n")
#' 
#' # Check if the downloaded SAFE is valid
#' safe_isvalid(s2_examplepath)
#' 
#' # Delete it if it is not recognised
#' rm_invalid_safe(s2_examplepath)
#' 
#' }

# TODO
# - make the output list uniform (es. level and tiles/id_tile)
# - add a parameter which provides the list of the available options
# - add check for format integrity


safe_getMetadata <- function(
  s2, 
  info = "all", 
  format = "default",
  simplify = TRUE, 
  abort = TRUE, 
  allow_oldnames = FALSE
) {
  .safe_getMetadata(
    s2, 
    info = info, 
    format = format, 
    simplify = simplify,
    abort = abort, 
    allow_oldnames = allow_oldnames, 
    action = "getmetadata"
  )
}


#' @name rm_invalid_safe
#' @rdname safe_getMetadata
#' @export
rm_invalid_safe <- function(s2, allow_oldnames = FALSE) {
  .safe_getMetadata(
    s2, 
    info = "fileinfo", 
    format = "not used", 
    simplify = NA,
    abort = FALSE, 
    allow_oldnames = allow_oldnames, 
    action = "rm_invalid"
  )
}


#' @name safe_isvalid
#' @rdname safe_getMetadata
#' @param check_file Logical: if TRUE (default), the content of the provided
#'  paths is checked; if FALSE, only the validity of SAFE names is tested.
#' @export
safe_isvalid <- function(s2, allow_oldnames = FALSE, check_file = TRUE) {
  info <- if (check_file == TRUE) {c("exists", "validname")} else {"validname"}
  .safe_getMetadata(
    s2, 
    info = info, 
    format = "not used", 
    simplify = NA,
    abort = FALSE, 
    allow_oldnames = allow_oldnames, 
    action = "isvalid"
  )
}


# internal function: action="getmetadata" causes the execution of safe_getMetadata(),
# action="rm_invalid" causes the execution of rm_invalid_safe().
.safe_getMetadata <- function(s2, info, format, simplify, abort, allow_oldnames, action) {
  
  # define regular expressions to identify products
  s2_regex <- list(
    "oldname_main_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_SAFL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.xml$",
                              "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")),
    "oldname_main_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_PRD\\_MSIL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.SAFE$",
                               "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")),
    "compactname_main_xml" = list("regex" = "^MTD\\_MSIL([12][AC])\\.xml$", "elements" = c("level")),
    "compactname_main_path" = list("regex" = "^S(2[AB])\\_MSIL([12][AC])\\_([0-9]{8}T[0-9]{6})\\_N([0-9]{4})\\_R([0-9]{3})\\_T([A-Z0-9]{5})\\_([0-9]{8}T[0-9]{6})\\.SAFE$",
                                   "elements" = c("mission","level","sensing_datetime","id_baseline","id_orbit","id_tile","creation_datetime")),
    "oldname_granule_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\.xml$",
                                 "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","id_tile")),
    "oldname_granule_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MSI\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\_N([0-9]{2})\\.([0-9]{2})$",
                                  "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","id_tile","proc_baseline_x","proc_baseline_y")),
    "compactname_granule_xml" = list("regex" = "^MTD\\_TL\\.xml$", "elements" = character(0)),
    "compactname_granule_path" = list("regex" = "^L([12][AC])\\_T([A-Z0-9]{5})\\_A([0-9]{6})\\_([0-9]{8}T[0-9]{6})$",
                                      "elements" = c("level","id_tile","orbit_number","creation_datetime")),
    "oldname_L1C_jp2" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_([A-Z]{3})\\_L1C\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})_(B[0-9A]{2})\\.jp2$",
                             "elements" = c("mission","file_class","additional_product","centre","creation_datetime","orbit_number","id_tile","bandname")),
    "oldname_L2A_jp2" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_([A-Z]{3})\\_L2A\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\_?(B[0-9A]{2})?\\_([126]0m)\\.jp2$",
                             "elements" = c("mission","file_class","additional_product","centre","creation_datetime","orbit_number","id_tile","bandname","res")),
    "compactname_L1C_jp2" = list("regex" = "^T([A-Z0-9]{5})\\_([0-9]{8}T[0-9]{6})\\_(B[0-9A]{2})\\.jp2$",
                                 "elements" = c("id_tile","sensing_datetime","bandname")),
    "compactname_L2A_jp2" = list("regex" = "^(?:L2A\\_)?T([A-Z0-9]{5})\\_([0-9]{8}T[0-9]{6})\\_([0-9A-Z]{3})\\_([126]0m)\\.jp2$",
                                 "elements" = c("id_tile","sensing_datetime","bandname","res"))) # here bandname can be also additional_product
  
  # define all possible elements to scan
  info_base <- c("validname", "prod_type", "version") # information always retrieved
  info_general <- c("exists", "tiles", "utm", "xml_main", "xml_granules") # information retrieved if the product is scanned
  info_name <- c("level","creation_datetime", "id_tile", "mission", "centre", "file_class",
                 "id_orbit", "orbit_number", "sensing_datetime", "id_baseline") # information retrieved from name
  info_gdal <- c("clouds","direction","orbit_n","preview_url", # information retrieved by reading the file metadata
                 "proc_baseline","gdal_level","gdal_sensing_datetime",
                 "nodata_value","saturated_value")
  if (length(info)==1) {
    if (info=="all") {
      info <- c(info_base, info_general, info_name, info_gdal)
      # scan_file <- TRUE
    } else if (info=="fileinfo") {
      info <- c(info_base, info_general, info_name)
      # scan_file <- TRUE
    } else if (info=="nameinfo") {
      info <- c(info_base, info_name)
      # scan_file <- FALSE
      # } else {
      #   scan_file <- TRUE
    }
    # } else {
    #   scan_file <- TRUE
  }
  
  # check format attribute
  if (format == "default") {
    format <- if (length(info) == 1) {"vector"} else {"data.table"}
  } else if (!format %in% c("vector", "list", "data.frame", "data.table", "not used")) {
    print_message(
      type = "error", 
      '"format" not recognised (it must be one among "default", "vector", ',
      '"list", "data.frame" and "data.table")'
    )
  }
  if ("jp2list" %in% info) {format <- "list"} # coherce to list in case of jp2list
  
  # scan files only if necessary
  scan_file <- if (!action %in% c("isvalid", "rm_invalid")) {
    !all(info %in% c(info_base, info_name))
  } else {TRUE}
  
  message_type <- ifelse(abort==TRUE, "error", "warning")
  
  metadata <- list() # output object, with requested metadata
  
  
  ## Check the input format
  # if s2 is a safelist, use the product names and skip scanning content
  try_safelist <- suppressWarnings(try(as(s2, "safelist"), silent = TRUE))
  if (!inherits(try_safelist, "try-error")) {
    s2 <- names(s2)
  }
  
  # If s2 is a string, check it and retrieve file metadata
  if (!is(nn(s2), "character")) {
    stop("'s2' is not in the right format")
  }
  
  s2_names <- basename(nn(s2))
  
  for (i in seq_along(s2)) {
    
    s2_type <- s2_validname <- s2_version <- 
      nameinfo_target <- nameinfo_regex <- nameinfo_elements <-
      s2_main_xml <- s2_xml <- s2_granules_xml <- NULL
    
    # If s2 is a path:
    # convert in absolute path (and check that file exists)
    s2_path <- normalizePath(s2[i], mustWork=FALSE)
    s2_exists <- file.exists(s2_path)
    s2_name <- basename(s2[i])
    metadata[[i]] <- list()
    
    # if scan_file is FALSE, check the input as a product name without searching for files
    if (any(!scan_file, !s2_exists)) {
      
      nameinfo_target <- s2_name
      
      # retrieve type and version
      if (grepl("\\.xml$",nameinfo_target)) {
        if (any(
          grepl(s2_regex$compactname_main_xml$regex, s2_name),
          grepl(s2_regex$oldname_main_xml$regex, s2_name)
        )) {
          s2_type <- "product"
          s2_validname <- TRUE
          if (grepl(s2_regex$compactname_main_xml$regex, s2_name)) {
            s2_version <- "compact"
            nameinfo_regex <- s2_regex$compactname_main_xml$regex
            nameinfo_elements <- list(s2_regex$compactname_main_xml$elements)
          } else if (grepl(s2_regex$oldname_main_xml$regex, s2_name)) {
            nameinfo_regex <- s2_regex$oldname_main_xml$regex
            nameinfo_elements <- list(s2_regex$oldname_main_xml$elements)
            s2_version <- "old"
          }
        } else if (any(
          grepl(s2_regex$compactname_granule_xml$regex, s2_name),
          grepl(s2_regex$oldname_granule_xml$regex, s2_name)
        )) {
          s2_type <- "singlegranule"
          s2_validname <- TRUE
          if (grepl(s2_regex$compactname_granule_xml$regex, s2_name)) {
            s2_version <- "compact"
            nameinfo_regex <- s2_regex$compactname_granule_xml$regex
            nameinfo_elements <- list(s2_regex$compactname_granule_xml$elements)
          } else if (grepl(s2_regex$oldname_granule_xml$regex, s2_name)) {
            s2_version <- "old"
            nameinfo_regex <- s2_regex$oldname_granule_xml$regex
            nameinfo_elements <- list(s2_regex$oldname_granule_xml$elements)
          }
        } else {
          s2_validname <- FALSE
          if (action == "getmetadata") {
            print_message(
              type=message_type,
              "This product (",s2_name,") is not in the right format (not recognised)."
            )
          }
        }
      } else {
        if (any(
          grepl(s2_regex$compactname_main_path$regex, s2_name),
          grepl(s2_regex$oldname_main_path$regex, s2_name)
        )) {
          s2_type <- "product"
          s2_validname <- TRUE
          if (grepl(s2_regex$compactname_main_path$regex, s2_name)) {
            s2_version <- "compact"
            nameinfo_regex <- s2_regex$compactname_main_path$regex
            nameinfo_elements <- list(s2_regex$compactname_main_path$elements)
          } else if (grepl(s2_regex$oldname_main_path$regex, s2_name)) {
            nameinfo_regex <- s2_regex$oldname_main_path$regex
            nameinfo_elements <- list(s2_regex$oldname_main_path$elements)
            s2_version <- "old"
          }
        } else if (any(
          grepl(s2_regex$compactname_granule_path$regex, s2_name),
          grepl(s2_regex$oldname_granule_path$regex, s2_name)
        )) {
          s2_type <- "singlegranule"
          s2_validname <- TRUE
          if (grepl(s2_regex$compactname_granule_path$regex, s2_name)) {
            s2_version <- "compact"
            nameinfo_regex <- s2_regex$compactname_granule_path$regex
            nameinfo_elements <- list(s2_regex$compactname_granule_path$elements)
          } else if (grepl(s2_regex$oldname_granule_path$regex, s2_name)) {
            s2_version <- "old"
            nameinfo_regex <- s2_regex$oldname_granule_path$regex
            nameinfo_elements <- list(s2_regex$oldname_granule_path$elements)
          }
        } else {
          s2_validname <- FALSE
          if (action == "getmetadata") {
            print_message(
              type=message_type,
              "This product (",s2_name,") is not in the right format (not recognised)."
            )
          }
        }
      }
      
      # manage nusupported element
      if (all(!is.null(s2_version), s2_version == "old", allow_oldnames == FALSE)) {
        s2_validname <- FALSE
        if (action == "getmetadata") {
          print_message(
            type=message_type,
            "This product (",s2_name,") is not in the right format (old SAFE format)."
          )
        }
      }
      
      # if scan_file is TRUE, scan for file content
    } else {
      
      # retrieve the name of xml main file
      # if it is a directory, scan the content
      if (file.info(s2_path)$isdir) {
        compactname_main_xmlfile <- list.files(s2_path,s2_regex$compactname_main_xml$regex, full.names=TRUE)
        oldname_main_xmlfile <- list.files(s2_path,s2_regex$oldname_main_xml$regex, full.names=TRUE)
        compactname_granule_xmlfile <- list.files(s2_path,s2_regex$compactname_granule_xml$regex, full.names=TRUE)
        oldname_granule_xmlfile <- list.files(s2_path,s2_regex$oldname_granule_xml$regex, full.names=TRUE)
      } else {
        compactname_main_xmlfile <- s2_path[grep(s2_regex$compactname_main_xml$regex, basename(s2_path))]
        oldname_main_xmlfile <- s2_path[grep(s2_regex$oldname_main_xml$regex, basename(s2_path))]
        compactname_granule_xmlfile <- s2_path[grep(s2_regex$compactname_granule_xml$regex, basename(s2_path))]
        oldname_granule_xmlfile <- s2_path[grep(s2_regex$oldname_granule_xml$regex, basename(s2_path))]
        s2_path <- dirname(s2_path)
      }
      
      # check version (old / compact) and product type (product / singlegranule)
      if (length(oldname_main_xmlfile)+length(compactname_main_xmlfile)==1) {
        if (length(oldname_granule_xmlfile)+length(compactname_granule_xmlfile)==0) {
          s2_type <- "product"
          # Check product version
          if (length(compactname_main_xmlfile)==0) {
            if (length(oldname_main_xmlfile)==1) {
              s2_validname <- TRUE
              s2_version <- "old"
              s2_main_xml <- s2_xml <- oldname_main_xmlfile
              s2_granules_xml <- unlist(sapply(
                list.dirs(file.path(s2_path,"GRANULE"), recursive=FALSE, full.names=TRUE),
                list.files, s2_regex$oldname_granule_xml$regex, full.names=TRUE
              )) %>%
                paste(collapse = ",")
            } else if (length(oldname_main_xmlfile)==0) {
              s2_validname <- FALSE # not recognised
              if (action == "getmetadata") {
                print_message(
                  type=message_type,
                  "This product (",s2_name,") is not in the right format (not recognised)."
                )
              } else if (action == "rm_invalid" & s2_exists) {
                unlink(s2_path, recursive=TRUE)
              }
            } else {
              s2_validname <- FALSE # not univocally recognised (so not removed)
              if (action == "getmetadata") {
                print_message(
                  type=message_type,
                  "This product (",s2_name,") is not in the right format (not univocally recognised)."
                )
              }
            }
          } else if (length(compactname_main_xmlfile)==1) {
            if (length(oldname_main_xmlfile)==0) {
              s2_validname <- TRUE
              s2_version <- "compact"
              s2_main_xml <- s2_xml <- compactname_main_xmlfile
              s2_granules_xml <- unlist(sapply(
                list.dirs(file.path(s2_path,"GRANULE"), recursive=FALSE, full.names=TRUE),
                list.files, s2_regex$compactname_granule_xml$regex, full.names=TRUE
              )) %>%
                paste(collapse = ",")
            } else {
              s2_validname <- FALSE # not univocally recognised
              if (action == "getmetadata") {
                print_message(
                  type=message_type,
                  "This product (",s2_name,") is not in the right format (not univocally recognised)."
                )
              }
            }
          }
        } else {
          s2_validname <- FALSE # not univocally recognised
          if (action == "getmetadata") {
            print_message(
              type=message_type,
              "This product (",s2_name,") is not in the right format (not univocally recognised)."
            )
          }
        }
      } else if (length(oldname_main_xmlfile)+length(compactname_main_xmlfile)==0) {
        if (length(oldname_granule_xmlfile)+length(compactname_granule_xmlfile)==1) {
          s2_type <- "singlegranule"
          # Check product version
          if (length(compactname_granule_xmlfile)==0) {
            if (length(oldname_granule_xmlfile)==1) {
              s2_validname <- TRUE
              s2_version <- "old"
              s2_main_xml <- list.files(dirname(dirname(s2_path)), s2_regex$oldname_main_xml$regex, full.names=TRUE)
              s2_granules_xml <- s2_xml <- oldname_granule_xmlfile
            } else if (length(oldname_granule_xmlfile)==0) {
              s2_validname <- FALSE # not recognised
              if (action == "getmetadata") {
                print_message(
                  type=message_type,
                  "This product (",s2_name,") is not in the right format (not recognised)."
                )
              } else if (action == "rm_invalid" & s2_exists) {
                unlink(s2_path, recursive=TRUE)
              }
            }
          } else if (length(compactname_granule_xmlfile) == 1) {
            if (length(oldname_granule_xmlfile) == 0) {
              s2_validname <- TRUE
              s2_version <- "compact"
              s2_main_xml <- list.files(dirname(dirname(s2_path)), s2_regex$compactname_main_xml$regex, full.names=TRUE)
              s2_granules_xml <- s2_xml <- compactname_granule_xmlfile
            } else if (length(oldname_granule_xmlfile) == 1) {
              s2_validname <- FALSE # not univocally recognised
              if (action == "getmetadata") {
                print_message(
                  type=message_type,
                  "This product (",s2_name,") is not in the right format (not univocally recognised)."
                )
              }
            }
          }
        } else if (length(oldname_granule_xmlfile) + length(compactname_granule_xmlfile) == 0) {
          s2_validname <- FALSE # not recognised
          if (action == "getmetadata") {
            print_message(
              type=message_type,
              "This product (",s2_name,") is not in the right format (not recognised)."
            )
          } else if (action == "rm_invalid" & s2_exists) {
            unlink(s2_path, recursive=TRUE)
          }
        } else {
          s2_validname <- FALSE # not univocally recognised
          if (action == "getmetadata") {
            print_message(
              type=message_type,
              "This product (",s2_name,") is not in the right format (not univocally recognised)."
            )
          }
        }
      } else {
        s2_validname <- FALSE # not recognised
        if (action == "getmetadata") {
          print_message(
            type=message_type,
            "This product (",s2_name,") is not in the right format (not recognised)."
          )
        } else if (action == "rm_invalid" & s2_exists) {
          unlink(s2_path, recursive=TRUE)
        }
      }
      
      # metadata from file name are read
      # decide target, regex and elements to scan
      if (any(s2_version=="old")) {
        # for old names, retrieve from xml name
        if (s2_type=="product") {
          nameinfo_target <- basename(s2_xml)
          nameinfo_regex <- s2_regex$oldname_main_xml$regex
          nameinfo_elements <- list(s2_regex$oldname_main_xml$elements)
        } else if (s2_type=="singlegranule") {
          nameinfo_target <- c(basename(s2_xml), basename(s2_main_xml))
          nameinfo_regex <- c(s2_regex$oldname_granule_xml$regex, s2_regex$oldname_main_xml$regex)
          nameinfo_elements <- list(s2_regex$oldname_granule_xml$elements, s2_regex$oldname_main_xml$elements)
        }
      } else if (any(s2_version=="compact")) {
        # for compact names, retrieve from directory name
        if (s2_type=="product") {
          nameinfo_target <- basename(s2_path)
          nameinfo_regex <- s2_regex$compactname_main_path$regex
          nameinfo_elements <- list(s2_regex$compactname_main_path$elements)
        } else if (s2_type=="singlegranule") {
          nameinfo_target <- c(basename(s2_path), basename(dirname(s2_main_xml)))
          nameinfo_regex <- c(s2_regex$compactname_granule_path$regex, s2_regex$compactname_main_path$regex)
          nameinfo_elements <- list(s2_regex$compactname_granule_path$elements, s2_regex$compactname_main_path$elements)
        }
      }
      
    }
    
    ## Populate metadata list
    if (TRUE) { # always return it (necessary for rbindlist - eventually removed later)
      metadata[[i]][["name"]] <- basename(nameinfo_target)[1]
    }
    if ("validname" %in% info) { # return if the product has a valid SAFE name
      metadata[[i]][["validname"]] <- s2_validname
    }
    
    # if file is valid, continue reading subsequent metadata
    if (s2_validname) {
      
      if ("exists" %in% info) { # return if the file exists
        metadata[[i]][["exists"]] <- s2_exists
      }
      if ("prod_type" %in% info) { # return the type if required
        metadata[[i]][["prod_type"]] <- s2_type
      }
      if ("version" %in% info) { # return the version if required
        metadata[[i]][["version"]] <- s2_version
      }
      if ("xml_main" %in% info) { # return the path of the main xml file, if required
        metadata[[i]][["xml_main"]] <- s2_main_xml
      }
      if ("xml_granules" %in% info) { # return the version if required
        metadata[[i]][["xml_granules"]] <- s2_granules_xml
      }
      
      
      # scan
      metadata_nameinfo <- list()
      for (j in seq_along(nameinfo_target)) {
        for (sel_el in nameinfo_elements[[j]]) {
          metadata_nameinfo[[sel_el]] <- gsub(
            nameinfo_regex[j],
            paste0("\\",which(nameinfo_elements[[j]]==sel_el)),
            nameinfo_target[j])
          # format if it is a date or a time
          if (length(grep("\\_datetime",sel_el))==1) {
            metadata_nameinfo[[sel_el]] <- as.POSIXct(
              metadata_nameinfo[[sel_el]], format="%Y%m%dT%H%M%S", tz="UTC"
            )
          }
          # return if nameinfo is required
          if (sel_el %in% info) {
            metadata[[i]][[sel_el]] <- metadata_nameinfo[[sel_el]]
          }
        }
      }
      s2_level <- metadata_nameinfo[["level"]] # used as base info
      
      # info on tile[s]
      if (any(c("tiles","utm") %in% info) & s2_exists) {
        av_tiles <- gsub(
          s2_regex[[paste0(s2_version,"name_granule_path")]]$regex,
          paste0("\\",which(s2_regex[[paste0(s2_version,"name_granule_path")]]$elements=="id_tile")),
          basename(dirname(s2_granules_xml)))
        if ("tiles" %in% info) {
          metadata[[i]][["tiles"]] <- paste(av_tiles, collapse = ",")
        }
        if ("utm" %in% info) {
          metadata[[i]][["utm"]] <- as.integer(unique(substr(av_tiles,1,2)))
        }
      }
      
      # if requested, give band names
      if ("jp2list" %in% info & s2_exists) {
        
        # compute elements
        jp2_listall <- list.files(s2_path, s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$regex, recursive=TRUE, full.names=FALSE)
        jp2_bandname <- gsub(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$regex,
                             paste0("\\",which(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$elements == "bandname")),
                             basename(jp2_listall))
        jp2_layertype <- gsub(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$regex,
                              paste0("\\",which(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$elements == "additional_product")),
                              basename(jp2_listall))
        jp2_res <- gsub(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$regex,
                        paste0("\\",which(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$elements == "res")),
                        basename(jp2_listall))
        jp2_tile <- gsub(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$regex,
                         paste0("\\",which(s2_regex[[paste0(s2_version,"name_L",s2_level,"_jp2")]]$elements == "id_tile")),
                         basename(jp2_listall))
        # corrections for compact names
        if (s2_version=="compact") {
          jp2_layertype[grep("^B[0-9A]{2}$",jp2_bandname)] <- "MSI"
          jp2_layertype[jp2_layertype!="MSI"] <- jp2_bandname[jp2_layertype!="MSI"]
          jp2_bandname[jp2_layertype!="MSI"] <- ""
        }
        
        # correction B8A -> B08 (only one between them is used)
        jp2_bandname[jp2_bandname=="B8A"] <- "B08"
        
        # output data.frame
        jp2_list <- data.frame("layer" = basename(jp2_listall),
                               "tile" = jp2_tile,
                               "type" = jp2_layertype,
                               "band" = jp2_bandname,
                               "res" = jp2_res,
                               "relpath" = jp2_listall,
                               stringsAsFactors=FALSE)
        metadata[[i]][["jp2list"]] <-jp2_list[with(jp2_list, order(band,type,res,tile)),]
        
      }
      
      # if necessary, read the file for further metadata[[i]]
      if (any(info_gdal %in% info) & s2_exists) {
        
        # import python modules
        py <- init_python()
        
        s2_gdal <- suppressWarnings(py$gdal$Open(s2_xml))
        
        # workaround for L2A generated by Sen2Cor 2.8.0
        if (is.null(py_to_r(s2_gdal))) {
          s2_xml_new <- tempfile()
          s2_xml_content <- readLines(s2_xml)
          lines_tofix <- c(
            grep("</?Product_Info>", s2_xml_content),
            grep("</?Product_Organisation>", s2_xml_content)
          )
          s2_xml_content[lines_tofix] <- gsub(
            "Product_", "L2A_Product_",
            s2_xml_content[lines_tofix]
          )
          writeLines(s2_xml_content, s2_xml_new)
          s2_gdal <- py$gdal$Open(s2_xml_new)
        }
        
        # in case of error (old names), try to read a single granule
        if (s2_type=="product" & is(s2_gdal,"python.builtin.NoneType")) {
          first_granule <- list.files(file.path(s2_path,"GRANULE"),full.names=TRUE)[1]
          first_granule_xml <- list.files(first_granule,s2_regex[[paste0(s2_version,"name_granule_xml")]]$regex,full.names=TRUE)
          s2_gdal <- py$gdal$Open(first_granule_xml)
        }
        
      }
      
      # # If s2 is a gdal object, read metadata[[i]] directly
      # if (is(s2, "osgeo.gdal.Dataset")) {
      #   s2_gdal <- s2[i]
      # }
      
      # retrieve metadata[[i]] from file content
      if (exists("s2_gdal") & s2_exists) {
        
        # Read metadata[[i]]
        if ("clouds" %in% info) {
          metadata[[i]][["clouds"]] <- py_to_r(s2_gdal$GetMetadata()[["CLOUD_COVERAGE_ASSESSMENT"]])
        }
        if ("direction" %in% info) {
          metadata[[i]][["direction"]] <- py_to_r(s2_gdal$GetMetadata()[["DATATAKE_1_SENSING_ORBIT_DIRECTION"]])
        }
        if ("orbit_n" %in% info) {
          metadata[[i]][["orbit_n"]] <- py_to_r(s2_gdal$GetMetadata()[["DATATAKE_1_SENSING_ORBIT_NUMBER"]])
        }
        if ("preview_url" %in% info) {
          metadata[[i]][["preview_url"]] <- py_to_r(s2_gdal$GetMetadata()[["PREVIEW_IMAGE_URL"]])
        }
        if ("proc_baseline" %in% info) {
          metadata[[i]][["proc_baseline"]] <- py_to_r(s2_gdal$GetMetadata()[["PROCESSING_BASELINE"]])
        }
        # if ("level" %in% info) {
        #   metadata[[i]][["level"]] <- py_to_r(s2_gdal$GetMetadata()[["PROCESSING_LEVEL"]])
        # }
        if ("sensing_datetime" %in% info) {
          start_time <- as.POSIXct(
            py_to_r(s2_gdal$GetMetadata()[["PRODUCT_START_TIME"]]), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
          stop_time <- as.POSIXct(
            py_to_r(s2_gdal$GetMetadata()[["PRODUCT_STOP_TIME"]]), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
          metadata[[i]][["sensing_datetime"]] <- if (start_time == stop_time) {
            start_time
          } else {
            c(start_time, stop_time)
          }
        }
        if ("nodata_value" %in% info) {
          metadata[[i]][["nodata_value"]] <- py_to_r(s2_gdal$GetMetadata()[["SPECIAL_VALUE_NODATA"]])
        }
        if ("saturated_value" %in% info) {
          metadata[[i]][["saturated_value"]] <- py_to_r(s2_gdal$GetMetadata()[["SPECIAL_VALUE_SATURATED"]])
        }
        
      }
      
    } # end of s2_exists IF cycle
    
    # check if some info were skipped
    if (all(scan_file, !s2_exists, action == "getmetadata")) {
      print_message(
        type=message_type,
        "This product (",s2_name,") was not found on the system.",
        if (abort == FALSE) {paste0(
          ' Some elements required by the argument "info" (',
          paste(info[!info %in% names(metadata[[i]])], collapse = ", "), 
          ") are not returned for this product."
        )}
      )
      
      
    }
    
    
  } # end of s2_name FOR cycle
  
  names(metadata) <- s2_names
  
  # return
  out_metadata <- if (action == "rm_invalid") {
    sapply(metadata, function(m) {all(!m[["exists"]], m[["validname"]])})
  } else if (action == "isvalid") {
    if ("exists" %in% info) {
      sapply(metadata, function(m) {all(m[["exists"]], m[["validname"]])})
    } else {
      sapply(metadata, function(m) {m[["validname"]]})
    }
  } else if (format == "list" ) {
    for (i in seq_along(metadata)) {
      if (!is.null(metadata[[i]][["tiles"]])) {
        metadata[[i]][["tiles"]] <- unlist(strsplit(metadata[[i]][["tiles"]], ","))
      }
      if (!is.null(metadata[[i]][["xml_granules"]])) {
        metadata[[i]][["xml_granules"]] <- unlist(strsplit(metadata[[i]][["xml_granules"]], ","))
      }
    }
    if (simplify == TRUE & length(metadata) == 1) {
      metadata[[1]]
    } else {
      metadata
    }
  } else if (format %in% c("data.table", "data.frame", "vector")) {
    metadata_dt <- lapply(metadata, function(m) {
      unlist(m) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
    }) %>%
      rbindlist(fill=TRUE)
    if (!is.null(metadata_dt$validname)) {
      metadata_dt[,"validname" := as.logical(validname)]
    }
    if (!is.null(metadata_dt$exists)) {
      metadata_dt[,"exists" := as.logical(exists)]
    }
    if (!is.null(metadata_dt$sensing_datetime)) {
      metadata_dt[,"sensing_datetime" := format(
        as.POSIXct(sensing_datetime, format = "%s"), tz = "UTC", usetz = TRUE
      )]
    }
    if (!is.null(metadata_dt$creation_datetime)) {
      metadata_dt[,"creation_datetime" := format(
        as.POSIXct(creation_datetime, format = "%s"), tz = "UTC", usetz = TRUE
      )]
    }
    if (!is.null(metadata_dt$utm)) {
      metadata_dt[,"utm" := as.integer(utm)]
    }
    if (format == "data.frame") {
      data.frame(metadata_dt)
    } else if (format == "data.table") {
      metadata_dt
    } else if (format == "vector") {
      metadata_v <- lapply(as.list(metadata_dt), function(x) {
        names(x) <- metadata_dt$name; x
      })
      if (simplify == TRUE & length(info) == 1) {
        metadata_v[[info]]
      } else {
        metadata_v
      }
    }
  }
  out_metadata
  
}

#' s2_getMetadata
#' @description Get information from S2 file name or metadata
#' @details The function scan a Sentinel2 product (main path, granule path,
#'  main / granule xml file or GDAL object) to retrieve information about
#'  the product.
#' @param infile `character` or `osgeo.gdal.Dataset` This input parameter
#'  can be the main path of a S2 file, the path of the xml with metadata,
#'  th path of a single granule, the xml path of a single granule, or a
#'  `osgeo.gdal.Dataset` object (obtained reading the product with python).
#' @param info `character` (optional) Vector with the list of the metadata
#'  which should be provided. By default, all the metadata are provided.
#'  By choosing "nameinfo", only the metadata obtained by scanning the file
#'  name are provided (this is a bit faster). Alternatively, single
#'  information can be asked.
#' @return `list` list of the output metadata.
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom tools file_path_as_absolute
#' @importFrom reticulate import py_to_r

# TODO>
# - make the output list uniform (es. level and tiles/id_tile)
# - add a parameter which provides the list of the available options

s2_getMetadata <- function(s2, info="all") {

  # define regular expressions to identify products
  s2_regex <- list(
    "oldname_main_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_SAFL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.xml$",
                              "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")),
    "oldname_main_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_PRD\\_MSIL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.SAFE$",
                               "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")),
    "compactname_main_xml" = list("regex" = "^MTD\\_MSIL([12][AC])\\.xml$", "elements" = c("level")),
    "compactname_main_path" = list("regex" = "^S(2[AB])\\_MSIL([12][AC])\\_([0-9]{8}T[0-9]{6})\\_N([0-9]{4})\\_R([0-9]{3})\\_T([A-Z0-9]{5})_[0-9]{8}T[0-9]{6}\\.SAFE$",
                                   "elements" = c("mission","level","sensing_datetime","id_baseline","id_orbit","id_tile")),
    "oldname_granule_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\.xml$",
                                 "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","id_tile")),
    "oldname_granule_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MSI\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\_N([0-9]{2})\\.([0-9]{2})$",
                                  "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","id_tile","proc_baseline_x","proc_baseline_y")),
    "compactname_granule_xml" = list("regex" = "^MTD\\_TL\\.xml$", "elements" = character(0)),
    "compactname_granule_path" = list("regex" = "^L([12][AC])\\_T([A-Z0-9]{5})\\_A([0-9]{6})\\_([0-9]{8}T[0-9]{6})$",
                                      "elements" = c("level","id_tile","orbit_number","creation_datetime")))

  # define all possible elements to scan
  info_general <- c("prod_type", "version", "tiles") # information always retrieved
  info_gdal <- c("clouds","direction","orbit_n","preview_url", # information retrieved by reading the file metadata
                 "proc_baseline","level","sensing_datetime",
                 "nodata_value","saturated_value")
  if (info=="all") {
    info <- c(info_general, "nameinfo", info_gdal)
  } else if (info=="nameinfo") {
    info <- c(info_general, "nameinfo")
  }

  metadata <- list() # output object, with requested metadata

  # If s2 is a string, check it and retrieve file metadata
  if (is(s2, "character")) {

    # If s2 is a path:
    # convert in absolute path (and check that file exists)
    s2_path <- file_path_as_absolute(s2)

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
            s2_version <- "old"
            s2_xml <- oldname_main_xmlfile
          } else if (length(oldname_main_xmlfile)==0) {
            stop("This product is not in the right format (not recognised).")
          } else {
            stop("This product is not in the right format (not univocally recognised).")
          }
        } else if (length(compactname_main_xmlfile)==1) {
          if (length(oldname_main_xmlfile)==0) {
            s2_version <- "compact"
            s2_xml <- compactname_main_xmlfile
          } else {
            stop("This product is not in the right format (not univocally recognised).")
          }
        }
      } else {
        stop("This product is not in the right format (not univocally recognised).")
      }
    } else if (length(oldname_main_xmlfile)+length(compactname_main_xmlfile)==0) {
      if (length(oldname_granule_xmlfile)+length(compactname_granule_xmlfile)==1) {
        s2_type <- "singlegranule"
        # Check product version
        if (length(compactname_granule_xmlfile)==0) {
          if (length(oldname_granule_xmlfile)==1) {
            s2_version <- "old"
            s2_xml <- oldname_granule_xmlfile
          } else if (length(oldname_granule_xmlfile)==0) {
            stop("This product is not in the right format (not recognised).")
          }
        } else if (length(compactname_granule_xmlfile)==1) {
          if (length(oldname_granule_xmlfile)==0) {
            s2_version <- "compact"
            s2_xml <- compactname_granule_xmlfile
          } else if (length(oldname_granule_xmlfile)==1) {
            stop("This product is not in the right format (not univocally recognised).")
          }
        }
      } else if (length(oldname_granule_xmlfile)+length(compactname_granule_xmlfile)==0) {
        stop("This product is not in the right format (not recognised).")
      } else {
        stop("This product is not in the right format (not univocally recognised).")
      }
    } else {
      stop("This product is not in the right format (not univocally recognised).")
    }

    if ("prod_type" %in% info) { # return the type if required
      metadata[["prod_type"]] <- s2_type
    }
    if ("version" %in% info) { # return the version if required
      metadata[["version"]] <- s2_version
    }

    # if nameinfo is required, metadata from file name are read
    if ("nameinfo" %in% info) {

      # decide target, regex and elements to scan
      if (s2_version=="old") {
        # for old names, retrieve from xml name
        nameinfo_target <- basename(s2_xml)
        if (s2_type=="product") {
          nameinfo_regex <- s2_regex$oldname_main_xml$regex
          nameinfo_elements <- s2_regex$oldname_main_xml$elements
        } else if (s2_type=="singlegranule") {
          nameinfo_regex <- s2_regex$oldname_granule_xml$regex
          nameinfo_elements <- s2_regex$oldname_granule_xml$elements
        }
      } else {
        # for compact names, retrieve from directory name
        nameinfo_target <- basename(s2_path)
        if (s2_type=="product") {
          nameinfo_regex <- s2_regex$compactname_main_path$regex
          nameinfo_elements <- s2_regex$compactname_main_path$elements
        } else if (s2_type=="singlegranule") {
          nameinfo_regex <- s2_regex$compactname_granule_path$regex
          nameinfo_elements <- s2_regex$compactname_granule_path$elements
        }
      }

      # scan
      for (sel_el in nameinfo_elements) {
        metadata[[sel_el]] <- gsub(
          nameinfo_regex,
          paste0("\\",which(nameinfo_elements==sel_el)),
          nameinfo_target)
        # format if it is a date or a time
        if (length(grep("\\_datetime",sel_el))==1) {
          metadata[[sel_el]] <- as.POSIXct(metadata[[sel_el]], format="%Y%m%dT%H%M%S", tz="UTC")
        }
      }

    }

    # info on tile[s]
    if ("tiles" %in% info) {
      if (s2_type=="product") {
        granules <- list.files(file.path(s2_path,"GRANULE"),full.names=TRUE)
        granules_xml <- sapply(granules, list.files, s2_regex[[paste0(s2_version,"name_granule_xml")]]$regex, full.names=TRUE)
        granules_notempty <- unlist(sapply(granules_xml,dirname))
      } else {
        granules_notempty <- s2_path
      }
      metadata[["tiles"]] <- gsub(
        s2_regex[[paste0(s2_version,"name_granule_path")]]$regex,
        paste0("\\",which(s2_regex[[paste0(s2_version,"name_granule_path")]]$elements=="id_tile")),
        basename(granules_notempty))
    }


    # if necessary, read the file for further metadata
    if (any(c("clouds","direction","orbit_n","preview_url",
              "proc_baseline","level","sensing_datetime",
              "nodata_value","saturated_value") %in% info)) {

      gdal <- import("osgeo",convert=FALSE)$gdal

      s2_gdal <- gdal$Open(s2_xml)
      # in case of error (old names), try to read a single granule
      if (s2_type=="product" & is(s2_gdal,"python.builtin.NoneType")) {
        first_granule <- list.files(file.path(s2_path,"GRANULE"),full.names=TRUE)[1]
        first_granule_xml <- list.files(first_granule,s2_regex[[paste0(s2_version,"name_granule_xml")]]$regex,full.names=TRUE)
        s2_gdal <- gdal$Open(first_granule_xml)
      }

    }

  }

  # If s2 is a gdal object, read metadata directly
  if (is(s2, "osgeo.gdal.Dataset")) {
    gdal <- import("osgeo",convert=FALSE)$gdal
    s2_gdal <- s2
  }

  # retrieve metadata from file content
  if (exists("s2_gdal")) {

    # Read metadata
    if ("clouds" %in% info) {
      metadata[["clouds"]] <- py_to_r(s2_gdal$GetMetadata()[["CLOUDY_PIXEL_PERCENTAGE"]])
    }
    if ("direction" %in% info) {
      metadata[["direction"]] <- py_to_r(s2_gdal$GetMetadata()[["DATATAKE_1_SENSING_ORBIT_DIRECTION"]])
    }
    if ("orbit_n" %in% info) {
      metadata[["orbit_n"]] <- py_to_r(s2_gdal$GetMetadata()[["DATATAKE_1_SENSING_ORBIT_NUMBER"]])
    }
    if ("preview_url" %in% info) {
      metadata[["preview_url"]] <- py_to_r(s2_gdal$GetMetadata()[["PREVIEW_IMAGE_URL"]])
    }
    if ("proc_baseline" %in% info) {
      metadata[["proc_baseline"]] <- py_to_r(s2_gdal$GetMetadata()[["PROCESSING_BASELINE"]])
    }
    if ("level" %in% info) {
      metadata[["level"]] <- py_to_r(s2_gdal$GetMetadata()[["PROCESSING_LEVEL"]])
    }
    if ("sensing_datetime" %in% info) {
      start_time <- as.POSIXct(
        py_to_r(s2_gdal$GetMetadata()[["PRODUCT_START_TIME"]]), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
      stop_time <- as.POSIXct(
        py_to_r(s2_gdal$GetMetadata()[["PRODUCT_STOP_TIME"]]), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
      metadata[["sensing_datetime"]] <- if (start_time == stop_time) {
        start_time
      } else {
        c(start_time, stop_time)
      }
    }
    if ("nodata_value" %in% info) {
      metadata[["nodata_value"]] <- py_to_r(s2_gdal$GetMetadata()[["SPECIAL_VALUE_NODATA"]])
    }
    if ("saturated_value" %in% info) {
      metadata[["saturated_value"]] <- py_to_r(s2_gdal$GetMetadata()[["SPECIAL_VALUE_SATURATED"]])
    }

  }

  # return
  if (length(metadata)>1) {
    return(metadata)
  } else {
    return(unlist(metadata))
  }

}

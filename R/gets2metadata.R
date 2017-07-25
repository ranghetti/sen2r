
#' @importFrom tools file_path_as_absolute


s2_getmetadata <- function(s2, info, readfile) {

  # define regular expressions to identify products
  s2_regex <- list("compactname_main_xml" = list("regex" = "^MTD\\_MSIL([12][AC])\\.xml$", "elements" = c("level")),
                "compactname_main_path" = list("regex" = "^S(2[AB])\\_MSIL([12][AC])\\_([0-9]{8}T[0-9]{6})\\_N([0-9]{4})\\_R([0-9]{3})\\_T([A-Z0-9]{5})_[0-9]{8}T[0-9]{6}\\.SAFE$",
                                               "elements" = c("mission","level","sensing_datetime","id_baseline","id_orbit","id_tile")),
                "oldname_main_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_PRD\\_MSIL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.SAFE$",
                                           "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")),
                "oldname_granule_path" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MSI\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\_N([0-9]{2})\\.([0-9]{2})$",
                                              "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","proc_baseline_x","proc_baseline_y")),
                "oldname_granule_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_L([12][AC])\\_TL\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_A([0-9]{6})\\_T([A-Z0-9]{5})\\.xml$",
                                              "elements" = c("mission","file_class","level","centre","creation_datetime","orbit_number","proc_baseline_x","proc_baseline_y")),
                "oldname_main_xml" = list("regex" = "^S(2[AB])\\_([A-Z]{4})\\_MTD\\_SAFL([12][AC])\\_(.{4})\\_([0-9]{8}T[0-9]{6})\\_R([0-9]{3})\\_V[0-9]{8}T[0-9]{6}\\_([0-9]{8}T[0-9]{6})\\.xml$",
                                          "elements" = c("mission","file_class","level","centre","creation_datetime","id_orbit","sensing_datetime")))

  metadata <- list() # output object, with requested metadata

  # If s2 is a string, check it and retrieve file metadata
  if (is(s2, "character")) {

    # If s2 is a path:
    # convert in absolute path (and check that file exists)
    s2_path <- file_path_as_absolute(s2)

    # retrieve the name of xml main file
    # if it is a directory, scan the content
    if (file.info(s2_path)$isdir) {
      compactname_xmlfile <- list.files(s2_path,s2_regex$compactname_main_xml$regex, full.names=TRUE)
      oldname_xmlfile <- list.files(s2_path,s2_regex$oldname_main_xml$regex, full.names=TRUE)
    } else {
      compactname_xmlfile <- s2_path[grep(s2_regex$compactname_main_xml$regex, basename(s2_path))]
      oldname_xmlfile <- s2_path[grep(s2_regex$oldname_main_xml$regex, basename(s2_path))]
    }

    # check version
    if (length(compactname_xmlfile)==0) {
      if (length(oldname_xmlfile)==0) {
        prod_notfound <- 1
      } else {
        s2_version <- "old"
        name_xmlfile <- oldname_xmlfile
      }
    } else {
      if (length(oldname_xmlfile)==0) {
        s2_version <- "compact"
        name_xmlfile <- compactname_xmlfile
      } else {
        stop("This product is not in the right format (two xml metadata files were found).")
      }
    }

    if ("version" %in% info) { # return the version if required
      metadata[["version"]] <- s2_version
    }

    # if nameinfo is required, metadata from file name are read
    if ("nameinfo" %in% info) {
      # for old names, retreive from xml name
      if (s2_version=="old") {
        for (var in s2_regex$oldname_main_xml$elements) {
          metadata[[var]] <- gsub(
            s2_regex$oldname_main_xml$regex,
            paste0("\\",which(s2_regex$oldname_main_xml$elements==var)),
            name_xmlfile)
          # format if it is a date or a time
          if (length(grep("\\_datetime",var))==1) {
            metadata[[var]] <- as.POSIXct(metadata[[var]], format="%Y%m%dT%H%M%S", tz="UTC")
          }
        }
      } else { # for compact names, retrieve from directory name
        # TODO
      }
    }

    # info on tile[s]
    if ("tiles" %in% info) {
      # TODO
    }


    # if necessary, read the file for further metadata
    if (any(c("clouds","direction","orbit_n","preview_url",
              "proc_baseline","level","sensing_datetime",
              "nodata_value","saturated_value") %in% info)) {

      gdal <- import("osgeo",convert=FALSE)$gdal

      s2_gdal <- gdal$Open(name_xmlfile)
      # in case of error (old names), try to read a single granule
      if (is(s2_gdal,"python.builtin.NoneType")) {
        first_granule <- list.files(file.path(dirname(name_xmlfile),"GRANULE"),full.names=TRUE)[1]
        first_granule_xml <- list.files(first_granule,s2_regex$oldname_granule_xml$regex,full.names=TRUE)
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
      metadata[["sensing_datetime"]] <- as.POSIXct(
        py_to_r(s2_gdal$GetMetadata()[["SENSING_TIME"]]), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
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

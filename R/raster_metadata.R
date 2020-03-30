#' @title Get metadata from raster paths
#' @description This accessory function extract some useful metadata from
#'  a vector of raster paths.
#' @param raster_paths A vector of raster paths.
#' @param meta Vector with the desired metadata: one or more values among
#'  'res', 'size', 'bbox', 'proj', 'unit', 'outformat', 'type'.
#'  Alternatively meta = 'all' (default) allows to return all metadata.
#' @param format One between `data.table` (default), `data.frame` and `list`.
#' @return A data.table, data.frame or list of the output metadata.
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export
#' @import data.table
#' @importFrom stars read_stars st_dimensions
#' @importFrom sf gdal_crs st_bbox st_crs st_as_text gdal_utils
#' @importFrom methods is
#' @examples
#' # Define product names
#' examplenames <- c(
#'   system.file("tif/L7_ETMs.tif", package="stars"),
#'   system.file("nc/bcsd_obs_1999.nc", package = "stars"),
#'   system.file("extdata/out/S2A2A_20190723_022_Barbellino_BOA_10.tif",
#'     package = "sen2r")
#' )
#'
#' \donttest{
#' # Return metadata as data.table
#' raster_metadata(examplenames)
#' }
#'
#' # Return some metadata as data.table
#' raster_metadata(examplenames, c("res", "size", "bbox", "outformat"))
#' 
#' # Return some metadata as list
#' raster_metadata(examplenames, c("res", "size", "bbox", "proj"), format = "list")
#'
#' # Output with an invalid raster
#' examplenames <- c(
#'   examplenames, 
#'   system.file("extdata/settings/gdal_formats.json", package="sen2r")
#' )
#' raster_metadata(examplenames, c("bbox", "proj"))

raster_metadata <- function(raster_paths, meta = "all", format = "data.table") {
  
  # to avoid NOTE on check
  . <- NULL
  
  # levels requiring a stars object
  meta_lev_stars <- c("res", "size", "bbox", "proj", "unit")
  # levels requiring a gdalinfo output
  meta_lev_gdalinfo <- c("outformat", "type")
  # all available levels of 'meta' argument
  meta_lev <- c(meta_lev_stars, meta_lev_gdalinfo)
  # check metadata to be returned
  if (all(meta == "all")) {
    meta <- c("res", "size", "nbands", "bbox", "proj", "unit", "outformat", "type")
  } else if (any(! meta %in% meta_lev)) {
    print_message(
      type = "error",
      "argument 'meta' must contain one of more among '",
      paste(meta_lev, collapse = "', '"),"'."
    )
  }
  # metadata groups
  meta_stars <- any(meta %in% meta_lev_stars)
  meta_gdalinfo <- any(meta %in% meta_lev_gdalinfo)
  
  
  out_list <- list()
  for (i in seq_along(raster_paths)) {
    
    raster_path <- raster_paths[i]
    if (meta_stars) {
      sel_raster <- suppressWarnings(suppressMessages(try(
        read_stars(raster_path, proxy = TRUE, quiet = TRUE),
        silent = TRUE
      )))
    }
    if (meta_gdalinfo) {
      metadata_raw <- suppressWarnings(suppressMessages(try(
        trimws(unlist(
          strsplit(gdal_utils("info", raster_path, quiet = TRUE), "\n")
        )),
        silent = TRUE
      )))
    }
    sel_raster_isvalid <- if (meta_stars) {
      !is(sel_raster, "try-error")
    } else {
      !is(metadata_raw, "try-error")
    }
    
    # read metadata
    out_list[[i]] <- list(
      "path" = raster_path,
      "valid" = sel_raster_isvalid
    )
    
    if (sel_raster_isvalid) {
      
      if (any(c("res", "size", "nbands") %in% meta)) {
        ref_res <- sapply(st_dimensions(sel_raster), function(xy){abs(xy$delta)})
        ref_size <- sapply(st_dimensions(sel_raster), function(xy){xy$to})
        if (length(ref_size) > 2) {
          ref_nbands <- as.integer(ref_size[3])
          ref_size <- ref_size[1:2]
          ref_res <- ref_res[1:2]
        } else {
          ref_nbands <- 1
        }
      }
      if ("res" %in% meta) {
        out_list[[i]][["res"]] <- ref_res
      }
      if ("size" %in% meta) {
        out_list[[i]][["size"]] <- ref_size
      }
      if ("nbands" %in% meta) {
        out_list[[i]][["nbands"]] <- ref_nbands
      }
      
      if (any(c("bbox", "proj", "unit") %in% meta)) {
        ref_bbox <- st_bbox(sel_raster)
        ref_proj <- attr(ref_bbox, "crs")
        # Temporary patch: remove this when stars will be fixed to be 
        ## compatible with sf >= 0.9.0 (#295)
        if (is.na(ref_proj)) {
          ref_proj <- gdal_crs(raster_path)
          ref_bbox <- st_bbox(setNames(as.numeric(ref_bbox), names(ref_bbox)), crs = ref_proj)
        }
      }
      if ("bbox" %in% meta) {
        out_list[[i]][["bbox"]] <- ref_bbox
      }
      if ("proj" %in% meta) {
        out_list[[i]][["proj"]] <- ref_proj
      }
      if ("unit" %in% meta) {
        out_list[[i]][["unit"]] <- as.character(projpar(ref_proj, "unit"))
      }
      
      if ("outformat" %in% meta) {
        out_list[[i]][["outformat"]] <- gsub(
          "Driver: ?([A-Za-z0-9_]+)/.*$", "\\1",
          metadata_raw[grepl("Driver:", metadata_raw)]
        )
      }
      
      if ("type" %in% meta) {
        out_list[[i]][["type"]] <- gsub(
          "Band [0-9]+.+Type ?= ?([A-Za-z0-9]+),.*$", "\\1",
          metadata_raw[grepl("Band [0-9]+.+Type ?=", metadata_raw)][1]
        )
      }
      
      # if (format %in% c("data.frame", "data.table")) {
      #   data.frame(
      #     "path" = raster_path,
      #     "valid" = sel_raster_isvalid,
      #     "res.x" = ref_res["x"],
      #     "res.y" = ref_res["y"],
      #     "size.x" = ref_size["x"],
      #     "size.y" = ref_size["y"],
      #     "xmin" = ref_bbox$xmin,
      #     "ymin" = ref_bbox$ymin,
      #     "xmax" = ref_bbox$xmax,
      #     "ymax" = ref_bbox$ymax,
      #     "proj" = ref_proj$proj4string,
      #     "unit" = ref_unit,
      #     "outformat" = ref_outformat,
      #     "type" = ref_outtype,
      #     stringsAsFactors = FALSE
      #   )
      # } else if (format == "list") {
      #   list(
      #     "path" = raster_path,
      #     "valid" = sel_raster_isvalid,
      #     "res" = ref_res,
      #     "size" = ref_size,
      #     "bbox" = ref_bbox,
      #     "proj" = ref_proj,
      #     "unit" = ref_unit,
      #     "outformat" = ref_outformat,
      #     "type" = ref_outtype
      #   )
      # }
      
    }
    
  } # end of foreach cycle
  
  if (format %in% c("data.frame", "data.table")) {
    out_dt <- rbindlist(lapply(out_list, function(l) {
      sel_dt <- data.frame(
        "path" = l$path,
        "valid" = l$valid
      )
      if (l$valid) {
        if ("res" %in% meta) {
          sel_dt$res.x <- l$res["x"]
          sel_dt$res.y <- l$res["y"]
        }
        if ("size" %in% meta) {
          sel_dt$size.x <- l$size["x"]
          sel_dt$size.y <- l$size["y"]
        }
        if ("nbands" %in% meta) {sel_dt$nbands <- l$nbands}
        if ("bbox" %in% meta) {
          sel_dt$xmin <- l$bbox["xmin"]
          sel_dt$ymin <- l$bbox["ymin"]
          sel_dt$xmax <- l$bbox["xmax"]
          sel_dt$ymax <- l$bbox["ymax"]
        }
        if ("proj" %in% meta) {
          sel_dt$proj <- if (!is.na(l$proj$epsg)) {
            paste0("EPSG:",l$proj$epsg)
          } else if (!is.na(l$proj)) {
            st_as_text_2(l$proj)
          } else {
            NA
          }
        }
        if ("unit" %in% meta) {sel_dt$unit <- l$unit}
        if ("outformat" %in% meta) {sel_dt$outformat <- l$outformat}
        if ("type" %in% meta) {sel_dt$type <- l$type}
      }
      sel_dt
    }), fill = TRUE)
    if (format == "data.frame") {
      as.data.frame(out_dt)
    } else {
      out_dt
    }
  } else if (format == "list") {
    out_list
  }
  
}

#' @title Compute maps of spectral indices
#' @description Create maps of a set of spectral indices. Since
#'  `gdal_calc.py` is used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#' @param infiles A vector of input filenames. Input files are paths
#'  of BOA (or TOA) products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the fidolasen-S2 naming convention
#'  ([s2_shortname]).
#' @param indices Character vector with the names of the required
#'  indices. Values should be included in names corresponding to the
#'  Abbreviations of the following indices:
#'  [IDB](http://www.indexdatabase.de/db/is.php?sensor_id=96)
#'  FIXME the list of the accepted values is a subset; this reference
#'  will be replaced with an internal html page integrated in the
#'  shiny interface).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param parameters (optional) Values of index parameters. This variable
#'  must be a named list, in which each element is a list of parameters,
#'  i.e.:
#'  `parameters = list("SAVI" = list("a" = 0.5))`
#'  Values can be both numeric values or band names (e.g. "band_1").
#'  If not specified, parameters are set to default values.
#' @param source (optional) Vector with the products from which computing
#'  the indices. It can be "BOA", "TOA" or both (default). If both values
#'  are provided, indices are computed from the available products ("TOA"
#'  if TOA is available, BOA if BOA is available); in the case both are
#'  available, two files are produced (they can be distinguished from the
#'  level component - S2x1C or S2x2A - in the filename).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or "GTiff" in case of VRT input images).
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param dataType (optional) Numeric datatype of the ouptut rasters:
#'  if "Float32" (default) or "Float64" is chosen, numeric values are not rescaled;
#'  if "Int16" or "UInt16", values are multiplicated by a 10000
#'  scale factor.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @return A vector with the names of the created products.
#' @export
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @importFrom rgdal GDALinfo
#' @importFrom reticulate import
#' @importFrom magrittr "%>%"
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_calcindices <- function(infiles,
                           indices,
                           outdir=".",
                           parameters=NULL,
                           source=c("TOA","BOA"),
                           format=NA,
                           subdirs=NA,
                           compress="DEFLATE",
                           dataType="Float32",
                           overwrite=FALSE) {

  prod_type <- . <- NULL

  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal

  # Load GDAL paths
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (is.null(binpaths$gdalinfo)) {
    check_gdal()
  }
  
  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(c("n_index","name","longname","s2_formula","a","b","x"))

  # check that the required indices exists
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type="error",
      if (!any(indices %in% indices_db$name)) {"The "} else {"Some of the "},
      "requested index names (\"",
      paste(indices[!indices %in% indices_db$name], collapse="\", \""),
      ,"\") are not recognisable; please use accepted ",
      "values. To list accepted index names, type ",
      "'sort(list_indices(\"name\"))'.")
  }
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type="warning",
      "Some of the specified index names (",
      paste(indices[!indices %in% indices_db$name],collapse=", "),
      ") are not recognisable and will be skipped.")
    indices <- indices[indices %in% indices_db$name]
  }
  # exstract needed indices_db
  indices_info <- indices_db[match(indices,indices_db$name),]

  # check output format
  if (!is.na(format)) {
    sel_driver <- gdal$GetDriverByName(format)
    if (is.null(py_to_r(sel_driver))) {
      print_message(
        type="error",
        "Format \"",format,"\" is not recognised; ",
        "please use one of the formats supported by your GDAL installation.\n\n",
        "To list them, use the following command:\n",
        "gdalUtils::gdalinfo(formats=TRUE)\n\n",
        "To search for a specific format, use:\n",
        "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]")
    }
  }

  # Get files metadata
  infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
  infiles <- infiles[infiles_meta$prod_type %in% source]
  infiles_meta <- infiles_meta[prod_type %in% source,]

  # create subdirs (if requested)
  if (is.na(subdirs)) {
    subdirs <- ifelse(length(indices)>1, TRUE, FALSE)
  }
  if (subdirs) {
    sapply(file.path(outdir,indices), dir.create, showWarnings=FALSE)
  }

  # read TOA/BOA image
  outfiles <- character(0)
  for (i in seq_along(infiles)) {
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])
    sel_format <- suppressWarnings(ifelse(
      !is.na(format), format, attr(GDALinfo(sel_infile), "driver")
    )) %>% ifelse(.!="VRT",.,"GTiff")
    sel_out_ext <- ifelse(
      sel_format=="ENVI", "dat",
      unlist(strsplit(paste0(py_to_r(sel_driver$GetMetadataItem(gdal$DMD_EXTENSIONS))," ")," "))[1])

    # check bands to use
    if (sel_infile_meta$prod_type=="TOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:12],"band"=paste0("band_",1:12))
    } else if (sel_infile_meta$prod_type=="BOA") {
      gdal_bands <- data.frame("letter"=LETTERS[1:11],"band"=paste0("band_",c(1:9,11:12)))
    } else {
      print_message(type="error", "Internal error (this should not happen).")
    }

    # compute single indices
    for (j in seq_along(indices)) {

      # extract parameters
      sel_parameters <- parameters[[indices[j]]]

      # define output filename
      sel_outfile <- paste0(
        "S2",sel_infile_meta$mission,sel_infile_meta$level,"_",
        strftime(sel_infile_meta$sensing_date,"%Y%m%d"),"_",
        sel_infile_meta$id_orbit,"_",
        sel_infile_meta$id_tile,"_",
        indices_info[j,"name"],"_",
        gsub("m$","",sel_infile_meta$res),".",
        sel_out_ext)

      # define subdir
      out_subdir <- ifelse(subdirs, file.path(outdir,indices[j]), outdir)

      # if output already exists and overwrite==FALSE, do not proceed
      if (!file.exists(file.path(out_subdir,sel_outfile)) | overwrite==TRUE) {

        # change index formula to be used with bands
        for (sel_par in c("a","b","x")) {
          assign(sel_par, if (is.null(sel_parameters[[sel_par]])) {indices_info[j,sel_par]} else {sel_parameters[[sel_par]]})
        }
        sel_formula <- indices_info[j,"s2_formula"]
        for (sel_par in c("a","b","x")) {
          sel_formula <- gsub(paste0("([^0-9a-zA-Z])par\\_",sel_par,"([^0-9a-zA-Z])"),
                              paste0("\\1",get(sel_par),"\\2"),
                              sel_formula)
        }
        for (sel_band in seq_len(nrow(gdal_bands))) {
          sel_formula <- gsub(paste0("([^0-9a-zA-Z])",gdal_bands[sel_band,"band"],"([^0-9a-zA-Z])"),
                              paste0("\\1",gdal_bands[sel_band,"letter"],".astype(float)\\2"),
                              sel_formula)
        }
        if (dataType %in% c("Int16","UInt16","Int32","UInt32")) {
          sel_formula <- paste0("10000*(",sel_formula,")")
        }

        # apply gdal_calc
        system(
         paste0(
           binpaths$gdal_calc.py," ",
           paste(apply(gdal_bands,1,function(l){
             paste0("-",l["letter"]," \"",sel_infile,"\" --",l["letter"],"_band=",which(gdal_bands$letter==l["letter"]))
           }), collapse=" ")," ",
           "--outfile=\"",file.path(out_subdir,sel_outfile),"\" ",
           "--type=\"",dataType,"\" ",
           "--format=\"",sel_format,"\" ",
           if (overwrite==TRUE) {"--overwrite"},
           if (sel_format=="GTiff") {paste0("--co=\"COMPRESS=",toupper(compress),"\" ")},
           "--calc=\"",sel_formula,"\""
         ),
         intern = Sys.info()["sysname"] == "Windows"
       )

      } # end of overwrite IF cycle

      outfiles <- c(outfiles, file.path(out_subdir,sel_outfile))

    }

  } # end cycle on infiles

  return(outfiles)

}


# function to create maps of spectral indices
# from TOA / BOA images (create vrt if missing), load the index formula from a json
# and compute final map




#' @title Compute maps of spectral indices
#' @description Create maps of a set of spectral indices.
#' @param infiles A vector of input filenames. Input files are paths
#'  of BOA (or TOA) products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the fidolasen-S2 naming convention
#'  ([s2_shortname]).
#' @param indices Character vector with the names of the required
#'  indices. Values should be included in names corresponding to the
#'  Abbreviations of the following indices:
#'  [IDB](http://www.indexdatabase.de/db/is.php?sensor_id=96)
#'  # FIXME the list of the accepted values is a subset; this reference
#'  will be replaced with an internal html page integrated in the
#'  shiny interface).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param tmpdir (optional) Path where intermediate VRT will be created.
#'  Default is in a hidden subdirectory (called `.vrt`) of the common parent
#'  directory of `infiles`. Set `tmpdir=tempdir()` if you do not want to
#'  keep the intermediate files after reboot.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param dataType (optional) Numeric datatype of the ouptut rasters:
#'  if "Float32" or "Float64" is chosen, numeric values are not rescaled;
#'  if "Int16" (default) or "UInt16", values are multiplicated by a 10000
#'  scale factor.
#' @return A vector with the names of the created products.
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_calcindices <- function(infiles,
                           indices,
                           outdir=".",
                           subdirs=NA,
                           tmpdir=NA,
                           compress="DEFLATE",
                           dataType="Int16") {

  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(c("n_index","name","longname","s2_formula"))

  # check that the required indices exists
  if (!any(indices %in% indices_db$name)) {
    print_message(
      type="error",
      "The requested index names are not recognisable; please use accepted ",
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

  # Get files metadata
  infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
  infiles_meta <- infiles_meta[prod_type %in% c("TOA","BOA"),]

  # read TOA/BOA image
  outfiles <- character(0)
  for (i in seq_along(infiles)) {
    sel_infile <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])

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

      # define output filename
      sel_outfile <- paste0(
        "S2",sel_infile_meta$mission,sel_infile_meta$level,"_",
        strftime(sel_infile_meta$sensing_date,"%Y%m%d"),"_",
        sel_infile_meta$id_orbit,"_",
        sel_infile_meta$id_tile,"_",
        indices_info[j,"name"],"_",
        gsub("m$","",sel_infile_meta$res),".",
        sel_infile_meta$file_ext)

      # change index formula to be used with bands
      sel_formula <- indices_info[j,"s2_formula"]
      for (b in seq_len(nrow(gdal_bands))) {
        sel_formula <- gsub(paste0("([^0-9a-zA-Z])",gdal_bands[b,"band"],"([^0-9a-zA-Z])"),
                            paste0("\\1",gdal_bands[b,"letter"],"\\2"),
                            sel_formula)
      }
      if (dataType %in% c("Int16","UInt16")) {
        sel_formula <- paste0("10000*(",sel_formula,")")
      }

      # apply gdal_calc
      system(
        paste0(
          Sys.which("gdal_calc.py")," ",
          paste(apply(gdal_bands,1,function(l){
            paste0("-",l["letter"]," \"",sel_infile,"\" --",l["letter"],"_band=",which(gdal_bands$letter==l["letter"]))
          }), collapse=" ")," ",
          "--outfile=\"",file.path(outdir,sel_outfile),"\" ",
          "--type=\"",dataType,"\" ",
          "--format=\"","VRT","\" ", # FIXME correct format
          "--calc=\"",sel_formula,"\""
        ),
        intern = Sys.info()["sysname"] == "Windows"
      )

      outfiles <- c(outfiles, file.path(outdir,sel_outfile)) # TODO use out_subdir for different indices

    }

  } # end cycle on infiles

  return(outfiles)

}






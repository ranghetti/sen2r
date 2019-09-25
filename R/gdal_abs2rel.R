#' @title Convert absolute from/to relative paths in a virtual file
#' @description The two functions read the content of a GDAL virtual
#'  file (VRT) and check the presence of paths to linked files.
#'
#'  [gdal_abs2rel] scans the presence of absolute paths: when an
#'  absolute path has a common parent directory with the path in which
#'  the VRT is, this is replaced with a relative. This is useful when
#'  VRT are on a remote driver, which can be mounted to several points.
#' @param in_vrt The path of the VRT to be read.
#' @param out_vrt (optional) The path of the output VRT file (default
#'  is to overwrite `in_vrt`).
#' @return NULL (the function is called for its side effects)
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom magrittr "%>%"
#' @name gdal_abs2rel
#' @rdname gdal_abs2rel_rel2abs
#' @examples
#' # Load a VRT containing a relative path
#' ex_vrt <- system.file(
#'   "extdata/example_files/out_ref/S2A2A_20170703_022_Barbellino_RGB432B_10.vrt", 
#'   package = "sen2r"
#' )
#' abs_vrt <- tempfile(fileext = "_abs.vrt")
#' rel_vrt <- tempfile(fileext = "_rel.vrt")
#' gdal_rel2abs(ex_vrt, abs_vrt)
#' gdal_abs2rel(ex_vrt, rel_vrt)
#' 
#' # Show differences
#' ex_vrt_content <- readLines(ex_vrt)
#' abs_vrt_content <- readLines(abs_vrt)
#' rel_vrt_content <- readLines(rel_vrt)
#' ex_vrt_content[ex_vrt_content != abs_vrt_content] # Original line
#' abs_vrt_content[ex_vrt_content != abs_vrt_content] # Modified line
#' rel_vrt_content[ex_vrt_content != rel_vrt_content] # No edits

gdal_abs2rel <- function(in_vrt, out_vrt=NA) {
  
  # to avoid NOTE on check
  . <- NULL
  
  if (is.na(out_vrt)) {
    out_vrt <- in_vrt
  }
  
  if (!file.exists(in_vrt)) {
    print_message(type="error", "Input file (\"",in_vrt,"\") does not exists.")
  }
  
  vrt_text <- readLines(in_vrt)
  
  # path_regex <- "^ *<SourceFilename relativeToVRT=\"0\">(.*)</SourceFilename> *$"
  path_regex <- "^.* relativeToVRT=\"0\">(.*)</.*"
  
  for (sel_line in grep(path_regex, vrt_text)) {
    abs_path <- gsub(path_regex, "\\1", vrt_text[sel_line])
    rel_path <- suppressWarnings(abs2rel(abs_path, dirname(in_vrt)))
    if (abs_path != rel_path) {
      vrt_text[sel_line] <- vrt_text[sel_line] %>%
        gsub(abs_path, rel_path, ., fixed=TRUE) %>%
        gsub("relativeToVRT=\"0\"", "relativeToVRT=\"1\"", ., fixed=TRUE)
    }
  }
  writeLines(vrt_text, out_vrt)
  
}


#' @description [gdal_rel2abs] checks the presence of relative paths,
#'  and replace them with the corresponding absolute path (symbolic
#'  links are followed). This is useful to grant that VRT can be moved
#'  (if the files they link to are not moved).
#' @export
#' @importFrom magrittr "%>%"
#' @name gdal_rel2abs
#' @rdname gdal_abs2rel_rel2abs
gdal_rel2abs <- function(in_vrt, out_vrt=NA) {
  
  # to avoid NOTE on check
  . <- NULL
  
  if (is.na(out_vrt)) {
    out_vrt <- in_vrt
  }
  
  if (!file.exists(in_vrt)) {
    print_message(type="error", "Input file (\"",in_vrt,"\") does not exists.")
  }
  
  vrt_text <- readLines(in_vrt)
  
  # path_regex <- "^ *<SourceFilename relativeToVRT=\"1\">(.*)</SourceFilename> *$"
  path_regex <- "^.* relativeToVRT=\"1\">(.*)</.*"
  
  for (sel_line in grep(path_regex, vrt_text)) {
    rel_path <- gsub(path_regex, "\\1", vrt_text[sel_line])
    abs_path <- expand_path(rel_path, dirname(in_vrt), normalize=TRUE)
    vrt_text[sel_line] <- vrt_text[sel_line] %>%
      gsub(rel_path, abs_path, ., fixed=TRUE) %>%
      gsub("relativeToVRT=\"1\"", "relativeToVRT=\"0\"", ., fixed=TRUE)
  }
  
  writeLines(vrt_text, out_vrt)
  
}

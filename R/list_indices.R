#' @title List spectral indices
#' @description Return a table with attributes of the spectral indices
#'  computable with the package.
#' @param values A vector of attributes which will be returned, being
#'  one or more within the followings:
#'  - `n_index`: internal index identifiers;
#'  - `name`: index name;
#'  - `longname`: index description;
#'  - `link`: URL to the index description page;
#'  - `s2_formula`: expression containing the formula to compute the index;
#'  - `s2_formula_mathml`: MathML version of the formula;
#'  - `checked`: logical (TRUE for verified indices);
#'  - `a`, `b`, `x`: parameter values (NA for non required parameters).
#' @param pattern A regular expression on index names.
#' @param all Logical: if TRUE, all the indices retrieved from 
#'  [IDB](http://www.indexdatabase.de/db/is.php?sensor_id=96) are returned;
#'  if FALSE (default), only indices checked by the authors are returned.
#' @return A data.frame with the required information. The table contains
#'  also the following attributes:
#'  - `creation_date`: timestamp of the creation date of the indices archive;
#'  - `pkg_version`: version of the `sen2r` package used to
#'      create the indices archive.
#' @export
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @examples
#' # Show index names
#' list_indices(c("name","longname"))
#' 
#' # Return the MSAVI2 formula
#' list_indices("s2_formula", "^MSAVI2$")
#' 
#' # Return all index names (including unchecked)
#' list_indices("name", all = TRUE)

list_indices <- function(values, pattern="", all=FALSE) {
  
  # generate indices.json if missing
  create_indices_db()
  
  # read indices database
  json_path <- system.file("share/indices.json",package="sen2r")
  indices <- jsonlite::fromJSON(json_path)
  
  # select requested values from the table
  for (sel_par in c("a","b","x")) {
    if (is.null(indices$indices[[sel_par]])) {
      indices$indices[[sel_par]] <- as.numeric(NA)
    }
  }
  
  #filter only checked indices
  if (all == FALSE) {
    indices$indices <- indices$indices[indices$indices$checked,]
  }
  
  indices$indices <- indices$indices[grep(pattern,indices$indices$name),values]
  attr(indices$indices, "pkg_version") <- package_version(indices$pkg_version)
  attr(indices$indices, "creation_date") <- as.POSIXct(indices$creation_date)
  
  # return requested values
  return(indices$indices)
  
}

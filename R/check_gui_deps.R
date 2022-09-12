#' @title Check GUI dependencies
#' @description Check if suggested dependencies required to run the 
#'  sen2r GUI are present.
#' @details Packages required to run the GUI (**`shiny`**- and 
#'  **`leaflet`**-related packages), which were **`sen2r`** dependencies until
#'  version 1.3.4, are now suggested dependencies.
#'  **`sen2r`** can be used without them with the exception of the GUI.
#' @param abort Logical parameter: if TRUE (default), the function aborts in case
#'  some packages need to be installed; if FALSE, a warning is shown.
#' @return Logical (invisible): TRUE if all the required packages are installed,
#'  FALSE (if `abort = FALSE`) or an error if some is missing.
#'
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @export
#' @keywords internal
#' @examples
#' \donttest{
#' check_gui_deps()
#' }

check_gui_deps <- function(abort = TRUE) {
  
  # Packages required to run the GUI
  gui_deps <- c(
    "leaflet",
    "leafpm",
    "mapedit",
    "shiny",
    "shinyFiles",
    "shinydashboard",
    "shinyjs",
    "shinyWidgets"
  )
  gui_deps_missing <- !sapply(gui_deps, requireNamespace, quietly = TRUE)

  # Install missing dependencies  
  if (sum(gui_deps_missing) > 0) {
    print_message(
      type = ifelse(abort, "error", "warning"),
      "Some missing packages are needed to run the GUI; ",
      "please install them with the command \n",
      " > install.packages(c(\"",
      paste(names(gui_deps_missing)[gui_deps_missing], collapse = "\", \""),"\"))"
    )
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
  
}
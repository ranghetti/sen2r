#' @title Order S2 products (deprecated).
#' @description This function is deprecated and will be removed.
#' @param s2_prodlist deprecated
#' @param export_prodlist deprecated
#' @param delay deprecated
#' @param apihub deprecated
#' @param service deprecated
#' @param reorder deprecated
#' @return deprecated
#' @export

s2_order <- function(
  s2_prodlist = NULL, 
  export_prodlist = TRUE, 
  delay = 0.5, 
  apihub = NA, 
  service = NA,
  reorder = TRUE
) {
  return(invisible(NULL))
}

# internal function, used internally in order not to repeat the check
# for online availability
.s2_order <- function(
  s2_prodlist = NULL,
  export_prodlist = TRUE, 
  delay = 0.5,
  apihub = NA,
  service = NA,
  reorder = TRUE,
  .s2_availability = NULL,
  .log_path = TRUE # TRUE to log all, FALSE to skip the path of the json
) {
  return(invisible(NULL))
}

#' @title Check a path
#' @description Accessory functions to check that a directory exists and
#'  the user have write permissions on it (to be used in a Shiny context)
#' @param path `string` full path to a folder
#' @param mustbe_empty `logical` if TRUE, accept only empty directories
#' @param mustbe_writable `logical` if TRUE, accept only directories with write 
#'  permissions
#' @rdname path_check
#' @author Luigi Ranghetti, PhD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @author Lorenzo Busetto, PhD (2019) \email{lbusett@@gmail.com>}
#' @importFrom shiny renderUI span renderText
path_check <- function(path, mustbe_empty = FALSE, mustbe_writable = TRUE) {
  if (all(length(path)>0, path[1]!="")) {
    if (!dir.exists(path)) {
      out <- renderUI(span(
        style="color:red",
        "\u2718 (the directory does not exist)"
      ))
      attr(out, "isvalid") <- FALSE
    } else if (mustbe_empty==TRUE & length(list.files(path, all.files = TRUE))>2) {
      out <- renderUI(span(
        style="color:red",
        "\u2718 (the directory is not empty)"
      ))
      attr(out, "isvalid") <- FALSE
    } else if (mustbe_writable == TRUE & file.access(path, mode=2)<0) {
      out <- renderUI(span(
        style="color:red",
        "\u2718 (missing write permissions)"
      ))
      attr(out, "isvalid") <- FALSE
    } else {
      out <- renderUI(span(
        style="color:darkgreen",
        "\u2714"
      ))
      attr(out, "isvalid") <- TRUE
    }
    #
  } else {
    out <- renderText("")
    attr(out, "isvalid") <- NA
  }
  return(out)
}

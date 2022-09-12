#' @title Selective suppress warnings
#' @description Suppress warnings matching particular regular expressions.
#' @details See https://stackoverflow.com/questions/16517795/selective-suppresswarnings-that-filters-by-regular-expression
#' @param .expr Code to evaluate
#' @param .f A regular expression (which will be passed to `grepl()`).
#' @return The warning message as character string, invisibly.
#' @keywords internal
suppress_warnings <- function(.expr, .f) {
  eval.parent(substitute(
    withCallingHandlers( .expr, warning = function(w) {
      cm <- conditionMessage(w)
      # cond <- 
      #   if(is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm,...)
      # (simplified to avoid calling rlang)
      cond <- grepl(.f, cm)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}

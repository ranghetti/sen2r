#' @title Find the longest common starting substring or directory
#' @description The function search for the longest common prefix between
#'  multiple strings.
#' @param data A vector of strings
#' @param sep A character which is used to separate elements; default ("")
#'  is used to compare single characters; other useful alternatives are
#'  "/" (or "\\\\\\\\" in Windows) to find the longest common directory, or
#'  " " to compare words instead of characters.
#' @return A character with the longest common initial substring
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note Modified from a suggestion taken from
#'  [stackoverflow](https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of).
#' @export
#' @importFrom methods is
#' @importFrom magrittr "%>%"
#'
#' @examples
#' strings <- c("/home/lranghetti/git/fidolasen",
#'              "/home/lranghetti/git_data/fidolasen/ex/vrt/01_translate/")
#'
#' comsub(strings)
#'
#' comsub(strings, sep="/")

comsub <- function(data, sep="") {
  . <- NULL # to avoid NOTE on check
  data_spl <- strsplit(data,sep)
  data_spl_maxlength <- max(sapply(strsplit(data,sep), length))
  which_max <- if (length(unique(data)) > 1) {
    lapply(data_spl, `length<-`, data_spl_maxlength) %>%
      do.call(rbind,.) %>%
      apply(2, function(i){!length(unique(i))==1}) %>%
    which.max() -1
  } else {
    length(data_spl[[1]]) - 1 # FIXME ok for dir  with "/", but not with ""
  }
  paste(c(data_spl[[1]][seq_len(which_max)],""), collapse=sep)
}






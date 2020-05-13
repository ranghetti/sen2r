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
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note Modified from a suggestion taken from
#'  [stackoverflow](https://stackoverflow.com/questions/28273716/r-implementation-for-finding-the-longest-common-starting-substrings-in-a-set-of).
#' @export
#' @importFrom methods is
#'
#' @examples
#' strings <- c("/home/user/git/sen2r",
#'              "/home/user/git_data/sen2r/ex/vrt/01_translate/")
#'
#' comsub(strings)
#'
#' comsub(strings, sep="/")

comsub <- function(data, sep="") {
  . <- NULL # to avoid NOTE on check
  data_spl <- strsplit(data,sep)
  data_spl_maxlength <- max(sapply(strsplit(data,sep), length))
  which_max <- if (length(unique(data)) > 1) {
    which.max(apply(
      do.call(rbind, lapply(data_spl, `length<-`, data_spl_maxlength)),
      2, function(i){!length(unique(i))==1}
    )) -1
  } else {
    length(data_spl[[1]]) - 1 # FIXME ok for dir with "/", but not with ""
  }
  paste(c(data_spl[[1]][seq_len(which_max)],""), collapse=sep)
}

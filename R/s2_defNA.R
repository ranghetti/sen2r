#' @title Set NA value of a specific product type
#' @description Internal function to determine the NA value to be used for
#'  each product type (except for spectral indices, whose NA value is 
#'  managed by [s2_calcindices]).
#' @param prod_types Character vector of the input product types
#' @return Numeric NA values (NA not to seet any NA value), corresponding to 
#'  `prod_types`.
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#'
#' @examples
#' fidolasen:::s2_defNA("BOA")
#' fidolasen:::s2_defNA(c("BOA","BOA","SCL","TCI"))


s2_defNA <- function(prod_types) {
  
  sapply(
    prod_types,
    switch,
    BOA = 2^16-2, # default value for UInt16
    TOA = 2^16-2, # default value for UInt16
    SCL = 0, # specific for SCL
    TCI = 0, # no specific NA value because it is a Byte format
    0 # default value is not yet NA because this causes errors in s2_mask
  )
  
}


# # define output NA value
# inraster_na <- if (is.na(NAvalue(inraster)) | NAvalue(inraster)==Inf) {
#   switch(
#     dataType(inraster),
#     INT1S = 127,
#     INT1U = 255,
#     INT2S = 2^15-1,
#     INT2U = 2^16-2,
#     INT4S = 2^31-1,
#     INT4U = 2^32,
#     FLT4S = 3.4e+38,
#     FLT8S = 1.7e+308
#   )
# } else if (NAvalue(inraster)==-Inf) {
#   switch(
#     dataType(inraster),
#     INT1S = -127,
#     INT1U = 0,
#     INT2S = -2^15-1,
#     INT2U = 0,
#     INT4S = -2^31-1,
#     INT4U = 0,
#     FLT4S = -3.4e+38,
#     FLT8S = -1.7e+308
#   )
# } else {
#   NAvalue(inraster)
# }

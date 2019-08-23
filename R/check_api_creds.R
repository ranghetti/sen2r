#' @title check_api_creds
#' @description Checks ifprovided username and password are valid Scihub credentials
#' @param user `string` User Name
#' @param pwd `string`  password
#' @return TRUE if credentials are valid, FALSE elsewhere
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  check_api_creds("user", "user")
#'  }
#' }
#' @rdname check_api_creds
#' @author Lorenzo Busetto, phD (2019) \email{busetto.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom httr GET authenticate
check_api_creds <- function(user, pwd) {
  h1 <- httr::handle("")
  check_creds <- httr::GET(
    url = "https://scihub.copernicus.eu/apihub/odata/v1",
    handle = h1, 
    httr::authenticate(user, pwd))
  if (check_creds$status == "401") {
    FALSE
  } else {
    TRUE
  }
}

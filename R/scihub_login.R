#' @title Import / export SciHub username and password
#' @description Read the SciHub login information or save new username and
#'  password. Login information is stored in a file `apihub.txt` inside the
#'  directory of `s2download`. This functions allow to read or write this
#'  file, and to edit them from inside the GUI.
#'  Remember that, in case login information is not provided, default `user` 
#'  username and `user` password are used, but this can cause timeouts, since
#'  [a limit of two parallel downloads per user exists](https://scihub.copernicus.eu/twiki/do/view/SciHubWebPortal/APIHubDescription).
#' @param apihub_path Path of the file in which login information is saved.
#'  If NA (default) it is automatically read from `s2download` path.
#' @param username SciHub username.
#' @param password SciHub password.
#' @return NULL
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate py_to_r
#' @importFrom shiny a actionButton icon modalButton modalDialog passwordInput tagList textInput

#' @name read_scihub_login
#' @rdname scihub_login

read_scihub_login <- function(apihub_path=NA) {
  
  # if apihub_path is not specified, 
  # retrieve from the current s2download installation
  if (is.na(apihub_path)) {
    # import s2download
    s2download <- import_s2download(convert=FALSE)
    apihub_path <- file.path(py_to_r(s2download$inst_path),"apihub.txt")
  }
  
  # return user and password
  if (file.exists(apihub_path)) {
    readLines(apihub_path)[1] %>%
      strsplit(" ") %>%
      unlist()
  } else {
    # if apihub does not exists, return default credentials
    c("user","user")
  }
  
}


#' @name write_scihub_login
#' @rdname scihub_login

write_scihub_login <- function(username, password, apihub_path=NA) {
  
  # if apihub_path is not specified, 
  # retrieve from the current s2download installation
  if (is.na(apihub_path)) {
    # import s2download
    s2download <- import_s2download(convert=FALSE)
    apihub_path <- file.path(py_to_r(s2download$inst_path),"apihub.txt")
  }
  
  # write credentials
  writeLines(
    paste(username, password),
    apihub_path
  )
  
}

#' @name scihub_modal
#' @rdname scihub_login

# write dialog content
scihub_modal <- function(username=NA, password=NA) {
  # read scihub user/password
  if (anyNA(c(username,password))) {
    apihub <- read_scihub_login()
    username <- apihub[1]
    password <- apihub[2]
  }
  modalDialog(
    title = "Set SciHub username and password",
    size = "s",
    textInput("scihub_username", "Username", username),
    passwordInput("scihub_password", "Password", password),
    a("Register new account", href="https://scihub.copernicus.eu/dhus/#/self-registration", target="_blank"),
    "\u2000\u2014\u2000",
    a("Forgot password?", href="https://scihub.copernicus.eu/dhus/#/forgot-password", target="_blank"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_apihub", "\u2000Save", icon=icon("save")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}


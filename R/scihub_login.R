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
#' @importFrom stringr str_split_fixed
#' @importFrom shiny a actionButton icon modalButton modalDialog passwordInput tagList textInput
#' @importFrom shinyFiles shinyFileSave

#' @name read_scihub_login
#' @rdname scihub_login
#' @export

read_scihub_login <- function(apihub_path=NA) {
  
  # if apihub_path is not specified, 
  # retrieve from the current s2download installation
  if (any(c(is.na(apihub_path), is.null(apihub_path)))) {
    # import s2download
    s2download <- import_s2download(convert=FALSE)
    apihub_path <- file.path(py_to_r(s2download$inst_path),"apihub.txt")
  }
  
  # return user and password
  if (file.exists(apihub_path)) {
    readLines(apihub_path) %>%
      str_split_fixed(" ", 2)
  } else {
    # if apihub does not exists, return default credentials
    matrix(c("user","user"), nrow = 1)
  }
  
}


#' @name write_scihub_login
#' @param append Logical: if TRUE, new credentials are added 
#'  to the ones existing within `apihub_path`; 
#'  if FALSE (default), `apihub_path` is replaced with the new ones.
#' @rdname scihub_login
#' @export

write_scihub_login <- function(username, password, apihub_path=NA, append=FALSE) {

  # if apihub_path is not specified, 
  # retrieve from the current s2download installation
  if (any(c(is.na(apihub_path), is.null(apihub_path)))) {
    # import s2download
    s2download <- import_s2download(convert=FALSE)
    apihub_path <- file.path(py_to_r(s2download$inst_path),"apihub.txt")
  }
  
  # if append is required, read the old file
  # in order to exclude duplicated entries and 
  # add the new credentials to the top instead that to the bottom
  apihub <- matrix(c(username, password), nrow = 1)
  if (append) {
    apihub <- rbind(apihub, read_scihub_login(apihub_path))
    apihub <- apihub[!duplicated(apihub[,1]),]
  }
  
  # write credentials
  writeLines(apply(apihub, 1, paste, collapse=" "), apihub_path)
  
}


# #' @name scihub_modal
# #' @rdname scihub_login

# write dialog content
.scihub_modal <- function() {
  # read scihub user/password
  s2download <- import_s2download(convert=FALSE) # import s2download
  apihub_path <- file.path(py_to_r(s2download$inst_path),"apihub.txt")
  apihub <- read_scihub_login(apihub_path)
  # launch modal
  modalDialog(
    title = "Set SciHub username and password",
    size = "s",
    # checkboxInput(
    #   "apihub_multiple", 
    #   label = "Use multiple credentials",
    #   value = FALSE
    # ),
    conditionalPanel(
      condition = "input.apihub_multiple == 1",
      if (nrow(apihub)>0) {
        HTML(paste0(
          "Existing usernames:<ul><li>",
          paste(apihub[,1], collapse="</li><li>")
        ))
      }
    ),
    textInput("scihub_username", "Username", apihub[1,1]),
    passwordInput("scihub_password", "Password", apihub[1,2]),
    a("Register new account", href="https://scihub.copernicus.eu/dhus/#/self-registration", target="_blank"),
    "\u2000\u2014\u2000",
    a("Forgot password?", href="https://scihub.copernicus.eu/dhus/#/forgot-password", target="_blank"),
    checkboxInput(
      "apihub_default",
      label = span(
        "Store inside the package\u2000",
        actionLink("help_apihub", icon("question-circle"))
      ),
      value = TRUE
    ),
    easyClose = FALSE,
    footer = tagList(
      div(style="display:inline-block;vertical-align:top;",
          conditionalPanel(
            condition = "output.switch_save_apihub == 'custom'",
            shinySaveButton(
              "apihub_path_sel", 
              "Save as...", "Specify path for apihub text file", 
              filetype=list(plain="txt"), 
              class = "scihub_savebutton"
            )
          )),
      div(style="display:inline-block;vertical-align:top;",
          conditionalPanel(
            condition = "output.switch_save_apihub == 'default'",
            actionButton(
              "save_apihub", "\u2000Save", 
              icon=icon("save"), 
              class = "scihub_savebutton"
            )
          )),
      div(style="display:inline-block;vertical-align:top;",
          modalButton("\u2000Cancel", icon = icon("ban")))
    )
  )
}


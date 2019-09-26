#' @title Import / export / check SciHub username and password
#' @description Read the SciHub login information (`read_scihub_login()`), 
#'  save new username and password (`write_scihub_login()`)
#'  or check their validity (`check_scihub_login()`). 
#'  Login information is stored in a file `apihub.txt` inside the
#'  "extdata" directory. This functions allow to read or write this
#'  file, and to edit them from inside the GUI.
#' @param apihub_path Path of the file in which login information is saved.
#'  If NA (default) it is automatically read from the package default location.
#' @param username SciHub username.
#' @param password SciHub password.
#' @return `read_scihub_login` returns a matrix of credentials, 
#'  in which `username` is in the first column, `password` in the second.
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom reticulate py_to_r
#' @importFrom shiny a actionButton icon modalButton modalDialog passwordInput tagList textInput
#' @importFrom shinyFiles shinyFileSave
#' @examples
#'   check_scihub_login("user", "user")
#'   write_scihub_login("user", "user")
#'   read_scihub_login()
#'   check_scihub_connection()

#' @name read_scihub_login
#' @rdname scihub_login
#' @export

read_scihub_login <- function(apihub_path=NA) {
  
  # if apihub_path is not specified, 
  # retrieve from the current installation
  if (any(c(is.na(apihub_path), length(nn(apihub_path))==0))) {
    # apihub_path <- file.path(system.file("extdata", package="sen2r"), "apihub.txt")
    apihub_path <- file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt")
    attr(apihub_path, "default") <- TRUE
  } else {
    attr(apihub_path, "default") <- FALSE
  }
  
  # return user and password
  if (file.exists(apihub_path)) {
    readLines(apihub_path) %>%
      strsplit(" ") %>% sapply(c) %>% t()
  } else {
    # if apihub does not exists, return an error
    print_message(
      type="error",
      "File apihub.txt with the SciHub credentials is missing. ",
      if (attr(apihub_path, "default")) {
        "Launch function write_scihub_login('<username>', '<password>') to create it."
      }
    )
  }
  
}


#' @name check_scihub_login
#' @return `check_scihub_login` returns TRUE if credentials are valid, 
#'  FALSE elsewhere.
#' @importFrom httr GET authenticate handle
#' @author Lorenzo Busetto, phD (2019) \email{busetto.l@@irea.cnr.it}
#' @rdname scihub_login
#' @export

check_scihub_login <- function(username, password) {
  check_creds <- httr::GET(
    url = "https://scihub.copernicus.eu/apihub/odata/v1",
    handle = httr::handle(""), 
    httr::authenticate(username, password))
  if (check_creds$status == "401") {
    FALSE
  } else {
    TRUE
  }
}

#' @name check_scihub_connection
#' @return `check_scihub_connection` returns TRUE if internet connection 
#'  is available and SciHub is accessible, FALSE otherwise. 
#' @importFrom httr RETRY handle
#' @rdname scihub_login
#' @export
check_scihub_connection <- function() {
  check_online <- try(
    httr::RETRY(
      "GET",
      url = "https://scihub.copernicus.eu/apihub/",
      handle = httr::handle("")
    )
  )
  !inherits(check_online, "try-error")
}


#' @name write_scihub_login
#' @param check Logical: if TRUE (default), new credentials are checked
#'  before writing them on `apihub_path` (if they are invalid, an error 
#'  is provided); 
#'  if FALSE, they are directly written.
#' @param append Logical: if TRUE, new credentials are added 
#'  to the ones existing within `apihub_path`; 
#'  if FALSE (default), `apihub_path` is replaced with the new ones.
#' @return `write_scihub_login` returns NULL.
#' @rdname scihub_login
#' @export

write_scihub_login <- function(username, password, 
                               apihub_path = NA, 
                               check = TRUE, 
                               append = FALSE) {
  
  # check credentials (if required)
  if (check == TRUE) {
    if (!check_scihub_login(username, password)) {
      print_message(
        type = "error",
        "The provided credentials are not valid, ",
        "so the will not be saved."
      )
    }
  }
  
  # if apihub_path is not specified, 
  # retrieve from the current installation
  if (any(c(is.na(apihub_path), length(nn(apihub_path))==0))) {
    apihub_path <- file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt")
    dir.create(dirname(apihub_path), showWarnings = FALSE)
  }
  
  # if append is required, read the old file
  # in order to exclude duplicated entries and 
  # add the new credentials to the top instead that to the bottom
  apihub <- matrix(c(username, password), nrow = 1)
  if (append) {
    apihub <- rbind(apihub, read_scihub_login(apihub_path))
    apihub <- apihub[!duplicated(apihub[,1]),]
    apihub <- matrix(apihub, ncol = 2)
  }
  
  # write credentials
  writeLines(apply(apihub, 1, paste, collapse=" "), apihub_path)
  
}




# #' @name scihub_modal
# #' @rdname scihub_login

# write dialog content
.scihub_modal <- function() { #nocov start
  # read scihub user/password
  apihub_path <- file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt")
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
    a("Register new account", href="https://scihub.copernicus.eu/apihub/#/self-registration", target="_blank"),
    "\u2000\u2014\u2000",
    a("Forgot password?", href="https://scihub.copernicus.eu/apihub/#/forgot-password", target="_blank"),
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
  #nocov end
}

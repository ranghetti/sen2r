#' @title Import / export / check SciHub username and password
#' @description Read the SciHub login information (`read_scihub_login()`),
#'  save new username and password (`write_scihub_login()`)
#'  or check their validity (`check_scihub_login()`).
#'  Login information is stored in a file `apihub.txt` inside the
#'  ".sen2r" subfolder of the home directory. This function allows reading
#'  or writing this file, and editing it from the GUI.
#'  In case file `apihub.txt` is missing, `read_scihub_login()` searches inside
#'  the environmental variables `SCIHUB_USER` and `SCIHUB_PASSWORD`.
#' @param apihub_path Path of the file in which login information is saved.
#'  If NA (default) it is automatically read from the package default location.
#' @param username SciHub username.
#' @param password SciHub password.
#' @param service Character: it can be `"dhus"` or `"apihub"` (default).
#' @return `read_scihub_login` returns a matrix of credentials,
#'  in which `username` is in the first column, `password` in the second.
#' @details Notice that new/recently updated SciHub credentials are recognised by API Hub
#'  with a delay of about one week (see \url{https://scihub.copernicus.eu/twiki/do/view/SciHubWebPortal/APIHubDescription} for details).
#'
#' For this reason, newly created credentials can not immediately be used by `sen2r`, 
#' and password edits on old credentials are not immediately recognised.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' 
#' @examples
#' \donttest{
#' check_scihub_connection()
#' }
#' \dontrun{
#' check_scihub_login("username", "password")
#' write_scihub_login("username", "password")
#' read_scihub_login()
#' }

#' @name read_scihub_login
#' @rdname scihub_login
#' @export

read_scihub_login <- function(apihub_path = NA) {
  
  # if apihub_path is not specified,
  # retrieve from the current installation
  if (any(c(is.na(apihub_path), length(nn(apihub_path))==0))) {
    apihub_path <- file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt")
    attr(apihub_path, "default") <- TRUE
  } else {
    attr(apihub_path, "default") <- FALSE
  }
  
  # return user and password
  if (file.exists(apihub_path)) {
    t(sapply(strsplit(readLines(apihub_path), " "), c))
  } else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    # in GitHub actions, read from secrets
    scihub_user <- if (Sys.getenv("OS_NAME_STRING") == "ubuntu-20.04") {
      paste0("sen2r_travis_", Sys.getenv("R_VERSION_STRING"))
    } else if (Sys.getenv("OS_NAME_STRING") == "windows-latest") {
      "sen2r_ci_windows"
    } else if (Sys.getenv("OS_NAME_STRING") == "macOS-latest") {
      "sen2r_ci_mac"
    }
    scihub_user <- paste0("sen2r_travis_", Sys.getenv("R_VERSION_STRING")) # FIXME temp remove
    matrix(c(scihub_user, Sys.getenv("CI_PASSWORD")), ncol = 2)
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
#' @importFrom httr RETRY authenticate handle
#' @author Lorenzo Busetto, phD (2019)
#' @rdname scihub_login
#' @export

check_scihub_login <- function(username, password, service = "apihub") {
  if (!check_scihub_connection(service = service)) {
    print_message(
      type = "error",
      "Impossible to reach the SciHub server ",
      "(internet connection or SciHub may be down)." 
    )
  }
  check_creds <- RETRY(
    verb = "GET",
    url = paste0("https://scihub.copernicus.eu/",service,"/odata/v1"),
    handle = handle(""),
    config = authenticate(username, password)
  )
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
check_scihub_connection <- function(service = "apihub") {
  check_online <- try(
    RETRY(
      "GET",
      url = paste0("https://scihub.copernicus.eu/",service,"/"),
      handle = handle("")
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
    if (!check_scihub_login(username, password, service = "apihub")) {
      if (!check_scihub_login(username, password, service = "dhus")) {
        print_message(
          type = "error",
          "The provided credentials are not valid, ",
          "so they will not be saved. "
        )
      } else {
        print_message(
          type = "error",
          "The provided credentials are not yet recognised by API Hub, ",
          "although being valid on ESA SciHub; ",
          "probably they were created recently. ",
          "Please notice that new/recently updated SciHub credentials are recognised by API Hub ",
          "with a delay of about one week. ",
          "For this reason, newly created SciHub credentials can not immediately be used by sen2r", 
          "and password edits on old credentials are not immediately recognised."
        )
      }
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
.scihub_modal <- function() { # nocov start
  
  # Define internal functions as aliases of shiny* - leaflet* ones,
  # so to avoid using "shiny::" every time
  a <- shiny::a
  actionButton <- shiny::actionButton
  actionLink <- shiny::actionLink
  checkboxInput <- shiny::checkboxInput
  conditionalPanel <- shiny::conditionalPanel
  div <- shiny::div
  HTML <- shiny::HTML
  icon <- shiny::icon
  modalButton <- shiny::modalButton
  modalDialog <- shiny::modalDialog
  passwordInput <- shiny::passwordInput
  shinySaveButton <- shinyFiles::shinySaveButton
  span <- shiny::span
  tagList <- shiny::tagList
  textInput <- shiny::textInput
  
  # read scihub user/password
  apihub_path <- file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt")
  apihub <- if (file.exists(apihub_path)) {
    read_scihub_login(apihub_path)
  } else {
    matrix(c("", ""), nrow = 1)
  }
  
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
    actionLink(
      "help_register_scihub", 
      "Register new account\u2000\u2014\u2000Forgot password?"
    ),
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
} # nocov end

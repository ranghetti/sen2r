#' @title Import / export / check SciHub username and password (deprecated)
#' @description These functions are deprecated and will be removed.
#' @return deprecated

#' @name read_scihub_login
#' @rdname scihub_login
#' @param apihub_path deprecated
#' @param username deprecated
#' @param password deprecated
#' @param service deprecated
#' @export
read_scihub_login <- function(apihub_path = NA) {
    matrix(c("",""), ncol = 2)
}


#' @name is_scihub_configured
#' @rdname scihub_login
#' @export
is_scihub_configured <- function() {
  FALSE
}

#' @name check_scihub_login
#' @rdname scihub_login
#' @export
check_scihub_login <- function(username, password, service = "apihub") {
  FALSE
}

#' @name check_scihub_connection
#' @rdname scihub_login
#' @export
check_scihub_connection <- function(service = "apihub") {
  FALSE
}

#' @name write_scihub_login
#' @rdname scihub_login
#' @param check deprecated
#' @param append deprecated
#' @export

write_scihub_login <- function(username, password,
                               apihub_path = NA,
                               check = TRUE,
                               append = FALSE) {
  invisible(NULL)
}

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

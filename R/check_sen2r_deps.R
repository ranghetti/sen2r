#' @title Check package dependencies
#' @description The function allows to graphically check that all the
#'  optional runtime dependencies are installed.
#' @return NULL (the function is called for its side effects)
#' @details This package needs some external dependencies in order to run
#'  specific actions:
#' - Sen2Cor for atmospheric correction;
#' - GDAL for cloud mask smoothing and buffering;
#' - aria2 to download SAFE images with an alternative downloader.
#' 
#' This function opens a GUI which allows to check that these dependencies
#' are installed. This check is highly suggested before using the library for
#' the fist time, in order to avoid errors.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom utils capture.output
#' @importFrom httr RETRY write_disk progress
#' @importFrom jsonlite fromJSON toJSON
#' @export
#' @examples
#' if (interactive()) {
#'   check_sen2r_deps()
#' }

check_sen2r_deps <- function() {
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  # Check shiny* / leaflet* suggested dependencies to be installed
  check_gui_deps()
  
  # Define internal functions as aliases of shiny* - leaflet* ones,
  # so to avoid using "shiny::" every time
  actionButton <- shiny::actionButton
  addResourcePath <- shiny::addResourcePath
  br <- shiny::br
  code <- shiny::code
  column <- shiny::column
  conditionalPanel <- shiny::conditionalPanel
  disable <- shinyjs::disable
  disabled <- shinyjs::disabled
  div <- shiny::div
  em <- shiny::em
  enable <- shinyjs::enable
  extendShinyjs <- shinyjs::extendShinyjs
  fluidPage <- shiny::fluidPage
  fluidRow <- shiny::fluidRow
  getVolumes <- shinyFiles::getVolumes
  h <- shiny::h
  h3 <- shiny::h3
  helpText <- shiny::helpText
  hide <- shinyjs::hide
  hr <- shiny::hr
  HTML <- shiny::HTML
  html <- shinyjs::html
  htmlOutput <- shiny::htmlOutput
  icon <- shiny::icon
  modalButton <- shiny::modalButton
  modalDialog <- shiny::modalDialog
  observe <- shiny::observe
  observeEvent <- shiny::observeEvent
  outputOptions <- shiny::outputOptions
  p <- shiny::p
  parseDirPath <- shinyFiles::parseDirPath
  parseFilePaths <- shinyFiles::parseFilePaths
  radioButtons <- shiny::radioButtons
  reactive <- shiny::reactive
  reactiveFileReader <- shiny::reactiveFileReader
  reactivePoll <- shiny::reactivePoll
  reactiveValues <- shiny::reactiveValues
  removeModal <- shiny::removeModal
  renderText <- shiny::renderText
  renderUI <- shiny::renderUI
  runApp <- shiny::runApp
  shinyApp <- shiny::shinyApp
  shinyDirButton <- shinyFiles::shinyDirButton
  shinyDirChoose <- shinyFiles::shinyDirChoose
  shinyFileChoose <- shinyFiles::shinyFileChoose
  shinyFilesButton <- shinyFiles::shinyFilesButton
  showModal <- shiny::showModal
  span <- shiny::span
  stopApp <- shiny::stopApp
  strong <- shiny::strong
  textInput <- shiny::textInput
  textOutput <- shiny::textOutput
  uiOutput <- shiny::uiOutput
  updateTextInput <- shiny::updateTextInput
  useShinyjs <- shinyjs::useShinyjs
  verbatimTextOutput <- shiny::verbatimTextOutput
  
  # get server volumes
  volumes <- c(
    "Home" = normalize_path("~"),
    "sen2r" = system.file(package = "sen2r"),
    getVolumes()()
  )
  
  settings.ui <- fluidPage(
    
    # header
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    
    fluidRow(column(
      title="Dependencies",
      width=12,
      
      h3("Sen2Cor"),
      helpText(em(
        "Sen2Cor is used to perform atmospheric correction of Sentinel-2",
        "Level-1C products: it is required by the package,",
        "unless you choose not to correct products locally",
        "(using only Level-1C \u2013 TOA products",
        "or downloading directly Level-2A products)."
      )),
      span(style="display:inline-block;vertical-align:center;padding-top:5px;",
           actionButton("check_sen2cor", "Check Sen2Cor", width=200),
           "\u2000"),
      span(style="display:inline-block;vertical-align:center;",
           htmlOutput("check_sen2cor_icon")),
      
      h3("GDAL"),
      helpText(em(
        "An external GDAL runtime environment is required in order to smooth and",
        "buffer a cloud mask, and is optionally used to compute spectral indices,",
        "RGB images and thumbnails.",
        "Starting from version 1.3.5, GDAL is no longer a mandatory dependency.",
        if (Sys.info()["sysname"] == "Windows") {span(
          "On Windows",
          strong("it is strictly required to install GDAL using OSGeo4W"),
          "in order to avoid errors.",
          "To satisfy this requirement, click on \"Check GDAL\" and,",
          "whenever the search of a valid installation will finish, download",
          "the OSGeo4W installer and install it in the default directory",
          "(or, in any case, maintain the directory name \"OSGeo4W64\")."
        )}
      )),
      span(style="display:inline-block;vertical-align:center;padding-top:5px;",
           actionButton("where_check_gdal", "Check GDAL", width=200),
           "\u2000"),
      span(style="display:inline-block;vertical-align:center;",
           htmlOutput("check_gdal_icon")),
      
      h3("aria2"),
      helpText(em(
        "aria2 is an alternative optional downloader downloader which can be",
        "used to download SAFE archives."
      )),
      span(style="display:inline-block;vertical-align:center;padding-top:5px;",
           actionButton("check_aria2", "Check aria2", width=200),
           "\u2000"),
      span(style="display:inline-block;vertical-align:center;",
           htmlOutput("check_aria2_icon")),
      
      hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
      uiOutput("footer_buttons")
      
    )) # end of fluidRow Dependencies
    
  ) # end of fluidPage
  
  settings.server <- function(input, output, session) {
    
    # link to www directory and objects
    addResourcePath("www", system.file("www", package="sen2r"))
    
    rv <- reactiveValues()
    
    # output OS name (to be used in conditionalPanel)
    output$os_name <- renderText(Sys.info()["sysname"])
    outputOptions(output, "os_name", suspendWhenHidden = FALSE)
    
    # list of dependencies
    dependencies <- c(
      "gdalbuildvrt", "gdal_translate", "gdalwarp", "gdal_calc", "gdaldem", "gdalinfo", "ogrinfo",
      "python", "sen2cor"
    )
    # load binpaths
    binpaths <- reactivePoll(1000, session, function() {}, load_binpaths)
    
    
    ##-- Perform checks of dependencies --##
    
    #-- Check GDAL --#
    
    # build the icon of the check
    observe({
      input$check_gdal
      rv$check_gdal_isvalid <- if (!is.null(binpaths()$gdal_calc)) {
        file.exists(binpaths()$gdal_calc)
      } else {FALSE}
    })
    output$check_gdal_isvalid <- renderText(rv$check_gdal_isvalid)
    output$check_gdal_icon <- renderUI({
      if (is.na(rv$check_gdal_isvalid)) {
        ""
      } else if (rv$check_gdal_isvalid) {
        span(style="color:darkgreen;", "\u2714")
      } else {
        span(style="color:red;", "\u2718")
      }
    })
    outputOptions(output, "check_gdal_isvalid", suspendWhenHidden = FALSE)
    
    # build the modal dialog
    check_gdal_modal <- reactive({
      modalDialog(
        title = "GDAL check",
        size = "s",
        uiOutput("check_gdal_message"),
        verbatimTextOutput("check_gdal_outmessages"),
        easyClose = FALSE,
        footer = NULL
      )
    })
    
    # use a reactive output for install GDAL message
    # (this because otherwise it does not react to check_gdal_valid chages)
    output$check_gdal_message <- renderUI({
      if (is.na(rv$check_gdal_isvalid)) {
        div(
          align="center",
          p(style="text-align:center;font-size:500%;color:darkgrey;",
            icon("cog", class = "fa-spin"))
        )
      } else if (!rv$check_gdal_isvalid) {
        if (Sys.info()["sysname"] == "Windows") {
          div(
            p(style="text-align:center;font-size:500%;color:red;",
              icon("times-circle")),
            p(HTML(
              "GDAL needs to be installed or searched in a different directory.",
              "To install it:<ol>",
              "<li>download the OSGeo4W installer using the button below;</li>",
              "<li>when the file will be automatically opened,",
              "give the administrator rules when required;</li>",
              "<li>choose the \"Advanced install\";</li>",
              "<li>continue clicking \"Next\";</li>",
              "<li>when the window for choosing the packages to install will appear,",
              "check the package \"gdal-python\" and install it.</li></ol>"
            )),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                actionButton("install_gdal_button", strong("\u2000Download"), icon=icon("download")),
                modalButton("\u2000Cancel", icon = icon("ban")))
          )
        } else {
          div(
            p(style="text-align:center;font-size:500%;color:red;",
              icon("times-circle")),
            p("GDAL needs to be installed or searched in a different directory.",
              "To install it, install the package \"python-gdal\",",
              "then repeat this check."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          )
        }
      } else if (rv$check_gdal_isvalid) {
        div(
          p(style="text-align:center;font-size:500%;color:darkgreen;",
            icon("check-circle")),
          p("GDAL is correctly installed."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;",
              modalButton("\u2000Close", icon = icon("check")))
        )
      }
    })
    
    # build the modal dialog
    install_gdal_modal <- reactive({
      modalDialog(
        title = "Install GDAL",
        size = "s",
        uiOutput("install_gdal_message"),
        easyClose = FALSE,
        footer = NULL
      )
    })
    
    
    # ask where searching GDAL before checking
    observeEvent(input$where_check_gdal, {
      showModal(modalDialog(
        title = "Check GDAL",
        size = "s",
        radioButtons(
          "path_gdal_isauto", NULL,
          choices = c("Search GDAL in a default path" = TRUE,
                      "Specify where GDAL should be searched" = FALSE),
          selected = TRUE, width = "100%"
        ),
        conditionalPanel(
          condition = "input.path_gdal_isauto == 'FALSE'",
          div(
            p("Specify the path:"),
            div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                    shinyDirButton("path_gdalman_sel", "Select", "Specify directory in which GDAL should be searched")),
                div(style="display:inline-block;vertical-align:top;width:180px;",
                    textInput("path_gdalman_textin", NULL, ""))),
            div(style="height:20px;vertical-aling:top;",
                htmlOutput("path_gdalman_errormess"))
          )
        ),
        easyClose = FALSE,
        footer = div(
          disabled(actionButton("check_gdal_button", strong("\u2000Check"), icon=icon("check"))),
          modalButton("\u2000Cancel", icon = icon("ban"))
        )
      ))
    })
    
    # check the gdal path
    # observeEvent(input$path_gdalman_textin, {
    #   path_gdalman_errormess <- path_check(
    #     input$path_gdalman_textin,
    #     mustbe_empty = FALSE, 
    #     mustbe_writable = FALSE
    #   )
    #   output$path_gdalman_errormess <- path_gdalman_errormess
    # })
    observeEvent(c(input$path_gdalman_textin, input$path_gdal_isauto), {
      path_gdalman_errormess <- path_check(
        input$path_gdalman_textin,
        mustbe_empty = FALSE, 
        mustbe_writable = FALSE
      )
      output$path_gdalman_errormess <- path_gdalman_errormess
      if (any(
        input$path_gdal_isauto == TRUE, 
        TRUE %in% attr(path_gdalman_errormess, "isvalid")
      )) {
        enable("check_gdal_button")
      } else {
        disable("check_gdal_button")
      }
    })
    shinyDirChoose(input, "path_gdalman_sel", roots = volumes)
    observeEvent(input$path_gdalman_sel, {
      path_gdalman_string <- parseDirPath(volumes, input$path_gdalman_sel)
      updateTextInput(session, "path_gdalman_textin", value = path_gdalman_string)
    })
    
    # do the check when button is pressed
    observeEvent(input$check_gdal_button, {
      
      # reset check value
      rv$check_gdal_isvalid <- NA # FIXME not working
      
      # open modaldialog
      showModal(check_gdal_modal())
      
      shinyjs::html(
        "check_gdal_message",
        as.character(div(
          align="center",
          p(style="text-align:center;font-size:500%;color:darkgrey;",
            icon("spinner", class = "fa-pulse"))
        ))
      )
      
      # do the check
      withCallingHandlers({
        shinyjs::html("check_gdal_outmessages", "")
        rv$check_gdal_isvalid <- check_gdal(
          gdal_path = if (input$path_gdalman_textin == "") {
            NULL
          } else {
            input$path_gdalman_textin
          },
          abort = FALSE, force = TRUE
        )
      },
      message = function(m) {
        shinyjs::html(id = "check_gdal_outmessages", html = m$message, add = TRUE)
      })
      
      shinyjs::hide("check_gdal_outmessages")
      
      
    })
    
    # install osgeo
    observeEvent(input$install_gdal_button, {
      
      showModal(install_gdal_modal())
      
      # create the text to show in the modaldialog
      shinyjs::html(
        "install_gdal_message",
        as.character(div(
          br(),
          p(style="color:darkgrey;text-align:center;font-size:500%;","\u23F3"),
          p("Wait while the OSGeo4W installer is being downloaded...")
        )),
        add=FALSE
      )
      
      # Download osgeo4w
      osgeo4w_url <- paste0(
        "http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
        if (Sys.info()["machine"]=="x86-64") {"_64"},".exe"
      )
      osgeo4w_path <- tempfile(pattern="dir",fileext = ".exe")
      out_bar <- if (inherits(stdout(), "terminal")) {
        NULL
      } else {
        file(out_bar_path <- tempfile(), open = "a")
      }
      RETRY(
        verb = "GET",
        url = osgeo4w_url,
        times = 5, pause_cap = 8,
        progress(con = if (length(out_bar) > 0) {out_bar} else {stdout()}),
        write_disk(osgeo4w_path, overwrite = TRUE)
      )
      if (length(out_bar) > 0) {
        close(out_bar)
        invisible(file.remove(out_bar_path))
      }
      if (file.exists(osgeo4w_path)) {
        
        shinyjs::html(
          "install_gdal_message",
          as.character(div(
            p("OSGeo4W was correctly downloaded."),
            p("The installer window was opened;",
              "please install the package \"gdal-python\" following the instructions.")
          )),
          add = TRUE
        )
        
        shell(osgeo4w_path)
        
        shinyjs::html(
          "install_gdal_message",
          as.character(div(
            p("The installation was terminated;",
              "please close the interface,",strong("restart R"),
              "and repeat the check to be sure all was correctly installed."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          )),
          add = TRUE
        )
        
        
      } else {
        
        shinyjs::html(
          "install_gdal_message",
          as.character(div(
            p("Something went wrong during the download; please retry."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          )),
          add = TRUE
        )
        
      }
      
      
    })
    
    
    #-- Check aria2 --#
    
    # build the icon of the check
    observe({
      input$check_aria2
      rv$check_aria2_isvalid <- if (!is.null(binpaths()$aria2c)) {
        file.exists(binpaths()$aria2c)
      } else {FALSE}
    })
    output$check_aria2_isvalid <- renderText(rv$check_aria2_isvalid)
    output$check_aria2_icon <- renderUI({
      if (is.na(rv$check_aria2_isvalid)) {
        ""
      } else if (rv$check_aria2_isvalid) {
        span(style="color:darkgreen;", "\u2714")
      } else {
        span(style="color:red;", "\u2718")
      }
    })
    outputOptions(output, "check_aria2_isvalid", suspendWhenHidden = FALSE)
    
    output$check_aria2_message <- renderUI({
      if (is.na(rv$check_aria2_isvalid)) {
        ""
      } else if (!rv$check_aria2_isvalid) {
        div(
          align = if (Sys.info()["sysname"] == "Windows") {"left"} else {"center"},
          p(style="color:red;text-align:center;font-size:500%;",
            icon("times-circle")),
          if (Sys.info()["sysname"] == "Windows") {
            div(
              p("aria2 needs to be linked to sen2r, or downloaded if missing."),
              radioButtons(
                "aria2_link_or_install", NULL,
                c("Download a new aria2 binary" = "install",
                  "Set the path of an existing binary" = "link"),
                selected = "install"
              ),
              conditionalPanel(
                condition = "input.aria2_link_or_install == 'install'",
                div(
                  p("Please provide the path of a directory ",
                    "in which installing it:"),
                  div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                          shinyDirButton("path_newaria2_sel", "Select", "Specify directory in which installing aria2")),
                      div(style="display:inline-block;vertical-align:top;width:180px;",
                          textInput("path_newaria2_textin", NULL, ""))),
                  div(style="height:20px;vertical-aling:top;",
                      htmlOutput("path_newaria2_errormess")),
                  hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
                  div(style="text-align:right;",
                      disabled(actionButton("install_aria2_button", strong("\u2000Download"), icon=icon("download"))),
                      modalButton("\u2000Cancel", icon = icon("ban")))
                )
              ),
              conditionalPanel(
                condition = "input.aria2_link_or_install == 'link'",
                div(
                  p("Please provide the path of an existing aria2 binary:"),
                  div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                          shinyFilesButton("path_exiaria2_sel", "Select", "Specify the aria2 path", multiple = FALSE)),
                      div(style="display:inline-block;vertical-align:top;width:180px;",
                          textInput("path_exiaria2_textin", NULL, ""))),
                  div(style="height:20px;vertical-aling:top;",
                      htmlOutput("path_exiaria2_errormess")),
                  hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
                  div(style="text-align:right;",
                      disabled(actionButton("link_aria2_button", strong("\u2000Ok"), icon=icon("check"))),
                      modalButton("\u2000Cancel", icon = icon("ban")))
                )
              )
            )
          } else {
            div(
              p("aria2 needs to be installed",
                "To do it, install the package \"aria2\",",
                "then repeat this check."),
              hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
              div(style="text-align:right;",
                  modalButton("\u2000Close", icon = icon("check")))
            )
          }
        )
      } else if (rv$check_aria2_isvalid) {
        div(
          align = "center",
          p(style="text-align:center;font-size:500%;color:darkgreen;",
            icon("check-circle")),
          p("aria2 is correctly installed."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;",
              modalButton("\u2000Close", icon = icon("check")))
        )
      }
    })
    
    # check the aria2 path
    observeEvent(input$path_newaria2_textin, {
      path_newaria2_errormess <- path_check(
        input$path_newaria2_textin,
        mustbe_writable = TRUE,
        mustbe_empty = FALSE
      )
      output$path_newaria2_errormess <- path_newaria2_errormess
      if (TRUE %in% attr(path_newaria2_errormess, "isvalid")) {
        enable("install_aria2_button")
      } else {
        disable("install_aria2_button")
      }
    })
    shinyDirChoose(input, "path_newaria2_sel", roots = volumes)
    observeEvent(input$path_newaria2_sel, {
      path_newaria2_string <- parseDirPath(volumes, input$path_newaria2_sel)
      updateTextInput(session, "path_newaria2_textin", value = path_newaria2_string)
    })
    observeEvent(input$path_exiaria2_textin, {
      if (any(length(input$path_exiaria2_textin)==0, input$path_exiaria2_textin[1]=="")) {
        output$path_exiaria2_errormess <- renderText("")
        disable("link_aria2_button")
      } else if (!file.exists(input$path_exiaria2_textin)) {
        output$path_exiaria2_errormess <- renderUI(span(
          style="color:red",
          "\u2718 (the file does not exist)"
        ))
        disable("link_aria2_button")
      } else if (!grepl("^aria2c?\\.exe$", basename(input$path_exiaria2_textin))) {
        output$path_exiaria2_errormess <- renderUI(span(
          style="color:red",
          "\u2718 (this is not aria2c.exe)"
        ))
        disable("link_aria2_button")
      } else {
        output$path_exiaria2_errormess <- renderUI(span(
          style="color:darkgreen",
          "\u2714"
        ))
        enable("link_aria2_button")
      }
    })
    shinyFileChoose(input, "path_exiaria2_sel", roots = volumes)
    observeEvent(input$path_exiaria2_sel, {
      path_exiaria2_string <- parseFilePaths(volumes, input$path_exiaria2_sel)$datapath
      updateTextInput(session, "path_exiaria2_textin", value = path_exiaria2_string)
    })
    
    # build the modalDialog
    check_aria2_modal <- modalDialog(
      title = "aria2 check",
      size = "s",
      uiOutput("check_aria2_message"),
      easyClose = FALSE,
      footer = NULL
    )
    
    # open the modaldialog when button is pressed
    observeEvent(input$check_aria2, {
      
      # update the check
      rv$check_aria2_isvalid <- if (
        !is.null(suppressWarnings(load_binpaths("aria2c")$aria2c))
      ) {
        file.exists(load_binpaths()$aria2c)
      } else {
        FALSE
      }
      
      # open modaldialog
      showModal(check_aria2_modal)
      
    })
    
    # install aria2
    observeEvent(input$install_aria2_button, {
      
      shinyjs::html(
        "check_aria2_message",
        as.character(div(
          align="center",
          p(style="text-align:center;font-size:500%;color:darkgrey;",
            icon("cog", class = "fa-spin")),
          p("Wait while aria2 is being installed...")
        ))
      )
      
      Sys.sleep(0.5)
      check_aria2_outerr <- tryCatch(
        install_aria2(input$path_newaria2_textin),
        error = function(e) {print(e)}
      )
      
      # remove the text
      if (is(check_aria2_outerr, "error")) {
        shinyjs::html(
          "check_aria2_message",
          as.character(div(
            align="center",
            p(style="text-align:center;font-size:500%;color:red;",
              icon("times-circle")),
            p("Some errors occurred:"),
            p(code(check_aria2_outerr)),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
        rv$check_aria2_isvalid <- FALSE
      } else {
        shinyjs::html(
          "check_aria2_message",
          as.character(div(
            align="center",
            p(style="text-align:center;font-size:500%;color:darkgreen;",
              icon("check-circle")),
            p("aria2 was correctly installed."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
        rv$check_aria2_isvalid <- TRUE
      }
      
    })
    
    # link an existing aria2
    observeEvent(input$link_aria2_button, {
      binpaths_content <- load_binpaths()
      binpaths_content$aria2c <- normalize_path(input$path_exiaria2_textin)
      writeLines(jsonlite::toJSON(binpaths_content, pretty=TRUE), attr(binpaths(), "path"))
      rv$check_aria2_isvalid <- TRUE
      removeModal()
    })
    
    
    #-- Check sen2cor --#
    
    # build the icon of the check
    observe({
      input$check_sen2cor # redo when the button is pressed
      rv$check_sen2cor_isvalid <- if (!is.null(binpaths()$sen2cor)) {
        file.exists(binpaths()$sen2cor)
      } else {FALSE}
    })
    output$check_sen2cor_isvalid <- renderText(rv$check_sen2cor_isvalid)
    output$check_sen2cor_icon <- renderUI({
      if (is.na(rv$check_sen2cor_isvalid)) {
        ""
      } else if (rv$check_sen2cor_isvalid) {
        span(style="color:darkgreen;", "\u2714")
      } else {
        span(style="color:red;", "\u2718")
      }
    })
    outputOptions(output, "check_sen2cor_isvalid", suspendWhenHidden = FALSE)
    
    output$check_sen2cor_message <- renderUI({
      if (is.na(rv$check_sen2cor_isvalid)) {
        ""
      } else if (rv$check_sen2cor_isvalid) {
        div(
          align = "center",
          p(style="color:darkgreen;text-align:center;font-size:500%;",
            icon("check-circle")),
          p("Sen2Cor is correctly installed."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;",
              modalButton("\u2000Close", icon = icon("check")))
        )
      } else {
        div(
          align = "left",
          p(style="color:red;text-align:center;font-size:500%;",
            icon("times-circle")),
          div(
            style = "margin-bottom: 1em;",
            p("Sen2Cor needs to be linked to sen2r, or downloaded if missing.")
          ),
          radioButtons(
            "sen2cor_link_or_install", NULL,
            c("Install a new Sen2Cor environment" = "install",
              "Set the path of an existing Sen2Cor" = "link"),
            selected = "install"
          ),
          conditionalPanel(
            condition = "input.sen2cor_link_or_install == 'install'",
            div(
              p("Please provide the path of an empty directory ",
                "in which installing it:"),
              div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                      shinyDirButton("path_newsen2cor_sel", "Select", "Specify directory in which installing Sen2Cor")),
                  div(style="display:inline-block;vertical-align:top;width:480px;",
                      textInput("path_newsen2cor_textin", NULL, normalize_path(
                        file.path(dirname(attr(binpaths(), "path")), "sen2cor"), mustWork = FALSE
                      )))),
              div(style="height:20px;vertical-aling:top;",
                  htmlOutput("path_newsen2cor_errormess")),
              radioButtons(
                "sen2cor_version", "Sen2Cor version to be installed:",
                c("Stable version (2.5.5)" = "2.5.5",
                  "Newer, lighter version (2.8.0)" = "2.8.0"),
                selected = "2.5.5"
              ),
              shiny::tags$small(em(p(
                "Sen2Cor 2.8.0 is faster and makes use of less RAM, but",
                "it only works for SAFE products version >= 14.2 and",
                "some problems were encountered running it on Windows;",
                "it is recommended to use Sen2Cor 2.5.5."
              )))
            )
          ),
          conditionalPanel(
            condition = "input.sen2cor_link_or_install == 'link'",
            div(
              p("Please provide the path of the directory ",
                "in which Sen2Cor is currently installed:"),
              div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                      shinyDirButton("path_exisen2cor_sel", "Select", "Specify directory in which Sen2Cor is installed")),
                  div(style="display:inline-block;vertical-align:top;width:480px;",
                      textInput("path_exisen2cor_textin", NULL, normalize_path(
                        file.path(dirname(attr(binpaths(), "path")), "sen2cor"), mustWork = FALSE
                      )))),
              div(style="height:20px;vertical-aling:top;",
                  htmlOutput("path_exisen2cor_errormess"))
            )
          ),
          # radioButtons(
          #   "sen2cor_use_dem", "Use DEM for topographic correction?",
          #   c("Yes (as done in ESA Hub Level-2 products)" = TRUE,
          #     "No (default Sen2Cor setting)" = FALSE,
          #     "Use existing choice (Sen2Cor reinstallation / link)" = NA),
          #   selected = TRUE, width = "100%"
          # ),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          conditionalPanel(
            condition = "input.sen2cor_link_or_install == 'install'",
            div(style="text-align:right;",
                disabled(actionButton("install_sen2cor_button", strong("\u2000Download"), icon=icon("download"))),
                modalButton("\u2000Cancel", icon = icon("ban")))
          ),
          conditionalPanel(
            condition = "input.sen2cor_link_or_install == 'link'",
            div(style="text-align:right;",
                disabled(actionButton("link_sen2cor_button", strong("\u2000Ok"), icon=icon("check"))),
                modalButton("\u2000Cancel", icon = icon("ban")))
          )
        )
      }
    })
    
    # check the Sen2Cor paths
    observeEvent(input$path_newsen2cor_textin, {
      path_newsen2cor_errormess <- path_check(
        input$path_newsen2cor_textin,
        mustbe_writable = TRUE,
        mustbe_empty = TRUE
      )
      output$path_newsen2cor_errormess <- path_newsen2cor_errormess
      if (TRUE %in% attr(path_newsen2cor_errormess, "isvalid")) {
        enable("install_sen2cor_button")
      } else {
        disable("install_sen2cor_button")
      }
    })
    shinyDirChoose(input, "path_newsen2cor_sel", roots = volumes)
    observeEvent(input$path_newsen2cor_sel, {
      path_newsen2cor_string <- parseDirPath(volumes, input$path_newsen2cor_sel)
      updateTextInput(session, "path_newsen2cor_textin", value = path_newsen2cor_string)
    })
    observeEvent(input$path_exisen2cor_textin, {
      path_exisen2cor_errormess <- path_check(
        input$path_exisen2cor_textin,
        mustbe_writable = FALSE,
        mustbe_empty = FALSE
      )
      if (TRUE %in% attr(path_exisen2cor_errormess, "isvalid")) {
        if (.sen2cor_exists(input$path_exisen2cor_textin)) {
          output$path_exisen2cor_errormess <- path_exisen2cor_errormess
          enable("link_sen2cor_button")
        } else {
          output$path_exisen2cor_errormess <- renderUI(span(
            style="color:red",
            "\u2718 (Sen2Cor was not found here)"
          ))
          disable("link_sen2cor_button")
        }
      } else {
        output$path_exisen2cor_errormess <- path_exisen2cor_errormess
        disable("link_sen2cor_button")
      }
    })
    shinyDirChoose(input, "path_exisen2cor_sel", roots = volumes)
    observeEvent(input$path_exisen2cor_sel, {
      path_exisen2cor_string <- parseDirPath(volumes, input$path_exisen2cor_sel)
      updateTextInput(session, "path_exisen2cor_textin", value = path_exisen2cor_string)
    })
    
    
    # build the modalDialog
    check_sen2cor_modal <- modalDialog(
      title = "Sen2Cor check",
      size = "m",
      uiOutput("check_sen2cor_message"),
      easyClose = FALSE,
      footer = NULL
    )
    
    # open the modaldialog when button is pressed
    observeEvent(input$check_sen2cor, {
      
      # open the dialog
      showModal(check_sen2cor_modal)
      
    })
    
    # install sen2cor
    observeEvent(input$install_sen2cor_button, {
      
      # create the text to show in the modaldialog
      shinyjs::html(
        "check_sen2cor_message",
        as.character(div(
          align="center",
          p(style="text-align:center;font-size:500%;color:darkgrey;",
            icon("cog", class = "fa-spin")),
          p("Wait while Sen2Cor is being installed...")
        ))
      )
      
      check_sen2cor_outmess <- capture.output(
        check_sen2cor_outerr <- tryCatch(
          .install_sen2cor(
            input$path_newsen2cor_textin, 
            version = input$sen2cor_version, 
            interactive = FALSE
          ),
          error = function(e) {print(e)}
        ),
        type = "message"
      )
      
      # remove the text
      if (is(check_sen2cor_outerr, "error")) {
        rv$check_sen2cor_isvalid <- FALSE
        shinyjs::html(
          "check_sen2cor_message",
          as.character(div(
            p(code(check_sen2cor_outmess)),
            p(code(check_sen2cor_outerr)),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
      } else {
        rv$check_sen2cor_isvalid <- TRUE
        shinyjs::html(
          "check_sen2cor_message",
          as.character(div(
            align="center",
            p(style="text-align:center;font-size:500%;color:darkgreen;",
              icon("check-circle")),
            p("Sen2Cor was correctly installed."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;",
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
      }
      
    })
    
    
    # link an existing sen2cor
    observeEvent(input$link_sen2cor_button, {
      link_sen2cor(input$path_exisen2cor_textin)
      rv$check_sen2cor_isvalid <- TRUE
      removeModal()
    })
    
    # ##-- Footer buttons --##
    # observe({
    #   rv$check_all_isvalid <- all(c(
    #     rv$check_gdal_isvalid, rv$check_aria2_isvalid,
    #     rv$check_sen2cor_isvalid
    #   ))
    # })
    
    output$footer_buttons <- renderUI({
      div(
        style = "vertical-align:center;text-align:right;",
        # if (rv$check_all_isvalid) {
        #   span(
        #     style = "display:inline-block;",
        #     "All the dependencies are satisfied, you can safely use the library.\u2000"
        #   )
        #   # actionButton("close_gui", "\u2000Close", icon = icon("check"), class = "darkbutton")
        # },
        actionButton(
          "close_gui", "\u2000Close",
          # icon = icon(ifelse(rv$check_all_isvalid, "check", "exclamation-triangle")),
          icon = icon("check"),
          class = "darkbutton"
        )
        
      )
      
    })
    
    # Close the connection when button is pressed
    observeEvent(input$close_gui, {
      # if (!rv$check_all_isvalid) {
      #   confirmSweetAlert(
      #     session = session, inputId = "confirm_close", type = "warning",
      #     title = "Closing the GUI?",
      #     text = paste0(
      #       "Are you sure do you want to quit? ",
      #       "Running the package with unsatisfied ",
      #       "dependencies can lead to errors."
      #     ),
      #     danger_mode = TRUE, btn_labels = c("Cancel", "Close window")
      #   )
      # } else {
        shinyjs::js$closeWindow()
        stopApp()
      # }
    })
    observeEvent(input$confirm_close, {
      if (input$confirm_close) {
        shinyjs::js$closeWindow()
        stopApp()
      }
    })
    
    # Close the connection when window is closed
    session$onSessionEnded(function() {
      stopApp()
    })
    
  }
  
  
  settings.shiny <- shinyApp(
    ui = settings.ui, server = settings.server,
    options = list(width="400px", height="400px")
  )
  
  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(runApp(settings.shiny))
  } else {
    stop("The function must be run from an interactive R session.")
  }
  
}

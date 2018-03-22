#' @title Check package dependencies
#' @description The function allows to graphically check that all the
#'  dependencies are installed.
#' @return NULL
#' @details This package needs some external dependencies to run:
#' - Python
#' - GDAL
#' - Wget
#' - sen2cor
#' - s2download
#' 
#' This function opens a GUI which allows to check that these dependencies
#' are installed. This check is highly suggested before using the library for 
#' the fist time, in order to avoid errors.
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom shiny actionButton br code conditionalPanel div em
#'  fluidPage fluidRow h3 helpText htmlOutput icon modalButton
#'  modalDialog observe observeEvent outputOptions p reactive
#'  reactiveFileReader reactiveValues renderText renderUI runApp
#'  shinyApp showModal span strong textOutput uiOutput
#' @importFrom shinyjs html useShinyjs
#' @importFrom utils capture.output
#' @examples
#' \dontrun{
#' 
#' check_dependencies()
#' }

check_dependencies <- function() {
  
library(shiny)

settings.ui <- fluidPage(
  
  # header
  shinyjs::useShinyjs(),
  
  fluidRow(
    title="Dependencies",
    width=12,
    
    h3("GDAL"),
    helpText(em(
      "GDAL is a mandatory dependency of the package:",
      "it is needed for all the processing operations",
      "and to retrieve metadata from SAFE products"
    )),
    span(style="display:inline-block;vertical-align:center;padding-top:5px;",
         actionButton("check_gdal", "Check GDAL", width=200),
         "\u2000"),
    span(style="display:inline-block;vertical-align:center;",
         htmlOutput("check_gdal_icon")),
    
    conditionalPanel(
      condition = "output.os_name == 'Windows'",
      
      h3("Wget"),
      helpText(em(
        "Wget is the downloader used by the package;",
        "it is required by the package, unless you choose to work offline."
      )),
      span(style="padding-top:5px;",
           actionButton("check_wget", "Check wget", width=200),
           "\u2000")
      
      # Do not check Python (always present in Linux, and provided by GDAL in Windows)
      # h3("Python"),
      # helpText(em(
      #   "Python is a mandatory dependency:",
      #   "it is used by s2download scripts",
      #   "and to retrieve metadata from SAFE products."
      # )),
      # span(style="padding-top:5px;",
      #     actionButton("check_python", "Check Python", width=200)),
      
    ),
    
    h3("sen2cor"),
    helpText(em(
      "sen2cor is used to perform atmospheric correction of Sentinel-2",
      "Level-1C products: it is required by the package,",
      "unless you choose not to correct products locally",
      "(using only Level-1C - TOA products",
      "or dowloading directly Level-2A products)."
    )),
    span(style="display:inline-block;vertical-align:center;padding-top:5px;",
         actionButton("check_sen2cor", "Check sen2cor", width=200),
         "\u2000"),
    span(style="display:inline-block;vertical-align:center;",
         htmlOutput("check_sen2cor_icon")),
    
    h3("s2download"),
    helpText(em(
      "s2download is a collection of python scripts used to find and download",
      "Sentinel-2 products; it is required by the package, unless you choose",
      "to work offline (using already downloaded SAFE products)."
    )),
    span(style="padding-top:5px;",
         actionButton("check_s2download", "Check s2download scripts", width=200),
         "\u2000"),
    span(style="display:inline-block;vertical-align:center;",
         htmlOutput("check_s2download_icon"))
    
  ) # end of fluidRow Dependencies
  
) # end of fluidPage

settings.server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  # output OS name (to be used in conditionalPanel)  
  output$os_name <- renderText(Sys.info()["sysname"])
  outputOptions(output, "os_name", suspendWhenHidden = FALSE)
  
  # list of dependencies
  dependencies <- c(
    "gdalbuildvrt", "gdal_translate", "gdalwarp", "gdal_calc", "gdaldem", "gdalinfo", "ogrinfo",
    "python", "sen2cor", "s2download", "wget"
  )
  # load binpaths
  # check that git, python2 and wget are installed
  binpaths_file <- file.path(system.file("extdata",package="salto"),"paths.json")
  # binpaths <- if (file.exists(binpaths_file)) {
  #   jsonlite::fromJSON(binpaths_file)
  # } else {
  #   sapply(dependencies,function(x){x=NULL})
  # }
  binpaths <- reactiveFileReader(intervalMillis = 1000, session = session, filePath = binpaths_file, readFunc = jsonlite::fromJSON)
  

  ##-- Perform checks of dependencies --##

  #-- Check GDAL --#
  
  # build the icon of the check
  observe({
    input$check_gdal
    rv$check_gdal_isvalid <- if (!is.null(binpaths()$gdalinfo)) {
      file.exists(binpaths()$gdalinfo)
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
      conditionalPanel(
        condition = "output.check_gdal_isvalid == 'NA'", 
        p(style="text-align:center;font-size:500%;color:darkgrey;", "\u23F3")
      ),
      conditionalPanel(
        condition = "output.check_gdal_isvalid == 'TRUE'", 
        p(style="text-align:center;font-size:500%;color:darkgreen;", "\u2714")
      ),
      conditionalPanel(
        condition = "output.check_gdal_isvalid == 'FALSE'", 
        p(style="text-align:center;font-size:500%;color:red;", "\u2718")
      ),
      textOutput("check_gdal_outext"),
      easyClose = FALSE,
      footer = conditionalPanel(
        condition = "output.check_gdal_isvalid != 'NA'",  
        modalButton("\u2000Close", icon = icon("check"))
      )
    )
  })
  
  # do the check when button is pressed
  observeEvent(input$check_gdal, {
    
    # open modaldialog
    showModal(check_gdal_modal())
    
    # reset check value
    rv$check_gdal_isvalid <- NA # FIXME not working
    
    # do the check
    withCallingHandlers({
      shinyjs::html("check_gdal_outext", "")
      rv$check_gdal_isvalid <- check_gdal(abort = FALSE, force = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "check_gdal_outext", html = m$message, add = TRUE)
    })
    
  })
  
  
  #-- Check Wget --#
  
  # build the icon of the check
  observe({
    input$check_wget
    rv$check_wget_isvalid <- if (!is.null(binpaths()$wget)) {
      file.exists(file.path(binpaths()$wget,"wget.py")) &
        file.exists(file.path(binpaths()$wget,"Sentinel-download/Sentinel_download.py"))
    } else {FALSE}
  })
  output$check_wget_isvalid <- renderText(rv$check_wget_isvalid)
  output$check_wget_icon <- renderUI({
    if (is.na(rv$check_wget_isvalid)) {
      ""
    } else if (rv$check_wget_isvalid) {
      span(style="color:darkgreen;", "\u2714")
    } else {
      span(style="color:red;", "\u2718")
    }
  })
  outputOptions(output, "check_wget_isvalid", suspendWhenHidden = FALSE)
  
  # build the modalDialog
  check_wget_modal <- modalDialog(
    title = "wget check",
    size = "s",
    conditionalPanel(
      condition = "output.check_wget_isvalid == 'TRUE'", 
      div(
        p(style="color:darkgreen;text-align:center;font-size:500%;","\u2714"),
        p("wget is correctly installed.")
      )
    ),
    conditionalPanel(
      condition = "output.check_wget_isvalid == 'FALSE'", 
      div(
        p(style="color:red;text-align:center;font-size:500%;","\u2718"),
        p("wget needs to be downloaded."),
        actionButton("install_wget_button", strong("\u2000Download"), icon=icon("download")),
        modalButton("\u2000Cancel", icon = icon("ban"))#,
        # hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
        # verbatimTextOutput("install_wget_outext")
      )
    ),
    uiOutput("install_wget_outext"),
    easyClose = FALSE,
    footer = conditionalPanel(
      condition = "output.check_wget_isvalid == 'TRUE'",  
      modalButton("\u2000Close", icon = icon("check"))
    )
  )
  
  # open the modaldialog when button is pressed
  observeEvent(input$check_wget, {

    # open the dialog
    showModal(check_wget_modal)
    
  })
  
  # install wget
  observeEvent(input$install_wget_button, {
    
    # create the text to show in the modaldialog
    shinyjs::html(
      "install_wget_outext", 
      as.character(div(
        br(),
        p(style="color:darkgrey;text-align:center;font-size:500%;","\u23F3"),
        p("Wait while wget is being installed...")
      ))
    )
    
    check_wget_outerr <- tryCatch(
      install_wget(),
      error = function(e) {print(e)}
    )
    
    # remove the text
    if (is(check_wget_outerr, "error")) {
      rv$check_wget_isvalid <- FALSE
      shinyjs::html(
        "install_wget_outext", 
        as.character(div(
          br(), p("Some errors occurred:"),
          p(code(check_wget_outerr))
        ))
      )
    } else {
      rv$check_wget_isvalid <- TRUE
      shinyjs::html("install_wget_outext", "")
    }
    
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
  
  # build the modalDialog
  check_sen2cor_modal <- modalDialog(
    title = "sen2cor check",
    size = "s",
    conditionalPanel(
      condition = "output.check_sen2cor_isvalid == 'TRUE'", 
      div(
        p(style="color:darkgreen;text-align:center;font-size:500%;","\u2714"),
        p("sen2cor is correctly installed.")
      )
    ),
    conditionalPanel(
      condition = "output.check_sen2cor_isvalid == 'FALSE'", 
      div(
        p(style="color:red;text-align:center;font-size:500%;","\u2718"),
        p("sen2cor needs to be downloaded and installed."),
        actionButton("install_sen2cor_button", strong("\u2000Download"), icon=icon("download")),
        modalButton("\u2000Cancel", icon = icon("ban"))#,
        # hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
        # verbatimTextOutput("install_sen2cor_outext")
      )
    ),
    uiOutput("install_sen2cor_outext"),
    easyClose = FALSE,
    footer = conditionalPanel(
      condition = "output.check_sen2cor_isvalid == 'TRUE'",  
      modalButton("\u2000Close", icon = icon("check"))
    )
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
      "install_sen2cor_outext", 
      as.character(div(
        br(),
        p(style="color:darkgrey;text-align:center;font-size:500%;","\u23F3"),
        p("Wait while sen2cor is being installed...")
      ))
    )
    
    check_sen2cor_outmess <- capture.output(
      check_sen2cor_outerr <- tryCatch(
        install_sen2cor(),
        error = function(e) {print(e)}
      ),
      type = "message"
    )
    
    # remove the text
    if (is(check_sen2cor_outerr, "error")) {
      rv$check_sen2cor_isvalid <- FALSE
      shinyjs::html(
        "install_sen2cor_outext", 
        as.character(div(
          br(), p("Some errors occurred:"),
          p(code(check_sen2cor_outmess)),
          p(code(check_sen2cor_outerr))
        ))
      )
    } else {
      rv$check_sen2cor_isvalid <- TRUE
      shinyjs::html("install_sen2cor_outext", "")
    }

  })
  
  
  #-- Check s2download --#
  
  # build the icon of the check
  observe({
    input$check_s2download # redo when the button is pressed
    rv$check_s2download_isvalid <- if (!is.null(binpaths()$s2download)) {
      file.exists(file.path(binpaths()$s2download,"s2download.py")) &
        file.exists(file.path(binpaths()$s2download,"Sentinel-download/Sentinel_download.py"))
    } else {FALSE}
  })
  output$check_s2download_isvalid <- renderText(rv$check_s2download_isvalid)
  output$check_s2download_icon <- renderUI({
    if (is.na(rv$check_s2download_isvalid)) {
      ""
    } else if (rv$check_s2download_isvalid) {
      span(style="color:darkgreen;", "\u2714")
    } else {
      span(style="color:red;", "\u2718")
    }
  })
  outputOptions(output, "check_s2download_isvalid", suspendWhenHidden = FALSE)
  
  # build the modalDialog
  check_s2download_modal <- modalDialog(
    title = "s2download check",
    size = "s",
    conditionalPanel(
      condition = "output.check_s2download_isvalid == 'TRUE'", 
      div(
        p(style="color:darkgreen;text-align:center;font-size:500%;","\u2714"),
        p("s2download is correctly installed.")
      )
    ),
    conditionalPanel(
      condition = "output.check_s2download_isvalid == 'FALSE'", 
      div(
        p(style="color:red;text-align:center;font-size:500%;","\u2718"),
        p("s2download needs to be downloaded."),
        actionButton("install_s2download_button", strong("\u2000Download"), icon=icon("download")),
        modalButton("\u2000Cancel", icon = icon("ban"))#,
        # hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
        # verbatimTextOutput("install_s2download_outext")
      )
    ),
    uiOutput("install_s2download_outext"),
    easyClose = FALSE,
    footer = conditionalPanel(
      condition = "output.check_s2download_isvalid == 'TRUE'",  
      modalButton("\u2000Close", icon = icon("check"))
    )
  )
  
  # open the modaldialog when button is pressed
  observeEvent(input$check_s2download, {
    
    # on Windows, check Python before s2download
    if (is.null(binpaths()$python) & Sys.info()["sysname"] == "Windows") {
      
      showModal(modalDialog(
        size = "s",
        p("Check GDAL before s2download",
          "(since Python is needed by these scripts)."),
        easyClose = TRUE,
        footer = NULL
      ))
      
    } else {
      
      # open the dialog
      showModal(check_s2download_modal)
      
    }
    
  })
  
  # install s2download
  observeEvent(input$install_s2download_button, {
    
    # create the text to show in the modaldialog
    shinyjs::html(
      "install_s2download_outext", 
      as.character(div(
        br(),
        p(style="color:darkgrey;text-align:center;font-size:500%;","\u23F3"),
        p("Wait while s2download is being installed...")
      ))
    )
    
    check_s2download_outmess <- capture.output(
      check_s2download_outerr <- tryCatch(
        install_s2download(),
        error = function(e) {print(e)}
      ),
      type = "message"
    )
    
    # remove the text
    if (is(check_s2download_outerr, "error")) {
      rv$check_s2download_isvalid <- FALSE
      shinyjs::html(
        "install_s2download_outext", 
        as.character(div(
          br(), p("Some errors occurred:"),
          p(code(check_s2download_outmess)),
          p(code(check_s2download_outerr))
        ))
      )
    } else {
      rv$check_s2download_isvalid <- TRUE
      shinyjs::html("install_s2download_outext", "")
    }
    
  })
  
}


settings.shiny <- shinyApp(ui = settings.ui, server = settings.server)

# run
if (interactive()) {
  options(device.ask.default = FALSE)
  return(runApp(settings.shiny))
} else {
  stop("The function must be run from an interactive R session.")
}

}
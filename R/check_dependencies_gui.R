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
#' @importFrom shinyjs hide html useShinyjs
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom utils capture.output
#' @importFrom jsonlite fromJSON toJSON
#' @examples
#' \dontrun{
#' 
#' check_sen2r_deps()
#' }

check_sen2r_deps <- function() {
  
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
        span(style="display:inline-block;vertical-align:center;padding-top:5px;",
             actionButton("check_wget", "Check Wget", width=200),
             "\u2000"),
        span(style="display:inline-block;vertical-align:center;",
             htmlOutput("check_wget_icon"))
        
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
        "(using only Level-1C \u2013 TOA products",
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
           htmlOutput("check_s2download_icon")),
      
      hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
      uiOutput("footer_buttons")
      
    ) # end of fluidRow Dependencies
    
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
      "python", "sen2cor", "s2download", "wget"
    )
    # load binpaths
    binpaths_file <- file.path(system.file("extdata",package="sen2r"),"paths.json")
    
    binpaths <- reactivePoll(1000, session, function() {
    }, function() {
      if (file.exists(binpaths_file)) {
        fromJSON(binpaths_file)
      } else {
        sapply(dependencies,function(x){x=NULL})
      }
    })
    
    
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
            p("GDAL needs to be installed.",
              "To do it, download the OSGeo4W installer using the button below:",
              "then, give the administrator rules when required,",
              "choose the \"Advanced install\" and",
              "continue clicking \"Next\";",
              "when the window to chose the packages to install will appear,",
              "check the package \"gdal-python\" and install it."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;", 
                actionButton("install_gdal_button", strong("\u2000Download"), icon=icon("download")),
                modalButton("\u2000Cancel", icon = icon("ban")))
          )
        } else {
          div(
            p(style="text-align:center;font-size:500%;color:red;", 
              icon("times-circle")),
            p("GDAL needs to be installed.",
              "To do it, install the package \"python-gdal\",",
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
    
    
    # do the check when button is pressed
    observeEvent(input$check_gdal, {
      
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
        rv$check_gdal_isvalid <- check_gdal(abort = FALSE, force = TRUE)
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
      
      # Download wget
      osgeo4w_url <- paste0(
        "http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
        if (Sys.info()["machine"]=="x86-64") {"_64"},".exe"
      )
      osgeo4w_path <- tempfile(pattern="dir",fileext = ".exe")
      # use wget instead of download.file() because the internal function
      # fails in downloading .exe (the output file is different - biggger - from the source).
      system(paste0(
        "\"",binpaths()$wget, "\" -q \"", osgeo4w_url, 
        "\" -O  \"", osgeo4w_path, "\""
      ), intern=TRUE)
      # download.file(osgeo4w_url, osgeo4w_path)
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
              "please repeat the check to be sure all was correctly installed."),
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
    
    
    
    
    #-- Check Wget --#
    
    # build the icon of the check
    observe({
      input$check_wget
      rv$check_wget_isvalid <- if (Sys.info()["sysname"] != "Windows") {
        TRUE
      } else if (!is.null(binpaths()$wget)) {
        file.exists(binpaths()$wget)
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

    output$check_wget_message <- renderUI({
      if (is.na(rv$check_wget_isvalid)) {
        ""
      } else if (!rv$check_wget_isvalid) {
        div(
          align = "center",
          p(style="color:red;text-align:center;font-size:500%;",
            icon("times-circle")),
          p("wget needs to be downloaded."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;",
              actionButton("install_wget_button", strong("\u2000Download"), icon=icon("download")),
              modalButton("\u2000Cancel", icon = icon("ban")))
        )
      } else if (rv$check_wget_isvalid) {
        div(
          align = "center",
          p(style="text-align:center;font-size:500%;color:darkgreen;", 
            icon("check-circle")),
          p("Wget is correctly installed."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;", 
              modalButton("\u2000Close", icon = icon("check")))
        )
      }
    })
    
    # build the modalDialog
    check_wget_modal <- modalDialog(
      title = "wget check",
      size = "s",
      uiOutput("check_wget_message"),
      easyClose = FALSE,
      footer = NULL
    )
    
    # open the modaldialog when button is pressed
    observeEvent(input$check_wget, {
      
      # open the dialog
      showModal(check_wget_modal)
      
    })
    
    # install wget
    observeEvent(input$install_wget_button, {
      
      shinyjs::html(
        "check_wget_message", 
        as.character(div(
          align="center",
          p(style="text-align:center;font-size:500%;color:darkgrey;", 
            icon("cog", class = "fa-spin")),
          p("Wait while Wget is being installed...")
        ))
      )
      
      Sys.sleep(0.5)
      check_wget_outerr <- tryCatch(
        install_wget(),
        error = function(e) {print(e)}
      )

      # remove the text
      if (is(check_wget_outerr, "error")) {
        shinyjs::html(
          "check_wget_message", 
          as.character(div(
            align="center",
            p(style="text-align:center;font-size:500%;color:red;", 
              icon("times-circle")),
            p("Some errors occurred:"),
            p(code(check_wget_outerr)),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;", 
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
        rv$check_wget_isvalid <- FALSE
      } else {
        shinyjs::html(
          "check_wget_message", 
          as.character(div(
            align="center",
            p(style="text-align:center;font-size:500%;color:darkgreen;", 
              icon("check-circle")),
            p("Wget was correctly installed."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;", 
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
        rv$check_wget_isvalid <- FALSE
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
    
    output$check_sen2cor_message <- renderUI({
      if (is.na(rv$check_sen2cor_isvalid)) {
        ""
      } else if (rv$check_sen2cor_isvalid) {
        div(
          align = "center",
          p(style="color:darkgreen;text-align:center;font-size:500%;",
            icon("check-circle")),
          p("sen2cor is correctly installed."),
          div(style="text-align:right;", 
              modalButton("\u2000Close", icon = icon("check")))
          )
      } else {
        div(
          align = "center",
          p(style="color:red;text-align:center;font-size:500%;",
            icon("times-circle")),
          p("sen2cor needs to be downloaded and installed."),
          hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
          div(style="text-align:right;", 
              actionButton("install_sen2cor_button", strong("\u2000Download"), icon=icon("download")),
              modalButton("\u2000Cancel", icon = icon("ban")))
        )
      }
    })
    
    # build the modalDialog
    check_sen2cor_modal <- modalDialog(
      title = "sen2cor check",
      size = "s",
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
          p("Wait while sen2cor is being installed...")
        ))
      )
      
      check_sen2cor_outmess <- capture.output(
        check_sen2cor_outerr <- tryCatch(
          .install_sen2cor(interactive = FALSE),
          error = function(e) {print(e)}
        ),
        type = "message"
      )
browser()
      
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
            p("sen2cor was correctly installed."),
            hr(style="margin-top: 0.75em; margin-bottom: 0.75em;"),
            div(style="text-align:right;", 
                modalButton("\u2000Close", icon = icon("check")))
          ))
        )
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
          p(style="color:darkgreen;text-align:center;font-size:500%;",
            icon("check-circle")),
          p("s2download is correctly installed.")
        )
      ),
      conditionalPanel(
        condition = "output.check_s2download_isvalid == 'FALSE'", 
        div(
          p(style="color:red;text-align:center;font-size:500%;",
            icon("times-circle")),
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
          .install_s2download(interactive = FALSE),
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
    
    
    
    ##-- Footer buttons --##
    observe({
      rv$check_all_isvalid <- all(c(
        rv$check_gdal_isvalid, rv$check_wget_isvalid, 
        rv$check_sen2cor_isvalid, rv$check_s2download_isvalid
      ))
    })
    
    output$footer_buttons <- renderUI({
      div(
        style = "vertical-align:center;text-align:right;",
        if (rv$check_all_isvalid) {
          span(
            style = "display:inline-block;",
            "All the dependencies are satisfied, you can safely use the library.\u2000"
          )
          # actionButton("close_gui", "\u2000Close", icon = icon("check"), class = "darkbutton")
        },
        actionButton(
          "close_gui", "\u2000Close", 
          icon = icon(ifelse(rv$check_all_isvalid, "check", "exclamation-triangle")), 
          class = "darkbutton"
        )
        
      )
      
    })
    
    # Close the connection when button is pressed
    observeEvent(input$close_gui, {
      if (!rv$check_all_isvalid) {
        confirmSweetAlert(
          session = session, inputId = "confirm_close", type = "warning",
          title = "Closing the GUI?",
          text = paste0(
            "Are you sure do you want to quit? ",
            "Running the package with unsatisfied ",
            "dependencies can lead to errors."
          ), 
          danger_mode = TRUE, btn_labels = c("Cancel", "Close window")
        )
      } else {
        stopApp()
      }
    })
    observeEvent(input$confirm_close, {
      if (input$confirm_close) {
        stopApp()
      }
    })
    
    # Close the connection when window is closed
    session$onSessionEnded(function() {
      stopApp()
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
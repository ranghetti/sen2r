

s2_gui <- function(param_list=NULL) {

  require(shinydashboard)
  require(mapview)
  require(mapedit)
  require(shiny)
  library(leaflet)
  library(leaflet.extras)
  library(reticulate)
  library(shinyFiles)
  library(shinyjs)

  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal
  osr <- import("osgeo",convert=FALSE)$osr

  # require(RColorBrewer)
  # require(ggplot2)

  # TODO: populate parameter values with param_list content, if provided


  # extract and import tiles kml
  s2tiles_kmz <- system.file("extdata","vector","s2_tiles.kmz",package="fidolasen")
  s2tiles_kml <- gsub("\\.kmz$",".kml",s2tiles_kmz)
  if (!file.exists(s2tiles_kml)) {
    unzip(zipfile = s2tiles_kmz,
          files   = basename(s2tiles_kml),
          exdir   = dirname(s2tiles_kml),
          unzip   = "internal")
  }
  s2tiles <- st_read(s2tiles_kml, stringsAsFactors=FALSE)
  s2tiles[,!names(s2tiles)%in%c("Name","geometry")] <- NULL
  names(s2tiles) <- gsub("^Name$","tile_id",names(s2tiles))

  # shiny
  s2_gui.ui <- dashboardPage(
      dashboardHeader(title="fidolasen"),

      dashboardSidebar(

        sidebarMenu(
          menuItem("Processing steps", tabName = "tab_steps", icon = icon("image"))
        ),
        sidebarMenu(
          menuItem("Spatial-temporal selection", tabName = "tab_query", icon = icon("image"))
        ),
        sidebarMenu(
          menuItem("Product selection", tabName = "tab_prod", icon = icon("image"))
        ),
        sidebarMenu(
          menuItem("Output geometry", tabName = "tab_geom", icon = icon("image"))
        ),
        sidebarMenu(
          menuItem("Set directories", tabName = "tab_path", icon = icon("image"))
        )
      ),

      dashboardBody(
        tabItems(

          ### Main tab (select steps to perform) ###
          tabItem(
            tabName = "tab_steps",
            title="Main options",

            fluidRow(box(
              title=h2("Select sensors"),
              width=12,
              # TODO move elsewhere
              checkboxGroupInput("sel_sensor", NULL, #"Select sensors",
                                 choices = list("Sentinel-2A" = "s2a",
                                                "Sentinel-2B" = "s2b"),
                                 selected=c("s2a","s2b"),
                                 inline = TRUE)
            )), # end of fluidRow/box "Select sensors"


            fluidRow(box(
              title=h2("Processing steps"),
              width=12,

              column(
                width=4,
                # step_download (online/offline mode)
                radioButtons("step_download", h3("Download"),
                             choices = list("Download tiles" = 1,
                                            "Process existing tiles" = 2),
                             selected = character(0)),

                # delete_safe
                conditionalPanel(
                  condition = "input.step_download == 1",
                  radioButtons("rm_safe", "Delete SAFE tiles?",
                               choices = list("Yes" = "all",
                                              "Only L1C" = "l1c",
                                              "No" = "no"),
                               selected="no")
                )

              ),

              column(
                width=8,
                # step_atmcorr (perform or not sen2cor and how)
                uiOutput("step_atmcorr")
              ),

              # steps_preprocess
              column(
                width=12,
                checkboxGroupInput("steps_reqout", h3("Select required outputs"),
                             choices = list("Raw SAFE format" = "return_safe",
                                            "Single tiles in another format" = "return_tiles",
                                            "Selected extent" = "return_clipped"),
                             selected=c("return_clipped"))
              )



            )), # end of fluidRow/box "Processing steps"





            # Save button
            tags$head(tags$script(src = "message-handler.js")),
            actionButton("save_param", "Save parameters")

          ), # end of tabItem tab_steps


          ### Querying parameters (spatio-temporal) ###
          tabItem(
            tabName = "tab_query",

            fluidRow(box(
              title="Temporal parameters",
              width=12,

              column(
                width=6,
                dateRangeInput("timewindow", label = "Time interval")
              ),

              column(
                width=6,
                radioButtons("timeperiod", label = "Time period type",
                             choices = list("Full" = "full",
                                            "Seasonal" = "seasonal"),
                             selected = "full",
                             inline = TRUE)
              )

            )), # end of fluidRow/box "Temporal parameters"

            fluidRow(box(
              title="Spatial extent",
              width=12,

              column(
                width=9,
                editModUI("extent_editor")
              ),

              column(
                width=3,
                uiOutput("s2tiles_selID"),
                strong("Orbits selected"),
                helpText(em("Not yet impemented."))
              )


            )) # end of fluidRow/box "Spatial extent"

          ), # end of tabItem tab_query

          tabItem(
            tabName = "tab_prod",
            title="Product selection",

            box(
              width=12,
              title="Products to be exported",
              checkboxGroupInput("check_prods",
                                 NULL,
                                 choices = list("TOA (top-of-atmosphere) Surface Reflectance" = "TOA",
                                                "BOA (bottom-of-atmosphere) Surface Reflectance" = "BOA",
                                                "SCL (surface classification map)" = "SCL",
                                                "TCI (true-color) RGB 8-bit image" = "TCI"),
                                 selected = c("BOA"))
            ), # end of box for product selection

            box(
              width=12,
              title="Index selection",

              column(
                width=8,
                textInput("filter_indices", "Filter indices"),
                uiOutput("check_indices")
              ),

              column(
                width=4,
                uiOutput("select_index"),
                uiOutput("show_formula")
              ) # TODO change and improve

            ) # end of column/box for indices selection



          ), # end of tabItem tab_prod

          ### Output tab (geometry and parameters) ###
          tabItem(
            tabName = "tab_geom",
            title="Output parameters",

            fluidRow(
              h2("\u00a0 Output geometry"),

              box(
                width=4,
                radioButtons("resolution", label = "Spatial resolution",
                             choices = list("10 metres" = "10m",
                                            "20 metres" = "20m",
                                            "60 metres" = "60m",
                                            "Custom" = "custom"),
                             selected = "10m"),
                conditionalPanel(
                  condition = "input.resolution == 'custom'",
                  numericInput("resolution_custom", "Specify resolution (in metres)",
                               # width="100px",
                               value = 10,
                               min = 0)
                )
              ), # end of box resolution

              box(
                width=4,
                radioButtons("outproj", label = "Output projection",
                             choices = list("Input projection (do not reproject)" = "no_reproj",
                                            "UTM projection" = "utm",
                                            "Custom EPSG CRS" = "epsg",
                                            "Custom proj4" = "proj4"),
                             selected = "no_reproj"),

                conditionalPanel(
                  condition = "input.outproj == 'utm'",
                  numericInput("outproj_utmzone", "Specify UTM timezone:",
                               value=character(0), min=1, max=60, width="150pt")
                ),
                conditionalPanel(
                  condition = "input.outproj == 'epsg'",
                  numericInput("outproj_epsg", "Specify EPSG code:",
                               value=character(0), min=0, width="150pt")
                ),
                conditionalPanel(
                  condition = "input.outproj == 'proj4'",
                  textInput("outproj_proj4", "Enter custom proj4string:",
                            value=character(0), width="150pt")
                ),

                htmlOutput("outproj_message")
              ), # end of box reprojection

              box(
                width=4,
                radioButtons("resampling", label = "Resampling method",
                             choices = list("Nearest neighbour" = "near",
                                            "Mode" = "mode"),
                             selected = "near")
              ) # end of box resampling

            ), # end of fluidRow "Output geometry"


            fluidRow(
              h2("\u00a0 Output format"),

              box(
                width=4,
                radioButtons("outformat", label = "Output format",
                             choices = list("GeoTiff" = "GTiff",
                                            "ENVI" = "ENVI"),
                             # TODO add others common formats
                             selected = "GTiff"),

                conditionalPanel(
                  condition = "input.outformat == 'GTiff'",
                  radioButtons("compression", label = "Output compression",
                               choices = list("Uncompressed" = "NONE",
                                              "Low (packbits)" = "PACKBITS",
                                              "Medium (lzw)" = "LZW",
                                              "High (deflate)" = "DEFLATE"),
                               selected = "LZW")
                )
              ),

              box(
                width=4,
                radioButtons("overwrite", label = "Overwrite existing outputs",
                             choices = list("Yes" = TRUE,
                                            "No (skip if outputs exist)" = FALSE),
                             selected = FALSE)
              )

            ) # end of fluidRow "Output format"

          ), # end of tabItem tab_geom


          ### Path tab (set all the paths) ###
          tabItem(
            tabName = "tab_path",
            title="Set directories",

            div(style="display:inline-block;vertical-align:top;",
                strong("Directory for level-1C SAFE products: \u00a0")),
            div(style="display:inline-block;vertical-align:top;",
                htmlOutput("path_l1c_errormess")),
            div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                    shinyDirButton("path_l1c_sel", "Select", "Specify directory for level-1C SAFE products")),
                div(style="display:inline-block;vertical-align:top;",
                    textInput("path_l1c_textin", NULL, "Enter directory..."))),

            div(style="display:inline-block;vertical-align:top;",
                strong("Directory for level-2A SAFE products: \u00a0")),
            div(style="display:inline-block;vertical-align:top;",
                htmlOutput("path_l2a_errormess")),
            div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                    shinyDirButton("path_l2a_sel", "Select", "Specify directory for level-2A SAFE products")),
                div(style="display:inline-block;vertical-align:top;",
                    textInput("path_l2a_textin", NULL, "Enter directory..."))),

            div(style="display:inline-block;vertical-align:top;",
                strong("Directory for entire tiles in custom format: \u00a0")),
            div(style="display:inline-block;vertical-align:top;",
                htmlOutput("path_tiles_errormess")),
            div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                    shinyDirButton("path_tiles_sel", "Select", "Specify directory for entire tiles in custom format")),
                div(style="display:inline-block;vertical-align:top;",
                    textInput("path_tiles_textin", NULL, "Enter directory..."))),

            div(style="display:inline-block;vertical-align:top;",
                strong("Directory for output pre-processed products: \u00a0")),
            div(style="display:inline-block;vertical-align:top;",
                htmlOutput("path_out_errormess")),
            div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                    shinyDirButton("path_out_sel", "Select", "Specify directory for output pre-processed products")),
                div(style="display:inline-block;vertical-align:top;",
                    textInput("path_out_textin", NULL, "Enter directory...")))






          ) # end of tabItem tab_steps

        ) # end of tabItems
      ) # end of dashboardBody

    ) # end of s2_gui.ui dashboardPage

  s2_gui.server <- function(input, output, session) {

    ## Steps module ##

    # Create widgets for step_atmcorr basing on step_download
    output$step_atmcorr <- renderUI({
      if (length(input$step_download)==0) {
        HTML(paste(
          h3("Atmospheric correction"),
          br(),
          helpText(em("Please select download method")),
          sep="\n"
        ))
      } else {
        radioButtons("step_atmcorr", h3("Atmospheric correction"),
                     choices = if (length(input$step_download)==0) {
                       list("Please select download method" = 0)
                     } else if(input$step_download == 1) {
                       list("Automatically download or correct basing on availability" = 1,
                            "Use only L2A products already available for download" = 2,
                            "Always download L1C products and correct locally" = 3,
                            "Never perform (use only L1C products)" = 4)
                     } else if (input$step_download == 2) {
                       list("Use only L2A products already available" = 2,
                            "Use L1C products already available and correct locally" = 3,
                            "Do not perform (use only L1C products already available)" = 4)
                     },
                     selected = character(0))
      }
    })



    # Exit and save
    observeEvent(input$save_param, {
      return_list <- list()
      return_list$drawn_extent <- rv$drawn_extent
      return_list$s2tiles_intersect <- rv$s2tiles_selected
      stopApp(return_list)
    })

    ## end of steps module ##


    ## Extent module ##

    # default view
    extent_sf <- st_sf(
      st_sfc(
        st_polygon(list(matrix(c(-1E6,4E6,2E6,4E6,2E6,7E6,-1E6,7E6,-1E6,4E6),ncol=2, byrow=TRUE))),
        # st_polygon(),
        crs = 32632))

    # initialise rv
    rv <- reactiveValues()
    rv$s2tiles_selected <- s2tiles[1,]
    rv$drawn_extent <- NULL
    extent_ns <- NS("extent_editor")

    base_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = 11.96, lat = 44.86, zoom = 10)

    # (http://r-spatial.org/r/2017/06/09/mapedit_0-2-0.html)
    extent_edits <- callModule(editModPoly, "extent_editor", base_map)
    # observe({print(extent_edits())})

    # Create the extent map
    observe({
      # req(extent_edits()$finished)
      if(length(extent_edits()$finished) > 0) {
        rv$drawn_extent <- extent_edits()$finished
        rv$s2tiles_selected <- s2tiles[unique(unlist(st_intersects(st_transform(extent_edits()$finished,4326), s2tiles))),]
        output$s2tiles_selID <- renderUI({
          checkboxGroupInput("tiles_checkbox",
                             "Tiles selected",
                             choices = setNames(
                               as.list(rv$s2tiles_selected$tile_id),
                               rv$s2tiles_selected$tile_id),
                             selected = rv$s2tiles_selected$tile_id)
        })

        leafletProxy(extent_ns("extent_edits")) %>% # FIXME why is it not working?
          addPolygons(data = rv$s2tiles_selected,
                      fill = TRUE,
                      fillColor = "blue",
                      fillOpacity = .8,
                      stroke = TRUE,
                      weight = 3,
                      color = "white")

      } else {
        rv$s2tiles_selected <- s2tiles[1,]
        rv$drawn_extent <- NULL
        output$s2tiles_selID <- renderUI({
          checkboxGroupInput("tiles_checkbox",
                             "Tiles selected",
                             choices = NULL)
        })
      }
    })

    # TODO activate/deactivate editing and allow other modes (bbox?, load shape)
    # (basing on https://github.com/r-spatial/mapedit/issues/61)

    ## end of extent module ##



    ## Product module ##

    create_indices_db()
    indices_db <- data.table(list_indices(c("n_index","name","longname","s2_formula_mathml","link")))
    indices_db[,extendedname:=paste0(name," (",longname,")")]
    setkey(indices_db, "name")

    indices_filtered <- reactive({
      indices_matches <- indices_db[grep(tolower(input$filter_indices),
                                         tolower(indices_db$extendedname)),
                                    name]
      indices_db[unique(c(indices_checked(),indices_matches)),
                 list(name,extendedname)]
    })
    indices_checked <- reactive({
      sort(input$list_indices)
    })
    # observe({
    #   names(indices_checked) <- indices_db[match(indices_checked, indices_db$name),extendedname]
    # })

    output$check_indices <- renderUI({
      checkboxGroupInput("list_indices",
                         "Indices to be exported",
                         choices = setNames(as.list(indices_filtered()$name),
                                            indices_filtered()$extendedname),
                         selected = indices_checked())
    })

    output$select_index <- renderUI({
      selectInput("select_indexformula",
                  "Select index to be shown",
                  choices = setNames(as.list(indices_filtered()$name),
                                     indices_filtered()$name),
                  selected = "NDVI")
    })

    output$show_formula <- renderUI({
      withMathJax(indices_db[name==input$select_indexformula,
                 HTML(s2_formula_mathml)])
    })





    ## end of product module ##




    ## Geometry module ##

    # reprojection
    observe({
      outproj_in <- switch(input$outproj,
                           no_reproj = "",
                           utm = ifelse(is.na(input$outproj_utmzone),"",input$outproj_utmzone),
                           epsg = ifelse(is.na(input$outproj_epsg),"",input$outproj_epsg),
                           proj4 = ifelse(is.na(input$outproj_proj4),"",input$outproj_proj4))
      if (outproj_in!="") {
        proj4_check <- try(
          outproj_validated <- switch(input$outproj,
                                      no_reproj = NA,
                                      utm = CRS(paste0("+proj=utm ",
                                                       "+zone=",input$outproj_utmzone," ",
                                                       "+datum=WGS84 +units=m +no_defs ",
                                                       "+ellps=WGS84 +towgs84=0,0,0"))@projargs,
                                      epsg = CRS(paste0("+init=epsg:",input$outproj_epsg))@projargs,
                                      proj4 = CRS(input$outproj_proj4)@projargs)

        )
        if (class(proj4_check)=="try-error") {
          output$outproj_message <- renderUI(span(style="color:red",
                                                "The projection is not recognised."))
        } else if (is.na(outproj_validated)) {
          output$outproj_message <- renderText("")
        } else {
          proj4_wkt <- outproj_validated %>%
            rgdal::showWKT() %>%
            r_to_py() %>%
            osr$SpatialReference()
          proj4_name <- if (is.projected(CRS(outproj_validated))) {
            proj4_wkt$GetAttrValue('projcs')
          } else {
            proj4_wkt$GetAttrValue('geogcs')
          }
          output$outproj_message <- renderUI(HTML(paste(strong(span(style="color:darkgreen",
                                                                  "Selected projection:")),
                                                      br(),
                                                      gsub("\\_"," ",proj4_name),
                                                      sep="\n"))
          )
        }
      } else {
        output$outproj_message <- renderText("")
      }
    })

    ## end of geometry module ##


    ## Path module ##

    # accessory functions to check that the new directory exists and is writable
    path_check <- function(path) {
      if (length(path)>0 & path[1]!="") {
        if (!dir.exists(path)) {
          return(renderUI(span(style="color:red",
                               "(the directory does not exist)")))
        } else if (file.access(path, mode=2)<0) {
          return(renderUI(span(style="color:red",
                               "(the directory is not writable)")))
        } else {
          return(renderText(""))
        }
        #
      } else {
        return(renderText(""))
      }
    }

    volumes <- c("Home"=path.expand("~"), getVolumes()())
    path_l1c_string <- path_l2a_string <- path_tiles_string <- path_out_string <- ""

    shinyDirChoose(input, "path_l1c_sel", roots = volumes)
    shinyDirChoose(input, "path_l2a_sel", roots = volumes)
    shinyDirChoose(input, "path_tiles_sel", roots = volumes)
    shinyDirChoose(input, "path_out_sel", roots = volumes)

    # if paths change after using the shinyDirButton, update the values and the textInput
    observe({
      path_l1c_string <- parseDirPath(volumes, input$path_l1c_sel)
      updateTextInput(session, "path_l1c_textin", value = path_l1c_string)
      output$path_l1c_errormess <- path_check(path_l1c_string)
    })
    observe({
      path_l2a_string <- parseDirPath(volumes, input$path_l2a_sel)
      updateTextInput(session, "path_l2a_textin", value = path_l2a_string)
      output$path_l2a_errormess <- path_check(path_l2a_string)
    })
    observe({
      path_tiles_string <- parseDirPath(volumes, input$path_tiles_sel)
      updateTextInput(session, "path_tiles_textin", value = path_tiles_string)
      output$path_tiles_errormess <- path_check(path_tiles_string)
    })
    observe({
      path_out_string <- parseDirPath(volumes, input$path_out_sel)
      updateTextInput(session, "path_out_textin", value = path_out_string)
      output$path_out_errormess <- path_check(path_out_string)
    })

    # if path changes after using the textInput, update the value
    observe({
      path_l1c_string <- input$path_l1c_textin
      # FIXME add a sort of "updateShinyDirButton" as in the case above
      output$path_l1c_errormess <- path_check(path_l1c_string)
    })
    observe({
      path_l2a_string <- input$path_l2a_textin
      output$path_l2a_errormess <- path_check(path_l2a_string)
    })
    observe({
      path_tiles_string <- input$path_tiles_textin
      output$path_tiles_errormess <- path_check(path_tiles_string)
    })
    observe({
      path_out_string <- input$path_out_textin
      output$path_out_errormess <- path_check(path_out_string)
    })


    ## end of path module ##







  } # end of s2_gui.server function


  s2_gui.shiny <- shinyApp(
    ui = s2_gui.ui,
    server = s2_gui.server
  )

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(runApp(s2_gui.shiny))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}




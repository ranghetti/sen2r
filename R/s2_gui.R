#' @title Launch the GUI for Sentinel-2 products
#' @description Launch the GUI to set parameters for the processing
#'  chain of Sentinel-2 products.
#' @param param_list List of parameters for initialising the GUI values
#'  (if empty, default values are used).
#' @param thunderforest_api Character value with the API for thinderforest
#'  layers (now not used).
#' @return A list of parameters.
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom data.table data.table ":="
#' @importFrom geojsonio geojson_json
#' @importFrom leaflet addLayersControl addPolygons addProviderTiles addTiles clearShapes hideGroup labelOptions layersControlOptions leaflet leafletProxy setView
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions removeDrawToolbar
#' @importFrom mapedit editModUI
#' @importFrom reticulate import
#' @importFrom sf st_intersects st_polygon st_read st_sf st_sfc st_transform
#' @importFrom shiny a actionButton br callModule checkboxGroupInput checkboxInput column conditionalPanel dateRangeInput div em fluidRow h2 h3 helpText HTML htmlOutput icon isolate NS numericInput observe p radioButtons reactive reactiveValues renderText renderUI runApp selectInput shinyApp span stopApp strong textInput uiOutput updateCheckboxGroupInput updateDateRangeInput updateRadioButtons updateTextInput withMathJax
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath shinyDirButton shinyDirChoose shinyFileChoose shinyFileSave shinyFilesButton shinySaveButton
#' @importFrom sprawl check_proj4string get_rastype get_vectype
#' @importFrom stats setNames
#' @importFrom utils head unzip
#'
#' @export


s2_gui <- function(param_list=NULL,
                   thunderforest_api=NA) {

  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal
  osr <- import("osgeo",convert=FALSE)$osr

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
        menuItem("Processing steps", tabName = "tab_steps", icon = icon("list-ol"))
      ),
      sidebarMenu(
        menuItem("Spatial-temporal selection", tabName = "tab_query", icon = icon("object-ungroup"))
      ),
      sidebarMenu(
        menuItem("Product selection", tabName = "tab_prod", icon = icon("clone"))
      ),
      sidebarMenu(
        menuItem("Output geometry", tabName = "tab_geom", icon = icon("th"))
      ),
      sidebarMenu(
        menuItem("Set directories", tabName = "tab_path", icon = icon("folder-open"))
      )
    ),

    dashboardBody(
      tabItems(

        ### Main tab (select steps to perform) ###
        tabItem(
          tabName = "tab_steps",
          title="Main options",

          fluidRow(box(
            title="Select sensors",
            width=12,
            # TODO move elsewhere
            checkboxGroupInput("sel_sensor", NULL, #"Select sensors",
                               choices = list("Sentinel-2A" = "s2a",
                                              "Sentinel-2B" = "s2b"),
                               selected=c("s2a","s2b"),
                               inline = TRUE)
          )), # end of fluidRow/box "Select sensors"


          fluidRow(box(
            title="Processing steps",
            width=12,

            column(
              width=4,
              # step_download (online/offline mode)
              radioButtons("step_download", h3("Download"),
                           choices = list("Download tiles" = 1,
                                          "Process existing tiles" = 2),
                           selected = 1),

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
              # uiOutput("step_atmcorr")
              radioButtons("step_atmcorr", h3("Atmospheric correction"),
                           choices = list("Automatically download or correct basing on availability" = 1,
                                          "Use only L2A products already available for download" = 2,
                                          "Always download L1C products and correct locally" = 3,
                                          "Never perform (use only L1C products)" = 4),
                           selected = 1)

            ),

            # steps_preprocess
            column(
              width=12,
              checkboxGroupInput("steps_reqout", h3("Select required outputs"),
                                 choices = list("Raw SAFE format" = "safe",
                                                "Single tiles in another format" = "tiles",
                                                "Selected extent" = "clipped"),
                                 selected=c("safe","clipped"))
            )



          )) # end of fluidRow/box "Processing steps"

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
              width=4,
              radioButtons("extent_type", "Use as extent:",
                           choices = list("Bounding box coordinates" = "bbox",
                                          "Upload vector file" = "vectfile",
                                          "Draw on the map" = "draw"),
                           selected = "draw"),

              radioButtons("extent_as_mask", "Mask outside the polygons?",
                           choices = list("Yes" = TRUE,
                                          "No" = FALSE),
                           selected = FALSE)
            ),

            column(
              width=8,
              conditionalPanel(
                condition = "input.extent_type == 'vectfile'",
                div(style="display:inline-block;vertical-align:top;",
                    strong("Select vector file: \u00a0")),
                div(style="display:inline-block;vertical-align:top;",
                    htmlOutput("path_vectfile_errormess")),
                div(#div(style="display:inline-block;vertical-align:top;width:50pt;", # FIXME 2 choosing file with the button the extent is not drawn yet, and the toolbar is not added/removed changing extent_type
                        # shinyFilesButton("path_vectfile_sel",
                        #                  "Select",
                        #                  "Specify the file to be used as extent",
                        #                  multiple = FALSE)),
                    div(style="display:inline-block;vertical-align:top;",
                        textInput("path_vectfile_textin", NULL, "Enter file path...")))
              ),
              conditionalPanel(
                condition = "input.extent_type == 'draw' || input.extent_type == 'vectfile'",
                radioButtons("dissolve_extent", "Number of output extents:",
                             choices = list("Unique (dissolve all the polygons)" = TRUE,
                                            "Multiple (each polygon is an extent)" = FALSE),
                             selected = TRUE)
              ),
              conditionalPanel(
                condition = "input.extent_type == 'vectfile' && input.dissolve_extent == 'FALSE'",
                selectInput("extent_id", "Select the field to use as extent name",
                            choices = list("No one (using row numbers)" = "nrow",
                                           "take","dynamically","from","shape","attributes"),
                            selected = "nrow")
              ),
              conditionalPanel(
                condition = "input.extent_type == 'bbox'",
                div(
                  strong("Insert bounding box coordinates:"),
                  div(div(style="display:inline-block;position:relative;margin-left:55px;padding-top:10px;",
                          numericInput("bbox_ymax", NULL, value=NULL, width="100px")),
                      div(style="display:inline-block;position:relative;bottom:0;margin-left:65px;padding-top:10px;",
                          span(style="color:grey", "upper north."))),
                  div(div(style="display:inline-block;position:relative;",
                          numericInput("bbox_xmin", NULL, value=NULL, width="100px")),
                      div(style="display:inline-block;position:relative;margin-left:10px;",
                          numericInput("bbox_xmax", NULL, value=NULL, width="100px")),
                      div(style="display:inline-block;position:relative;bottom:0;margin-left:10px;",
                          span(style="color:grey", "left-right east."))),
                  div(div(style="display:inline-block;position:relative;margin-left:55px;",
                          numericInput("bbox_ymin", NULL, value=NULL, width="100px")),
                      div(style="display:inline-block;position:relative;bottom:0;margin-left:65px;",
                          span(style="color:grey", "lower north."))),
                  div(div(style="display:inline-block;position:relative;",
                          textInput("bboxproj", "Projection of the coordinates:",
                                    value="4326", width="210px")),
                      div(style="display:inline-block;position:relative;bottom:0;margin-left:10px;",
                          htmlOutput("bboxproj_message")))
                )
              )
            ),


            column(
              width=9,
              # conditionalPanel(
              #   condition = "input.extent_type == 'draw'",
                editModUI("extent_editor")
              # )
            ),



            column(
              width=3,

              # conditionalPanel(
              #   condition = "input.extent_type == 'draw'",
                div(
                  checkboxGroupInput("tiles_checkbox",
                                     "Tiles selected",
                                     choices = character(0),
                                     selected = character(0)),
                  # uiOutput("s2tiles_selID"),
                  strong("Orbits selected"),
                  helpText(em("Not yet impemented."))
                )
              # )
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
              # uiOutput("select_index"),
              uiOutput("show_formula")
            )

          ) # end of column/box for indices selection



        ), # end of tabItem tab_prod

        ### Output tab (geometry and parameters) ###
        tabItem(
          tabName = "tab_geom",
          title="Output parameters",

          fluidRow(

            box(
              width=8,
              radioButtons("use_reference", label = "Use an existing raster as reference?",
                           choices = list("Yes" = TRUE,
                                          "No" = FALSE),
                           selected = FALSE),
              conditionalPanel(
                condition = "input.use_reference == 'TRUE'",
                div(
                  div(
                    style="display:inline-block;vertical-align:top;",
                    shinyFilesButton("reference_file_button", "Select", "Select reference file", multiple=FALSE),
                    "\u2001"),
                  div(
                    style="display:inline-block;vertical-align:top;width:calc(100% - 65pt);",
                    textInput("reference_file_textin", label = NULL, "Enter file path...", width="100%"))),
                  uiOutput("reference_file_message")

              )
            ),

            conditionalPanel(
              condition = "input.use_reference == 'TRUE'",
              box(
                width=4,
                checkboxGroupInput("reference_usefor", label = "Use the file as a reference for:",
                                   choices = list("Projection" = "proj",
                                                  "Resolution" = "res",
                                                  #"Extent" = "ext", # TODO
                                                  "Output format" = "outformat"),
                                   selected = c("proj","res","ext"))
              )
            )

          ), # end of fluidrow Reference

          h2("\u00a0 Output geometry"),
          fluidRow(
            box(
              width=4,
              strong("Spatial resolution"),
              conditionalPanel(
                condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('res') < 0",
                radioButtons("resolution", label = NULL,
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

              ),

              htmlOutput("outres_message")

            ), # end of box resolution

            box(
              width=4,
              strong("Output projection"),
              conditionalPanel(
                condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('proj') < 0",
                radioButtons("reproj", label = NULL,
                             choices = list("Input projection (do not reproject)" = FALSE,
                                            "Custom projection" = TRUE),
                             selected = FALSE),
                conditionalPanel(
                  condition = "input.reproj == 'TRUE'",
                  textInput("outproj", NULL,
                            value=character(0), width="100%")
                )
              ),

              htmlOutput("outproj_message")
            ), # end of box reprojection

            box(
              width=4,
              strong("Resampling method"),
              radioButtons("resampling", label = NULL,
                           choices = list("Nearest neighbour" = "near",
                                          "Mode" = "mode"),
                           selected = "near")
            ) # end of box resampling

          ), # end of fluidRow "Output geometry"


          fluidRow(
            h2("\u00a0 Output format"),

            box(
              width=4,
              strong("Output format"),
              conditionalPanel(
                condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('outformat') < 0",
                radioButtons("outformat", label = NULL,
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

              htmlOutput("outformat_message")
            ), # end of outformat box

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
                  textInput("path_tiles_textin", NULL, "Enter directory...")),
              "\u2001", # quad space
              div(style="display:inline-block;vertical-align:top;",
                  checkboxInput("path_tiles_subdirs", "Group products in subdirectories", value = TRUE))),

          div(style="display:inline-block;vertical-align:top;",
              strong("Directory for output pre-processed products: \u00a0")),
          div(style="display:inline-block;vertical-align:top;",
              htmlOutput("path_out_errormess")),
          div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                  shinyDirButton("path_out_sel", "Select", "Specify directory for output pre-processed products")),
              div(style="display:inline-block;vertical-align:top;",
                  textInput("path_out_textin", NULL, "Enter directory...")),
              "\u2001", # quad space
              div(style="display:inline-block;vertical-align:top;",
                  checkboxInput("path_out_subdirs", "Group products in subdirectories", value = TRUE))),


          # Save buttons
          br(),
          tags$head(tags$script(src = "message-handler.js")),
          p(
            shinySaveButton("export_param", "Export parameters", "Export parameters as ...", filetype=list(json="json")),
            "\u2001", # quad space
            shinyFilesButton("import_param", "Import parameters", "Import a JSON file with parameters", multiple=FALSE)
          ),
          p(
            actionButton("return_param", strong("Save and close GUI")),
            "\u2001", # quad space
            actionButton("exit_gui", "Close without saving")
          )
          # TODO client buttons
          # fileInput("import_param_client", "Import parameters from client",
          #           accept = c(
          #             "text/json",
          #             ".json")
          # )

        ) # end of tabItem tab_steps

      ) # end of tabItems
    ) # end of dashboardBody

  ) # end of s2_gui.ui dashboardPage

  s2_gui.server <- function(input, output, session) {

    extendedname <- link <- longname <- name <- providers <- s2_formula_mathml <- NULL

    # initialise rv
    # (list of reactive values to be passed as output)
    rv <- reactiveValues()

    # get server volumes
    volumes <- c("Home"=path.expand("~"), getVolumes()())

    ## Steps module ##

    # Update widgets for step_atmcorr basing on step_download
    observe({
      if (input$step_download == 1) {
        updateRadioButtons(session, "step_atmcorr",
                           choices = list("Automatically download or correct basing on availability" = 1,
                                "Use only L2A products already available for download" = 2,
                                "Always download L1C products and correct locally" = 3,
                                "Never perform (use only L1C products)" = 4),
                           selected = input$step_atmcorr)
      }
      if (input$step_download == 2) {
        updateRadioButtons(session, "step_atmcorr",
                           choices = list("Use only L2A products already available" = 2,
                                "Use L1C products already available and correct locally" = 3,
                                "Do not perform (use only L1C products already available)" = 4),
                           selected = input$step_atmcorr)
      }
    })
    ## end of steps module ##


    ## Extent module ##

    # default view
    extent_sf <- st_sf(
      st_sfc(
        st_polygon(list(matrix(c(-1E6,4E6,2E6,4E6,2E6,7E6,-1E6,7E6,-1E6,4E6),ncol=2, byrow=TRUE))),
        # st_polygon(),
        crs = 32632))


    ## Bbox mode

    # message for bboxproj
    output$bboxproj_message <- renderUI({
      bboxproj_validated <- try(
        suppressWarnings(check_proj4string(input$bboxproj, abort=TRUE)),
        silent=TRUE
      )
      if (input$bboxproj=="") {
        rv$bboxproj <- NA
        ""
      }  else if (bboxproj_validated=="invalid") {
        rv$bboxproj <- NA
        span(style="color:red", "\u2718") # ballot
        # span(style="color:red",
        #      "Insert a valid projection (UTM timezone, EPSG code or PROJ4 string).")
      } else {
        rv$bboxproj <- bboxproj_validated
        span(style="color:darkgreen", "\u2714") # check
        # div(strong("Selected projection:"),
        #     br(),
        #     projname(bboxproj_validated),
        #     style="color:darkgreen")
      }
    })

    # create bbox from coordinates
    observe({
      rv$bbox <- if (!is.na(input$bbox_xmin) & !is.na(input$bbox_xmax) &
                     !is.na(input$bbox_ymin) & !is.na(input$bbox_ymax) &
                     !(is.null(rv$bboxproj) || is.na(rv$bboxproj))) {
        if (input$bbox_xmin < input$bbox_xmax &
            input$bbox_ymin < input$bbox_ymax) {
          st_sf(st_sfc(
            st_polygon(list(matrix(
              c(input$bbox_xmin,input$bbox_ymin,
                input$bbox_xmin,input$bbox_ymax,
                input$bbox_xmax,input$bbox_ymax,
                input$bbox_xmax,input$bbox_ymin,
                input$bbox_xmin,input$bbox_ymin),
              ncol=2,
              byrow=TRUE))),
            crs = rv$bboxproj))
        } else {
          st_polygon()
        }
      } else {
        st_polygon()
      }
    })





    ## Vector file mode

    # # read the specified file
    shinyFileChoose(input, "path_vectfile_sel", roots=volumes, session=session)

    # if paths change after using the shinyDirButton, update the values and the textInput
    observe({
      path_vectfile_string <- parseFilePaths(volumes,input$path_vectfile_sel)$datapath %>%
        as.character()
      updateTextInput(session, "path_vectfile_textin", value = path_vectfile_string)
    })
    # if path changes after using the textInput, update the value
    observe({
      # output$path_vectfile_errormess <- vectfile_check(input$path_vectfile_textin)
      output$path_vectfile_errormess <- if (length(input$path_vectfile_textin)>0 &
                                            input$path_vectfile_textin[1]!="") {
        if (!file.exists(input$path_vectfile_textin)) {
          rv$vectfile_path <- NA
          renderUI(span(style="color:red", "\u2718 (the file does not exist)"))
        } else if (try(get_vectype(input$path_vectfile_textin),silent=TRUE)!="vectfile") {
          rv$vectfile_path <- NA
          renderUI(span(style="color:red", "\u2718 (the file is not a recognised vector file)"))
        } else {
          rv$vectfile_path <- input$path_vectfile_textin
          renderUI(span(style="color:darkgreen", "\u2714"))
        }
        #
      } else {
        return(renderText(""))
      }
    })



    ## Drawn mode



    # namespace for extent selection
    extent_ns <- NS("extent_editor")
    rv$draw_tiles_overlapping <- s2tiles[1,]
    rv$extent <- NULL

    # Create the extent map
    base_map <- leaflet() %>%

      # add tiles
      addTiles(group = "OpenStreetMap") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "OpenStreetMap Outdoors") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels, group = "Names") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "Metal or death") %>%

      # view and controls
      setView(lng = 11.96, lat = 44.86, zoom = 10) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("Names","S2 tiles","Extent"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Names")

    # (http://r-spatial.org/r/2017/06/09/mapedit_0-2-0.html)
    extent_edits <- callModule(editModPoly, "extent_editor", base_map)
    # observe({print(extent_edits())})


    # Select vector (bbox, file or drawn) to be used
    observe({
      rv$extent <- switch(input$extent_type,
                          bbox = rv$bbox,
                          draw = extent_edits()$finished,
                          vectfile = if (!is.na(rv$vectfile_path)) {
                            st_read(rv$vectfile_path) #%>% st_geometry()
                          } else {
                            st_polygon()
                          },
                          imported = if (!is.null(imported_param())) {
                            st_read(imported_param()$extent)
                          } else {
                            NULL
                          })
    })


    # Actions when the polygon layer changes
    observe({
      if(length(rv$extent) > 0) {
        rv$draw_tiles_overlapping <- s2tiles[unique(unlist(st_intersects(st_transform(rv$extent,4326), s2tiles))),]
        updateCheckboxGroupInput(session, "tiles_checkbox",
                                 choices = setNames(
                                   as.list(rv$draw_tiles_overlapping$tile_id),
                                   rv$draw_tiles_overlapping$tile_id),
                                 selected = rv$draw_tiles_overlapping$tile_id)
        leafletProxy(extent_ns("map")) %>%
          clearShapes() %>%
          addPolygons(data = rv$draw_tiles_overlapping,
                      group = "S2 tiles",
                      label = ~tile_id,
                      labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
                      fill = TRUE,
                      fillColor = "orange",
                      fillOpacity = .3,
                      stroke = TRUE,
                      weight = 3,
                      color = "red") %>%
          # add extent
          addPolygons(data = rv$extent,
                      group = "Extent",
                      # label = ~tile_id,
                      # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
                      fill = TRUE,
                      fillColor = "darkcyan",
                      fillOpacity = .3,
                      stroke = TRUE,
                      weight = 3,
                      color = "blue") #%>%
      } else {
        rv$draw_tiles_overlapping <- NULL
        updateCheckboxGroupInput(session, "tiles_checkbox",
                                 choices = NULL)
      }
    })






    # WIP

    # If draw mode is off, deactivate editing toolbar
    observe({
      if (input$extent_type!="draw") {
        leafletProxy(extent_ns("map")) %>%
          removeDrawToolbar(clearFeatures = FALSE)
      } else {
        leafletProxy(extent_ns("map")) %>%
          addDrawToolbar(
            polylineOptions = FALSE,
            markerOptions = FALSE,
            editOptions = editToolbarOptions())
      }
    })







    ## end of extent module ##



    ## Product module ##

    create_indices_db()
    indices_db <- data.table(list_indices(c("n_index","name","longname","s2_formula_mathml","link")))
    indices_db[,extendedname:=paste0(name," (",longname,")")]
    setkey(indices_db, "name")

    indices_rv <- reactiveValues()
    observe({
      indices_rv$matches <- indices_db[grep(tolower(input$filter_indices),
                                            tolower(indices_db$extendedname)),
                                       name]
      indices_rv$filtered <- indices_db[unique(c(indices_rv$checked,indices_rv$matches)),
                                        list(name,extendedname)]
    })
    observe({
      indices_rv$checked <- sort(input$list_indices)
    })

    output$check_indices <- renderUI({
      checkboxGroupInput("list_indices",
                         "Indices to be exported",
                         choices = setNames(as.list(indices_rv$filtered$name),
                                            indices_rv$filtered$extendedname),
                         selected = indices_rv$checked)
    })

    index_details <- function(index) {
      extendedname <- link <- longname <- name <- providers <- s2_formula_mathml <- NULL
      return(box(
        width=12,
        title=indices_db[name==index,name],
        p(em(indices_db[name==index,longname])),
        p(strong("Formula:"),
          br(),
          withMathJax(indices_db[name==index,
                                 HTML(s2_formula_mathml)])),
        p(a("More info",
            target="_blank",
            href=indices_db[name==index,link]))
      ))
    }

    output$show_formula <- renderUI({
      div(lapply(indices_rv$checked, index_details))
    })



    ## end of product module ##




    ## Geometry module ##

    ## Reference file
    shinyFileChoose(input, "reference_file_button", roots = volumes)

    reference <- reactive({

      if (!is.null(input$reference_file_button)) {

        reference_path <- input$reference_file_textin
        reference_spatype <- try(
          get_rastype(reference_path),
          silent=TRUE)
        if (reference_spatype == "rastfile") {

          # get metadata
          ref_metadata <- suppressWarnings(GDALinfo(reference_path))
          ref_res <- ref_metadata[c("res.x","res.y")]
          ref_ll <- ref_metadata[c("ll.x","ll.y")]
          ref_size <- ref_metadata[c("columns","rows")]
          ref_bbox <- matrix(
            c(ref_ll, ref_ll + ref_size * ref_res),
            ncol=2)
          dimnames(ref_bbox) <- list(c("x","y"),c("min","max"))
          ref_unit <- attr(ref_metadata, "projection") %>%
            projpar("unit")
          ref_proj <- attr(ref_unit, "proj4string")
          ref_outformat <- attr(ref_metadata, "driver")

          return(list("metadata" = ref_metadata,
                      "res" = ref_res,
                      "bbox" = ref_bbox,
                      "proj" = ref_proj,
                      "unit" = ref_unit,
                      "outformat" = ref_outformat))
        }

      }

      return(NULL)

    })

    observe({
      path_reference_file <- parseFilePaths(volumes,input$reference_file_button)$datapath %>%
        as.character()
      updateTextInput(session, "reference_file_textin", value = path_reference_file)
      output$reference_file_message <- renderUI({
        if (!is.null(reference())) {
          span(style="color:darkgreen", "\u2001\u2714") # check
        } else if (!is.null(input$reference_file_button)) {
          span(style="color:red", "\u2001\u2718") # ballot
        }
      })
    })




    ## Update resolution from reference file
    output$outres_message <- renderUI({
      if(input$use_reference==TRUE & "res" %in% input$reference_usefor) {
        if (!is.null(reference())) {
          span(style="color:darkgreen",
               paste0(
                 paste(reference()$res,
                       collapse="\u2006\u00d7\u2006"),  # shortspace times shortspace
                 switch(reference()$unit,
                        Degree="\u00b0",  # shortspace degree
                        Meter="\u2006m",  # shortspace m
                        "")))
        } else {
          span(style="color:grey",
               "Specify a valid raster file.")
        }
      } else {
        ""
      }
    })

    ## Reprojection
    output$outproj_message <- renderUI({

      # if required, take from reference raster
      if(input$use_reference==TRUE & "proj" %in% input$reference_usefor) {

        if (!is.null(reference())) {
          span(style="color:darkgreen",
               projname(reference()$proj))
        } else {
          span(style="color:grey",
               "Specify a valid raster file.")
        }

        # else, take the specified one
      } else {

          outproj_validated <- try(
            suppressWarnings(check_proj4string(input$outproj, abort=TRUE)),
            silent=TRUE
          )
          if (input$reproj==FALSE | input$outproj=="") {
            ""
          }  else if (outproj_validated=="invalid") {
            span(style="color:red",
                 "Insert a valid projection (UTM timezone, EPSG code or PROJ4 string).")
          } else {
            div(strong("Selected projection:"),
                br(),
                projname(outproj_validated),
                style="color:darkgreen")
          }

      }

    })

    ## Update output format from reference file
    output$outformat_message <- renderUI({
      if(input$use_reference==TRUE & "outformat" %in% input$reference_usefor) {
        if (!is.null(reference())) {
          span(style="color:darkgreen",
               reference()$outformat)
        } else {
          span(style="color:grey",
               "Specify a valid raster file.")
        }
      } else {
        ""
      }
    })




    ## end of geometry module ##


    ## Path module ##

    # accessory functions to check that the new directory exists and is writable
    path_check <- function(path) {
      if (length(path)>0 & path[1]!="") {
        if (!dir.exists(path)) {
          return(renderUI(span(style="color:red",
                               "\u2718 (the directory does not exist)")))
        } else if (file.access(path, mode=2)<0) {
          return(renderUI(span(style="color:red",
                               "\u2718 (the directory is not writable)")))
        } else {
          return(renderUI(span(style="color:darkgreen",
                               "\u2714")))
        }
        #
      } else {
        return(renderText(""))
      }
    }


    shinyDirChoose(input, "path_l1c_sel", roots = volumes)
    shinyDirChoose(input, "path_l2a_sel", roots = volumes)
    shinyDirChoose(input, "path_tiles_sel", roots = volumes)
    shinyDirChoose(input, "path_out_sel", roots = volumes)

    # if paths change after using the shinyDirButton, update the values and the textInput
    observe({
      path_l1c_string <- parseDirPath(volumes, input$path_l1c_sel)
      updateTextInput(session, "path_l1c_textin", value = path_l1c_string)
    })
    observe({
      path_l2a_string <- parseDirPath(volumes, input$path_l2a_sel)
      updateTextInput(session, "path_l2a_textin", value = path_l2a_string)
    })
    observe({
      path_tiles_string <- parseDirPath(volumes, input$path_tiles_sel)
      updateTextInput(session, "path_tiles_textin", value = path_tiles_string)
    })
    observe({
      path_out_string <- parseDirPath(volumes, input$path_out_sel)
      updateTextInput(session, "path_out_textin", value = path_out_string)
    })

    # if path changes after using the textInput, update the value
    observe({
      output$path_l1c_errormess <- path_check(input$path_l1c_textin)
    })
    observe({
      output$path_l2a_errormess <- path_check(input$path_l2a_textin)
    })
    observe({
      output$path_tiles_errormess <- path_check(input$path_tiles_textin)
    })
    observe({
      output$path_out_errormess <- path_check(input$path_out_textin)
    })


    ## Exit and save

    # function to create a list to objects to be returned
    create_return_list <- function() {
      rl <- list()

      # processing steps
      rl$sel_sensor <- input$sel_sensor # sensors to use ("s2a", "s2b")
      rl$step_download <- input$step_download # 1 if online mode, 2 if offline mode
      rl$rm_safe <- ifelse(input$step_download==1, input$rm_safe, "no") # "yes" to delete all SAFE, "l1c" to delete only l1c, "no" not to remove
      rl$step_atmcorr <- input$step_atmcorr # download_method in sen2cor: 1 to "auto", 2 to "l2a", 3 to "scihub", 4 to "no"
      rl$steps_reqout <- input$steps_reqout # vector of required outputs: "safe", "tiles", "clipped" (one or more)

      # spatio-temporal selection
      rl$timewindow <- input$timewindow # range of dates
      rl$timeperiod <- input$timeperiod # "full" or "seasonal"
      rl$extent <- geojson_json(rv$extent) # polygons
      # rl$s2tiles_selected <- rv$draw_tiles_overlapping[rv$draw_tiles_overlapping$tile_id %in% input$tiles_checkbox,] # MULTIPOLYGON with the selected tiles
      rl$s2tiles_selected <- input$tiles_checkbox # selected tile IDs

      # product selection
      rl$check_prods <- input$check_prods # TOA, BOA, SCL, TCI (for now)
      rl$list_indices <- input$list_indices # index names

      # output geometry
      # path of the reference file (NULL if not provided)
      rl$reference_path <- ifelse(input$use_reference==TRUE,
                                  input$reference_file_textin,
                                  NA)
      # spatial resolution (2-length numeric vector)
      rl$res <- if (input$use_reference==TRUE &
                    "res" %in% input$reference_usefor) {
        reference()$res
      } else {
        if (input$resolution == "10m") {
          c(10,10)
        } else if (input$resolution == "20m") {
          c(20,20)
        } else if (input$resolution == "60m") {
          c(60,60)
        } else if (input$resolution == "custom") {
          rep(input$resolution_custom,2)
        }
      }
      # unit of measure ("Meter" or "Degree")
      rl$unit <- ifelse(input$use_reference==TRUE &
                          "res" %in% input$reference_usefor,
                        reference()$unit,
                        "Meter") # TODO allow degrees if outproj is longlat
      # output proj4string
      rl$proj <- if (input$use_reference==TRUE &
                     "proj" %in% input$reference_usefor) {
        reference()$proj
      } else if (input$reproj==FALSE) {
        NA
      } else {
        check_proj4string(input$outproj)
      }
      # resampling method ("near" or "mode")
      rl$resampling <- input$resampling
      # output format (GDAL format name)
      rl$outformat <- ifelse(input$use_reference==TRUE &
                               "outformat" %in% input$reference_usefor,
                             reference()$outformat,
                             input$outformat)
      # output compression ("LZW", "DEFLATE" etc.)
      rl$compression <- ifelse(rl$outformat=="GTiff",
                               input$compression,
                               NA)
      # overwrite or skip existing files (logical)
      rl$overwrite <- input$overwrite

      # set directories
      rl$path_l1c <- input$path_l1c_textin # path of L1C SAFE products
      rl$path_l2a <- input$path_l2a_textin # path of L2A SAFE products
      rl$path_tiles <- input$path_tiles_textin # path of entire tiled products
      rl$path_out <- input$path_out_textin # path of output pre-processed products
      rl$path_tiles_subdirs <- input$path_tiles_subdirs # logical (use subdirs)
      rl$path_out_subdirs <- input$path_out_subdirs # logical (use subdirs)

      return(rl)
    }

    # function to import saved parameters
    import_param_list <- function(pl) {
browser()

      # processing steps
      updateCheckboxGroupInput(session, "sel_sensor", selected = pl$sel_sensor)
      updateRadioButtons(session, "step_download", selected = pl$step_download)
      updateRadioButtons(session, "rm_safe", selected = pl$rm_safe)
      updateRadioButtons(session, "step_atmcorr", selected = pl$step_atmcorr)
      updateCheckboxGroupInput(session, "steps_reqout", selected = pl$steps_reqout)

      # spatio-temporal selection
      updateDateRangeInput(session, "timewindow", start=pl$timewindow[1], end=pl$timewindow[2])
      updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
      updateRadioButtons(session, "extent_type",
                         choices = list("Bounding box coordinates" = "bbox",
                                        "Upload vector file" = "vectfile",
                                        "Draw on the map" = "draw",
                                        "Imported from JSON" = "imported"),
                         selected = "imported")
      isolate(rv$extent <- st_read(pl$extent))
      updateCheckboxGroupInput(session, "tiles_checkbox",
                               selected = pl$s2tiles_selected)
      # rl$s2tiles_selected <- input$tiles_checkbox # selected tile IDs

      # product selection
      updateCheckboxGroupInput(session, "check_prods", selected = pl$check_prods)
      indices_rv$checked <- pl$list_indices
      # updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices) # FIXME 1 not working since it is reactive

      # set directories
      updateTextInput(session, "path_l1c_textin", value = pl$path_l1c)
      updateTextInput(session, "path_l2a_textin", value = pl$path_l2a)
      updateTextInput(session, "path_tiles_textin", value = pl$path_tiles)
      updateTextInput(session, "path_out_textin", value = pl$path_out)
      updateRadioButtons(session, "path_tiles_subdirs", selected = pl$path_tiles_subdirs)
      updateRadioButtons(session, "path_out_subdirs", selected = pl$path_out_subdirs)

      # output geometry
      updateTextInput(session, "reference_file_textin", value = pl$reference_path)
      updateRadioButtons(session, "use_reference", selected = ifelse(is.na(pl$reference_path), FALSE, TRUE))

      if (is.na(pl$reference_path)) {
        updateTextInput(session, "resolution_custom", value = pl$res[1])
        updateRadioButtons(session, "resolution", selected = {
          if (pl$res[1]==10) {
            "10m"
          } else if (pl$res[1]==20) {
            "20m"
          } else if (pl$res[1]==60) {
            "60m"
          } else {
            "custom"
          }
        })
        updateRadioButtons(session, "reproj", selected = {
          if (is.na(pl$proj)) {
            FALSE
          } else {
            TRUE
          }
        })
        updateTextInput(session, "outproj", value = ifelse(!is.na(pl$proj),
                                                           pl$proj,
                                                           character(0)))
        updateRadioButtons(session, "outformat", selected = pl$outformat)
      }
      updateRadioButtons(session, "resampling", selected = pl$resampling)
      updateRadioButtons(session, "compression", selected = ifelse(pl$outformat=="GTiff",
                                                                   pl$compression,
                                                                   character(0)))
      updateRadioButtons(session, "overwrite", selected = pl$overwrite)

    }

    # if Return is pressend, exit from GUI and return values
    observe({
      if (input$return_param==1) {
        return_list <- create_return_list() # run creation of return_list
        stopApp(return_list)
      }
    })

    # if Exit is pressend, exit from GUI
    observe({
      if (input$exit_gui==1) {
        stopApp()
      }
    })

    # if Export is pressed, export the values
    observe({
      shinyFileSave(input, "export_param", roots=volumes, session=session)
      export_param_path <- parseSavePath(volumes, input$export_param)
      if (nrow(export_param_path)>0) {
        return_list <- create_return_list() # run creation of return_list
        writeLines(toJSON(return_list, pretty=TRUE),
                   as.character(export_param_path$datapath))
      }
    })

    # if Import is pressed, read a json object
    imported_param <- reactive({
      shinyFileChoose(input, "import_param", roots=volumes, session=session,
                      filetypes = c("JSON"="json"))
      import_param_path <- parseFilePaths(volumes,input$import_param)
      if (nrow(import_param_path)>0) {
        import_param_path$datapath %>%
          as.character() %>%
          readLines() %>%
          fromJSON()
      } else {
        NULL
      }
    })
    observe({
      if (exists("imported_param") && !is.null(imported_param())) {
        import_param_list(imported_param()) # FIXME 3 same as fixme2 (after importing parameters, map is not drawn)
      }
    })
    observe({
      if (!is.null(param_list)) {
browser()
        import_param_list(param_list)
      }
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




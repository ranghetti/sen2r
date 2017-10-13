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
#' @importFrom leaflet addLayersControl addPolygons addProviderTiles
#'  addTiles clearShapes hideGroup labelOptions layersControlOptions
#'  leaflet leafletProxy setView
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#'  removeDrawToolbar
#' @importFrom mapedit editModUI
#' @importFrom reticulate import
#' @importFrom sf st_intersects st_polygon st_read st_sf st_sfc st_transform
#' @importFrom shiny a actionButton br callModule checkboxGroupInput
#'  checkboxInput column conditionalPanel dateRangeInput div em fluidRow h2 h3
#'  helpText hr HTML htmlOutput icon isolate NS numericInput observe p
#'  radioButtons reactive reactiveValues renderText renderUI runApp selectInput
#'  shinyApp span stopApp strong textInput uiOutput updateCheckboxGroupInput
#'  updateDateRangeInput updateRadioButtons updateTextInput withMathJax
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath
#'  shinyDirButton shinyDirChoose shinyFileChoose shinyFileSave
#'  shinyFilesButton shinySaveButton
#' @importFrom sprawl check_proj4string get_rastype get_vectype
#' @importFrom stats setNames
#' @importFrom utils unzip
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
        menuItem("Product selection", tabName = "tab_steps", icon = icon("image"))
      ),
      sidebarMenu(
        menuItem("Spatial-temporal selection", tabName = "tab_query", icon = icon("clone"))
      ),
      conditionalPanel(
        condition = "input.preprocess == 'TRUE'",
        sidebarMenu(
          menuItem("Processing settings", tabName = "tab_prepro", icon = icon("th"))
        ),
        conditionalPanel(
          condition = "input.steps_reqout.indexOf('indices') != -1",
          sidebarMenu(
            menuItem("Index selection", tabName = "tab_index", icon = icon("calculator"))
          )
        )
      ),
      # sidebarMenu(
      #   menuItem("Set directories", tabName = "tab_path", icon = icon("folder-open"))
      # ),

      HTML("<script src=\"message-handler.js\"></script>"),
      shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color
      div(
        style="position:absolute;top:250px;",
        p(style="margin-top:15pt;margin-left:11pt;",
          shinySaveButton("export_param", "Export parameters", "Export parameters as ...", filetype=list(json="json"), class="darkbutton")
        ),
        p(style="margin-top:5pt;margin-left:11pt;",
          shinyFilesButton("import_param", "Import parameters", "Import a JSON file with parameters", multiple=FALSE, class="darkbutton")
        ),
        p(style="margin-top:20pt;",
          actionButton("return_param", strong("\u2000Save and close GUI"), icon=icon("check"), class="darkbutton")
        ),
        p(style="margin-top:0pt;",
          actionButton("exit_gui", "\u2000Close without saving", icon=icon("ban"), class="darkbutton")
        )
      )

    ),

    dashboardBody(
      tabItems(

        ### Main tab (select steps to perform) ###
        tabItem(
          tabName = "tab_steps",
          title="Product selection",

          fluidRow(box(
            title="Processing steps",
            width=12,
            radioButtons("preprocess", NULL, #"Perform the following steps:",
                         choices = list("Only find and download SAFE products" = FALSE,
                                        "Find and download SAFE, process them" = TRUE),
                         selected=TRUE,
                         inline = TRUE)
          )),

          fluidRow(box(
            title="Select products and sensors",
            width=12,

            fluidRow(
              column(
                width=8,
                conditionalPanel(
                  condition = "input.preprocess == 'TRUE'",
                  checkboxGroupInput("list_prods",
                                     "Select products:",
                                     choiceNames = list("TOA (top-of-atmosphere) Reflectance",
                                                    "BOA (bottom-of-atmosphere) Surface Reflectance",
                                                    "SCL (surface classification map)",
                                                    "TCI (true-color) RGB 8-bit image"),
                                     choiceValues = list("TOA", "BOA", "SCL", "TCI"),
                                     selected = c("BOA"))
                ),
                conditionalPanel(
                  condition = "input.preprocess == 'FALSE'",
                  checkboxGroupInput("list_levels",
                                     "Select products:",
                                     choiceNames = list(a("Level-1C", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-1c", target="_blank"),
                                                        a("Level-2A", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a", target="_blank")),
                                     choiceValues = list("l1c", "l2a"),
                                     selected = c("l2a"))
                )
              ), # end of column for indices selection

              column(
                width=4,

                conditionalPanel(
                  condition = "input.preprocess == 'TRUE'",
                  uiOutput("levels_message"),
                  br()
                ),

                checkboxGroupInput("sel_sensor", "Select sensors:",
                                   choiceNames = list("Sentinel-2A",
                                                      span("Sentinel-2B",#
                                                           actionLink("sel_sensor_tempmessage", "(note)")) # temp
                                   ),
                                   choiceValues = list("s2a", "s2b"),
                                   selected=c("s2a","s2b"),
                                   inline = FALSE)
              ) # end of column for sensor selection
            )
          )), # end of fluidrow/box "Select products and sensors"

          fluidRow(box(
            title="SAFE options",
            width=12,

            fluidRow(
              # L1C directory
              column(
                width=6,
                div(style="display:inline-block;vertical-align:top;",
                    strong("Directory for level-1C SAFE products: \u00a0")),
                div(style="display:inline-block;vertical-align:top;",
                    htmlOutput("path_l1c_errormess")),
                div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                        shinyDirButton("path_l1c_sel", "Select", "Specify directory for level-1C SAFE products")),
                    div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                        textInput("path_l1c_textin", NULL, "Enter directory...")))
              ),
              # L2A directory
              conditionalPanel(
                condition = "output.req_l2a == 'TRUE'",
                column(
                  width=6,
                  div(style="display:inline-block;vertical-align:top;",
                      strong("Directory for level-2A SAFE products: \u00a0")),
                  div(style="display:inline-block;vertical-align:top;",
                      htmlOutput("path_l2a_errormess")),
                  div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                          shinyDirButton("path_l2a_sel", "Select", "Specify directory for level-2A SAFE products")),
                      div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                          textInput("path_l2a_textin", NULL, "Enter directory...")))
                )
              )
            ), # end of fluidrow for safe directories


            fluidRow(
              column(
                width=7,
                # online_mode (online/offline mode)
                radioButtons("download_safe", "Download SAFE products?",
                             choices = list("Yes, all (overwrite existing products)" = "all",
                                            "Download only new products" = "new",
                                            "No (offline mode, process only existing products)" = "no"),
                             selected = "new")
              ),
              column(
                width=5,
                # delete_safe
                conditionalPanel(
                  condition = "input.download_safe != 'no' && (input.preprocess == 'TRUE' || (input.preprocess == 'FALSE' && input.list_levels.indexOf('l1c')==-1 && input.list_levels.indexOf('l2a')!=-1))",
                  # condition = "input.download_safe != 'no'",
                  radioButtons("rm_safe", "Delete SAFE tiles after processing?",
                               choices = list("Yes" = "all",
                                              "Only unrequired L1C" = "l1c",
                                              "No" = "no"),
                               selected="no")
                )
              )
            ), # end of fluidRow download / delete SAFE

            # step_atmcorr (perform or not sen2cor and how)
            # uiOutput("step_atmcorr")
            conditionalPanel(
              condition = "output.req_l2a == 'TRUE'",
              radioButtons("step_atmcorr", "Perform atmospheric correction?", #"Atmospheric correction",
                           choices = list("Always (download all L1C products and correct them locally)" = "scihub",
                                          "If needed (download L2A or correct from L1C basing on L2A availability)" = "auto",
                                          "Never (use only L2A products already available for download)" = "l2a"),
                           selected = "auto")
            ),
            conditionalPanel(
              condition = "output.req_l2a == 'FALSE'",
              div(strong("Perform atmospheric correction?"),
                  br(),
                  span(style="color:grey", "Atmospheric correction is not needed (only L1C products were selected)."))
            )


          )) # end of fluidRow/box "SAFE options"

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


        ### Output tab (geometry and parameters) ###
        tabItem(
          tabName = "tab_prepro",
          title="Procesisng settings",

          conditionalPanel(
            condition = "input.preprocess == 'TRUE'",

            fluidRow(box(
              title="Processing options",
              width=12,

              fluidRow(

                column(
                  width=6,

                  #steps_preprocess
                  checkboxGroupInput("steps_reqout", "Required processing steps:",
                                     choices = list("Single tiles in custom format" = "tiles",
                                                    "Tiles spatially merged" = "merged",
                                                    "Images clipped and warped on output extent" = "out",
                                                    "Spectral indices" = "indices"),
                                     selected=c("out","indices")),

                  # set directories
                  conditionalPanel(
                    condition = "input.steps_reqout.indexOf('tiles') != -1",
                    div(div(style="display:inline-block;vertical-align:top;",
                            strong("Directory for single tiles in custom format: \u00a0")),
                        div(style="display:inline-block;vertical-align:top;",
                            htmlOutput("path_tiles_errormess")),
                        div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                                shinyDirButton("path_tiles_sel", "Select", "Specify directory for single tiles in custom format")),
                            div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                                textInput("path_tiles_textin", NULL, "Enter directory..."))))
                  ),

                  conditionalPanel(
                    condition = "input.steps_reqout.indexOf('merged') != -1",
                    div(div(style="display:inline-block;vertical-align:top;",
                            strong("Directory for tiles spatially merged: \u00a0")),
                        div(style="display:inline-block;vertical-align:top;",
                            htmlOutput("path_merged_errormess")),
                        div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                                shinyDirButton("path_merged_sel", "Select", "Specify directory for tiles spatially merged")),
                            div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                                textInput("path_merged_textin", NULL, "Enter directory..."))))
                  ),

                  conditionalPanel(
                    condition = "input.steps_reqout.indexOf('out') != -1",
                    div(div(style="display:inline-block;vertical-align:top;",
                            strong("Directory for output processed products: \u00a0")),
                        div(style="display:inline-block;vertical-align:top;",
                            htmlOutput("path_out_errormess")),
                        div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                                shinyDirButton("path_out_sel", "Select", "Specify directory for output processed products")),
                            div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                                textInput("path_out_textin", NULL, "Enter directory..."))))
                  ),

                  conditionalPanel(
                    condition = "input.steps_reqout.indexOf('indices') != -1",
                    div(div(style="display:inline-block;vertical-align:top;",
                            strong("Directory for spectral indices: \u00a0")),
                        div(style="display:inline-block;vertical-align:top;",
                            htmlOutput("path_indices_errormess")),
                        div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                                shinyDirButton("path_indices_sel", "Select", "Specify directory for spectral indices")),
                            div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                                textInput("path_indices_textin", NULL, "Enter directory..."))))
                  ),

                  checkboxInput("path_subdirs", "Group products in subdirectories", value = TRUE)

                ), # end of column with paths

                column(
                  width=6,
                  radioButtons("atm_mask", "Mask cloud-covered pixels?",
                               choices = list("Yes" = TRUE,
                                              "No" = FALSE),
                               selected = FALSE,
                               inline = TRUE),

                  conditionalPanel(
                    condition = "input.atm_mask == 'TRUE'",
                    radioButtons("atm_mask_type", "Apply mask to:",
                                 choices = list("No data" = "nodata",
                                                "No data or clouds (high probability)" = "cloud_high_proba",
                                                "No data or clouds (high or medium probability)" = "cloud_medium_proba",
                                                "No data or clouds (any probability)" = "cloud_low_proba",
                                                "No data, clouds or cloud shadows" = "cloud_and_shadow",
                                                "No data, clouds, cloud shadows or thin cirrus" = "cloud_shadow_cirrus"),
                                 selected = "cloud_medium_proba")
                  )

                ) # end of column of atmospheric masking

              ) # end of fluidrow inside processing options
            )), # end of fluidRow/box "Processing steps"

            conditionalPanel(
              condition = "input.steps_reqout.indexOf('out') != -1",
              fluidRow(box(
                width=12,
                title = "Output geometry",

                fluidRow( # fluidrow for spatial reference

                  column(
                    width=8,
                    radioButtons("use_reference", label = "Use an existing raster as a reference for output grid?",
                                 choices = list("Yes (load raster)" = TRUE,
                                                "No (define new output grid)" = FALSE),
                                 selected = FALSE,
                                 inline = TRUE),
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
                  ), # end of column reference

                  conditionalPanel(
                    condition = "input.use_reference == 'TRUE'",
                    column(
                      width=4,
                      checkboxGroupInput("reference_usefor", label = "Use the file as a reference for:",
                                         choices = list("Projection" = "proj",
                                                        "Resolution" = "res"),
                                         #"Extent" = "ext", # TODO
                                         # "Output format" = "outformat"),
                                         selected = c("proj","res"))
                    ) # end of column use reference for
                  ) # end of its conditional panel

                ), # end of fluidrow for spatial reference

                hr(),

                fluidRow( # fluidrow for output geometry settings

                  column(
                    width=4,

                    strong("Spatial resolution"),
                    conditionalPanel(
                      condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('res') < 0",
                      radioButtons("rescale", label = NULL,
                                   choices = list("Native" = FALSE,
                                                  "Custom" = TRUE),
                                   selected = FALSE,
                                   inline = TRUE),
                      conditionalPanel(
                        condition = "input.rescale == 'FALSE'",
                        radioButtons("resolution_s2", label = "Specify resolution",
                                     choices = list("10 metres" = "10m",
                                                    "20 metres" = "20m",
                                                    "60 metres" = "60m"),
                                     selected = "10m")
                      ),
                      conditionalPanel(
                        condition = "input.rescale == 'TRUE'",
                        numericInput("resolution_custom", "Specify resolution (in metres)",
                                     # width="100px",
                                     value = 10,
                                     min = 0)
                      )

                    ),

                    htmlOutput("outres_message")

                  ), # end of column spatial resolution

                  column(
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

                  ), # end of column output projection

                  column(
                    width=4,
                    selectInput("resampling", label = "Resampling method",
                                 choices = list("Nearest neighbour" = "near",
                                                "Average" = "average",
                                                "Bilinear" = "bilinear",
                                                "Cubic" = "cubic",
                                                "Cubic spline" = "cubicspline",
                                                "Lanczos windowed sinc" = "lanczos",
                                                "Mode" = "mode"),
                                 selected = "near"),
                    conditionalPanel(
                      condition = "input.list_prods.indexOf('SCL') != -1", # add here any additional discrete product
                      selectInput("resampling_scl", label = "Resampling method for SCL",
                                   choices = list("Nearest neighbour" = "near",
                                                  "Mode" = "mode"),
                                   selected = "near")

                    )
                  ) # end of column resampling method

                ) # end of fluidrow for output geometry settings

              )) # end of fluidrow/box "Output geometry"
            ), # end of conditionalpanel on output geometry

            fluidRow(box(
              title="Output settings",
              width=12,

              fluidRow(

                column(
                  width=6,
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
                  ),

                  htmlOutput("outformat_message")
                ), # end of outformat box

                column(
                  width=6,
                  radioButtons("overwrite", label = "Overwrite existing outputs",
                               choices = list("Yes (reprocess all)" = TRUE,
                                              "No (skip processing if outputs exist)" = FALSE),
                               selected = FALSE)
                )

              ) # end of fluidRow in output settings

            )) # end of fluidrow/box "Output settings"
          ) # end of conditionalPanel on tab_prepro

        ), # end of tabItem tab_prepro


        ### Indices tab ###
        tabItem(
          tabName = "tab_index",
          title="Index selection",

          conditionalPanel(
            condition = "input.preprocess == 'TRUE'",

            fluidRow(
              box(
                width=8,
                title="Index selection",
                radioButtons("index_source", "Build indices from:",
                             choices = list("TOA Reflectance" = "TOA",
                                            "BOA Reflectance" = "BOA"),
                             selected = "BOA",
                             inline = TRUE),
                textInput("filter_indices", "Filter indices"),
                uiOutput("check_indices")
              ),

              uiOutput("show_formula")

            )
          ) # end of conditionalpanel on tab_index

        ) # end of tabItem tab_index

        # ### Path tab (set all the paths) ###
        # tabItem(
        #   tabName = "tab_path",
        #   title="Set directories",
        #
        #
        #
        #
        #   # Save buttons
        #   br(),
        #   # HTML("<script src=\"message-handler.js\"></script>"),
        #   # # head(script(src = "message-handler.js")),
        #   # p(
        #   #   shinySaveButton("export_param", "Export parameters", "Export parameters as ...", filetype=list(json="json")),
        #   #   "\u2001", # quad space
        #   #   shinyFilesButton("import_param", "Import parameters", "Import a JSON file with parameters", multiple=FALSE)
        #   # ),
        #   # p(
        #   #   actionButton("return_param", strong("Save and close GUI")),
        #   #   "\u2001", # quad space
        #   #   actionButton("exit_gui", "Close without saving")
        #   # )
        #   # TODO client buttons
        #   # fileInput("import_param_client", "Import parameters from client",
        #   #           accept = c(
        #   #             "text/json",
        #   #             ".json")
        #   # )
        #
        # ) # end of tabItem tab_steps

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

    # Update widgets for step_atmcorr basing on online_mode
    # accepted products (update together with the same variables in s2_gui())
    l1c_prods <- c("TOA")
    l2a_prods <- c("BOA","SCL","TCI")
    observe({
      if (input$download_safe %in% c("all","new")) {
        updateRadioButtons(session, "step_atmcorr",
                           choices = list("Always (download all L1C products and correct them locally)" = "scihub",
                                          "If needed (download L2A or correct from L1C basing on L2A availability)" = "auto",
                                          "Never (use only L2A products already available for download or locally)" = "l2a"),
                           selected = input$step_atmcorr)
      } else if (input$download_safe %in% c("no")) {
        updateRadioButtons(session, "step_atmcorr",
                           choices = list("Always (use L1C products already available and correct them locally)" = "scihub",
                                          "Never (use only L2A products already available locally)" = "l2a"),
                           selected = input$step_atmcorr)
      }
    })

    # Reactive list of required SAFE levels
    safe_req <- reactiveValues()
    observe({
      if (input$preprocess==TRUE) {
        safe_req$l1c <- if (any(l1c_prods %in% input$list_prods) |
                            input$index_source=="TOA") {TRUE} else {FALSE}
        safe_req$l2a <- if (any(l2a_prods %in% input$list_prods) |
                            input$index_source=="BOA") {TRUE} else {FALSE}
      } else if (input$preprocess==FALSE) {
        safe_req$l1c <- if ("l1c" %in% input$list_levels) {TRUE} else {FALSE}
        safe_req$l2a <- if ("l2a" %in% input$list_levels) {TRUE} else {FALSE}
      }

    })
    # these output values are used for conditionalPanels:
    output$req_l2a <- renderText(safe_req$l2a)
    # options to update these values also if not visible
    outputOptions(output, "req_l2a", suspendWhenHidden = FALSE)


    # Message for levels needed
    output$levels_message <- renderUI({
      div(
        strong("SAFE levels needed:"),
        br(),
        if (safe_req$l1c==FALSE & safe_req$l2a==FALSE) {
          (span(style="color:red", "Select at least one product."))
        },
        if (safe_req$l1c==TRUE) {
          a("Level-1C", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-1c", target="_blank")
          # "Level-1C"
        },
        if (safe_req$l1c==TRUE & safe_req$l2a==TRUE) {
          br()
        },
        if (safe_req$l2a==TRUE) {
          a("Level-2A", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a", target="_blank")
          # "Level-2A"
        }
      )
    })

    # update rm_safe if preprocess is not required
    observe({
      if (input$preprocess == FALSE) {
        updateRadioButtons(session, "rm_safe", "Delete unrequired level-1C SAFE tiles?",
                     choices = list("Yes" = "l1c",
                                    "No" = "no"),
                     selected = "no")
      } else {
        updateRadioButtons(session, "rm_safe", "Delete SAFE tiles after processing?",
                           choices = list("Yes" = "all",
                                          "Only unrequired L1C" = "l1c",
                                          "No" = "no"),
                           selected = "no")
      }
      if (input$download_safe == 'no' |
          input$preprocess == FALSE &
          (!"l2a" %in% input$list_levels | "l1c" %in% input$list_levels)) {
        updateRadioButtons(session, "rm_safe",
                           selected = "no")
      }
    })

    # # Temporary message to alert that S2B are not retrievable automatically until they will be operational.
    # # This will be removed.
    observeEvent(input$sel_sensor_tempmessage, {
      # if (input$download_safe!="no" & "s2b" %in% input$sel_sensor) {
        showModal(modalDialog(
          title = "Sentinel-2B temporary alert",
          em(paste0("This tool find only products from SciHub operational ",
                    "hub, while currently only few Sentinel-2B products are ",
                    "available there. If you need more products, consider to ",
                    "manually search and download products from "),
             a("PreOps SciHub", href="https://scihub.copernicus.eu/s2b", target="_blank"),
             " and then to re-launch the tool in offline mode."),
          easyClose = TRUE,
          footer = NULL
        ))
      # }
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
      # addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
               group = "OpenTopoMap") %>%
      # addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
               group = "CartoDB") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               group = "Satellite") %>%
      # addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
               group = "Light names") %>%
      # addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
               group = "Dark names") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "Metal or death") %>%

      # view and controls
      setView(lng = 11.96, lat = 44.86, zoom = 10) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("Light names","Dark names","S2 tiles","Extent"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Light names","Dark names"))

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
browser()
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
      column(
        width=4,
        lapply(indices_rv$checked, index_details)
      )
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
    shinyDirChoose(input, "path_merged_sel", roots = volumes)
    shinyDirChoose(input, "path_out_sel", roots = volumes)
    shinyDirChoose(input, "path_indices_sel", roots = volumes)

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
      path_merged_string <- parseDirPath(volumes, input$path_merged_sel)
      updateTextInput(session, "path_merged_textin", value = path_merged_string)
    })
    observe({
      path_out_string <- parseDirPath(volumes, input$path_out_sel)
      updateTextInput(session, "path_out_textin", value = path_out_string)
    })
    observe({
      path_indices_string <- parseDirPath(volumes, input$path_indices_sel)
      updateTextInput(session, "path_indices_textin", value = path_indices_string)
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
      output$path_merged_errormess <- path_check(input$path_merged_textin)
    })
    observe({
      output$path_out_errormess <- path_check(input$path_out_textin)
    })
    observe({
      output$path_indices_errormess <- path_check(input$path_indices_textin)
    })


    ## Exit and save

    # functions to check that all is correctly set TODO
    check_param <- function() {

    }

    # function to create a list to objects to be returned
    create_return_list <- function() {
      rl <- list()

      # processing steps #
      rl$preprocess <- input$preprocess # TRUE to perform preprocessing steps, FALSE to download SAFE only
      rv$s2_levels <- c(if(safe_req$l1c==TRUE){"l1c"}, if(safe_req$l2a==TRUE){"l2a"}) # required S2 levels ("l1c","l2a")
      rl$sel_sensor <- input$sel_sensor # sensors to use ("s2a", "s2b")
      rl$online <- switch(input$download_safe, all = TRUE, new = TRUE, no = FALSE) # TRUE if online mode, FALSE if offline mode
      rl$overwrite_safe <- switch(input$download_safe, all = TRUE, new = FALSE, no = FALSE) # TRUE to overwrite existing SAFE, FALSE not to
      rl$rm_safe <- switch(input$download_safe, all = input$rm_safe, new = input$rm_safe, no = "no") # "yes" to delete all SAFE, "l1c" to delete only l1c, "no" not to remove
      rl$step_atmcorr <- ifelse(safe_req$l2a==TRUE, input$step_atmcorr, "no") # download_method in sen2cor: "auto", "l2a", "scihub" or "no"
      # rl$steps_reqout <- input$steps_reqout # vector of required outputs: "safe", "tiles", "clipped" (one or more)

      # spatio-temporal selection #
      rl$timewindow <- input$timewindow # range of dates
      rl$timeperiod <- input$timeperiod # "full" or "seasonal"
      # polygons
      rl$extent <- if (!is.null(rv$extent)) {
        rv$extent %>%
          st_transform(4326) %>%
          geojson_json(pretty=TRUE)
      } else {
        geojson_json(st_polygon())
      }
      # rl$s2tiles_selected <- rv$draw_tiles_overlapping[rv$draw_tiles_overlapping$tile_id %in% input$tiles_checkbox,] # MULTIPOLYGON with the selected tiles
      rl$s2tiles_selected <- if (is.null(rl$s2tiles_selected)) {NA} else {input$tiles_checkbox} # selected tile IDs
      rl$s2orbits_selected <- str_pad(c(1:143),3,"left","0") # temporary select all orbits (TODO implement)
      rl$extent_as_mask <- input$extent_as_mask # logical: TRUE to mask outside the extent polygon, FALSE not to

      # product selection #
      rl$list_prods <- input$list_prods # TOA, BOA, SCL, TCI (for now)
      rl$list_indices <- if ("indices" %in% input$steps_reqout) {input$list_indices} else {NA} # index names
      rl$index_source <- input$index_source # reflectance band for computing indices ("BOA" or "TOA")
      rl$mask_type <- if (input$atm_mask==FALSE) {NA} else {input$atm_mask_type} # atmospheric masking (accepted types as in s2_mask())

      # output geometry #
      # path of the reference file (NULL if not provided)
      rl$reference_path <- ifelse(input$use_reference==TRUE,
                                  input$reference_file_textin,
                                  NA)
      # spatial resolution for output products (2-length numeric vector)
      rl$res <- if (input$use_reference==TRUE &
                    "res" %in% input$reference_usefor) {
        reference()$res
      } else {
        if (input$rescale == FALSE) {
          NA
        } else if (input$rescale == TRUE) {
          rep(input$resolution_custom,2)
        }
      }
      # SAFE resolution to use ("10m", "20m" or "60m")
      rl$res_s2 <- if (input$rescale == TRUE) {
        "10m" # if a rescaling is needed, start always from 10m. TODO This should be changed to improve speed
      } else if (input$rescale == FALSE) {
        input$resolution_s2
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
      # resampling methods ("nearest","bilinear","cubic","cubicspline","lanczos","average","mode")
      rl$resampling <- input$resampling
      rl$resampling_scl <- input$resampling_scl
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

      # set directories #
      rl$path_l1c <- input$path_l1c_textin # path of L1C SAFE products
      rl$path_l2a <- input$path_l2a_textin # path of L2A SAFE products
      rl$path_tiles <- if ("tiles" %in% input$steps_reqout) {input$path_tiles_textin} else {NA} # path of entire tiled products
      rl$path_merged <- if ("merged" %in% input$steps_reqout) {input$path_merged_textin} else {NA} # path of entire tiled products
      rl$path_out <- if ("out" %in% input$steps_reqout) {input$path_out_textin} else {NA} # path of output pre-processed products
      rl$path_indices <- if ("indices" %in% input$steps_reqout) {input$path_indices_textin} else {NA} # path of spectral indices
      rl$path_subdirs <- input$path_subdirs # logical (use subdirs)

      return(rl)
    }

    # function to import saved parameters
    import_param_list <- function(pl) {

      # processing steps
      updateCheckboxGroupInput(session, "sel_sensor", selected = pl$sel_sensor)
      updateRadioButtons(session, "download_safe",
                         selected = if (pl$online==TRUE & pl$overwrite_safe==TRUE) {
                           "all"
                         } else if (pl$online==TRUE & pl$overwrite_safe==FALSE) {
                           "new"
                         } else if (pl$online==FALSE) {
                           "no"
                         })
      updateCheckboxGroupInput(session, "list_levels", selected = pl$s2_levels)
      updateRadioButtons(session, "rm_safe", selected = pl$rm_safe)
      updateRadioButtons(session, "step_atmcorr", selected = pl$step_atmcorr)
      updateCheckboxGroupInput(session, "steps_reqout",
                               selected = c(if(!is.na(input$path_tiles_textin)){"tiles"},
                                            if(!is.na(input$path_merged_textin)){"merged"},
                                            if(!is.na(input$path_out_textin)){"out"},
                                            if(!is.na(input$path_indices_textin)){"indices"}))

      # spatio-temporal selection
      updateDateRangeInput(session, "timewindow", start=pl$timewindow[1], end=pl$timewindow[2])
      updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
      updateRadioButtons(session, "extent_type",
                         choices = list("Bounding box coordinates" = "bbox",
                                        "Upload vector file" = "vectfile",
                                        "Draw on the map" = "draw",
                                        "Imported from JSON" = "imported"),
                         selected = "imported")
      isolate(rv$extent <- if (pl$extent==geojson_json(st_polygon())) {st_polygon()} else {st_read(pl$extent)})
      updateCheckboxGroupInput(session, "tiles_checkbox",
                               selected = pl$s2tiles_selected)
      # rl$s2tiles_selected <- input$tiles_checkbox # selected tile IDs
      updateRadioButtons(session, "extent_as_mask", selected = rv$extent_as_mask)

      # product selection
      updateCheckboxGroupInput(session, "list_prods", selected = pl$list_prods)
      indices_rv$checked <- pl$list_indices
      # updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices) # FIXME 1 not working since it is reactive
      updateRadioButtons(session, "atm_mask",
                         selected = ifelse(is.na(pl$mask_type),FALSE,TRUE))
      updateRadioButtons(session, "atm_mask_type",
                         selected = ifelse(is.na(pl$mask_type),"cloud_medium_proba",pl$mask_type))
      updateRadioButtons(session, "index_source", selected = pl$index_source)


      # set directories
      updateTextInput(session, "path_l1c_textin", value = pl$path_l1c)
      updateTextInput(session, "path_l2a_textin", value = pl$path_l2a)
      updateTextInput(session, "path_tiles_textin", value = pl$path_tiles)
      updateTextInput(session, "path_merged_textin", value = pl$path_merged)
      updateTextInput(session, "path_out_textin", value = pl$path_out)
      updateTextInput(session, "path_indices_textin", value = pl$path_indices)
      updateRadioButtons(session, "path_subdirs", selected = pl$path_subdirs)

      # output geometry
      updateTextInput(session, "reference_file_textin", value = pl$reference_path)
      updateRadioButtons(session, "use_reference", selected = ifelse(is.na(pl$reference_path), FALSE, TRUE))

      if (is.na(pl$reference_path)) {
        updateRadioButtons(session, "rescale", selected = if(any(is.na(pl$res))) {FALSE} else {TRUE})
        updateTextInput(session, "resolution_custom", value = pl$res[1])
        updateRadioButtons(session, "resolution_s2", selected = pl$res_s2)
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
      updateRadioButtons(session, "resampling_scl", selected = pl$resampling_scl)
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




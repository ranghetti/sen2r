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
#' @import data.table
#' @importFrom geojsonio geojson_json
#' @importFrom leaflet addLayersControl addPolygons addProviderTiles
#'  addTiles clearShapes fitBounds hideGroup labelOptions layersControlOptions
#'  leaflet leafletProxy
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#'  removeDrawToolbar
#' @importFrom mapedit editModUI
#' @importFrom reticulate import
#' @importFrom sf st_coordinates st_intersects st_polygon st_read st_sf st_sfc st_transform
#' @importFrom shiny a actionButton actionLink br callModule checkboxGroupInput
#'  checkboxInput column conditionalPanel dateRangeInput div em fluidRow h2 h3
#'  helpText hr HTML htmlOutput icon isolate NS numericInput observe p
#'  radioButtons reactive reactiveValues renderText renderUI runApp selectInput
#'  shinyApp showModal span stopApp strong textInput uiOutput updateCheckboxGroupInput
#'  updateDateRangeInput updateRadioButtons updateTextInput withMathJax
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath
#'  shinyDirButton shinyDirChoose shinyFileChoose shinyFileSave
#'  shinyFilesButton shinySaveButton
#' @importFrom shinyjs enable disable
#' @importFrom sprawl check_proj4string get_rastype get_vectype
#' @importFrom stats setNames
#' @importFrom utils unzip
#'
#' @export

s2_gui <- function(param_list = NULL,
                   thunderforest_api = NA) {
  .s2_gui(
    param_list = param_list,
    par_fun = "parent",
    thunderforest_api = thunderforest_api
  )
}

# Internal function with parameter par_fun (parent function):
# default is "parent", but it can be set with another value
# for internal purposes.
# For now, value "fidolasen_s2" is used when s2_gui() is launched from
# fidolasen_s2(), and is used to change save button label.
.s2_gui <- function(param_list = NULL,
                    par_fun = "parent",
                    thunderforest_api = NA) {
  
  # import python modules
  gdal <- import("osgeo",convert=FALSE)$gdal
  osr <- import("osgeo",convert=FALSE)$osr
  
  # TODO: populate parameter values with param_list content, if provided
  
  # create tempdir
  dir.create(tempdir(), showWarnings=FALSE)
  
  # extract and import tiles kml
  s2tiles_kmz <- system.file("extdata","vector","s2_tiles.kmz",package="fidolasen")
  s2tiles_kml <- gsub("\\.kmz$",".kml",s2tiles_kmz)
  if (!file.exists(s2tiles_kml)) {
    unzip(zipfile = s2tiles_kmz,
          files   = basename(s2tiles_kml),
          exdir   = dirname(s2tiles_kml),
          unzip   = "internal")
  }
  s2tiles <- st_read(s2tiles_kml, stringsAsFactors=FALSE, quiet=TRUE)
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
        menuItem("Spatio-temporal selection", tabName = "tab_query", icon = icon("clone"))
      ),
      conditionalPanel(
        condition = "input.preprocess == 'TRUE'",
        sidebarMenu(
          menuItem("Processing options", tabName = "tab_prepro", icon = icon("th"))
        ),
        conditionalPanel(
          condition = "input.list_prods.indexOf('indices') != -1",
          sidebarMenu(
            menuItem("Spectral indices selection", tabName = "tab_index", icon = icon("calculator"))
          )
        )
      ),
      # sidebarMenu(
      #   menuItem("Set directories", tabName = "tab_path", icon = icon("folder-open"))
      # ),
      
      HTML("<script src=\"message-handler.js\"></script>"),
      shinyjs::useShinyjs(),
      shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color
      div(
        style="position:absolute;top:250px;",
        p(style="margin-top:15pt;margin-left:11pt;",
          shinySaveButton("export_param", "Save options as...", "Save parameters as ...", filetype=list(json="json"), class="darkbutton")
        ),
        # # FIXME "Load options" was deactivated for unsolved problems
        # # with extent map.
        # p(style="margin-top:5pt;margin-left:11pt;",
        #   shinyFilesButton("import_param", "Load options", "Import a JSON file with parameters", multiple=FALSE, class="darkbutton")
        # ),
        actionButton("import_param_deactivated", label = "Load options", class = "darkbutton"),
        p(style="margin-top:20pt;",
          actionButton(
            "return_param",
            label = if (par_fun=="fidolasen_s2") {
              strong("\u2000Launch processing")
            } else {
              strong("\u2000Save and close GUI")
            },
            icon = icon("check"),
            class = "darkbutton"
          )
        ),
        p(style="margin-top:0pt;",
          actionButton(
            "exit_gui",
            label = if (par_fun=="fidolasen_s2") {
              "\u2000Close without processing"
            } else {
              "\u2000Close without saving"
            },
            icon = icon("ban"),
            class = "darkbutton"
          )
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
            title="Type of processing",
            width=12,
            radioButtons(
              "preprocess", NULL,
              choiceNames = list(
                "Processed spatial files (surface reflectance, spectral indices, ...) in custom format",
                span(
                  "Raw files in ",
                  a("raw SAFE format",
                    href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/data-formats",
                    target="_blank"),
                  " (downloaded and/or corrected with sen2cor)"
                )
              ),
              choiceValues = list(TRUE, FALSE),
              selected=TRUE,
              inline = FALSE
            )
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
                                                        "TCI (true-color) RGB 8-bit image",
                                                        "Spectral indices"),
                                     choiceValues = list("TOA", "BOA", "SCL", "TCI", "indices"),
                                     selected = c("BOA"))#,
                ),
                conditionalPanel(
                  condition = "input.preprocess == 'FALSE'",
                  checkboxGroupInput("list_levels",
                                     "Select products:",
                                     choiceNames = list(span("Raw", a("level-1C", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-1c", target="_blank"), "SAFE files"),
                                                        span("Raw", a("level-2A", href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a", target="_blank"), "SAFE files")),
                                     choiceValues = list("l1c", "l2a"),
                                     selected = c("l2a"))
                )
              ),
              
              column(
                width=4,
                
                conditionalPanel(
                  condition = "input.preprocess == 'TRUE'",
                  uiOutput("levels_message"),
                  br()
                ),
                
                checkboxGroupInput(
                  "sel_sensor", "Select sensors:",
                  choiceNames = list("Sentinel-2A", "Sentinel-2B"),
                  choiceValues = list("s2a", "s2b"),
                  selected = c("s2a","s2b"),
                  inline = FALSE
                )
              ) # end of column for sensor selection
              
            )
          )),
          
          fluidRow(box(
            title="SAFE options",
            width=12,
            
            fluidRow(
              # L1C directory
              conditionalPanel(
                condition = "output.req_l1c == 'TRUE'",
                column(
                  width=6,
                  div(style="display:inline-block;vertical-align:top;",
                      strong("Directory for level-1C SAFE products: \u00a0")),
                  div(style="display:inline-block;vertical-align:top;",
                      htmlOutput("path_l1c_errormess")),
                  div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                          shinyDirButton("path_l1c_sel", "Select", "Specify directory for level-1C SAFE products")),
                      div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                          textInput("path_l1c_textin", NULL, "")))
                )
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
                          textInput("path_l2a_textin", NULL, "")))
                )
              )
            ), # end of fluidrow for safe directories
            
            
            fluidRow(
              column(
                width=6,
                
                # online_mode (online/offline mode)
                radioButtons(
                  "online",
                  label = span(
                    "Download mode\u2000",
                    actionLink("help_online", icon("question-circle"))
                  ),
                  choiceNames = list("Online", "Offline"),
                  choiceValues = list(TRUE, FALSE),
                  selected = TRUE,
                  inline = TRUE
                ),
                
                # SciHub credentials
                conditionalPanel(
                  condition = "input.online == 'TRUE'",
                  actionButton(
                    "scihub",
                    label = "\u2000Login in SciHub",
                    icon=icon("user-circle")
                  )
                )

              ),
              column(
                width=6,
                
                # overwrite SAFE
                radioButtons(
                  "overwrite_safe",
                  label = span(
                    "Overwrite existing SAFE products?\u2000",
                    actionLink("help_overwrite_safe", icon("question-circle"))
                  ),
                  choiceNames = list("Yes", "No"),
                  choiceValues = list(TRUE, FALSE),
                  selected = TRUE,
                  inline = TRUE
                ),
                
                # delete_safe
                radioButtons("rm_safe", "Delete raw SAFE files after processing?",
                             choices = list("Yes" = "all",
                                            "Only level-1C" = "l1c",
                                            "No" = "no"),
                             selected = "no",
                             inline = TRUE)
                
              )
            ) # end of fluidRow download / delete SAFE
            
          )), # end of fluidRow/box "SAFE options"
          
          conditionalPanel(
            condition = "output.req_l2a == 'TRUE'",
            fluidRow(box(
              title="Atmospheric correction options",
              width=12,
              # step_atmcorr (perform or not sen2cor and how)
              # uiOutput("step_atmcorr")
              radioButtons(
                "step_atmcorr",
                label = span(
                  "Method to obtain level-2A corrected images\u2000",
                  actionLink("help_step_atmcorr", icon("question-circle"))
                ),
                choiceNames = list(
                  "Use only level-2A images available locally or online",
                  "Use sen2cor only for level-2A products not available locally or online",
                  "Always correct level-1C images with sen2cor locally"
                ),
                choiceValues = list("l2a","auto", "scihub"),
                selected = "auto")
            )) # end of fluidRow/box "sen2cor options"
          )
          
        ), # end of tabItem tab_steps
        
        
        ### Querying parameters (spatio-temporal) ###
        tabItem(
          tabName = "tab_query",
          
          fluidRow(box(
            title="Temporal parameters",
            width=12,
            
            conditionalPanel(
              condition = "input.online == 'FALSE'",
              radioButtons("query_time", label = "Use temporal filter?",
                           choices = list("Yes" = TRUE,
                                          "No (process all the input SAFE images)" = FALSE),
                           selected = TRUE,
                           inline = TRUE)
            ),
            
            conditionalPanel(
              condition = "input.query_time == 'TRUE'",
              fluidRow(
                
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
                
              )
            ) # end of conditionalPanel on temporal query
            
          )), # end of fluidRow/box "Temporal parameters"
          
          fluidRow(box(
            title="Spatial extent",
            width=12,
            
            conditionalPanel(
              condition = "input.online == 'FALSE'",
              radioButtons("query_space", label = "Use spatial filter/clip?",
                           choices = list("Yes" = TRUE,
                                          "No (process all the input SAFE images)" = FALSE),
                           inline = TRUE)
            ),
            
            conditionalPanel(
              condition = "input.query_space == 'TRUE'",
              fluidRow(
                
                column(
                  width=4,
                  radioButtons(
                    "extent_type", 
                    label = span(
                      "Method to select extent\u2000",
                      actionLink("alert_leaflet", icon("warning"), style="color:red")
                    ),
                    choices = list(
                      "Draw on the map" = "draw",
                      "Upload vector file" = "vectfile",
                      "Enter coordinates" = "bbox"
                    ),
                    selected = "draw")
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
                    radioButtons(
                      "dissolve_extent",
                      label = span(
                        "Consider multiple polygons as:\u2000",
                        actionLink("help_dissolve_extent", icon("question-circle"))
                      ),
                      choiceNames = list(
                        "A multipart geometry (create a single merged output)",
                        "Single part geometries (create separate outputs)"
                      ),
                      choiceValues = list(TRUE, FALSE),
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
                )
                
              ), # end of 1st fluidRow after conditionalPanel on spatial filter
              
              fluidRow(
                
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
                )
              ) # end of 1st fluidRow after conditionalPanel on spatial filter
              
            ) # end of conditionalPanel on spatial query
            
          )) # end of fluidRow/box "Spatial extent"
          
        ), # end of tabItem tab_query
        
        
        ### Output tab (geometry and parameters) ###
        tabItem(
          tabName = "tab_prepro",
          title="Processing options",
          
          fluidRow(box(
            title="Output files",
            width=12,
            
            fluidRow(
              column(
                width=12,
                conditionalPanel(
                  condition = "input.list_prods.length > 1 || (input.list_prods.length > 0 && input.list_prods.indexOf('indices') == -1)",
                  div(div(style="display:inline-block;vertical-align:top;",
                          strong("Directory for output processed products: \u00a0")),
                      div(style="display:inline-block;vertical-align:top;",
                          htmlOutput("path_out_errormess")),
                      div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                              shinyDirButton("path_out_sel", "Select", "Specify directory for output processed products")),
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                              textInput("path_out_textin", NULL, ""))))
                ),
                div(
                  style="margin-top: -15px",
                  checkboxInput(
                    "path_subdirs",
                    label = span(
                      "Group products in subdirectories\u2000",
                      actionLink("help_path_subdirs", icon("question-circle"))
                    ),
                    value = TRUE
                  )
                )
              )
            ),
            
            fluidRow(
              
              column(
                width=5,
                radioButtons("overwrite", label = "Overwrite existing outputs",
                             choices = list("Yes (reprocess all)" = TRUE,
                                            "No (skip processing if outputs exist)" = FALSE),
                             selected = FALSE)
              ),
              
              column(
                width=4,
                selectInput("outformat", label = "Output file format",
                            choices = list("GeoTiff" = "GTiff",
                                           "ENVI" = "ENVI"),
                            # TODO add others common formats
                            selected = "GTiff")
              ),
              
              conditionalPanel(
                condition = "input.outformat == 'GTiff'",
                column(
                  width=3,
                  selectInput("compression", label = "Output compression",
                              choices = list("Uncompressed" = "NONE",
                                             "Low (packbits)" = "PACKBITS",
                                             "Medium (lzw)" = "LZW",
                                             "High (deflate)" = "DEFLATE"),
                              selected = "LZW")
                )
              )
              
              # htmlOutput("outformat_message") # disabled
              
            ) # end of fluidRow in output settings
            
          )), # end of fluidrow/box "Output files"
          
          
          fluidRow(box(
            width=6,
            title = "Ouptut extent",
            
            fluidRow(
              column(
                width=12,
                radioButtons(
                  "clip_on_extent",
                  label = span(
                    "Clip outputs on the selected extent?\u2000",
                    actionLink("help_clip_on_extent", icon("question-circle"))
                  ),
                  choiceNames = list("Yes", "No"),
                  choiceValues = list(TRUE, FALSE),
                  selected = TRUE,
                  inline = TRUE
                )
              ),
              column(
                width=12,
                radioButtons(
                  "extent_as_mask",
                  label = "Mask data outside the selected polygons?",
                  choices = list("Yes" = TRUE, # TODO set nodata
                                 "No" = FALSE),
                  selected = FALSE,
                  inline = TRUE)
              )
            ),
            
            hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
            
            fluidRow(
              column(
                width=6,
                radioButtons("keep_tiles", "Save single tiles?",
                             choices = list("Yes" = TRUE,
                                            "No" = FALSE),
                             selected = FALSE,
                             inline = TRUE)
              ),
              conditionalPanel(
                condition = "input.clip_on_extent == 'TRUE'",
                column(
                  width=6,
                  radioButtons("keep_merged", "Save merged tiles?",
                               choices = list("Yes" = TRUE,
                                              "No" = FALSE),
                               selected = FALSE,
                               inline = TRUE))
              )
            ), # end of fluidRow keep_tiles
            
            fluidRow(
              column(
                width=12,
                conditionalPanel(
                  condition = "input.keep_tiles == 'TRUE'",
                  div(div(style="display:inline-block;vertical-align:top;",
                          strong("Output directory for single tiles: \u00a0")),
                      div(style="display:inline-block;vertical-align:top;",
                          htmlOutput("path_tiles_errormess")),
                      div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                              shinyDirButton("path_tiles_sel", "Select", "Specify directory for single tiles in custom format")),
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                              textInput("path_tiles_textin", NULL, ""))))
                )
              ),
              column(
                width=12,
                conditionalPanel(
                  condition = "input.clip_on_extent == 'TRUE' && input.keep_merged == 'TRUE'",
                  div(div(style="display:inline-block;vertical-align:top;",
                          strong("Output directory for merged tiles: \u00a0")),
                      div(style="display:inline-block;vertical-align:top;",
                          htmlOutput("path_merged_errormess")),
                      div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                              shinyDirButton("path_merged_sel", "Select", "Specify directory for tiles spatially merged")),
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                              textInput("path_merged_textin", NULL, ""))))
                )
              )
            ) # end of fluidRow keep_tiles
            
          ), # end of box "Output extent"
          
          box(
            title="Atmospheric mask",
            width=6,
            
            radioButtons("atm_mask", "Mask cloud-covered pixels?",
                         choices = list("Yes" = TRUE,
                                        "No" = FALSE),
                         selected = FALSE,
                         inline = TRUE),
            
            conditionalPanel(
              condition = "output.req_l2a_onlytomask == 'TRUE'",
              span(style="color:grey",
                   p(stype="margin-bottom:15pt",
                     "SCL is required to mask products, so Level-2A SAFE ",
                     "are also required. Return to \"Product selection\" menu ",
                     "to check sen2cor settings."))
            ),
            
            conditionalPanel(
              condition = "input.atm_mask == 'TRUE'",
              selectInput("atm_mask_type", "Apply mask to:",
                          choices = list("No data" = "nodata",
                                         "No data or clouds (high probability)" = "cloud_high_proba",
                                         "No data or clouds (high or medium probability)" = "cloud_medium_proba",
                                         "No data or clouds (any probability)" = "cloud_low_proba",
                                         "No data, clouds or cloud shadows" = "cloud_and_shadow",
                                         "No data, clouds, cloud shadows or thin cirrus" = "cloud_shadow_cirrus"),
                          selected = "cloud_medium_proba")
            )
            
          )), # end of fluidRow/box "Atmospheric mask"
          
          conditionalPanel(
            condition = "input.clip_on_extent == 'TRUE'",
            fluidRow(box(
              width=12,
              title = "Output geometry",
              
              fluidRow( # fluidrow for spatial reference
                
                column(
                  width=8,
                  radioButtons("use_reference", label = "Use an existing raster to define the output grid?",
                               choices = list("Yes" = TRUE,
                                              "No" = FALSE),
                               selected = FALSE,
                               inline = TRUE),
                  conditionalPanel(
                    condition = "input.use_reference == 'TRUE'",
                    div(
                      div(
                        style="display:inline-block;vertical-align:top;",
                        shinyFilesButton("reference_file_button", "Select raster", "Select reference file", multiple=FALSE),
                        "\u2001"),
                      div(
                        style="display:inline-block;vertical-align:top;width:calc(100% - 88pt);",
                        textInput("reference_file_textin", label = NULL, "Enter file path...", width="100%"))),
                    uiOutput("reference_file_message")
                    
                  )
                ), # end of column reference
                
                conditionalPanel(
                  condition = "input.use_reference == 'TRUE'",
                  column(
                    width=4,
                    checkboxGroupInput("reference_usefor", label = "Use the file to define:",
                                       choices = list("Projection" = "proj",
                                                      "Resolution" = "res"),
                                       #"Extent" = "ext", # TODO
                                       # "Output format" = "outformat"),
                                       selected = c("proj","res"))
                  ) # end of column use reference for
                ) # end of its conditional panel
                
              ), # end of fluidrow for spatial reference
              
              hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
              
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
          ) # end of conditionalpanel on output geometry
          
        ), # end of tabItem tab_prepro
        
        
        ### Indices tab ###
        tabItem(
          tabName = "tab_index",
          title="Spectral indices",
          
          conditionalPanel(
            condition = "input.preprocess == 'TRUE'",
            
            fluidRow(
              box(
                width=8,
                title="Spectral indices selection",
                
                div(div(style="display:inline-block;vertical-align:top;",
                        strong("Directory for spectral indices: \u00a0")),
                    div(style="display:inline-block;vertical-align:top;",
                        htmlOutput("path_indices_errormess")),
                    div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                            shinyDirButton("path_indices_sel", "Select", "Specify directory for spectral indices")),
                        div(style="display:inline-block;vertical-align:top;width:calc(100% - 55pt);",
                            textInput("path_indices_textin", NULL, ""))),
                    div(style="display:inline-block;vertical-align:top;",
                        actionButton("path_indices_cp", "Copy from directory for output processed products"))),
                
                br(),
                
                fluidRow(
                  column(
                    width = 5,
                    radioButtons(
                      "index_source",
                      label = span(
                        "Build indices from:\u2000",
                        actionLink("help_index_source", icon("question-circle"))
                      ),
                      choices = list("BOA" = "BOA",
                                     "TOA" = "TOA"),
                      selected = "BOA",
                      inline = TRUE)
                  ),
                  column(
                    width = 7,
                    checkboxInput(
                      "verified_indices",
                      label = span(
                        "Show only verified indices\u2000",
                        actionLink("note_list_indices", icon("warning"))
                      ),
                      value = TRUE
                    )
                  )
                ),
                
                hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
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
    # observe({
    #   if (input$online == TRUE & "atmcorr" %in% input$proc_steps) {
    #     updateRadioButtons(session, "step_atmcorr",
    #                        choices = list("Always (download all L1C products and correct them locally)" = "scihub",
    #                                       "If needed (download L2A or correct from L1C basing on L2A availability)" = "auto",
    #                                       "Never (use only L2A products already available for download or locally)" = "l2a"),
    #                        selected = input$step_atmcorr)
    #   } else if (input$online == FALSE & "atmcorr" %in% input$proc_steps) {
    #     updateRadioButtons(session, "step_atmcorr",
    #                        choices = list("Always (use L1C products already available and correct them locally)" = "scihub",
    #                                       "Never (use only L2A products already available locally)" = "l2a"),
    #                        selected = input$step_atmcorr)
    #   }
    # })
    
    # Reactive list of required SAFE levels
    safe_req <- reactiveValues()
    observe({
      if (input$preprocess==TRUE) {
        safe_req$l1c <- if (any(l1c_prods %in% input$list_prods) |
                            indices_req()==TRUE & input$index_source=="TOA" |
                            input$step_atmcorr %in% c("auto","scihub")) {TRUE} else {FALSE}
        safe_req$l2a <- if (any(l2a_prods %in% input$list_prods) |
                            input$atm_mask == TRUE |
                            indices_req()==TRUE & input$index_source=="BOA") {TRUE} else {FALSE}
        safe_req$l2a_onlytomask <- if (!any(l2a_prods %in% input$list_prods) &
                                       input$atm_mask == TRUE &
                                       (indices_req()==FALSE |
                                        indices_req()==TRUE & input$index_source=="BOA")) {TRUE} else {FALSE}
      } else {
        safe_req$l1c <- if ("l1c" %in% input$list_levels) {TRUE} else {FALSE}
        safe_req$l2a <- if ("l2a" %in% input$list_levels) {TRUE} else {FALSE}
        safe_req$l2a_onlytomask <- FALSE
      }
    })
    # these output values are used for conditionalPanels:
    output$req_l1c <- renderText(safe_req$l1c)
    output$req_l2a <- renderText(safe_req$l2a)
    output$req_l2a_onlytomask <- renderText(safe_req$l2a_onlytomask)
    # options to update these values also if not visible
    outputOptions(output, "req_l1c", suspendWhenHidden = FALSE)
    outputOptions(output, "req_l2a", suspendWhenHidden = FALSE)
    outputOptions(output, "req_l2a_onlytomask", suspendWhenHidden = FALSE)
    
    
    # Message for levels needed
    output$levels_message <- renderUI({
      div(
        strong("SAFE levels needed:"),
        br(),
        if (safe_req$l1c==FALSE & safe_req$l2a==FALSE) {
          (span(style="color:red", "Select at least one product or index."))
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
    
    # disable rm_safe if offline mode
    observe({
      if (input$online == FALSE) {
        disable("rm_safe")
        updateRadioButtons(session, "rm_safe",
                           selected = "no")
      } else {
        enable("rm_safe")
      }
    })
    
    # Disable overwrite_safe in some conditions
    observe({
      # toggleState("overwrite_safe",
      #             condition = {input$step_atmcorr %in% c("auto","scihub") | input$online==TRUE})
      if (input$step_atmcorr %in% c("auto","scihub") | input$online==TRUE) {
        enable("overwrite_safe")
      } else {
        disable("overwrite_safe")
        updateRadioButtons(session, "overwrite_safe",
                           selected = FALSE)
      }
    })
    
    # Edit scihub credentials
    observeEvent(input$scihub, {
      showModal(scihub_modal())
    })
    
    # save user/password
    observeEvent(input$save_apihub, {
      write_scihub_login(input$scihub_username, input$scihub_password)
      removeModal()
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
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("Light names","Dark names","S2 tiles","Extent"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Light names","Dark names"))
    
    # initial zoom
    base_map <- if (!is.null(isolate(rv$extent))) {
      fitBounds(
        base_map,
        lng1 = min(st_coordinates(rv$extent)[,"X"]),
        lat1 = min(st_coordinates(rv$extent)[,"Y"]),
        lng2 = max(st_coordinates(rv$extent)[,"X"]),
        lat2 = max(st_coordinates(rv$extent)[,"Y"])
      )
    } else {
      fitBounds(base_map, lng1 = 3, lat1 = 44, lng2 = 15, lat2 = 50)
    }
    
    
    # (http://r-spatial.org/r/2017/06/09/mapedit_0-2-0.html)
    extent_edits <- callModule(editModPoly, "extent_editor", base_map)
    base_map <- removeDrawToolbar(base_map, clearFeatures = FALSE)
    # observe({print(extent_edits())})
    
    # Select vector (bbox, file or drawn) to be used
    observe({
      # rv$extent <- NULL
      rv$extent <- switch(input$extent_type,
                          bbox = rv$bbox,
                          draw = extent_edits()$finished,
                          vectfile = if (!is.na(rv$vectfile_path)) {
                            st_read(rv$vectfile_path) %>% st_transform(4326)
                          } else {
                            st_polygon()
                          },
                          imported = if (!is.null(imported_param())) {
                            if (any(!is.na(imported_param()$extent))) {
                              st_read(imported_param()$extent)
                            } else {
                              st_polygon()
                            }
                          } else if (!is.null(param_list)) {
                            if (any(!is.na(param_list$extent))) {
                              st_read(param_list$extent)
                            } else {
                              st_polygon()
                            }
                          } else {
                            st_polygon()
                          })
    })
    
    
    # Actions when the polygon layer changes
    observe({
      if(length(rv$extent) > 0) {
        rv$draw_tiles_overlapping <- s2tiles[unique(unlist(st_intersects(st_transform(rv$extent,4326), s2tiles))),]
        # if the user is loading saved parameters, read the selected tiles ID;
        # for any otyher change, for now set all the tiles as selected
        rv$draw_tiles_selected <- if (!is.null(imported_param())) {
          if (any(!is.na(imported_param()$s2tiles_selected))) {
            s2tiles[match(imported_param()$s2tiles_selected,s2tiles$tile_id),]
          } else {
            rv$draw_tiles_overlapping
          }
        } else {
          rv$draw_tiles_overlapping
        }
        
        updateCheckboxGroupInput(
          session, "tiles_checkbox",
          choices = setNames(
            as.list(rv$draw_tiles_overlapping$tile_id),
            rv$draw_tiles_overlapping$tile_id),
          selected = rv$draw_tiles_selected$tile_id
        )
        leafletProxy(extent_ns("map")) %>%
          clearShapes() %>%
          fitBounds(
            lng1 = min(st_coordinates(rv$extent)[,"X"]),
            lat1 = min(st_coordinates(rv$extent)[,"Y"]),
            lng2 = max(st_coordinates(rv$extent)[,"X"]),
            lat2 = max(st_coordinates(rv$extent)[,"Y"])
          ) %>%
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
                      fillColor = "green",
                      fillOpacity = .3,
                      stroke = TRUE,
                      weight = 3,
                      color = "darkgreen") #%>%
      } else {
        rv$draw_tiles_overlapping <- NULL
        updateCheckboxGroupInput(session, "tiles_checkbox",
                                 choices = NULL)
        leafletProxy(extent_ns("map")) %>%
          clearShapes()
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
            circleOptions = FALSE,
            editOptions = editToolbarOptions())
      }
    })
    
    
    # disable dissolve_extent (not yet implemented, TODO)
    disable("dissolve_extent")
    
    
    ## end of extent module ##
    
    
    
    ## Product module ##
    
    # Reactive variable: TRUE if indices are required, FALSE if not
    indices_req <- reactive({
      # "indices" %in% input$steps_reqout &
      !is.null(input$list_indices)
    })
    # convert in output value to be used in conditionalPanel
    output$indices_req <- renderText(indices_req())
    # options to update these values also if not visible
    outputOptions(output, "indices_req", suspendWhenHidden = FALSE)
    
    create_indices_db()
    indices_db <- data.table(list_indices(c("n_index","name","longname","s2_formula_mathml","link","checked")))
    check_mark <- icon("check") %>%
      span(style="color:darkgreen;", .) %>%
      as.character() %>%
      gsub("\n *","",.)
    indices_db[,extendedname := paste0(
      name,
      " (",longname,")  ",
      ifelse(checked, check_mark, "")
    )]
    setkey(indices_db, "name")
    
    indices_rv <- reactiveValues()
    observe({
      indices_db_verified_idx <- if (input$verified_indices==TRUE) {
        indices_db$checked
      } else {
        rep(TRUE, nrow(indices_db))
      }
      indices_rv$matches <- indices_db[
        indices_db_verified_idx &
          grepl(tolower(input$filter_indices),
                tolower(indices_db$extendedname)),
        name
        ]
      indices_rv$filtered <- indices_db[unique(c(indices_rv$checked,indices_rv$matches)),
                                        list(name,extendedname)]
    })
    observe({
      indices_rv$checked <- sort(input$list_indices)
    })
    
    output$check_indices <- renderUI({
      checkboxGroupInput(
        "list_indices",
        label = "Indices to be exported",
        choiceNames = lapply(indices_rv$filtered$extendedname, HTML),
        # choiceNames = lapply(indices_rv$filtered$name, function(x){span(x,icon("info-circle"))}),
        choiceValues = as.list(indices_rv$filtered$name),
        selected = indices_rv$checked
      )
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
    
    # ## Update output format from reference file
    # (disabled: not yet possible to do it)
    # output$outformat_message <- renderUI({
    #   if(input$use_reference==TRUE & "outformat" %in% input$reference_usefor) {
    #     if (!is.null(reference())) {
    #       span(style="color:darkgreen",
    #            reference()$outformat)
    #     } else {
    #       span(style="color:grey",
    #            "Specify a valid raster file.")
    #     }
    #   } else {
    #     ""
    #   }
    # })
    
    
    
    
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
    
    # action button to copy indices_path from out_path
    observeEvent(input$path_indices_cp, {
      updateTextInput(session, "path_indices_textin", value = input$path_out_textin)
    })
    
    ## Question messages
    
    observeEvent(input$help_online, {
      showModal(modalDialog(
        title = "Download mode",
        p(HTML(
          "Selecting <strong>Online</strong> mode, the user must specify",
          "a spatial extent and a temporal window (in \"Spatio-temporal",
          "selection\" tab), and the list of required products is searched",
          "online (an internet connection is required);",
          "missing SAFE products are then downloaded."
        )),
        p(HTML(
          "In <strong>Offline</strong> mode, only already available SAFE",
          "products are used (level-2A images can be produced locally",
          "with sen2cor if the corresponding level-1C images are available);",
          "the user can still filter them spatially and temporally,",
          "but this is not mandatory (if no parameters are specified,",
          "all the SAFE images are processed).")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_overwrite_safe, {
      showModal(modalDialog(
        title = "Overwrite existing SAFE products?",
        p(HTML(
          "<strong>Yes</strong>:",
          "re-download all images matching the parameters set in",
          "\"Spatio-temporal selection\" tab and re-apply sen2cor if needed."
        )),
        p(HTML(
          "<strong>No</strong>:",
          "skip download and/or atmospheric correction for existing images."
        )),
        em("This option is not available if nor download neither atmospheric",
           "correction are required."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_step_atmcorr, {
      showModal(modalDialog(
        title = "Method to obtain level-2A corrected images",
        p(HTML(
          "<strong>Use only level-2A images available locally or online</strong>:",
          "sen2cor is never used locally: level-2A products are used only",
          "if they are already available locally or if they can be downloaded",
          "from SciHub.",
          "This option is useful if sen2cor is not available locally,",
          "or if its use must be avoided at all."
        )),
        p(HTML(
          "<strong>Use sen2cor only for level-2A products not available",
          "locally or online</strong>:",
          "level-2A images are first of all searched locally or online;",
          "only for not available products (with corresponding level-1C images",
          "available) sen2cor is used to produce them.",
          "This option is useful to optimize time of processing",
          "(downloading of level-2A images is faster than producing them","
          with sen2cor), and in most of the situations."
        )),
        p(HTML(
          "<strong>Always correct level-1C images with sen2cor locally</strong>:",
          "If level-2A images are not available locally, they are corrected",
          "applying sen2cor to their corresponding level-1C images.",
          "This option can be used to reduce internet traffic if a level-1C",
          "archive is already available, or if both level-1C and level-2A",
          "products are required for outputs."
        )),
        easyClose = TRUE,
        footer = NULL
        ))
    })
    
    observeEvent(input$help_dissolve_extent, {
      showModal(modalDialog(
        title = "Consider multiple polygons as:",
        p(HTML(
          "<strong>A multipart geometry</strong>:",
          "all the polygons drawn (or the entire vector file which was",
          "uploaded) are considered as a single multipart",
          "polygon, so the output will cover the extent of all the polygons."
        )),
        p(HTML(
          "<strong>Single part geometries</strong>:",
          "the number of outputs will be equal to the number of polygons",
          "drawn (or to the number of polygons inside the uploaded vector",
          "file).",
          "Subdirectories are created for each output product: the name is",
          "a consecutive integer number, or an attribute name for uploaded",
          "vector files."
        )),
        em(
          "This option was not yet implemented: for now, only a multipart",
          "geometry can be used."
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_path_subdirs, {
      showModal(modalDialog(
        title = "Group products in subdirectories",
        p("If checked, output products chosen in the first tab will be put",
          "in separate subdirectories (e.g., if \"TOA\" and \"BOA\" were","
          chosen, a subdirectory \"BOA\" will contain BOA files, and a",
          "subdirectory \"TOA\" will contain TOA files);",
          "otherwise, all the output files will be put in the directory",
          "specified above.",
          "Subdirectories can be created automatically, but the paren output",
          "directory must exist."),
        p("This option take effets also for spectral indices (for which",
          "subdirectory names are index names) and for intermediate files",
          "if the user chosed to save them."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_clip_on_extent, {
      showModal(modalDialog(
        title = "Clip outputs on the selected extent?",
        p(HTML(
          "<strong>Yes</strong>:",
          "the extent selected in the tab \"Spatio-temporal selection\"",
          "is used as extent for output products.",
          "The user can pass other geometry parameters in the box",
          "\"Output geometry\"."
        )),
        p(HTML(
          "<strong>No</strong>:",
          "the extent selected in the tab \"Spatio-temporal selection\"",
          "is used to select tiles overlapping it;",
          "output products maintain the full extent and the geometry of",
          "Sentinel-2 input tiles."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_index_source, {
      showModal(modalDialog(
        title = "Build indices from:",
        p(HTML(
          "<strong>BOA</strong>:",
          "Spectral indices are build from surface reflectances",
          " (Bottom Of Atmosphere).",
          "This is the default option."
        )),
        p(HTML(
          "<strong>TOA</strong>:",
          "Spectral indices are build from Top Of Atmosphere reflectances.",
          "It is strongly suggested not to use this option",
          "(use only if you are not interested to the absolute values",
          "of the indices, and if the atmospheric disturbance in your area",
          "of interest is sufficiently uniform)."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$note_list_indices, {
      showModal(modalDialog(
        title = "Spectral indices",
        HTML(
          "<table style=\"width:100%\">",
          "<tr>",
          "<td style=\"padding-right: 10px;\">",
          as.character(
            a(href="http://www.indexdatabase.de/db/is.php?sensor_id=96",
              target="_blank",
              img(
                src="http://www.indexdatabase.de/daten/grafik/logo.png",
                alt="IDB logo",
                height="70",
                width="125"
              )
            )
          ),
          "</td>",
          "<td>",
          "Spectral indices here listed were mostly taken from",
          "<a href=\"http://www.indexdatabase.de\" target=\"_blank\">Index DataBase</a>.",
          paste0("Indices marked as verified (",check_mark,") were checked"),
          "in order to ensure that the formula used to compute them",
          "is actually the formula used by the authors, and that",
          "Sentinel-2 bands associated to spectral bands are correct.",
          "</td></tr>"
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$alert_leaflet, {
      showModal(modalDialog(
        title = "Issue with extent selection",
        p("Some problems can occur with selection of extent,",
          "due to a conflict between two modules used in this part",
          "of the GUI."), # shinyFiles buttons (using NS()) and leaflet
        p("In particular, two are known:"),
        p(HTML(
          "<ol>",
          "<li>When an extent was already passed in input",
          "(as an argument of fidolasen_s2() function, or with a parameter list)",
          "the map is not updated instantaneously. To update it,",
          "select another method to select extent (e.g. \"draw\") and then",
          "select again \"Imported as parameter\".</li>",
          "<li>The module used to draw polygons by hand has some problems",
          "with the deleting function, so it is recommended to avoid to delete",
          "polygons. To do it anyway, uncheck the visualisation of",
          "\"S2 tiles\" and \"Extent\", otherwise it is not possible to click",
          "on them.</li>",
          "</ol>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$import_param_deactivated, {
      showModal(modalDialog(
        title = "Issue with import parameter function",
        p(HTML(
          "The function used to import parameter list was temporary",
          "deactivated, due to an incompatibility with another module.",
          "To import a JSON option file, launch <tt>fidolasen_s2()</tt>",
          "function with the path of the file as argument:"
        )),
        p(HTML(
          "<tt>fidolasen_s2(param_list = \"/path/of/the/file.json\")</tt>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    
    ## Exit and save
    
    # functions to check that all is correctly set TODO
    check_param <- function() {
      
    }
    
    # function to create a list to objects to be returned
    create_return_list <- function() {
      rl <- list()
      
      # processing steps #
      rl$preprocess <- as.logical(input$preprocess) # TRUE to perform preprocessing steps, FALSE to download SAFE only
      rl$s2_levels <- c(if(safe_req$l1c==TRUE){"l1c"}, if(safe_req$l2a==TRUE){"l2a"}) # required S2 levels ("l1c","l2a")
      rl$sel_sensor <- input$sel_sensor # sensors to use ("s2a", "s2b")
      rl$online <- as.logical(input$online) # TRUE if online mode, FALSE if offline mode
      rl$overwrite_safe <- as.logical(input$overwrite_safe) # TRUE to overwrite existing SAFE, FALSE not to
      rl$rm_safe <- input$rm_safe # "yes" to delete all SAFE, "l1c" to delete only l1c, "no" not to remove
      rl$step_atmcorr <- if (safe_req$l2a==TRUE) {input$step_atmcorr} else {"no"} # download_method in sen2cor: "auto", "l2a", "scihub" or "no"
      # rl$steps_reqout <- input$steps_reqout # vector of required outputs: "safe", "tiles", "clipped" (one or more)
      
      # spatio-temporal selection #
      rl$timewindow <- if (input$query_time==TRUE) { # range of dates
        input$timewindow
      } else {
        NA
      }
      rl$timeperiod <- if (input$query_time==TRUE) { # range of dates
        input$timeperiod # "full" or "seasonal"
      } else {
        "full"
      }
      
      # polygons
      rl$extent <- if (input$query_space == TRUE & !is.null(rv$extent)) {
        rv$extent %>%
          st_transform(4326) %>%
          geojson_json(pretty=TRUE)
      } else {
        NA
      }
      rl$s2tiles_selected <- if (input$query_space == TRUE & !is.null(input$tiles_checkbox)) {
        input$tiles_checkbox
      } else {
        NA
      } # selected tile IDs
      rl$s2orbits_selected <- NA # temporary select all orbits (TODO implement)
      
      # product selection #
      rl$list_prods <- input$list_prods[!input$list_prods %in% "indices"] # TOA, BOA, SCL, TCI (for now)
      rl$list_indices <- if (indices_req()==TRUE & "indices" %in% input$list_prods) {input$list_indices} else {NA} # index names
      rl$index_source <- input$index_source # reflectance band for computing indices ("BOA" or "TOA")
      rl$mask_type <- if (input$atm_mask==FALSE) {NA} else {input$atm_mask_type} # atmospheric masking (accepted types as in s2_mask())
      
      rl$clip_on_extent <- as.logical(input$clip_on_extent) # TRUE to clip (and warp) on the selected extent, FALSE to work at tiles/merged level
      rl$extent_as_mask <- as.logical(input$extent_as_mask) # TRUE to mask outside the polygons of extent, FALSE to use as boundig box
      
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
      rl$overwrite <- as.logical(input$overwrite)
      
      # set directories #
      rl$path_l1c <- if (safe_req$l1c==TRUE) {input$path_l1c_textin} else {NA} # path of L1C SAFE products
      rl$path_l2a <- if (safe_req$l2a==TRUE) {input$path_l2a_textin} else {NA} # path of L2A SAFE products
      rl$path_tiles <- if (rl$preprocess==TRUE & input$keep_tiles==TRUE) {input$path_tiles_textin} else {NA} # path of entire tiled products
      rl$path_merged <- if (rl$preprocess==TRUE & input$keep_merged==TRUE) {input$path_merged_textin} else {NA} # path of entire tiled products
      rl$path_out <- if (rl$preprocess==TRUE & (length(input$list_prods)>1 | length(input$list_prods)>0 & !"indices" %in% input$list_prods)) {input$path_out_textin} else {NA} # path of output pre-processed products
      rl$path_indices <- if (rl$preprocess==TRUE & indices_req()==TRUE) {input$path_indices_textin} else {NA} # path of spectral indices
      rl$path_subdirs <- if (rl$preprocess==TRUE) {as.logical(input$path_subdirs)} else {NA} # logical (use subdirs)
      
      # information about package version
      rl$fidolasen_version <- packageVersion("fidolasen") %>% as.character()
      
      return(rl)
    }
    
    # function to import saved parameters
    import_param_list <- function(pl) {
      
      # processing steps
      updateRadioButtons(session, "preprocess", selected = pl$preprocess)
      updateCheckboxGroupInput(session, "list_levels", selected = pl$s2_levels)
      updateCheckboxGroupInput(session, "sel_sensor", selected = pl$sel_sensor)
      updateRadioButtons(session, "online", selected = pl$online)
      updateRadioButtons(session, "overwrite_safe", selected = pl$overwrite_safe)
      updateRadioButtons(session, "rm_safe", selected = pl$rm_safe)
      updateRadioButtons(session, "step_atmcorr", selected = pl$step_atmcorr)
      
      
      # spatio-temporal selection
      if (any(is.na(pl$timewindow))) {
        updateRadioButtons(session, "query_time", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_time", selected = TRUE)
        updateDateRangeInput(session, "timewindow", start=pl$timewindow[1], end=pl$timewindow[2])
        updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
      }
      if (any(is.na(pl$extent)) & pl$online == FALSE) {
        updateRadioButtons(session, "query_space", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_space", selected = TRUE)
        # rl$s2tiles_selected <- input$tiles_checkbox # selected tile IDs
        updateRadioButtons(session, "extent_as_mask", selected = rv$extent_as_mask)
      }
      
      # product selection
      if (all(is.na(pl$list_prods))) {pl$list_prods <- NULL}
      updateCheckboxGroupInput(
        session, "list_prods",
        selected = if (any(!is.na(pl$list_indices))) {
          c(pl$list_prods,"indices")
        } else {
          pl$list_prods
        }
      )
      if (all(is.na(pl$list_indices))) {pl$list_indices <- NULL}
      indices_rv$checked <- pl$list_indices
      # updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices) # FIXME 1 not working since it is reactive
      updateRadioButtons(session, "atm_mask",
                         selected = ifelse(is.na(pl$mask_type),FALSE,TRUE))
      updateRadioButtons(session, "atm_mask_type",
                         selected = ifelse(is.na(pl$mask_type),"cloud_medium_proba",pl$mask_type))
      updateRadioButtons(session, "index_source", selected = pl$index_source)
      updateRadioButtons(session, "clip_on_extent", selected = pl$clip_on_extent)
      updateRadioButtons(session, "keep_tiles", selected = ifelse(is.na(pl$path_tiles),FALSE,TRUE))
      updateRadioButtons(session, "keep_merged", selected = ifelse(is.na(pl$path_merged),FALSE,TRUE))
      
      
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
      
      # update extent (at the end, not to interfere with outhers events
      if (!is.na(pl$extent)) {
        updateRadioButtons(session, "extent_type",
                           choices = list("Bounding box coordinates" = "bbox",
                                          "Upload vector file" = "vectfile",
                                          "Draw on the map" = "draw",
                                          "Imported as parameter" = "imported"),
                           selected = "imported")
      }
      # updating tiles_checkbox was moved outside this function
      # updateCheckboxGroupInput(session, "tiles_checkbox",
      #                          selected = pl$s2tiles_selected)
      
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
    
    # # if Import is pressed, read a json object
    # # FIXME "Load options" was deactivated for unsolved problems
    # # with extent map.
    imported_param <- reactive({NULL})
    # shinyFileChoose(input, "import_param", roots=volumes, session=session,
    #                 filetypes = c("JSON"="json"))
    # imported_param <- reactive({
    #   if (!is.null(input$import_param)) {
    #     import_param_path <- parseFilePaths(volumes,input$import_param)
    #     if (nrow(import_param_path)>0) {
    #       import_param_path$datapath %>%
    #         as.character() %>%
    #         readLines() %>%
    #         fromJSON()
    #     } else {
    #       NULL
    #     }
    #   }
    # })
    # observe({
    #   if (exists("imported_param") && !is.null(imported_param())) {
    #     import_param_list(imported_param()) # FIXME 3 same as fixme2 (after importing parameters, map is not drawn)
    #     # imported_param() <- NULL
    #   }
    # })
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




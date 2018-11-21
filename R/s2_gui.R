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
#'  leaflet leafletOutput leafletProxy
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#'  removeDrawToolbar
#' @importFrom mapedit editModUI
#' @importFrom utils packageVersion
#' @importFrom sf st_coordinates st_crs st_intersects st_polygon st_read st_bbox st_as_sfc st_transform
#' @importFrom shiny a actionButton actionLink addResourcePath br callModule checkboxGroupInput
#'  checkboxInput column conditionalPanel dateRangeInput div downloadButton downloadHandler em fileInput fluidRow h2 h3
#'  helpText hr HTML htmlOutput icon incProgress isolate NS numericInput observe p
#'  radioButtons reactive reactiveVal reactiveValues removeModal renderText renderUI runApp selectInput setProgress
#'  shinyApp showModal sliderInput span stopApp strong tagList textInput uiOutput updateCheckboxGroupInput
#'  updateDateRangeInput updateSliderInput updateRadioButtons updateTextInput withMathJax
#'  withProgress
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath
#'  shinyDirButton shinyDirChoose shinyFileChoose shinyFileSave
#'  shinyFilesButton shinySaveButton
#' @importFrom shinyjs click delay disable enable useShinyjs extendShinyjs
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom stats setNames
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
# For now, value "sen2r" is used when s2_gui() is launched from
# sen2r(), and is used to change save button label.
.s2_gui <- function(param_list = NULL,
                    par_fun = "parent",
                    thunderforest_api = NA) {
  
  # TODO: populate parameter values with param_list content, if provided
  
  # extract and import tiles kml
  s2tiles <- s2_tiles()
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  # shiny
  s2_gui.ui <- dashboardPage(
    title = "sen2r: an R toolbox to find, download and preprocess Sentinel-2 data",
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      
      # logo
      div(
        style = "text-align:center;padding-top:17px;padding-bottom:30px;",
        a(
          href='https://ranghetti.github.io/sen2r',
          target = "_blank",
          uiOutput("img_logo")
        )
      ),
      
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
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color
      shiny::tags$head(shiny::tags$style(".scihub_savebutton{width:90px;")), # fixed width
      shiny::tags$head(shiny::tags$script(src = "message-handler.js")), # for actionbuttons
      shiny::tags$head(shiny::tags$link(rel="icon", href="favicon.ico")),
      
      # shiny::tags$head(shiny::tags$script('
      #                                     var dimension = [0, 0];
      #                                     $(document).on("shiny:connected", function(e) {
      #                                     dimension[0] = window.innerWidth;
      #                                     dimension[1] = window.innerHeight;
      #                                     Shiny.onInputChange("dimension", dimension);
      #                                     });
      #                                     $(window).resize(function(e) {
      #                                     dimension[0] = window.innerWidth;
      #                                     dimension[1] = window.innerHeight;
      #                                     Shiny.onInputChange("dimension", dimension);
      #                                     });
      #                                     ')), # return the width/height of the window (used to set map height)
      div(
        style="position:absolute;top:430px;",
        # # client-side buttons
        # p(style="margin-top:15pt;margin-left:11pt;",
        #   downloadButton("export_param", "\u2000Save options as...", class="darkbutton")
        # ),
        # p(style="margin-top:5pt;margin-left:11pt;",
        #   fileInput(
        #     "import_param", label = NULL, buttonLabel = "Load options", 
        #     accept = "application/json", multiple = FALSE#, class = "darkbutton"
        #   )
        # ),
        # server-side buttons
        p(style="margin-top:15pt;",
          shinySaveButton(
            "export_param", 
            "Save options as...", "Save parameters as ...", 
            filetype=list(json="json"), 
            class="darkbutton"
          )
        ),
        p(style="margin-top:5pt;",
          shinyFilesButton(
            "import_param", 
            "Load options", "Import a JSON file with parameters", 
            multiple=FALSE, 
            class="darkbutton"
          )
        ),
        # actionButton("import_param_deactivated", label = "Load options", class = "darkbutton"),
        p(style="margin-top:20pt;",
          actionButton(
            "return_param",
            label = if (par_fun=="sen2r") {
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
            label = if (par_fun=="sen2r") {
              "\u2000Close without processing"
            } else {
              "\u2000Close without saving"
            },
            icon = icon("ban"),
            class = "darkbutton"
          )
        ),
        
        p(style="margin-top:20pt;",
          actionButton(
            "open_github_doc",
            label = "\u2000Open documentation",
            icon = icon("info-circle"),
            onclick ="window.open('https://ranghetti.github.io/sen2r', '_blank')",
            # onclick ="window.open('https://ranghetti.github.io/sen2r/articles/sen2r_gui.html', '_blank')",
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
                span(
                  "Raw files in ",
                  a("raw SAFE format",
                    href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/data-formats",
                    target="_blank"),
                  " (downloaded and/or corrected with sen2cor)"
                ),
                "Processed spatial files (surface reflectance, spectral indices, ...) in custom format"
              ),
              choiceValues = list(FALSE, TRUE),
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
                      div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
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
                      div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
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
                    actionLink("help_online", icon("question-circle")),
                    if (Sys.info()["sysname"] == "Windows") {
                      span(
                        "\u2000",
                        actionLink("fix_online", icon("warning"))
                      )
                    } else {
                      
                    }
                  ),
                  choiceNames = list("Online", "Offline"),
                  choiceValues = list(TRUE, FALSE),
                  selected = TRUE,
                  inline = TRUE
                ),
                
                # SciHub credentials
                conditionalPanel(
                  condition = "input.online == 'TRUE'",
                  div(
                    style = "padding-bottom:10px;",
                    actionButton(
                      "scihub",
                      label = "\u2000Login in SciHub",
                      icon=icon("user-circle")
                    )
                  ),
                  radioButtons(
                    "downloader",
                    label = span(
                      "Downloader\u2000",
                      actionLink("help_downloader", icon("question-circle"))
                    ),
                    choiceNames = list("Wget", "aria2"),
                    choiceValues = list("wget", "aria2"),
                    selected = "wget",
                    inline = TRUE
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
                  radioButtons(
                    "timeperiod", 
                    label = span(
                      "Time period type\u2000",
                      actionLink("help_time_period", icon("question-circle"))
                    ),
                    choices = list("Full" = "full",
                                   "Seasonal" = "seasonal"),
                    selected = "full",
                    inline = TRUE
                  )
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
                  width=6,
                  
                  div(style="display:inline-block;vertical-align:top;",
                      span(
                        strong("Extent name:\u2000"),
                        actionLink("help_extent_name", icon("question-circle"))
                      )),
                  div(style="vertical-align:top;",
                      textInput("extent_name_textin", NULL, "sen2r")),
                  
                  # Buttons to load the extent with modal dialogs
                  strong("Specify the extent:\u2000"),
                  span(
                    div(style="padding-top:5px;",
                        actionButton(
                          "button_extent_bbox",
                          label = "\u2000Specify a bounding box",
                          width = 200,
                          icon=icon("object-group")
                        )),
                    div(style="padding-top:10px;",
                        actionButton(
                          "button_extent_vectfile",
                          label = "\u2000Load a vector file",
                          width = 200,
                          icon=icon("upload")
                        )),
                    div(style="padding-top:10px;",
                        actionButton(
                          "button_extent_draw",
                          label = "\u2000Draw it on the map",
                          width = 200,
                          icon=icon("paint-brush")
                        )),
                    div(style="padding-top:10px;padding-bottom:10px;",
                        actionButton(
                          "button_refresh_map",
                          label = "\u2000Reload the extent on map",
                          width = 200,
                          icon=icon("retweet")
                        ))
                  )
                  
                ),
                
                column(
                  width=6,
                  div(
                    checkboxGroupInput(
                      "tiles_checkbox",
                      "Tiles selected",
                      choices = character(0),
                      selected = character(0),
                      inline = FALSE
                    ),
                    strong("Orbits selected"),
                    helpText(em("Not yet implemented."))
                  )
                )
                
                # TODO Multipart geometries: do not remove this part of code;
                # this functionality have to be implemented.
                # column(
                #   width=8,
                #   conditionalPanel(
                #     condition = "input.extent_type == 'draw' || input.extent_type == 'vectfile'",
                #     radioButtons(
                #       "dissolve_extent",
                #       label = span(
                #         "Consider multiple polygons as:\u2000",
                #         actionLink("help_dissolve_extent", icon("question-circle"))
                #       ),
                #       choiceNames = list(
                #         "A multipart geometry (create a single merged output)",
                #         "Single part geometries (create separate outputs)"
                #       ),
                #       choiceValues = list(TRUE, FALSE),
                #       selected = TRUE)
                #   ),
                #   conditionalPanel(
                #     condition = "input.extent_type == 'vectfile' && input.dissolve_extent == 'FALSE'",
                #     selectInput("extent_id", "Select the field to use as extent name",
                #                 choices = list("No one (using row numbers)" = "nrow",
                #                                "take","dynamically","from","shape","attributes"),
                #                 selected = "nrow")
                #   )#,
                # )
                
              ), # end of 1st fluidRow after conditionalPanel on spatial filter
              
              fluidRow(
                column(
                  width=12,
                  # Map
                  # uiOutput("view_map_leaflet")
                  leafletOutput("view_map", height=600, width="100%")
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
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                              textInput("path_out_textin", NULL, ""))))
                ),
                
                fluidRow(
                  column(
                    width=5,
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
                  ),
                  column(
                    width=7,
                    div(
                      style="margin-top: -15px",
                      checkboxInput(
                        "check_thumbnails",
                        label = span(
                          "Create thumbnails\u2000",
                          actionLink("help_thumbnails", icon("question-circle"))
                        ),
                        value = TRUE
                      )
                    )
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
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
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
                          div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                              textInput("path_merged_textin", NULL, ""))))
                )
              )
            ) # end of fluidRow keep_tiles
            
          ), # end of box "Output extent"
          
          box(
            title="Cloud mask",
            width=6,
            
            radioButtons(
              "atm_mask", 
              label = span(
                "Mask cloud-covered pixels?\u2000",
                actionLink("help_mask", icon("question-circle"))
              ),
              choices = list("Yes" = TRUE, "No" = FALSE),
              selected = FALSE,
              inline = TRUE
            ),
            
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
              selectInput(
                "atm_mask_type", 
                label = span(
                  "Apply mask to:\u2000",
                  actionLink("help_mask_classes", icon("question-circle"))
                ),
                choices = list(
                  "No data" = "nodata",
                  "No data and clouds (high probability)" = "cloud_high_proba",
                  "No data and clouds (high-medium prob.)" = "cloud_medium_proba",
                  "No data and clouds (any probability)" = "cloud_low_proba",
                  "No data, clouds and shadows" = "cloud_and_shadow",
                  "All except clear-sky" = "clear_sky",
                  "All except land surface" = "land",
                  "Custom mask" = "custom"
                ),
                selected = "cloud_medium_proba"
              ),
              
              conditionalPanel(
                condition = "input.atm_mask_type == 'custom'",
                checkboxGroupInput(
                  "atm_mask_custom", "Select the classes to mask:",
                  choiceNames = list(
                    HTML("<font style=\"family:monospace;background-color:#000000;color:white;\">\u20020\u2002</font>\u2002No data"),
                    HTML("<font style=\"family:monospace;background-color:#FF0000;color:white;\">\u20021\u2002</font>\u2002Saturated or defective"),
                    HTML("<font style=\"family:monospace;background-color:#424142;color:white;\">\u20022\u2002</font>\u2002Dark area pixels"),
                    HTML("<font style=\"family:monospace;background-color:#633400;color:white;\">\u20023\u2002</font>\u2002Cloud shadows"),
                    HTML("<font style=\"family:monospace;background-color:#29f329;color:black;\">\u20024\u2002</font>\u2002Vegetation"),
                    HTML("<font style=\"family:monospace;background-color:#ffff00;color:black;\">\u20025\u2002</font>\u2002Bare soils"),
                    HTML("<font style=\"family:monospace;background-color:#0000ff;color:white;\">\u20026\u2002</font>\u2002Water"),
                    HTML("<font style=\"family:monospace;background-color:#7b7d7b;color:white;\">\u20027\u2002</font>\u2002Cloud (low probability)"),
                    HTML("<font style=\"family:monospace;background-color:#bdbebd;color:black;\">\u20028\u2002</font>\u2002Cloud (medium probability)"),
                    HTML("<font style=\"family:monospace;background-color:#ffffff;color:black;\">\u20029\u2002</font>\u2002Cloud (high probability)"),
                    HTML("<font style=\"family:monospace;background-color:#63cbff;color:black;\">\u200510\u2005</font>\u2002Thin cirrus"),
                    HTML("<font style=\"family:monospace;background-color:#ff9aff;color:black;\">\u200511\u2005</font>\u2002Snow")
                  ),
                  choiceValues = as.list(0:11),
                  selected = list(0,1,8,9)
                )
              ),
              
              sliderInput(
                "max_masked_perc", 
                label = span(
                  "Maximum allowed cloud cover",
                  actionLink("help_masked_perc", icon("question-circle"))
                ),
                min = 0, max = 100, value = 80,
                step = 1, post = "%"
              ),
              
              radioButtons(
                "mask_apply_smooth", 
                label = span(
                  "Smooth / bufferize the cloud-covered surface?\u2000",
                  actionLink("help_mask_smooth", icon("question-circle"))
                ),
                choices = list("Yes" = TRUE,
                               "No" = FALSE),
                selected = FALSE,
                inline = TRUE
              ),
              
              conditionalPanel(
                condition = "input.mask_apply_smooth == 'TRUE'",
                
                fluidRow(
                  
                  column(
                    width=6,
                    numericInput("mask_smooth", "Smooth (m)",
                                 # width="100px",
                                 value = 250,
                                 min = 0)
                  ),
                  
                  column(
                    width=6,
                    numericInput("mask_buffer", "Buffer (m)",
                                 # width="100px",
                                 value = 250)
                  )
                  
                ) # end of smooth/buffer fluidRow
              ) # endo if conditionalPanel mask_apply_smooth
              
            ) # end of conditionalPanel atm_mask
            
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
                        style="display:inline-block;vertical-align:top;width:77pt;",
                        shinyFilesButton("reference_file_button", "Select raster", "Select reference file", multiple=FALSE),
                        "\u2001"),
                      div(
                        style="display:inline-block;vertical-align:top;width:calc(100% - 77pt - 3px);",
                        textInput("reference_file_textin", label = NULL, "", width="100%"))),
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
                        div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                            textInput("path_indices_textin", NULL, ""))),
                    div(style="display:inline-block;vertical-align:top;",
                        actionButton("path_indices_cp", "Copy from directory for output processed products"))),
                
                br(),
                
                fluidRow(
                  column(
                    width = 6,
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
                    width = 6,
                    selectInput(
                      "index_datatype", label = "Data type",
                      choices = list("Byte" = "Byte",
                                     "Integer (16 bits)" = "Int16",
                                     "Float (32 bits)" = "Float32",
                                     "Float (64 bits)" = "Float64"),
                      selected = "Int16"
                    )
                  )
                ),
                
                hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
                fluidRow(
                  column(
                    width = 5,
                    textInput("filter_indices", "Filter indices")
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
                uiOutput("check_indices")
              ),
              
              uiOutput("show_formula")
              
            )
          ) # end of conditionalpanel on tab_index
          
        ) # end of tabItem tab_index
        
      ) # end of tabItems
    ) # end of dashboardBody
    
  ) # end of s2_gui.ui dashboardPage
  
  s2_gui.server <- function(input, output, session) {
    
    # link to www directory and objects
    addResourcePath("www", system.file("www", package="sen2r"))
    output$img_logo<-renderUI(
      img(src='www/images/sen2r_logo_200px.png',height='133',width='200')
    )
    
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
                            !is.null(input$list_indices) & input$index_source=="TOA" |
                            input$step_atmcorr %in% c("auto","scihub")) {TRUE} else {FALSE}
        safe_req$l2a <- if (any(l2a_prods %in% input$list_prods) |
                            input$atm_mask == TRUE |
                            !is.null(input$list_indices) & input$index_source=="BOA") {TRUE} else {FALSE}
        safe_req$l2a_onlytomask <- if (!any(l2a_prods %in% input$list_prods) &
                                       input$atm_mask == TRUE &
                                       (is.null(input$list_indices) |
                                        !is.null(input$list_indices) & input$index_source=="BOA")) {TRUE} else {FALSE}
      } else {
        safe_req$l1c <- if ("l1c" %in% input$list_levels | 
                            input$step_atmcorr %in% c("auto","scihub")) {TRUE} else {FALSE}
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
    
    # # disable online mode on Windows
    # observe({
    #   if (Sys.info()["sysname"] == "Windows") {
    #     disable("online")
    #     updateRadioButtons(session, "online",
    #                        selected = FALSE)
    #   } else {
    #     enable("online")
    #   }
    # })
    
    # disable downloader aria2 if it is not installed
    binpaths <- load_binpaths()
    observeEvent(input$downloader, {
      if (is.null(binpaths$aria2c)) {
        updateRadioButtons(session, "downloader", selected = "wget")
        disable("downloader")
      } else {
        enable("downloader")
      }
    })
    
    # Edit scihub credentials
    observeEvent(input$scihub, {
      
      # open the modalDialog
      showModal(scihub_modal(
        username = if(!is.null(input$scihub_username)){input$scihub_username}else{NA},
        password = if(!is.null(input$scihub_password)){input$scihub_password}else{NA}
      ))
      
      # dummy variable to define which save button has to be used
      output$switch_save_apihub <- renderText({
        if (is.null(input$apihub_default)) {
          ""
        } else if (input$apihub_default) {
          "default"
        } else {
          "custom"
        }
      })
      outputOptions(output, "switch_save_apihub", suspendWhenHidden = FALSE)
      
      # initialise the shinyFiles Save as button
      observe({
        apihub_path_prev <- rv$apihub_path
        shinyFileSave(input, "apihub_path_sel", roots=volumes, session=session)
        apihub_path_raw <- parseSavePath(volumes, input$apihub_path_sel)
        rv$apihub_path <- if (nrow(apihub_path_raw)>0) {
          as.character(apihub_path_raw$datapath)
        } else {
          NA
        }
        if (!is.na(rv$apihub_path)) {
          if (!rv$apihub_path %in% apihub_path_prev) {
            # if a change in the path is detected (= the button has been used), 
            # close the modalDialog
            # FIXME if a user re-opne the modalDialog and does not change 
            # user nor password, the "Save as..." button will not close the dialog
            shinyjs::click("save_apihub")
          }
        }
      })
      
    })
    
    # save user/password
    observeEvent(input$save_apihub, {
      write_scihub_login(
        input$scihub_username, input$scihub_password, 
        apihub_path = if(!is.na(rv$apihub_path)){as.character(rv$apihub_path)}else{NA}
      )
      removeModal()
    })
    
    ## end of steps module ##
    
    
    
    ## Extent module ##
    
    # Force to use spatial and temporal filters in online mode
    observeEvent(input$online, {
      if (input$online) {
        updateRadioButtons(session, "query_time", selected = TRUE)
        updateRadioButtons(session, "query_space", selected = TRUE)
        disable("query_time")
        disable("query_space")
      } else {
        enable("query_time")
        enable("query_space")
      }
    })
    
    
    #-- Function to update the map and the list of tiles --#
    # it returns TRUE if the input extent source was correctly read, FALSE elsewhere.
    # argument extent_srouce determines which source to be used:
    # "bbox", "vectfile", "draw" from selection buttons, "imported" from parameter;
    # in this case, the argument "custom_source" is the source to be passed.
    update_extent <- function(extent_source, custom_source=NA) {
      
      # 1. Define rv$extent
      if (extent_source == "bbox") {
        
        # Bbox mode #
        
        # check that the polygon is valid
        if (attr(rv$bbox_polygon, "valid")) {
          rv$extent <- rv$bbox_polygon
          attr(rv$extent, "new") <- TRUE
        } else {
          return(FALSE)
        }
        
      } else if (extent_source == "vectfile") {
        
        # Vectfile mode #
        
        # check that the polygon is valid
        if (attr(rv$vectfile_polygon, "valid")) {
          rv$extent <- rv$vectfile_polygon
          attr(rv$extent, "new") <- TRUE
        } else {
          return(FALSE)
        }
        
      } else if (extent_source == "draw") {
        
        # Drawn mode #
        
        # namespace for extent selection
        sel_drawn <- if (!is.null(rv$extent_edits()$finished)) {
          x <- rv$extent_edits()$finished
          attr(x, "valid") <- TRUE
          attr(x, "new") <- TRUE
          x
        } else {
          x <- st_polygon(); attr(x, "valid") <- FALSE; x
        }
        
        if (!attr(sel_drawn, "valid")) {
          return(FALSE)
        }
        
        rv$extent <- sel_drawn
        
      } else if (extent_source == "imported") {
        
        # Imported from parameters #
        
        sel_imported_extent <- if (is.null(custom_source) | anyNA(custom_source)) {
          x <- st_polygon(); attr(x, "valid") <- FALSE; x
        } else {
          x <- st_read(custom_source, quiet=TRUE) %>% 
            st_transform(4326)
          attr(x, "valid") <- TRUE
          attr(x, "new") <- TRUE
          x
        }
        
        if (!attr(sel_imported_extent, "valid")) {
          return(FALSE)
        }
        
        rv$extent <- sel_imported_extent
        
      } else {
        
        # For any other value of extent_source, use the existing rv$extent
        
        if (is.null(rv$extent)) {
          return(FALSE)
        } else if (!attr(rv$extent, "valid")) {
          return(FALSE)
        } else {
          attr(rv$extent, "new") <- FALSE
        }
        
      }
      
      
      # 2. Update the list of overlapping tiles and the tiles on the map
      if(length(rv$extent) > 0) {
        
        rv$draw_tiles_overlapping <- s2tiles[unique(unlist(suppressMessages(st_intersects(st_transform(rv$extent,4326), s2tiles)))),]
        
        if (attr(rv$extent, "new")) {
          # update the list of tiles
          updateCheckboxGroupInput(
            session, "tiles_checkbox",
            choiceNames = lapply(rv$draw_tiles_overlapping$tile_id, span, style="family:monospace;"),
            choiceValues = rv$draw_tiles_overlapping$tile_id,
            selected = rv$draw_tiles_overlapping$tile_id,
            inline = nrow(rv$draw_tiles_overlapping) > 8 # inline if they are many
          )
        }
        
        # reset and update the map
        react_map(base_map()) 
        rv$draw_tiles_overlapping_ll <- st_transform(rv$draw_tiles_overlapping, 4326)
        leafletProxy("view_map") %>%
          clearShapes() %>%
          fitBounds(
            lng1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[,"X"]),
            lat1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[,"Y"]),
            lng2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[,"X"]),
            lat2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[,"Y"])
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
        # empty the list of tiles
        updateCheckboxGroupInput(session, "tiles_checkbox",
                                 choices = NULL)
        # reset the map
        react_map(base_map())
        # leafletProxy("view_map") %>%
        #   clearShapes()
      }
      
      return(TRUE)
      
    }
    
    
    #-- Create the map (once) --#
    base_map <- function() {
      leaflet() %>%
        
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
    }
    react_map <- reactiveVal({base_map()})
    output$view_map <- renderLeaflet({react_map()})
    # # renderUI of the leaflet so to update dinamically the width
    # output$view_map_leaflet <- renderUI({
    #   leafletOutput(
    #     "view_map",
    #     height = isolate(ifelse(!is.null(input$dimension),input$dimension[1]*3/5,600))
    #   )
    # })
    
    
    #-- Reactive objects for input selection (NOT map update) --#
    
    #- Bbox mode -#
    
    # message for bboxproj
    output$bboxproj_message <- renderUI({
      bboxproj_validated <- tryCatch(
        st_crs2(input$bboxproj), 
        error = function(e) {st_crs(NA)}
      )$proj4string
      if (input$bboxproj=="") {
        rv$bboxproj <- NA
        ""
      } else if (is.na(bboxproj_validated)) {
        rv$bboxproj <- NA
        # span(style="color:red", "\u2718") # ballot
        span(style="color:red",
             "Insert a valid projection (UTM timezone, EPSG code or PROJ4 string).")
      } else {
        rv$bboxproj <- bboxproj_validated
        # span(style="color:darkgreen", "\u2714") # check
        div(strong("Selected projection:"),
            br(),
            projname(bboxproj_validated),
            style="color:darkgreen")
      }
    })
    
    # create a new map (to be shown in modal dialog)
    react_map_bbox <- reactiveVal(base_map())
    output$view_map_bbox <- renderLeaflet({react_map_bbox()})
    
    # Open modal dialog to edit bbox
    observeEvent(input$button_extent_bbox, {
      showModal(load_extent_bbox())
    })
    
    # update the map dinamically
    observeEvent(c(
      input$bbox_xmin, input$bbox_xmax, 
      input$bbox_ymin, input$bbox_ymax, 
      rv$bboxproj
    ), {
      
      # Check that the bounding box is valid
      if (!anyNA(c(input$bbox_xmin, input$bbox_xmax, 
                   input$bbox_ymin, input$bbox_ymax)) & 
          !(is.null(rv$bboxproj) || is.na(rv$bboxproj))) {
        if (input$bbox_xmin != input$bbox_xmax &
            input$bbox_ymin != input$bbox_ymax) {
          # create the polygon
          rv$bbox_polygon <- st_as_sfc(
            st_bbox(
              c("xmin" = input$bbox_xmin,
                "ymin" = input$bbox_ymin,
                "xmax" = input$bbox_xmax,
                "ymax" = input$bbox_ymax), 
              crs = rv$bboxproj
            )
          ) %>% st_transform(4326)
          attr(rv$bbox_polygon, "valid") <- TRUE
        } else {
          rv$bbox_polygon <- st_polygon()
          attr(rv$bbox_polygon, "valid") <- FALSE
        }
      } else {
        rv$bbox_polygon <- st_polygon()
        attr(rv$bbox_polygon, "valid") <- FALSE
      }
      
      # if bbox is valid, update the map
      if (attr(rv$bbox_polygon, "valid")) {
        rv$bbox_ll <- st_bbox(st_transform(rv$bbox_polygon, 4326))
        leafletProxy("view_map_bbox") %>%
          clearShapes() %>%
          fitBounds(
            lng1 = as.numeric(rv$bbox_ll$xmin-(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
            lat1 = as.numeric(rv$bbox_ll$ymin-(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3),
            lng2 = as.numeric(rv$bbox_ll$xmax+(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
            lat2 = as.numeric(rv$bbox_ll$ymax+(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3)
          ) %>%
          addPolygons(data = rv$bbox_polygon,
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
        # if bbox is not valid, reset the map
        react_map_bbox(base_map())
      }
      
    })
    
    # use bbox
    observeEvent(input$save_extent_bbox, {
      # Add a progress bar while update_extent is running
      withProgress(message = 'Creating the extent...', value = 0, {
        bbox_valid <- update_extent(extent_source="bbox")
        if (bbox_valid) {
          removeModal()
        } else {
          sendSweetAlert(
            session, 
            title = "Invalid bounding box", 
            text = paste(
              "Please insert a valid bounding box."
            ), 
            type = "error",
            btn_labels = "Ok"
          )
        }
        # Fake progress
        for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
      })
    })
    
    
    #- Vector file mode -#
    
    # if 
    observeEvent(input$path_vectfile_sel, {
      uploaded_exts <- gsub("^.+\\.(.+)$","\\1",input$path_vectfile_sel$name)
      # checks
      if (length(unique(gsub("\\..+$","",input$path_vectfile_sel$name))) > 1) {
        # if more than one vector were chosen, give an alert and do not use the file
        sendSweetAlert(
          session, 
          title = "Invalid vector", 
          text = paste(
            "Please select a single vector",
            "(multiple selection is allowed only for shapefiles)."
          ),
          type = "error",
          btn_labels = "Ok"
        )
        rv$vectfile_path <- ""
      } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp","shx","dbf","prj")) {
        # if a single file was chosen and it is not a shapefile, use it
        rv$vectfile_path <- input$path_vectfile_sel$datapath
      } else if (anyNA(match(c("shp","shx","dbf","prj"),uploaded_exts))) {
        # if a shapefile was chosen but some files are missing, do not use it
        sendSweetAlert(
          session, 
          title = "Incomplete shapefile", 
          text = paste(
            "Please select all the files of the shapefile",
            "(at most .shp, .shx, .prj, .dbf)."
          ),
          type = "error",
          btn_labels = "Ok"
        )
        rv$vectfile_path <- ""
      } else {
        # if a shapefile was chosen and all the files are present,
        # rename the uploaded files in order to have the same filename and use them
        path_vectfile_sel_new_datapath <- file.path(
          dirname(input$path_vectfile_sel$datapath), input$path_vectfile_sel$name
        )
        for(i in seq_len(nrow(input$path_vectfile_sel))) {
          file.rename(input$path_vectfile_sel$datapath[i], path_vectfile_sel_new_datapath[i])
        }
        rv$vectfile_path <- path_vectfile_sel_new_datapath[
          input$path_vectfile_sel$type=="application/x-esri-shape"
          ]
      }
    })
    
    # create a new map (to be shown in modal dialog)
    react_map_vectfile <- reactiveVal(base_map())
    output$view_map_vectfile <- renderLeaflet({react_map_vectfile()})
    
    # Open modal dialog to load the vector file
    observeEvent(input$button_extent_vectfile, {
      rv$vectfile_path <- ""
      showModal(load_extent_vectfile())
    })
    
    # load the vector on the map
    observeEvent(rv$vectfile_path, {
      
      # Check that the vector is valid
      rv$vectfile_polygon <- tryCatch(
        {
          x <- st_read(rv$vectfile_path, quiet=TRUE) %>% 
            st_transform(4326)
          attr(x, "valid") <- TRUE
          attr(x, "new") <- TRUE
          x
        },
        error = function(e) {x <- st_polygon(); attr(x, "valid") <- FALSE; x}
      )
      
      if(attr(rv$vectfile_polygon, "valid")) {
        # if the vector is valid, update the map
        rv$vectfile_polygon_ll <- st_transform(rv$vectfile_polygon, 4326)
        leafletProxy("view_map_vectfile") %>%
          clearShapes() %>%
          fitBounds(
            lng1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
            lat1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"Y"]),
            lng2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
            lat2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"Y"])
          ) %>%
          addPolygons(data = rv$vectfile_polygon,
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
        # if the vector is not valid, reset the map
        react_map_vectfile(base_map())
      }
      
      
    })
    
    # use bbox
    observeEvent(input$save_extent_vectfile, {
      withProgress(message = 'Creating the extent...', value = 0, {
        vectfile_valid <- update_extent(extent_source="vectfile")
        if (vectfile_valid) {
          removeModal()
        } else {
          sendSweetAlert(
            session, 
            title = "Please specify a valid vector file.", 
            text = NULL,
            type = "error",
            btn_labels = "Ok"
          )
        }
        for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
      })
    })
    
    
    #- Draw mode -#
    
    # Open modal dialog to edit bbox
    observeEvent(input$button_extent_draw, {
      
      # create a new namespace every time the button is pushed,
      # in order not to make mess between modules
      extent_ns_name <- paste0("editor_",sample(1E9,1))
      extent_ns <- NS(extent_ns_name)
      rv$extent_edits <- callModule(editModPoly, extent_ns_name, base_map())
      
      # show the modal dialog
      showModal(load_extent_draw(extent_ns_name))
      
    })
    
    # use bbox
    observeEvent(input$save_extent_draw, {
      withProgress(message = 'Creating the extent...', value = 0, {
        drawn_valid <- update_extent(extent_source="draw")
        if (drawn_valid) {
          removeModal()
        } else {
          sendSweetAlert(
            session, 
            title = "Please draw a valid extent.", 
            text = NULL,
            type = "error",
            btn_labels = "Ok"
          )
        }
        for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
      })
      
    })
    
    
    #- Refresh the map if required -#
    observeEvent(input$button_refresh_map, {
      withProgress(message = 'Refreshing the map...', value = 0, {
        update_extent(extent_source="fake")
        for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
      })
    })
    
    
    # # disable dissolve_extent (not yet implemented, TODO)
    # disable("dissolve_extent")
    
    
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
      
      return(invisible(NULL))
      
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
    
    
    # Disable clipping and masking if no spatial filter was enabled
    observeEvent(input$query_space, {
      if (input$query_space) {
        enable("clip_on_extent")
        enable("extent_as_mask")
      } else {
        updateRadioButtons(session, "clip_on_extent", selected = FALSE)
        updateRadioButtons(session, "extent_as_mask", selected = FALSE)
        disable("clip_on_extent")
        disable("extent_as_mask")
      }
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
        
        outproj_validated <- tryCatch(
          st_crs2(input$outproj), 
          error = function(e) {st_crs(NA)}
        )$proj4string
        if (input$reproj==FALSE | input$outproj=="") {
          ""
        }  else if (is.na(outproj_validated)) {
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
    
    observeEvent(input$help_downloader, {
      showModal(modalDialog(
        title = "Downloader",
        p(HTML(
          "This selector allows to choose which downloader will be used",
          "to download Sentinel-2 SAFE archives."
        )), 
        p(HTML(
          "<strong>Wget</strong> is the default downloader, which is natively",
          "presenti in Linux systems and which can be installed in Windows",
          "using the function <code>check_sen2r_deps()</code> (graphically) or",
          "<code>install_wget()</code> (from commandline)."
        )),
        p(HTML(
          "<strong><a href=\"https://aria2.github.io\" target=\"_blank\">aria2</a></strong>",
          "is a faster downloader which can be installed in Linux systems",
          "from the default install manager (in Ubuntu, install the package \"aria2\"),",
          "or in Windows using the function <code>check_sen2r_deps()</code>",
          "(graphically) or <code>install_aria2()</code> (from commandline)."
        )),
        p(HTML(
          "This selector is active only in aria2 was already installed and",
          "recognised (to recognise it, launch <code>check_sen2r_deps()</code>)."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_apihub, {
      showModal(modalDialog(
        title = "SciHub username and password",
        p(HTML(
          "For security reasons, the SciHub username and password",
          "are not saved with the other parameters."
        )), 
        p(HTML(
          "By default, they are stored in a txt file inside the package,",
          "so to be the same for all the sen2r executions",
          "(the user have to set them only once)."
        )),
        p(HTML(
          "Since it is not possible to perform more than two queries at",
          "the same time, it can be useful to change them for a specific",
          "sen2r run (i.e. for a scheduled execution), in order not to",
          "interfer with other runs.",
          "In this case, this option can be checked, and the user and password",
          "will be saved in a different file, and will be used for this run",
          "(the path of the text file - and not the content - is added inside",
          "the parameter file)."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$fix_online, {
      showModal(modalDialog(
        title = "Download is not supported on Windows",
        p(
          "Currently, finding and downloading SAFE products is possible",
          "only over Linux systems. This will be fixed in a future release."
        ),
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
    
    observeEvent(input$help_time_period, {
      showModal(modalDialog(
        title = "Time period type",
        p(HTML(
          "<strong>Full</strong>:",
          "the specified time window is entirely processed",
          "(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return",
          "all the products in this time window which match the other parameters)."
        )),
        p(HTML(
          "<strong>Seasonal</strong>:",
          "the specified time window is processed from the first year to the",
          "last year, in the seasonal time windows from the first",
          "Julian day to the second Julian day",
          "(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return",
          "all the products from 2016-05-01 to 2016-09-30, from 2016-05-01 to",
          "2016-09-30 and from 2017-05-01 to 2017-09-30,",
          "which also match the other parameters)."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    # observeEvent(input$help_dissolve_extent, {
    #   showModal(modalDialog(
    #     title = "Consider multiple polygons as:",
    #     p(HTML(
    #       "<strong>A multipart geometry</strong>:",
    #       "all the polygons drawn (or the entire vector file which was",
    #       "uploaded) are considered as a single multipart",
    #       "polygon, so the output will cover the extent of all the polygons."
    #     )),
    #     p(HTML(
    #       "<strong>Single part geometries</strong>:",
    #       "the number of outputs will be equal to the number of polygons",
    #       "drawn (or to the number of polygons inside the uploaded vector",
    #       "file).",
    #       "Subdirectories are created for each output product: the name is",
    #       "a consecutive integer number, or an attribute name for uploaded",
    #       "vector files."
    #     )),
    #     em(
    #       "This option was not yet implemented: for now, only a multipart",
    #       "geometry can be used."
    #     ),
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # })
    
    observeEvent(input$help_extent_name, {
      showModal(modalDialog(
        title = "Name of the extent",
        p(
          "Enter an alphanumeric label, which cannot contain spaces, points",
          "nor underscores, and that cannot be a five-length string with",
          "the same structure of a tile ID",
          "(two numeric and three uppercase character values).",
          "The label can not be empty.",
          "This label is used in the output file names."
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
    
    observeEvent(input$help_thumbnails, {
      showModal(modalDialog(
        title = "Create thumbnails?",
        p("If checked, a thumbnail (a JPEG or PNG image with a width or height",
          "of 1024 pixels) with a corresponding",
          ".aux.xml file containing georeferencing metadata) will be created",
          "for each output file, and placed in a subdirectory named",
          "\"thumbnails\"."),
        p("Color schemes are set as follows:"),
        p(HTML(
          "<ul>",
          "<li>BOA and TOA reflectances are shown in false colours",
          "(SWIR-NIR-Red);</li>",
          "<li>SCL maps make use of the",
          as.character(a(
            href="https://sentinel.esa.int/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm",
            target="_blank",
            "original colour scheme:"
          )),
          as.character(img(
            src="https://sentinel.esa.int/documents/247904/322303/SUH-WIKI-MSI-230_Scene_Classification_figure_3.png",
            alt="SCL colour scheme"
          )),
          "<br></li>",
          "<li>spectral indices uses this",
          as.character(a(
            href="http://soliton.vm.bytemark.co.uk/pub/cpt-city/cmocean/tn/delta.png.index.html",
            target="_blank",
            "common colour ramp:"
          )),
          as.character(img(
            src="http://soliton.vm.bytemark.co.uk/pub/cpt-city/cmocean/delta.png",
            alt="indices colour ramp"
          )),
          "<br>with minimum and maximum values set to -1 and 1.</li>",
          "</ul>"
        )),
        p("For now, these color schemes cannot be modified."),
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
    
    observeEvent(input$help_mask, {
      showModal(modalDialog(
        title = "Mask cloud-covered pixels?",
        p(HTML(
          "<strong>Yes</strong>:",
          "the pixels classified as clouds are set to NA.",
          "The Surface Classification Map (SCL) included within Level-2A",
          "products is used to mask clouds.",
          "Use the selector below to define which classes have to be considered",
          "as clouds."
        )),
        p(HTML(
          "<strong>No</strong>:",
          "this step is not performed."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$help_mask_classes, {
      showModal(modalDialog(
        title = "Apply mask to:",
        p(HTML(
          "Select which type of mask have to be applied to rasters."
        )),
        p(HTML(
          "It is possible to choose between seven predefinite masks,",
          "or set a custom one."
        )),
        p(HTML(
          "The predefinite ones are shown in order of masking rate:<ul>",
          "<li><strong>No data:</strong> only missing values, i.e. areas without",
          "data (class 0 within SCL, corresponding to areas not covered",
          "by the current Sentinel orbit) or classified as \"Saturated or",
          "defective\" (class 1), are set to NA;</li>",
          "<li><strong>No data and clouds (high probability):</strong>",
          "areas classified as missing values (classes 0 and 1) or highly",
          "probably cloudy (class 9) are set to NA;</li>",
          "<li><strong>No data and clouds (high-medium prob.):</strong>",
          "areas classified as missing values (classes 0 and 1) or with a high",
          "or medium cloud probability (resp. classes 9 and 8) ",
          "are set to NA;</li>",
          "<li><strong>No data and clouds (any probability):</strong>",
          "areas classified as missing values (classes 0 and 1) or cloudly",
          "(classes 7, 8 and 9) are set to NA;</li>",
          "<li><strong>No data, clouds and shadows:</strong>",
          "areas classified as missing values (classes 0 and 1), cloudly",
          "(classes 7, 8 and 9) or shady (classes 2 and 3) are set to NA;</li>",
          "<li><strong>All except clear-sky:</strong>",
          "areas classified as missing values (classes 0 and 1), cloudly",
          "(classes 7, 8 and 9), shady (classes 2 and 3) and as cirrus",
          "(class 10) are set to NA (in other words, only pixels classified",
          "as vegetation (class 4), bare soil (class 5), water (class 6) and",
          "snow (class 11) are maintained);</li>",
          "<li><strong>All except land surface:</strong>",
          "areas classified as missing values (classes 0 and 1), cloudly",
          "(classes 7, 8 and 9), shady (classes 2 and 3), as cirrus (class 10),",
          "water (class 6) and snow (class 11) are set to NA (in other words,",
          "only pixels classified as vegetation (class 4) and bare soil", 
          "(class 5) are maintained);</li>",
          "</ul>"
        )),
        p(HTML(
          "If none of them is suitable for the user, it is possible to define",
          "a custom mask by manually selecting the classes to be masked.",
          "See the <a href='https://earth.esa.int/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm'",
          "target='_blank'>classification algorythm</a> for further details."
        )),
        p(HTML(
          "Notice that this functionality does not ensure to correctly mask",
          "all the clouds: in fact, the SCL product is an automatic",
          "classification performed by sen2cor, and it is subject to errors",
          "(see i.e. <a href='https://elib.dlr.de/119324/1/S2-validation_meeting_Main-Knorn_20180128.pdf'",
          "target='_blank'>this",
          "report</a>)."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$help_masked_perc, {
      showModal(modalDialog(
        title = "Maximum allowed cloud cover",
        p(HTML(
          "Set the maximum percentage of areas classified as cloudy which",
          "should be tolerate.",
          "If a higher value is computed, the output image is not produced."
        )),
        p(HTML(
          "Notice that the cloud covered surface is computed",
          "as the percentage of non-cloudy pixels on the total number",
          "of pixels included in the extent.",
          "So, if the user chosed not to mask outside the polygons used as extent,",
          "the considered pixels are the pixels included in the bounding box",
          "of the extent;",
          "conversely, if the area outside the polygons has been masked,",
          "the percentage is computed only on pixels within the polygons.",
          "Moreover, if the user chosed not to clip on the extent,",
          "the percentage is computed on all the pixels of the tile."
        )),
        p(HTML(
          "Notice also two more details:<ul>",
          "<li>the percentage is always computed on pixels, even if the user",
          "chosed to smooth the mask and/or to apply a buffer;</li>",
          "<li>pixels outside the current Sentinel-2 orbit (class 0 of SCL map)",
          "are threated as other cloud-masked pixels; i.e., an image which is",
          "50% nodata and with a 10% of the remaining pixels classified as cloudy",
          "will be produced only if the maximum allowed cloud surface is set",
          "> 55%.</li></ul>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_mask_smooth, {
      showModal(modalDialog(
        title = "Smooth / bufferize the cloud-covered surface?",
        size = "l",
        p(HTML(
          "By default, the cloud mask is applied at pixel level",
          "(pixels classified as cloudy are masked singularly).",
          "In this way, output image could appear grainy.",
          "Moreover, sometimes pixels adiacent to masked areas could appear",
          "cloudy in turn, so it could be useful to mask also cloud borders."
        )),
        p(HTML(
          "To prevent producing grainy output images, cloud masks can be ",
          "smoothed before applying them to output images.",
          "This has the effect to remove isolated masked pixels (holes)",
          " and to mask isolated non-masked pixels (isles).",
          "In order to mask cloud borders, a buffer can be also applied to masks.",
          "Using higher buffer radius will produce images with a higher",
          "probability to be clean, but with a higher data loss."
        )),
        a(href="www/images/mask_types.jpg", target="_blank",
          img(src="www/images/mask_types.jpg", width = "100%")),
        p(HTML(
          "The image above shows the effect of different smoothing / buffer",
          "values (click on the figure to enlarge it).",
          "Panel 1 shows a scene which was masked without applying any smoothing",
          "nor a buffer; panels 2 to 9 shows different combination of",
          "smoothing and buffer radiuses.",
          "Applying a buffer without smoothing the mask results in emphatising",
          "isolated masked pixels (panels 2 and 3), so this is a conservative",
          "choice that implicates some data loss.",
          "Conversely, using a smoothing radius allows not to loose isolated",
          "masked pixels (e.g. small urban areas), but in this way it is",
          "probably to maintain a high number of cloudy pixels (panels 4 and 7);",
          "for this reason, it is commonly recommended to use a smoothing radius",
          "in combination with a buffer radius with a similar magnitude (panel 5)."
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
          "(as an argument of sen2r() function, or with a parameter list)",
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
          "To import a JSON option file, launch <tt>sen2r()</tt>",
          "function with the path of the file as argument:"
        )),
        p(HTML(
          "<tt>sen2r(param_list = \"/path/of/the/file.json\")</tt>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    
    ## Exit and save
    
    # functions to check that all is correctly set TODO
    # return TRUE if check passes, FALSE if errors occur
    check_param <- function(param_list) {
      error_list <- check_param_list(param_list, type = "string", correct = FALSE)
      if (!is.null(error_list)) {
        # if errors occur:
        # build modal dialog
        check_param_modal <- modalDialog(
          title = "Parameter errors",
          size = "m",
          if (length(error_list)==1) {
            tagList(
              p("A parameter has not been correctly set:",
                br(), error_list),
              p("Please edit it using the GUI before continuing.")
            )
          } else {
            tagList(
              p(HTML(
                "Some parameters have not been correctly set:",
                "<ul><li>",
                paste(error_list, collapse="</li><li>"),
                "</li></ul>"
              )),
              p("Please edit them using the GUI before continuing.")
            )
          },
          easyClose = TRUE,
          footer = NULL
        )
        # show modal dialog
        showModal(check_param_modal)
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    
    # function to create a list to objects to be returned
    create_return_list <- function() {
      rl <- list()
      
      # processing steps #
      rl$preprocess <- as.logical(input$preprocess) # TRUE to perform preprocessing steps, FALSE to download SAFE only
      rl$s2_levels <- c(if(safe_req$l1c==TRUE){"l1c"}, if(safe_req$l2a==TRUE){"l2a"}) # required S2 levels ("l1c","l2a")
      rl$sel_sensor <- input$sel_sensor # sensors to use ("s2a", "s2b")
      rl$online <- as.logical(input$online) # TRUE if online mode, FALSE if offline mode
      rl$downloader <- input$downloader # downloader ("wget" or "aria2")
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
      rl$mask_type <- if (input$atm_mask==FALSE) {
        NA
      } else if (input$atm_mask_type=="custom") {
        paste0("scl_",paste(input$atm_mask_custom,collapse="_"))
      } else {
        input$atm_mask_type
      } # atmospheric masking (accepted types as in s2_mask())
      rl$max_mask <- input$max_masked_perc
      rl$mask_smooth <- if (input$mask_apply_smooth) {input$mask_smooth} else {0}
      rl$mask_buffer <- if (input$mask_apply_smooth) {input$mask_buffer} else {0}
      
      rl$clip_on_extent <- as.logical(input$clip_on_extent) # TRUE to clip (and warp) on the selected extent, FALSE to work at tiles/merged level
      rl$extent_as_mask <- as.logical(input$extent_as_mask) # TRUE to mask outside the polygons of extent, FALSE to use as boundig box
      rl$extent_name <- input$extent_name_textin # Character name of the extent
      
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
        tryCatch(
          st_crs2(input$outproj), 
          error = function(e) {st_crs(NA)}
        )$proj4string
      }
      # resampling methods ("nearest","bilinear","cubic","cubicspline","lanczos","average","mode")
      rl$resampling <- input$resampling
      rl$resampling_scl <- input$resampling_scl
      # output format (GDAL format name)
      rl$outformat <- ifelse(input$use_reference==TRUE &
                               "outformat" %in% input$reference_usefor,
                             reference()$outformat,
                             input$outformat)
      rl$index_datatype <- input$index_datatype
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
      rl$path_rgb <- rl$path_out # path of output pre-processed products # TODO add in GUI
      rl$path_indices <- if (rl$preprocess==TRUE & indices_req()==TRUE) {input$path_indices_textin} else {NA} # path of spectral indices
      rl$path_subdirs <- if (rl$preprocess==TRUE) {as.logical(input$path_subdirs)} else {NA} # logical (use subdirs)
      rl$thumbnails <- if (rl$preprocess==TRUE) {as.logical(input$check_thumbnails)} else {NA} # logical (create thumbnails)
      
      # save apihub.txt path if it was customly set
      if (!is.null(NULL) & !anyNA(NULL)) {rl$apihub_path <- rv$apihub_path}
      
      # information about package version
      rl$pkg_version <- packageVersion("sen2r") %>% as.character()
      
      return(rl)
    }
    
    # function to import saved parameters
    import_param_list <- function(pl) {
      
      # Add a progress bar while importing
      withProgress(message = 'Loading the parameters...', value = 0, {
        
        # processing steps
        updateRadioButtons(session, "preprocess", selected = pl$preprocess)
        updateCheckboxGroupInput(session, "list_levels", selected = pl$s2_levels)
        updateCheckboxGroupInput(session, "sel_sensor", selected = pl$sel_sensor)
        updateRadioButtons(session, "online", selected = pl$online)
        updateRadioButtons(session, "downloader", selected = pl$downloader)
        updateRadioButtons(session, "overwrite_safe", selected = pl$overwrite_safe)
        updateRadioButtons(session, "rm_safe", selected = pl$rm_safe)
        updateRadioButtons(session, "step_atmcorr", selected = pl$step_atmcorr)
        setProgress(0.2)
        
        # spatio-temporal selection
        if (anyNA(pl$timewindow)) {
          updateRadioButtons(session, "query_time", selected = FALSE)
        } else {
          updateRadioButtons(session, "query_time", selected = TRUE)
          updateDateRangeInput(session, "timewindow", start=pl$timewindow[1], end=pl$timewindow[2])
          updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
        }
        if (anyNA(pl$extent) & pl$online == FALSE) {
          updateRadioButtons(session, "query_space", selected = FALSE)
        } else {
          updateRadioButtons(session, "query_space", selected = TRUE)
          # rl$s2tiles_selected <- input$tiles_checkbox # selected tile IDs
          updateRadioButtons(session, "extent_as_mask", selected = rv$extent_as_mask)
        }
        setProgress(0.4)
        
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
        updateSliderInput(session, "max_masked_perc",
                          value = ifelse(is.na(pl$mask_type),80,pl$max_mask))
        updateNumericInput(session, "mask_apply_smooth", 
                           value = if (all(c(pl$mask_smooth, pl$mask_buffer)==0)) {FALSE} else {TRUE})
        updateNumericInput(session, "mask_smooth", value = pl$mask_smooth)
        updateNumericInput(session, "mask_buffer", value = pl$mask_buffer)
        updateRadioButtons(session, "atm_mask_type",
                           selected = if (is.na(pl$mask_type)) {"cloud_medium_proba"} else
                             if (grepl("^scl_",pl$mask_type)) {"custom"} else {pl$mask_type})
        updateCheckboxGroupInput(session, "atm_mask_custom",
                           selected = if (grepl("^scl\\_",pl$mask_type)) {strsplit(pl$mask_type,"_")[[1]][-1]} else {c(0,8:9)})
        updateRadioButtons(session, "index_source", selected = pl$index_source)
        updateRadioButtons(session, "clip_on_extent", selected = pl$clip_on_extent)
        updateRadioButtons(session, "extent_name_textin", selected = pl$extent_name)
        updateRadioButtons(session, "keep_tiles", selected = ifelse(is.na(pl$path_tiles),FALSE,TRUE))
        updateRadioButtons(session, "keep_merged", selected = ifelse(is.na(pl$path_merged),FALSE,TRUE))
        setProgress(0.6)
        
        
        # set directories
        updateTextInput(session, "path_l1c_textin", value = pl$path_l1c)
        updateTextInput(session, "path_l2a_textin", value = pl$path_l2a)
        updateTextInput(session, "path_tiles_textin", value = pl$path_tiles)
        updateTextInput(session, "path_merged_textin", value = pl$path_merged)
        updateTextInput(session, "path_out_textin", value = pl$path_out)
        updateTextInput(session, "path_indices_textin", value = pl$path_indices)
        updateRadioButtons(session, "path_subdirs", selected = pl$path_subdirs)
        updateRadioButtons(session, "check_thumbnails", selected = pl$thumbnails)
        
        # update apihub path
        rv$apihub_path <- pl$apihub_path
        
        # output geometry
        updateTextInput(session, "reference_file_textin", value = pl$reference_path)
        updateRadioButtons(session, "use_reference", selected = ifelse(is.na(pl$reference_path), FALSE, TRUE))
        
        if (is.na(pl$reference_path)) {
          updateRadioButtons(session, "rescale", selected = if(anyNA(pl$res)) {FALSE} else {TRUE})
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
        updateRadioButtons(session, "index_datatype", selected = pl$index_datatype)
        updateRadioButtons(session, "resampling", selected = pl$resampling)
        updateRadioButtons(session, "resampling_scl", selected = pl$resampling_scl)
        updateRadioButtons(session, "compression", selected = ifelse(pl$outformat=="GTiff",
                                                                     pl$compression,
                                                                     character(0)))
        updateRadioButtons(session, "overwrite", selected = pl$overwrite)
        setProgress(0.8)
        
        # update extent (at the end, not to interfer with other events
        # (the delay is required to update the map after the map is charged)
        shinyjs::delay(5E3, {
          update_extent("imported", custom_source = pl$extent)
          updateCheckboxGroupInput(session, "tiles_checkbox",
                                   selected = pl$s2tiles_selected)
          
        })
        setProgress(1)
        # sendSweetAlert(
        #   session, 
        #   title = "Parameters loaded.", 
        #   text = paste(
        #     "Parameters were correctly loaded.",
        #     "If you do not see the selected extent on the map, please use",
        #     "the button \"Refresh the map\"",
        #     "(notice that loading a big vector file can take some time)."
        #   ),
        #   type = "success",
        #   btn_labels = "Ok"
        # )
        
      })
      
    }
    
    # if Return is pressend, exit from GUI and return values
    observeEvent(input$return_param, {
      return_list <- create_return_list() # run creation of return_list
      check_param_result <- check_param(return_list)
      if (check_param_result) {
        shinyjs::js$closeWindow()
        stopApp(return_list)
      }
    })
    
    # if Exit is pressend, exit from GUI
    observeEvent(input$exit_gui, {
      shinyjs::js$closeWindow()
      stopApp()
    })
    
    # if Export is pressed, export the values (using server-side button)
    observe({
      shinyFileSave(input, "export_param", roots=volumes, session=session)
      export_param_path <- parseSavePath(volumes, input$export_param)
      if (nrow(export_param_path)>0) {
        return_list <- create_return_list() # run creation of return_list
        check_param_result <- check_param(return_list)
        if (check_param_result) {
          writeLines(toJSON(return_list, pretty=TRUE),
                     as.character(export_param_path$datapath))
        }
      }
    })
    
    # # if Export is pressed, export the values (using client-side button)
    # output$export_param <- downloadHandler(
    #   filename = function() {
    #     paste0("sen2r_", strftime(Sys.Date(),"%Y%m%d"), ".json")
    #   },
    #   content = function(file) {
    #     return_list <- create_return_list() # run creation of return_list
    #     check_param_result <- check_param(return_list)
    #     if (check_param_result) {
    #       writeLines(toJSON(return_list, pretty=TRUE), file)
    #     }
    #   },
    #   contentType = "application/json"
    # )
    
    # # if Import is pressed, read a json object (using server-side button)
    shinyFileChoose(input, "import_param", roots=volumes, session=session,
                    filetypes = c("JSON"="json"))
    
    observeEvent(input$import_param, {
      
      # server-side button
      import_param_path <- input$import_param
      import_param_path <- parseFilePaths(volumes,input$import_param)
      rv$imported_param <- if (nrow(import_param_path)>0) {
        import_param_path$datapath %>%
          as.character() %>%
          readLines() %>%
          fromJSON()
      } else {
        NULL
      }
      
      # # client-side button
      # rv$imported_param <- if (nrow(input$import_param$datapath)>0) {
      #   readLines(fromJSON(input$import_param$datapath))
      # } else {
      #   NULL
      # }
      
      if (!is.null(rv$imported_param)) {
        import_param_list(rv$imported_param)
        rv$imported_param <- NULL
      }
    })
    
    observeEvent(param_list, {
      if (!is.null(param_list)) {
        import_param_list(param_list)
      }
    })
    
    ## end of path module ##
    
    # Closing the connection when window is closed
    session$onSessionEnded(function() {
      stopApp()
    })
    
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

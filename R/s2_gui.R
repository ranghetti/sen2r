#' @title Launch the GUI for Sentinel-2 products
#' @description Launch the GUI to set parameters for the processing
#'  chain of Sentinel-2 products.
#' @param param_list List of parameters for initialising the GUI values
#'  (if empty, default values are used).
#' @param thunderforest_api Character value with the API for thunderforest
#'  layers (now not used).
#' @return A list of parameters.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON toJSON
#' @import data.table
#' @importFrom geojsonio geojson_json
#' @importFrom leaflet addLayersControl addMapPane addPolygons addProviderTiles
#'  addTiles clearShapes fitBounds hideGroup labelOptions layersControlOptions
#'  leaflet leafletOutput leafletProxy pathOptions removeLayersControl removeShape
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#'  removeDrawToolbar
#' @importFrom mapedit editModUI
#' @importFrom utils packageVersion
#' @importFrom sf st_coordinates st_crs st_geometry st_intersects st_polygon
#'  st_zm st_read st_bbox st_as_sfc st_transform
#' @importFrom shiny a actionButton actionLink addResourcePath br callModule
#'  checkboxGroupInput checkboxInput column conditionalPanel dateRangeInput
#'  div downloadButton downloadHandler em fileInput fluidRow h2 h3 helpText hr
#'  HTML htmlOutput icon img incProgress isolate NS numericInput observe p
#'  radioButtons reactive reactiveVal reactiveValues removeModal renderText
#'  renderUI req runApp selectInput setProgress shinyApp showModal sliderInput
#'  span stopApp strong tagList textInput uiOutput updateCheckboxGroupInput 
#'  updateCheckboxInput updateDateRangeInput updateNumericInput updateSliderInput
#'  updateSelectInput updateRadioButtons updateTextInput withMathJax withProgress
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath
#'  shinyDirButton shinyDirChoose shinyFileChoose shinyFileSave
#'  shinyFilesButton shinySaveButton
#' @importFrom shinyjs click delay disable enable hidden toggle useShinyjs extendShinyjs
#' @importFrom shinyWidgets sendSweetAlert switchInput pickerInput updatePickerInput updateSwitchInput
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
  
  # Check shiny & co. to be installed
  missing_pkgs <- !sapply(
    c("shiny", "shinydashboard", "shinyFiles", "shinyjs", "shinyWidgets", 
      "leaflet", "leaflet.extras", "mapedit"), 
    requireNamespace, quietly = TRUE
  )
  if (any(missing_pkgs)) {
    print_message(
      type = "error",
      "packages '",
      paste(names(missing_pkgs)[missing_pkgs], collapse = "', '"),"' ",
      "are required to run the sen2r Shiny GUI."
    )
  }

  # TODO: populate parameter values with param_list content, if provided
  
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  # shiny
  s2_gui.ui <- dashboardPage(
    title = "sen2r: Find, Download and Process Sentinel-2 Data",
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      
      # logo
      div(
        style = "text-align:center;padding-top:17px;padding-bottom:30px;",
        a(
          href='http://sen2r.ranghetti.info',
          target = "_blank",
          uiOutput("img_logo")
        )
      ),
      
      div(
        style="position:absolute;top:230px;",
        sidebarMenu(
          menuItem("Product selection", tabName = "tab_steps", icon = icon("image"))
        ),
        sidebarMenu(
          menuItem("Spatial-temporal selection", tabName = "tab_query", icon = icon("clone"))
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
          ),
          conditionalPanel(
            condition = "input.list_prods.indexOf('rgbimages') != -1",
            sidebarMenu(
              menuItem("RGB images selection", tabName = "tab_rgb", icon = icon("palette"))
            )
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
        style="position:absolute;top:460px;",
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
            "Save options as...", "Save parameters as...",
            filetype=list(json="json"),
            class="darkbutton"
          )
        ),
        p(style="margin-top:5pt;",
          shinyFilesButton(
            "import_param",
            "Load options...", "Import a JSON file with parameters",
            multiple=FALSE,
            class="darkbutton"
          )
        ),
        p(style="margin-top:20pt;",
          shinySaveButton(
            "save_log",
            "Create log...", "Create a log file...",
            filetype=list(logfile="log", textfile="txt"),
            class="darkbutton"
          )
        ),
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
            onclick ="window.open('http://sen2r.ranghetti.info', '_blank')",
            # onclick ="window.open('http://sen2r.ranghetti.info/articles/sen2r_gui.html', '_blank')",
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
            collapsible = TRUE,
            radioButtons(
              "preprocess", NULL,
              choiceNames = list(
                span(
                  "Raw files in ",
                  a("raw SAFE format",
                    href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/data-formats",
                    target="_blank"),
                  " (downloaded and/or corrected with Sen2Cor)"
                ),
                "Processed spatial files (surface reflectance, spectral indices, ...) in custom format"
              ),
              choiceValues = list(FALSE, TRUE),
              selected=TRUE,
              inline = FALSE
            )
          )),
          
          fluidRow(box(
            title="Products and sensors",
            width=12,
            collapsible = TRUE,
            
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
                                                        # "TCI (true-color) RGB 8-bit image",
                                                        "Spectral indices",
                                                        "RGB images"),
                                     # choiceValues = list("TOA", "BOA", "SCL", "TCI", "indices", "rgbimages"),
                                     choiceValues = list("TOA", "BOA", "SCL", "indices", "rgbimages"),
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
            collapsible = TRUE,
            
            fluidRow(
              # L1C directory
              column(
                width=6,
                conditionalPanel(
                  condition = "output.req_l1c == 'TRUE'",
                  div(style="display:inline-block;vertical-align:top;padding-bottom:5px;",
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
              column(
                width=6,
                conditionalPanel(
                  condition = "output.req_l2a == 'TRUE'",
                  div(style="display:inline-block;vertical-align:top;padding-bottom:5px;",
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
            
            hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
            
            fluidRow(
              column(
                width=3,
                
                # online_mode (online/offline mode)
                radioButtons(
                  "online",
                  label = span(
                    "Download mode\u2000",
                    actionLink("help_online", icon("question-circle"))#,
                    # if (Sys.info()["sysname"] == "Windows") {
                    #   span(
                    #     "\u2000",
                    #     actionLink("fix_online", icon("warning"))
                    #   )
                    # } else {
                    #   
                    # }
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
                    checkboxInput(
                      "make_lta_order",
                      label = span(
                        "Order from LTA\u2000",
                        actionLink("help_lta_order", icon("question-circle"))
                      ),
                      value = TRUE
                    ),
                    actionButton(
                      "scihub_md",
                      label = "\u2000Login to SciHub",
                      icon = icon("user-circle")
                    )
                  )
                )
              ),
              column(
                width=4,
                conditionalPanel(
                  condition = "input.online == 'TRUE'",
                  radioButtons(
                    "downloader",
                    label = span(
                      "Downloader\u2000",
                      actionLink("help_downloader", icon("question-circle"))
                    ),
                    choiceNames = list("Built-in", "aria2"),
                    choiceValues = list("builtin", "aria2"),
                    selected = "builtin",
                    inline = TRUE
                  ),
                  sliderInput(
                    "max_cloud_safe_perc",
                    label = span(
                      "Max. SAFE cloud cover\u2000",
                      actionLink("help_cloud_perc", icon("question-circle"))
                    ),
                    min = 0, max = 100, value = 100,
                    step = 1, post = "%",
                    ticks = FALSE
                  )
                )
                
              ),
              column(
                width=5,
                
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
                             choices = list("Yes" = "yes",
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
              title="Atmospheric correction",
              width=12,
              collapsible = TRUE,
              fluidRow(
                column(
                width = 7,
                radioButtons(
                  "step_atmcorr",
                  label = span(
                    "Method to obtain level-2A corrected images\u2000",
                    actionLink("help_step_atmcorr", icon("question-circle"))
                  ),
                  choiceNames = list(
                    "Use only level-2A images available locally or online",
                    "Use Sen2Cor only for level-2A products not available locally or online",
                    "Always correct level-1C images with Sen2Cor locally"
                  ),
                  choiceValues = list("l2a","auto", "scihub"),
                  selected = "auto"
                ),
              ),
              column(
                width = 5,
                radioButtons(
                  "use_dem",
                  label = span(
                    "Apply a topographic correction?\u2000",
                    actionLink("help_use_dem", icon("question-circle"))
                  ),
                  choices = list(
                    "Yes" = "TRUE",
                    "No" = "FALSE",
                    "Keep default" = "NA"
                  ),
                  selected = "NA"
                )
              )
              ),
              
            )) # end of fluidRow/box "sen2cor options"
          )
          
        ), # end of tabItem tab_steps
        
        
        ### Querying parameters (spatio-temporal) ###
        tabItem(
          tabName = "tab_query",
          
          fluidRow(box(
            title="Temporal range",
            width=12,
            collapsible = TRUE,
            
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
                  width=8,
                  dateRangeInput("timewindow", label = "Time interval")
                ),
                
                column(
                  width=4,
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
            
          )), # end of fluidRow/box "Temporal range"
          
          fluidRow(box(
            title="Area of interest",
            width=12,
            collapsible = TRUE,
            
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
                
                # Buttons to load the extent with modal dialogs
                column(
                  width=4,
                  div(
                    style="padding-bottom:5px;",
                    strong("Specify the extent:\u2000")
                  ),
                  div(
                    style="padding-bottom:10px;",
                    actionButton(
                      "button_extent_bbox",
                      label = "\u2000Set a bounding box",
                      width = "100%",
                      icon=icon("object-group")
                    )
                  ),
                  div(
                    style="padding-bottom:10px;",
                    actionButton(
                      "button_extent_vectfile",
                      label = "\u2000Load a vector file",
                      width = "100%",
                      icon=icon("upload")
                    )
                  ),
                  div(
                    style="padding-bottom:10px;",
                    actionButton(
                      "button_extent_draw",
                      label = "\u2000Draw it on the map",
                      width = "100%",
                      icon=icon("paint-brush")
                    )
                  )
                ),
                
                column(
                  width=8,
                  fluidRow(
                    
                    # Select tiles and orbits
                    column(
                      width=6,
                      pickerInput(
                        "tiles_checkbox", "Tiles selected",
                        choices = character(0),
                        options = list(
                          `selected-text-format` = "count > 3",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          title = "All overlapping tiles"
                        ),
                        multiple = TRUE
                      )
                    ),
                    
                    column(
                      width=6,
                      pickerInput(
                        "orbits_checkbox",
                        span(
                          "Orbits selected\u2000",
                          actionLink("help_orbits", icon("question-circle"))
                        ),
                        choices = str_pad2(1:143, 3, "left", "0"),
                        options = list(
                          `selected-text-format` = "count > 6",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          title = "All overlapping orbits"
                        ),
                        multiple = TRUE
                      )
                      # helpText(em("Not yet implemented."))
                    )
                  ),
                  
                  hr(style="margin-top: 0px; margin-bottom: 13px;"),
                  
                  # Specify the extent name and upload the map
                  fluidRow(
                    
                    column(
                      width=6,
                      textInput(
                        "extent_name_textin",
                        span(
                          strong("Extent name:\u2000"),
                          actionLink("help_extent_name", icon("question-circle"))
                        ),
                        "sen2r"
                      )
                    ),
                    
                    column(
                      width=6,
                      div(
                        style="margin-top:25px;padding-bottom:10px;",
                        actionButton(
                          "button_refresh_map",
                          label = "\u2000Refresh the map",
                          width = "100%",
                          icon=icon("retweet")
                        )
                      )
                    )
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
            
          )) # end of fluidRow/box "Area of interest"
          
        ), # end of tabItem tab_query
        
        
        ### Output tab (geometry and parameters) ###
        tabItem(
          tabName = "tab_prepro",
          title="Processing options",
          
          fluidRow(box(
            title="Output files",
            width=12,
            collapsible = TRUE,
            
            fluidRow(
              column(
                width=12,
                conditionalPanel(
                  condition = "input.list_prods.length > 1 || (input.list_prods.length > 0 && input.list_prods.indexOf('indices') == -1)",
                  div(div(style="display:inline-block;vertical-align:top;padding-bottom:5px;",
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
                                           "BigTiff" = "BigTIFF",
                                           "ENVI" = "ENVI"),
                            # TODO add others common formats
                            selected = "GTiff")
              ),
              
              conditionalPanel(
                condition = "input.outformat == 'GTiff' | input.outformat == 'BigTIFF' ",
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
            title = "Output extent",
            collapsible = TRUE,
            
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
            collapsible = TRUE,
            
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
                     "to check Sen2Cor settings."))
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
                  # "No data and clouds (any probability)" = "cloud_low_proba",
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
                    HTML("<font style=\"family:monospace;background-color:#7b7d7b;color:white;\">\u20027\u2002</font>\u2002Unclassified"),
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
                min = 0, max = 100, value = 100,
                step = 1, post = "%",
                ticks = FALSE
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
          
          fluidRow(
            conditionalPanel(
              condition = "input.clip_on_extent == 'TRUE'",
              box(
                width=8,
                title = "Output geometry",
                collapsible = TRUE,
                
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
                      "\u2001"
                    ),
                    div(
                      style="display:inline-block;vertical-align:top;width:calc(100% - 77pt - 10px - 10pt);",
                      textInput("reference_file_textin", label = NULL, "", width="100%")
                    ),
                    div(
                      style="display:inline-block;vertical-align:top;width:10pt;padding-left:5px;",
                      uiOutput("reference_file_message")
                    )
                    
                  ), # end of column reference
                  
                  conditionalPanel(
                    condition = "input.use_reference == 'TRUE'",
                    div(
                      div(
                        style = "display:inline-block;vertical-align:top;padding-top:2px;",
                        strong("Use the file to define:\u2001")
                      ),
                      div(
                        style = "display:inline-block;vertical-align:top;",
                        checkboxGroupInput(
                          "reference_usefor", label = NULL,
                          choices = list("Projection" = "proj",
                                         "Resolution" = "res"),
                          #"Extent" = "ext", # TODO
                          # "Output format" = "outformat"),
                          selected = c("proj","res"),
                          inline = TRUE
                        )
                      )
                    )
                  ) # end of its conditional panel
                  
                ), # end of fluidrow for spatial reference
                
                hr(style="margin-top: 0em; margin-bottom: 0.75em;"),
                
                fluidRow( # fluidrow for output geometry settings
                  
                  column(
                    width=6,
                    
                    strong("Spatial resolution"),
                    conditionalPanel(
                      condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('res') < 0",
                      div(
                        style="margin-top:0.25em;",
                        radioButtons("rescale", label = NULL,
                                     choices = list("Native" = FALSE,
                                                    "Custom" = TRUE),
                                     selected = FALSE,
                                     inline = TRUE)
                      ),
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
                        numericInput("resolution_custom", "Specify resolution (m)",
                                     # width="100px",
                                     value = 10,
                                     min = 0)
                      )
                      
                    ),
                    
                    htmlOutput("outres_message")
                    
                  ), # end of column spatial resolution
                  
                  column(
                    width=6,
                    
                    div(style="margin-bottom:-0.25em;", strong("Output projection")),
                    conditionalPanel(
                      condition = "input.use_reference == 'FALSE' || input.reference_usefor.indexOf('proj') < 0",
                      radioButtons(
                        "reproj", label = NULL,
                        choiceNames = list(
                          "Input projection (do not reproject)",
                          span(
                            "Custom projection\u2000",
                            actionLink("help_outprojinput", icon("question-circle"))
                          )
                        ),
                        choiceValues = c(FALSE, TRUE),
                        selected = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.reproj == 'TRUE'",
                        textInput("outproj", NULL,
                                  value=character(0), width="100%")
                      )
                    ),
                    
                    htmlOutput("outproj_message"),
                    
                    # ), # end of column output projection
                    # 
                    # column(
                    #   width=4,
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
                
              ) # end of column "Output geometry"
            ), # end of conditionalpanel on output geometry
            
            box(
              width=4,
              title = span(
                "Processing order",
                # "Processing order / parallelisation",
                span(
                  style = "font-size:smaller;", "\u2000",
                  actionLink("info_parallelisation", icon("info-circle"))
                )
              ),
              collapsible = TRUE,
              
              # processing order
              # column(
              #   width=4,
              # radioButtons(
              selectInput(
                "processing_order",
                span(
                  "Processing order\u2000",
                  actionLink("help_processing_order", icon("question-circle"))
                ),
                choices = list(
                  "Process by groups" = "by_groups",
                  "Process by date" = "by_date",
                  "Mixed processing" = "mixed",
                  "Process step by step" = "by_step"
                ),
                selected = "by_groups",
                width = "200%"
              ),
              # ),
              # 
              # # parallelise?
              # column(
              #   width=3,
              radioButtons(
                "parallel",
                label = span("Parallel computation?"),
                choiceNames = list("Yes", "No"),
                choiceValues = list(TRUE, FALSE),
                selected = TRUE,
                inline = TRUE
              ),
              # ),
              # 
              # # number of cores
              # column(
              #   width=5,
              conditionalPanel(
                condition = "input.parallel == 'TRUE'",
                div(
                  p(strong("Number of CPU cores")),
                  switchInput(
                    "n_cores_auto",
                    value = TRUE,
                    size = "mini",
                    onLabel = "Auto", offLabel = "Manual"
                  ),
                  hidden(sliderInput(
                    "n_cores", label = NULL,
                    min = 1, max = 8,
                    value = 6,
                    ticks = FALSE
                  ))
                )
                
              ) # end of "Processing order / parallelisation"
              
            )
            
          ) # end of fluidRow output geometry / parallelisation
          
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
                collapsible = TRUE,
                
                div(div(style="display:inline-block;vertical-align:top;padding-bottom:5px;",
                        strong("Directory for spectral indices: \u00a0")),
                    div(style="display:inline-block;vertical-align:top;",
                        htmlOutput("path_indices_errormess")),
                    div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                            shinyDirButton("path_indices_sel", "Select", "Specify directory for spectral indices")),
                        div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                            textInput("path_indices_textin", NULL, "", placeholder = "The same used for output products")))),
                
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
          
        ), # end of tabItem tab_index
        
        
        ### RGB images tab ###
        tabItem(
          tabName = "tab_rgb",
          title="RGB images",
          
          conditionalPanel(
            condition = "input.preprocess == 'TRUE'",
            fluidRow(
              box(
                width=12,
                title="RGB images selection",
                collapsible = TRUE,
                
                div(div(style="display:inline-block;vertical-align:top;padding-bottom:5px;",
                        strong("Directory for RGB images: \u00a0")),
                    div(style="display:inline-block;vertical-align:top;",
                        htmlOutput("path_rgb_errormess")),
                    div(div(style="display:inline-block;vertical-align:top;width:50pt;",
                            shinyDirButton("path_rgb_sel", "Select", "Specify directory for RGB images")),
                        div(style="display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                            textInput("path_rgb_textin", NULL, "", placeholder = "The same used for output products")))),
                
                uiOutput("checkbox_list_rgb"),
                
                div(
                  style = "display:inline-block;padding-top:10px;padding-right:10px;",
                  actionButton(
                    "new_rgb",
                    label = "\u2000Define custom RGB image",
                    icon = icon("plus")
                  )
                ),
                div(
                  style = "display:inline-block;padding-top:10px;",
                  actionButton(
                    "rm_rgb",
                    label = "\u2000Remove unselected RGB from list",
                    icon = icon("trash-alt")
                  )
                )
                
                
                
              ) # end of box
            ) # end of fluidRow
          ) # end of conditionalpanel on tab_rgb
          
        ) # end of tabItem tab_rgb
        
      ) # end of tabItems
    ) # end of dashboardBody
    
  ) # end of s2_gui.ui dashboardPage
  
  s2_gui.server <- function(input, output, session) {
    
    # to avoid NOTE on check
    . <- checked <- NULL
    
    # open a waiting modal dialog
    showModal(modalDialog(
      div(
        p(
          style = paste0("text-align:center;font-size:500%;color:grey;"),
          icon("spinner", class = "fa-pulse")
        ),
        p(
          style = paste0("text-align:center;"),
          "GUI is initialising, please wait..."
        )
      ),
      size = "s",
      easyClose = FALSE,
      footer = NULL
    ))
    
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
    
    # extract and import tiles kml
    s2tiles <- s2_tiles()
    
    
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
        style="height:55px;",
        div(style="padding-bottom:5px;", strong("SAFE levels needed:")),
        if (safe_req$l1c==FALSE & safe_req$l2a==FALSE) {
          span(style="color:red", "Select at least one product or index.")
        },
        if (safe_req$l1c==TRUE) {
          a("Level-1C",
            href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-1c",
            target="_blank")
        },
        if (safe_req$l1c==TRUE & safe_req$l2a==TRUE) {
          br()
        },
        if (safe_req$l2a==TRUE) {
          a("Level-2A",
            href="https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a",
            target="_blank")
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
        updateRadioButtons(session, "downloader", selected = "builtin")
        disable("downloader")
      } else {
        enable("downloader")
      }
    })
    
    # Edit scihub credentials
    observeEvent(input$scihub_md, {
      
      # open the modalDialog
      # showModal(.scihub_modal(
      #   username = if(!is.null(input$scihub_username)){input$scihub_username}else{NA},
      #   password = if(!is.null(input$scihub_password)){input$scihub_password}else{NA}
      # ))
      showModal(.scihub_modal())
      
      # do not activate save button until bot have been provided
      observeEvent(c(input$scihub_username, input$scihub_password), {
        if (any(input$scihub_username == "", input$scihub_password == "")) {
          disable("save_apihub")
          disable("apihub_path_sel")
        } else {
          enable("save_apihub")
          enable("apihub_path_sel")
        }
      })
      
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
        if (!any(c(is.na(rv$apihub_path), length(nn(rv$apihub_path))==0))) {
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
        apihub_path = as.character(rv$apihub_path),
        # append = input$apihub_multiple # removed after disabling apihub multiple login from the gui
        append = FALSE,
        check = FALSE
      )
      removeModal()
    })
    
    ## Parallelisation / processing order
    observeEvent(input$n_cores_auto, ignoreInit = TRUE, {
      toggle("n_cores")
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
          names(sf::st_geometry(x)) <- NULL
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
          x <- if (is.character(custom_source)) {
            st_read(custom_source, quiet=TRUE)
          } else {
            custom_source
          }
          x <- st_transform(st_zm(x), 4326)
          names(sf::st_geometry(x)) <- NULL
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
        
        rv$draw_tiles_overlapping <- tiles_intersects(rv$extent, all=TRUE, out_format="sf", .s2tiles=s2tiles)
        names(sf::st_geometry(rv$draw_tiles_overlapping)) <- NULL
        
        if (attr(rv$extent, "new")) {
          # update the list of tiles
          # updateCheckboxGroupInput(
          #   session, "tiles_checkbox",
          #   choiceNames = lapply(rv$draw_tiles_overlapping$tile_id, span, style="family:monospace;"),
          #   choiceValues = rv$draw_tiles_overlapping$tile_id,
          #   selected = rv$draw_tiles_overlapping$tile_id,
          #   inline = nrow(rv$draw_tiles_overlapping) > 12 # inline if they are many
          # )
          updatePickerInput(
            session, "tiles_checkbox",
            choices = rv$draw_tiles_overlapping$tile_id,
            selected = tiles_intersects(rv$extent, .s2tiles = rv$draw_tiles_overlapping)
          )
        }
        
        # reset and update the map
        react_map(base_map())
        rv$draw_tiles_overlapping_ll <- st_transform(rv$draw_tiles_overlapping, 4326)
        clearShapes(leafletProxy("view_map"))
        fitBounds(
          leafletProxy("view_map"),
          lng1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[,"X"]),
          lat1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[,"Y"]),
          lng2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[,"X"]),
          lat2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[,"Y"])
        )
        # add extent
        addPolygons(
          leafletProxy("view_map"),
          data = rv$extent,
          group = "Extent",
          options = pathOptions(pane = "extent"),
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
        
        # update the [un]selected tiles on the map
        rv$update_tiles_on_map <- sample(1E6, 1)
        
      } else {
        rv$draw_tiles_overlapping <- NULL
        # empty the list of tiles
        updatePickerInput(session, "tiles_checkbox", choices = character(0))
        # reset the map
        react_map(base_map())
        # clearShapes(leafletProxy("view_map"))
      }
      
      return(TRUE)
      
    }
    
    
    #-- Create the map (once) --#
    base_map <- function() {
      m <- leaflet()
      
      # add tiles
      m <- addTiles(m, group = "OpenStreetMap")
      # m <- addTiles(
      #   m, paste0(
      #     "https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png",
      #     if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}
      #   ), group = "OpenStreetMap Outdoors"
      # )
      # m <- addProviderTiles(m, providers$OpenTopoMap, group = "OpenTopoMap")
      m <- addTiles(
        m, "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        group = "OpenTopoMap"
      )
      # m <- addProviderTiles(m, providers$CartoDB.Positron, group = "CartoDB")
      m <- addTiles(
        m, "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
        group = "CartoDB"
      )
      # m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "Satellite")
      m <- addTiles(
        m, "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        group = "Satellite"
      )
      # m <- addProviderTiles(m, providers$CartoDB.PositronOnlyLabels, group = "Dark names")
      m <- addTiles(
        m, "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
        group = "Satellite"
      )
      # m <- addProviderTiles(m, providers$CartoDB.DarkMatterOnlyLabels, group = "Dark names")
      # m <- addTiles(
      #   m, "https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
      #   group = "Dark names"
      # )
      
      # Order group
      m <- addMapPane(m, "extent", zIndex = 430)
      m <- addMapPane(m, "tiles_selected", zIndex = 420)
      m <- addMapPane(m, "tiles_notselected", zIndex = 410)
      
      # view and controls
      m <- addLayersControl(
        m,
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      )
      m
    }
    react_map <- reactiveVal({
      basemap <- base_map()
      removeLayersControl(basemap)
      addLayersControl(
        basemap,
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("S2 tiles","Extent"),
        options = layersControlOptions(collapsed = TRUE)
      )
    })
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
    
    # message for bboxcrs
    output$bboxproj_message <- renderUI({
      bboxcrs_validated <- tryCatch(
        st_crs2(input$bboxproj),
        error = function(e) {st_crs(NA)}
      )
      if (input$bboxproj=="") {
        rv$bboxcrs <- st_crs(NA)
        ""
      } else if (is.na(bboxcrs_validated)) {
        rv$bboxcrs <- st_crs(NA)
        # span(style="color:red", "\u2718") # ballot
        span(style="color:red",
             "Insert a valid projection (UTM timezone, EPSG code or PROJ4 string).")
      } else {
        rv$bboxcrs <- bboxcrs_validated
        # span(style="color:darkgreen", "\u2714") # check
        if (projname(bboxcrs_validated) != "unknown") {
          div(style="color:darkgreen",
              strong("Selected projection:"), br(),
              projname(bboxcrs_validated))
        } else if (!is.na(bboxcrs_validated$epsg)) {
          div(style="color:darkgreen", 
              strong("Selected projection:"), br(),
              paste("EPSG:", bboxcrs_validated$epsg))
        } else {
          div(style="color:darkgreen", strong("Valid projection"))
        }
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
      rv$bboxcrs
    ), {
      
      # Check that the bounding box is valid
      if (!anyNA(c(input$bbox_xmin, input$bbox_xmax,
                   input$bbox_ymin, input$bbox_ymax)) &
          !(is.null(rv$bboxcrs) || is.na(rv$bboxcrs))) {
        if (input$bbox_xmin != input$bbox_xmax &
            input$bbox_ymin != input$bbox_ymax) {
          # create the polygon
          rv$bbox_polygon <- st_transform(
            st_as_sfc(
              st_bbox(
                c("xmin" = input$bbox_xmin,
                  "ymin" = input$bbox_ymin,
                  "xmax" = input$bbox_xmax,
                  "ymax" = input$bbox_ymax),
                crs = rv$bboxcrs
              )
            ),
            4326
          )
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
        clearShapes(leafletProxy("view_map_bbox"))
        fitBounds(
          leafletProxy("view_map_bbox"),
          lng1 = as.numeric(rv$bbox_ll$xmin-(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
          lat1 = as.numeric(rv$bbox_ll$ymin-(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3),
          lng2 = as.numeric(rv$bbox_ll$xmax+(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
          lat2 = as.numeric(rv$bbox_ll$ymax+(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3)
        )
        addPolygons(
          leafletProxy("view_map_bbox"),
          data = rv$bbox_polygon,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
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
    
    shinyFileChoose(input, "path_vectfile_sel", roots=volumes, session=session)
    observeEvent(input$path_vectfile_sel, {
      # uploaded_exts <- gsub("^.+\\.(.+)$","\\1",input$path_vectfile_sel$name)
      # # checks
      # if (length(unique(gsub("\\..+$","",input$path_vectfile_sel$name))) > 1) {
      #   # if more than one vector were chosen, give an alert and do not use the file
      #   sendSweetAlert(
      #     session,
      #     title = "Invalid vector",
      #     text = paste(
      #       "Please select a single vector",
      #       "(multiple selection is allowed only for shapefiles)."
      #     ),
      #     type = "error",
      #     btn_labels = "Ok"
      #   )
      #   rv$vectfile_path <- ""
      # } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp","shx","dbf","prj")) {
      #   # if a single file was chosen and it is not a shapefile, use it
      #   rv$vectfile_path <- input$path_vectfile_sel$datapath
      # } else if (anyNA(match(c("shp","shx","dbf","prj"),uploaded_exts))) {
      #   # if a shapefile was chosen but some files are missing, do not use it
      #   sendSweetAlert(
      #     session,
      #     title = "Incomplete shapefile",
      #     text = paste(
      #       "Please select all the files of the shapefile",
      #       "(at most .shp, .shx, .prj, .dbf)."
      #     ),
      #     type = "error",
      #     btn_labels = "Ok"
      #   )
      #   rv$vectfile_path <- ""
      # } else {
      #   # if a shapefile was chosen and all the files are present,
      #   # rename the uploaded files in order to have the same filename and use them
      # 
      #   path_vectfile_sel_new_datapath <- file.path(
      #     dirname(input$path_vectfile_sel$datapath), input$path_vectfile_sel$name
      #   )
      #   for(i in seq_len(nrow(input$path_vectfile_sel))) {
      #     file.rename(input$path_vectfile_sel$datapath[i], path_vectfile_sel_new_datapath[i])
      #   }
      #   rv$vectfile_path <- path_vectfile_sel_new_datapath[
      #     grepl("\\.shp$", input$path_vectfile_sel$name)
      #     ]
      # }
      vectfile_path <- parseFilePaths(volumes,input$path_vectfile_sel)
      rv$vectfile_path <- if (nrow(vectfile_path)>0) {
        as.character(vectfile_path$datapath)
      } else {
        NULL
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
      req(rv$vectfile_path)
      
      # Check that the vector is valid
      rv$vectfile_polygon <- tryCatch(
        {
          x <- st_read(rv$vectfile_path, quiet=TRUE)
          x <-st_transform(st_zm(x), 4326)
          names(sf::st_geometry(x)) <- NULL
          attr(x, "valid") <- TRUE
          attr(x, "new") <- TRUE
          x
        },
        error = function(e) {x <- st_polygon(); attr(x, "valid") <- FALSE; x}
      )
      
      if(attr(rv$vectfile_polygon, "valid")) {
        # if the vector is valid, update the map
        rv$vectfile_polygon_ll <- st_transform(rv$vectfile_polygon, 4326)
        clearShapes(leafletProxy("view_map_vectfile"))
        fitBounds(
          leafletProxy("view_map_vectfile"),
          lng1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
          lat1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"Y"]),
          lng2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
          lat2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"Y"])
        )
        addPolygons(
          leafletProxy("view_map_vectfile"),
          data = rv$vectfile_polygon,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
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
    
    
    #- Update tile colours when single tiles are [de]activated -#
    observeEvent(c(input$tiles_checkbox, rv$update_tiles_on_map), ignoreNULL = FALSE, {
      req(rv$draw_tiles_overlapping)
      rv$draw_tiles_overlapping$selected <- rv$draw_tiles_overlapping$tile_id %in% input$tiles_checkbox
      # rv$draw_tiles_overlapping$colour <- ifelse(rv$draw_tiles_overlapping$selected | !any(rv$draw_tiles_overlapping$selected), "red", "grey")
      # rv$draw_tiles_overlapping$fillcolour <- ifelse(rv$draw_tiles_overlapping$selected | !any(rv$draw_tiles_overlapping$selected), "orange", "lightgrey")
      removeShape(leafletProxy("view_map"), rv$draw_tiles_overlapping$tile_id)
      addPolygons(
        leafletProxy("view_map"),
        data = rv$draw_tiles_overlapping[rv$draw_tiles_overlapping$selected | !any(rv$draw_tiles_overlapping$selected),],
        group = "S2 tiles",
        options = pathOptions(pane = "tiles_selected"),
        layerId = ~tile_id,
        label = ~tile_id,
        labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
        fill = TRUE,
        fillColor = "orange",
        fillOpacity = .3,
        stroke = TRUE,
        weight = 3,
        color = "red"
      )
      if (!all(rv$draw_tiles_overlapping$selected) & any(rv$draw_tiles_overlapping$selected)) {
        addPolygons(
          leafletProxy("view_map"),
          data = rv$draw_tiles_overlapping[!rv$draw_tiles_overlapping$selected,],
          group = "S2 tiles",
          options = pathOptions(pane = "tiles_notselected"),
          layerId = ~tile_id,
          label = ~tile_id,
          labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "lightgrey",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "grey"
        )
      }
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
    rgb_req <- reactive({
      !is.null(rv$list_rgb_ranges)
    })
    # convert in output value to be used in conditionalPanel
    output$indices_req <- renderText(indices_req())
    output$rgb_req <- renderText(rgb_req())
    # options to update these values also if not visible
    outputOptions(output, "indices_req", suspendWhenHidden = FALSE)
    outputOptions(output, "rgb_req", suspendWhenHidden = FALSE)
    
    create_indices_db()
    indices_db <- data.table(list_indices(
      c("n_index","name","longname","s2_formula_mathml","link","checked"),
      all = TRUE
    ))
    check_mark <- span(style="color:darkgreen;", icon("check"))
    check_mark <- gsub("\n *","", as.character(check_mark))
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
      indices_rv$checked <- sort(nn(input$list_indices))
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
        collapsible = TRUE,
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
        fluidRow(
          lapply(indices_rv$checked, index_details)
        )
      )
    })
    
    
    
    ## end of product module ##
    
    
    
    
    ## Geometry module ##
    
    ## Reference file
    shinyFileChoose(input, "reference_file_button", roots = volumes)
    
    observe({
      req(input$reference_file_textin)
      test_reference <- raster_metadata(input$reference_file_textin, format = "list")[[1]]
      rv$reference <- if (test_reference$valid) {test_reference} else {NULL}
    })
    
    observe({
      path_reference_file <- as.character(
        parseFilePaths(volumes,input$reference_file_button)$datapath
      )
      updateTextInput(session, "reference_file_textin", value = path_reference_file)
      output$reference_file_message <- renderUI({
        if (!is.null(rv$reference)) {
          span(style="color:darkgreen", "\u2001\u2714") # check
        } else if (!is.null(input$reference_file_button)) {
          span(style="color:red", "\u2001\u2718") # ballot
        } else {
          ""
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
      if (input$use_reference==TRUE & "res" %in% input$reference_usefor) {
        div(
          style = "padding-top:0.25em;",
          if (!is.null(rv$reference)) {
            span(style="color:darkgreen",
                 paste0(
                   paste(rv$reference$res,
                         collapse="\u2006\u00d7\u2006"), # shortspace times shortspace
                   switch(rv$reference$unit,
                          Degree="\u00b0", # shortspace degree
                          Meter="\u2006m", # shortspace m
                          "")))
          } else {
            span(style="color:grey",
                 "Specify a valid raster file.")
          }
        )
      } else {
        ""
      }
    })
    
    ## Reprojection
    output$outproj_message <- renderUI({
      
      # if required, take from reference raster
      if(input$use_reference==TRUE & "proj" %in% input$reference_usefor) {
        
        div(
          style = "padding-top:0.5em;",
          if (!is.null(rv$reference)) {
            if (projname(rv$reference$proj) != "unknown") {
              div(style="color:darkgreen", projname(rv$reference$proj))
            } else if (!is.na(rv$reference$proj$epsg)) {
              div(style="color:darkgreen", paste("EPSG:", rv$reference$proj$epsg))
            } else {
              div(style="color:darkgreen", "Valid")
            }
          } else {
            span(style="color:grey",
                 "Specify a valid raster file.")
          }
        )
        
        # else, take the specified one
      } else {
        
        outcrs_validated <- tryCatch(
          st_crs2(input$outproj),
          warning = function(w) {
            x <- suppress_warnings(st_crs2(input$outproj), "PROJ >\\= 6")
            attr(x, "warning") <- w$message
            x
          },
          error = function(e) {st_crs(NA)}
        )
        # show a warning (once) in case PROJ.4 is used
        if (all(
          !is.null(attr(outcrs_validated, "warning")),
          grepl("PROJ >\\= 6", attr(outcrs_validated, "warning")),
          is.null(rv$proj_alert_was_already_shown)
        )) {
          sendSweetAlert(
            session, NULL,
            attr(outcrs_validated, "warning"),
            type = "warning"
          )
          rv$proj_alert_was_already_shown <- TRUE
        }
        # Print proj name
        if (input$reproj==FALSE | input$outproj=="") {
          ""
        } else if (is.na(outcrs_validated)) {
          span(style="color:red",
               "Insert a valid projection (UTM timezone, EPSG code or WKT .prj file path).")
        } else {
          if (projname(outcrs_validated) != "unknown") {
            div(style="color:darkgreen",
                strong("Selected projection:"), br(),
                projname(outcrs_validated))
          } else if (!is.na(outcrs_validated$epsg)) {
            div(style="color:darkgreen", 
                strong("Selected projection:"), br(),
                paste("EPSG:", outcrs_validated$epsg))
          } else {
            div(style="color:darkgreen", strong("Valid projection"))
          }
        }
        
      }
      
    })
    
    # ## Update output format from reference file
    # (disabled: not yet possible to do it)
    # output$outformat_message <- renderUI({
    #   if(input$use_reference==TRUE & "outformat" %in% input$reference_usefor) {
    #     if (!is.null(rv$reference)) {
    #       span(style="color:darkgreen",
    #            rv$reference$outformat)
    #     } else {
    #       span(style="color:grey",
    #            "Specify a valid raster file.")
    #     }
    #   } else {
    #     ""
    #   }
    # })
    
    
    
    
    ## end of geometry module ##
    
    
    ## RGB module ##
    
    # list with the names of Sentinel-2 bands
    s2_bands <- list("TOA" = list(
      "Band 1 (443 nm)" = "band1",
      "Band 2 (490 nm)" = "band2",
      "Band 3 (560 nm)" = "band3",
      "Band 4 (665 nm)" = "band4",
      "Band 5 (705 nm)" = "band5",
      "Band 6 (740 nm)" = "band6",
      "Band 7 (783 nm)" = "band7",
      "Band 8 (842 nm)" = "band8",
      "Band 9 (940 nm)" = "band9",
      "Band 10 (1375 nm)" = "band10",
      "Band 11 (1610 nm)" = "band11",
      "Band 12 (2190 nm)" = "band12"
    ))
    s2_bands[["BOA"]] <- s2_bands[["TOA"]][c(1:9,11:12)]
    
    # Define default RGB images
    rv$list_rgb_ranges <- list(
      "RGB432B" = c(0, 2500),
      "RGB843B" = matrix(c(0, 0, 0, 7500, 2500, 2500), ncol = 2),
      "RGBb84B" = matrix(c(0, 0, 0, 7500, 7500, 2500), ncol = 2)
    )
    
    # Open modalDialog to add a new RGB
    observeEvent(input$new_rgb, {
      showModal(add_rgb_image(s2_bands))
    })
    # Add the new defined RGB
    observeEvent(input$add_new_rgb, {
      newrgb_ranges <- if (all(c(
        input$band_r_range==input$band_g_range,
        input$band_g_range==input$band_b_range
      ))) {
        list(input$band_r_range*1E4)
      } else {
        list(t(matrix(
          c(input$band_r_range, input$band_g_range, input$band_b_range)*1E4,
          nrow = 2
        )))
      }
      names(newrgb_ranges) <- paste0(
        "RGB",
        paste(as.hexmode(c(
          as.integer(gsub("^band","",input$band_r)),
          as.integer(gsub("^band","",input$band_g)),
          as.integer(gsub("^band","",input$band_b))
        )), collapse=""),
        substr(input$newrgb_source,1,1)
      )
      rv$list_rgb_ranges <- append(newrgb_ranges, rv$list_rgb_ranges)
      # new added RGB replaces existing one, if it is was already present
      rv$list_rgb_ranges <- rv$list_rgb_ranges[!duplicated(names(rv$list_rgb_ranges))]
      # order by name
      rv$list_rgb_ranges <- rv$list_rgb_ranges[order(names(rv$list_rgb_ranges))]
      removeModal()
    })
    
    # List of defined RGB images
    output$checkbox_list_rgb <- renderUI({
      checkboxGroupInput(
        "list_rgbimages",
        label = span(
          "RGB images:\u2000",
          actionLink("help_rgb", icon("question-circle"))
        ),
        # choiceNames = lapply(names(rv$list_rgb_ranges), HTML),
        choiceNames = lapply(names(rv$list_rgb_ranges), function(x) {
          ranges <- if (length(rv$list_rgb_ranges[[x]]) == 6) {
            c("min_r"=1, "min_g"=2, "min_b"=3, "max_r"=4, "max_g"=5, "max_b"=6)
          } else if (length(rv$list_rgb_ranges[[x]]) == 2) {
            c("min_r"=1, "min_g"=1, "min_b"=1, "max_r"=2, "max_g"=2, "max_b"=2)
          }
          HTML(paste0(
            "<strong>",x,":</strong> ",
            "<ul><li><i>source:</i> ",substr(x,7,7),"OA</li>",
            "<li><span style='color:red;'><i>red: </i>",
            "band ",strtoi(paste0("0x", substr(x,4,4))),
            " (reflectance range ",
            rv$list_rgb_ranges[[x]][ranges["min_r"]]/1E4," \u2013 ",
            rv$list_rgb_ranges[[x]][ranges["max_r"]]/1E4,")</span></li>",
            "<li><span style='color:darkgreen;'><i>green: </i>",
            "band ",strtoi(paste0("0x", substr(x,5,5))),
            " (reflectance range ",
            rv$list_rgb_ranges[[x]][ranges["min_g"]]/1E4," \u2013 ",
            rv$list_rgb_ranges[[x]][ranges["max_g"]]/1E4,")</span></li>",
            "<li><span style='color:blue;'><i>blue: </i>",
            "band ",strtoi(paste0("0x", substr(x,6,6))),
            " (reflectance range ",
            rv$list_rgb_ranges[[x]][ranges["min_b"]]/1E4," \u2013 ",
            rv$list_rgb_ranges[[x]][ranges["max_b"]]/1E4,")</span></li></ul>"
          ))
        }),
        # choiceNames = lapply(indices_rv$filtered$name, function(x){span(x,icon("info-circle"))}),
        choiceValues = as.list(names(rv$list_rgb_ranges)),
        selected = input$list_rgbimages
      )
    })
    outputOptions(output, "checkbox_list_rgb", suspendWhenHidden = FALSE)
    # this to avoid errors in case a json were imported before activating checkbox_list_rgb
    # TODO: with this trick, also indices_rv could be avoid and simplified
    
    # Remove selected RGB images
    observeEvent(input$rm_rgb, {
      rv$list_rgb_ranges <- rv$list_rgb_ranges[names(rv$list_rgb_ranges) %in% input$list_rgbimages]
    })
    
    
    # Update RGB bands and ranges when changing RGB source
    observeEvent(input$newrgb_source, {
      updatePickerInput(
        session, "band_r",
        choices = s2_bands[[input$newrgb_source]],
        selected = input$band_r
      )
      updatePickerInput(
        session, "band_g",
        choices = s2_bands[[input$newrgb_source]],
        selected = input$band_g
      )
      updatePickerInput(
        session, "band_b",
        choices = s2_bands[[input$newrgb_source]],
        selected = input$band_b
      )
    })
    observeEvent(input$band_r, {
      updateSliderInput(
        session, "band_r_range",
        value = if (input$band_r %in% paste0("band",1:5)) {
          c(0, 0.25)
        } else if (input$band_r %in% paste0("band",6:12)) {
          c(0, 0.75)
        }
      )
    })
    observeEvent(input$band_g, {
      updateSliderInput(
        session, "band_g_range",
        value = if (input$band_g %in% paste0("band",1:5)) {
          c(0, 0.25)
        } else if (input$band_g %in% paste0("band",6:12)) {
          c(0, 0.75)
        }
      )
    })
    observeEvent(input$band_b, {
      updateSliderInput(
        session, "band_b_range",
        value = if (input$band_b %in% paste0("band",1:5)) {
          c(0, 0.25)
        } else if (input$band_b %in% paste0("band",6:12)) {
          c(0, 0.75)
        }
      )
    })
    
    ## end of RGb module ##
    
    
    ## Path module ##
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
    observe({
      path_rgb_string <- parseDirPath(volumes, input$path_rgb_sel)
      updateTextInput(session, "path_rgb_textin", value = path_rgb_string)
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
    observe({
      output$path_rgb_errormess <- path_check(input$path_rgb_textin)
    })
    
    # # action button to copy indices_path and rgb_path from out_path
    # observeEvent(input$path_indices_cp, {
    #   updateTextInput(session, "path_indices_textin", value = input$path_out_textin)
    # })
    # observeEvent(input$path_rgb_cp, {
    #   updateTextInput(session, "path_rgb_textin", value = input$path_out_textin)
    # })
    
    ## Question messages
    
    observeEvent(input$help_online, {
      showModal(modalDialog(
        title = "Download mode",
        p(HTML(
          "Selecting <strong>Online</strong> mode, the user must specify",
          "a spatial extent and a temporal window (in \"Spatial-temporal",
          "selection\" tab), and the list of required products is searched",
          "online (an internet connection is required);",
          "missing SAFE products are then downloaded."
        )),
        p(HTML(
          "In <strong>Offline</strong> mode, only already available SAFE",
          "products are used (level-2A images can be produced locally",
          "with Sen2Cor if the corresponding level-1C images are available);",
          "the user can still filter them spatially and temporally,",
          "but this is not mandatory (if no parameters are specified,",
          "all the SAFE images are processed).")),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_lta_order, {
      showModal(modalDialog(
        title = "Order from LTA",
        p(HTML(
          "Starting from September 2019, SAFE archives older than 12 months",
          "(Level-1C) or 18 months (Level-2A) are generally not available",
          "for direct download, but must be ordered from the Long Term Archive",
          "(see <a href='https://inthub.copernicus.eu/userguide/LongTermArchive'",
          "target='_blank'>this page</a> for any details)."
        )),
        p(HTML(
          "Checking this option, products which are not available for direct",
          "download are ordered, so to be available at a later time.",
          "There is no way to know when the will be made available; the user",
          "can re-launch the same sen2r processing chain at a later time:",
          "in this way, when missing SAFE archives will be made available",
          "they will be downloaded and the output prodcut archive will be updated."
        )),
        p(HTML(
          "Alternatively, specific non-interactive functions are available",
          "to manage orders (see",
          "<a href='https://sen2r.ranghetti.info/reference/safe_is_online.html'",
          "target='_blank'><tt>safe_is_online()</tt></a> and",
          "<a href='https://sen2r.ranghetti.info/reference/s2_order.html'",
          "target='_blank'><tt>s2_order()</tt></a>)."
        )),
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
          "<strong>Built-in</strong> is the downloader which is used by default",
          "through the package 'httr'."
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
    
    observeEvent(input$help_cloud_perc, {
      showModal(modalDialog(
        title = "Maximum allowed SAFE cloud cover",
        p(HTML(
          "Set the maximum percentage of cloud covered area",
          "used to search and download SAFE products."
        )),
        p(HTML(
          "This value corresponds to the \"Cloud cover percentage\"",
          "metadata present within SAFE products, and it is applied only",
          "to non-existing archives (existing SAFE are always used),",
          "and it is useful to limit the downloads of SAFE archives",
          "and the time of processing."
        )),
        p(HTML(
          "In order to set a limit of masked area which relates to the output",
          "extent and the cloud mask set with processing options,",
          "use the \"Maximum allowed cloud cover\" slider in the",
          "\"Processing options\" tab."
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
          "are not saved with the other parameters, but in a dedicated txt file,",
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
    
    observeEvent(input$help_register_scihub, {
      showModal(modalDialog(
        title = "New/edit SciHub credentials",
        size = "s",
        p(HTML(
          "Notice that SciHub credentials are recognised by API Hub",
          "(used by sen2r) with a delay of one week (see",
          "<a href='https://scihub.copernicus.eu/twiki/do/view/SciHubWebPortal/APIHubDescription'",
          "target='_blank'>this alert</a>);",
          "for this reason, newly created credentials and password edits",
          "are generally not immediately recognised."
        )),
        a("Register new account", href="https://scihub.copernicus.eu/dhus/#/self-registration", target="_blank"),
        "\u2000\u2014\u2000",
        a("Forgot password?", href="https://scihub.copernicus.eu/dhus/#/forgot-password", target="_blank"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    # observeEvent(input$fix_online, {
    #   showModal(modalDialog(
    #     title = "Download is not supported on Windows",
    #     p(
    #       "Currently, finding and downloading SAFE products is possible",
    #       "only over Linux systems. This will be fixed in a future release."
    #     ),
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # })
    
    observeEvent(input$help_overwrite_safe, {
      showModal(modalDialog(
        title = "Overwrite existing SAFE products?",
        p(HTML(
          "<strong>Yes</strong>:",
          "re-download all images matching the parameters set in",
          "\"Spatial-temporal selection\" tab and re-apply Sen2Cor if needed."
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
          "Sen2Cor is never used locally: level-2A products are used only",
          "if they are already available locally or if they can be downloaded",
          "from SciHub.",
          "This option is useful if Sen2Cor is not available locally,",
          "or if its use must be avoided at all."
        )),
        p(HTML(
          "<strong>Use Sen2Cor only for level-2A products not available",
          "locally or online</strong>:",
          "level-2A images are first of all searched locally or online;",
          "only for not available products (with corresponding level-1C images",
          "available) Sen2Cor is used to produce them.",
          "This option is useful to optimize time of processing",
          "(downloading of level-2A images is faster than producing them","
          with Sen2Cor), and in most of the situations."
        )),
        p(HTML(
          "<strong>Always correct level-1C images with Sen2Cor locally</strong>:",
          "If level-2A images are not available locally, they are corrected",
          "applying Sen2Cor to their corresponding level-1C images.",
          "This option can be used to reduce internet traffic if a level-1C",
          "archive is already available, or if both level-1C and level-2A",
          "products are required for outputs."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_use_dem, {
      showModal(modalDialog(
        title = "Topographic correction in Sen2Cor",
        p(HTML(
          "Sen2Cor can correct the effect of different topographic exposures",
          "using a Digital Elevation Model (DEM) during atmoshperic correction",
          "(which is what is done for producing the level 2A SAFE products",
          "downloadable from ESA Hub).",
          "By default, Sen2Cor does not perform this operation, causing an",
          "inhomogeneity between SAFE downloaded from ESA Hub and locally",
          "produced with Sen2Cor."
        )),
        p(HTML(
          "In this option:<ul>",
          "<li>set <strong>Yes</strong> to perform the topographic correction:",
          "DEM is searched in a default directory and downloaded from",
          "<a href='http://srtm.csi.cgiar.org/' target='_blank'>CGIAR-CSI",
          "SRTM</a> if not found locally;</li>",
          "<li>set <strong>No</strong> to avoid applying a topographic",
          "correction;</li>",
          "<li>set <strong>Keep default</strong> to mantain the default",
          "behaviour (which is \"No\", unless the user manually edited",
          "the GIPP XML file containing the Sen2Cor settings).</li></ul>"
        )),
        p(HTML(
          "<strong>Note:</strong> Currently the default value is \"Keep",
          "default\" in order to grant backward compatibility.",
          "In a future release of sen2r, the default value will be set to TRUE,",
          "so to grant homogeneity between Level-2A products downloaded from",
          "ESA Hub and generated using Sen2Cor."
        )),
        p(HTML(
          "To make custom edits to Sen2Cor parameters (e.g. changing the default",
          "DEM directory, or customising atmospheric correction parameters),",
          "set the argument <span style='family:monospace;'>\"sen2cor_gipp\"</span> ",
          "of function <span style='family:monospace;'>sen2r()</span> using the command line",
          "(refer to the <a href='https://sen2r.ranghetti.info/reference/sen2cor'",
          "target='_blank'>documentation of function",
          "<span style='family:monospace;'>sen2cor()</span></a>",
          "- argument \"gipp\" - for details)."
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
    
    observeEvent(input$help_orbits, {
      showModal(modalDialog(
        title = "Tiles and orbits selected",
        p(HTML(
          "Selectors \"Tiles selected\" and \"Orbits selected\" allow to",
          "restrict the processing to the specified tiles and <a href=",
          "'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/satellite-description/orbit'",
          "target='_blank'>orbits</a>.",
          "The list of tiles which can be selected is dynamically updated",
          "basing on the selected extent",
          "(only tiles overlapping the extent are shown),",
          "as well as the colour of tiles shown in the map is dynamically set",
          "(selected tiles are shown in red, unselected ones in grey).",
          "Instead, the list of orbits is static, and orbits are not shown",
          "on the map."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
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
          "the extent selected in the tab \"Spatial-temporal selection\"",
          "is used as extent for output products.",
          "The user can pass other geometry parameters in the box",
          "\"Output geometry\"."
        )),
        p(HTML(
          "<strong>No</strong>:",
          "the extent selected in the tab \"Spatial-temporal selection\"",
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
          "target='_blank'>classification algorithm</a> for further details."
        )),
        p(HTML(
          "Notice that this functionality does not ensure to correctly mask",
          "all the clouds: in fact, the SCL product is an automatic",
          "classification performed by Sen2Cor, and it is subject to errors",
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
    
    observeEvent(input$help_outprojinput, {
      showModal(modalDialog(
        title = "Enter a valid projection",
        size = "s",
        p(HTML(
          "To identify the desired output projection,",
          "please type one of the followings:<ul>",
          "<li>the EPSG code (e.g. 32632);</li>",
          "<li>the WKT text representation;</li>",
          "<li>the path of a spatial file or of a text file containing a WKT",
          "(e.g. a .proj file of a shapefile);</li>",
          "<li>the PROJ.4 string (<strong>warning:</strong> this is a",
          "deprecated representation with PROJ >= 3, so avoid using it!)</li></ul>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$info_parallelisation, {
      showModal(modalDialog(
        title = "Processing order and parallelisation",
        size = "s",
        p(HTML(
          "Settings of this box do not influence the produced output files,",
          "but only the order used to produce them and the exploitment",
          "of multiple CPU cores to speed up computation."
        )),
        p(HTML(
          "If you are not confident with these settings, use the default values."
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$help_processing_order, {
      showModal(modalDialog(
        title = "Processing order",
        p(HTML(
          "The order used to execute the processing chain affects the speed",
          "of computation and the usage of system resources.",
          "Changing this setting can be useful to optimise system performance,",
          "particularly if the user is processing a high amount of data",
          "(a large area of interest and/or an extensive time window)."
        )),
        p(HTML(
          "The four available orders are described below."
        )),
        p(HTML(
          "<strong>Process by groups</strong> (default):",
          "it provides a good compromise between processing speed and disk usage.",
          "Processing is done as follows:<ul>",
          "<li>the list of required SAFE and output product names is computed;</li>",
          "<li>the required dates are grouped in <em>g</em> groups, where",
          "<em>g</em> is the number of dates divided by the number of CPU;</li>",
          "<li>groups are then processed sequentially; for each group:<ul>",
          "<li>the required SAFE archives are downloaded;</li>",
          "<li>Sen2Cor is applied in parallel using one core per L1C SAFE archive;</li>",
          "<li>the remaining processing operations are executed using parallel",
          "R sessions (one core for each date).</li></ul></ul>"
        )),
        p(HTML(
          "<strong>Process by date</strong>:",
          "this allows minimising the requirements of disk usage",
          "(in particular if SAFE archives are deleted after processing).",
          "It is similar to the default execution, but each group is composed",
          "by a single date: so the disk space occupied by SAFE archives",
          "and temporary files is lower,",
          "but it is generally slower than the default one because",
          "parallel computation over dates for products' generation is not possible."
        )),
        p(HTML(
          "<strong>Mixed processing</strong>:",
          "this allows maximising CPU usage and processing speed.",
          "The cycle on groups is ignored, and all the required SAFE are",
          "first of all downloaded and/or produced, and then dates are",
          "processed in parallel.",
          "This mode is faster than the default mode, but it requires",
          "all SAFE archives to be downloaded and processed before performing",
          "subsequent steps, thus increasing disk space requirements."
        )),
        p(HTML(
          "<strong>Process step by step</strong>:",
          "this is the legacy mode, in which the cycle on groups is ignored",
          "as well as the parallel computation over dates.",
          "All SAFE archives are first downloaded/processed,",
          "then the processing steps are performed sequentially.",
          "This mode is similar to the previous one in terms of disk usage",
          "but it is slightly slower; its advantage are the lower RAM requirements."
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
    
    observeEvent(input$help_rgb, {
      showModal(modalDialog(
        title = "RGB images:",
        p(HTML(
          "RGB images can be built using different spectral bands.",
          "To distinguish each other, the naming convention",
          "\"<strong>x</strong>RGB<strong>rgb</strong>\", where:",
          "<ul><li><strong>x</strong> is B (if source is BOA)",
          "or T (is source is TOA);</li>",
          "<li><strong>r</strong>, <strong>g</strong> and <strong>b</strong>",
          "are the the number of the bands to be used respectively for red,",
          "green and blue, in hexadecimal format",
          "(so i.e. band 8 is 8, band 11 is b).</li></ul>"
        )),
        p(HTML(
          "By default, three RGB images are defined:",
          "<ul><li><strong>RGB432B</strong> is the true colour image,",
          "computed from BOA, with all the three bands rescaled from",
          "reflectance 0 (black) to 0.25 (white);</li>",
          "<li><strong>RGB843B</strong> is the standard false colour image,",
          "where NIR is represented as red, red as green and green as blue;",
          "it is computed from BOA, and NIR band is rescaled from",
          "reflectance 0 (black) to 0.75 (white),",
          "while red and green from 0 to 0.25 (this because NIR reflectance",
          "is usually higher than red and green);</li>",
          "<li><strong>RGBb84B</strong> is a false colour image",
          "commonly used to emphatise vegetated area,",
          "in which SWIR is represented as red, NIR as green and red as blue;",
          "it is computed from BOA, SWIR and NIR band are rescaled from",
          "reflectance 0 (black) to 0.75 (white),",
          "while red one from 0 to 0.25.</li></ul>"
        )),
        p(HTML(
          "The button \"Define custom RGB image\" can be used to define",
          "different combinations on bands, to compute images from TOA",
          "or to change scale ranges."
        )),
        p(HTML(
          "Notice that, after defining new images, they must be checked",
          "in order to be computed (only checked products are considered,",
          "as for spectral indices)."
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
      rl$order_lta <- as.logical(input$make_lta_order) # TRUE to order from LTA, FALSE to skip
      rl$downloader <- input$downloader # downloader ("builtin" or "aria2")
      rl$overwrite_safe <- as.logical(input$overwrite_safe) # TRUE to overwrite existing SAFE, FALSE not to
      rl$rm_safe <- input$rm_safe # "yes" to delete all SAFE, "l1c" to delete only l1c, "no" not to remove
      rl$max_cloud_safe <- input$max_cloud_safe_perc # maximum SAFE cloud coverage (0-100)
      rl$step_atmcorr <- if (safe_req$l2a==TRUE) {input$step_atmcorr} else {"l2a"} # download_method in sen2cor: "auto", "l2a" or "scihub"
      rl$sen2cor_use_dem <- as.logical(input$use_dem) # apply topographic correction
      rl$sen2cor_gipp <- param_list$sen2cor_gipp # do not modify (GUI not yet implemented)
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
        geojson_json(
          st_transform(rv$extent, 4326),
          pretty = TRUE
        )
      } else {
        NA
      }
      rl$s2tiles_selected <- if (input$query_space == TRUE & length(nn(input$tiles_checkbox)>0)) {
        input$tiles_checkbox
      } else {
        NA
      } # selected tile IDs
      rl$s2orbits_selected <- if (input$query_space == TRUE & length(nn(input$orbits_checkbox)>0)) {
        input$orbits_checkbox
      } else {
        NA
      } # selected orbit IDs
      
      # product selection #
      rl$list_prods <- input$list_prods[!input$list_prods %in% c("indices","rgbimages")] # TOA, BOA, SCL, TCI (for now)
      rl$list_indices <- if (indices_req()==TRUE & "indices" %in% input$list_prods) {input$list_indices} else {NA} # index names
      rl$list_rgb <- if (all(rgb_req()==TRUE, "rgbimages" %in% input$list_prods, length(input$list_rgbimages)>0)) {
        input$list_rgbimages
      } else {NA} # RGB images names
      rl$rgb_ranges <- if (all(rgb_req()==TRUE, "rgbimages" %in% input$list_prods, length(input$list_rgbimages)>0)) {
        setNames(rv$list_rgb_ranges[input$list_rgbimages], NULL)
      } else {NA} # RGB images names
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
        rv$reference$res
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
                        rv$reference$unit,
                        "Meter") # TODO allow degrees if outproj is longlat
      # output WKT/WKT2
      rl$proj <- if (input$use_reference==TRUE &
                     "proj" %in% input$reference_usefor) {
        st_as_text_2(rv$reference$proj)
      } else if (input$reproj==FALSE) {
        NA
      } else {
        tryCatch(
          st_as_text_2(input$outproj),
          error = function(e) {NA}
        )
      }
      # resampling methods ("nearest","bilinear","cubic","cubicspline","lanczos","average","mode")
      rl$resampling <- input$resampling
      rl$resampling_scl <- input$resampling_scl
      # output format (GDAL format name)
      rl$outformat <- ifelse(input$use_reference==TRUE &
                               "outformat" %in% input$reference_usefor,
                             rv$reference$outformat,
                             input$outformat)
      rl$rgb_outformat <- rl$outformat # TODO add a widget to set it
      rl$index_datatype <- input$index_datatype
      # output compression ("LZW", "DEFLATE" etc.)
      rl$compression <- ifelse(rl$outformat %in% c("GTiff","BigTIFF"),
                               input$compression,
                               NA)
      rl$rgb_compression <- rl$compression # TODO add a widget to set it
      # overwrite or skip existing files (logical)
      rl$overwrite <- as.logical(input$overwrite)
      
      # set directories #
      rl$path_l1c <- if (safe_req$l1c==TRUE) {input$path_l1c_textin} else {NA} # path of L1C SAFE products
      rl$path_l2a <- if (safe_req$l2a==TRUE) {input$path_l2a_textin} else {NA} # path of L2A SAFE products
      rl$path_tiles <- if (rl$preprocess==TRUE & input$keep_tiles==TRUE) {input$path_tiles_textin} else {NA} # path of entire tiled products
      rl$path_merged <- if (rl$preprocess==TRUE & input$keep_merged==TRUE) {input$path_merged_textin} else {NA} # path of entire tiled products
      rl$path_out <- if (rl$preprocess==TRUE & (length(input$list_prods)>1 | length(input$list_prods)>0 & !"indices" %in% input$list_prods)) {input$path_out_textin} else {NA} # path of output pre-processed products
      rl$path_rgb <- if (rl$preprocess==TRUE & rgb_req()==TRUE) {input$path_rgb_textin} else {NA} # path of RGB images
      rl$path_indices <- if (rl$preprocess==TRUE & indices_req()==TRUE) {input$path_indices_textin} else {NA} # path of spectral indices
      rl$path_subdirs <- if (rl$preprocess==TRUE) {as.logical(input$path_subdirs)} else {NA} # logical (use subdirs)
      rl$thumbnails <- if (rl$preprocess==TRUE) {as.logical(input$check_thumbnails)} else {NA} # logical (create thumbnails)
      
      # logs and parallelisation
      rl$log <- if (!is.null(rv$log_path)) {rv$log_path} else {NA}
      rl$parallel <- if (input$parallel==FALSE) {FALSE} else if (input$n_cores_auto==TRUE) {TRUE} else {input$n_cores}
      rl$processing_order <- input$processing_order
      
      # save apihub.txt path if it was customly set
      if (!is.null(NULL) & !anyNA(NULL)) {rl$apihub_path <- rv$apihub_path}
      
      # information about package version
      rl$pkg_version <- as.character(packageVersion("sen2r"))
      
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
        updateCheckboxInput(session, "make_lta_order", value = pl$order_lta)
        updateRadioButtons(session, "downloader", selected = pl$downloader)
        updateRadioButtons(session, "overwrite_safe", selected = pl$overwrite_safe)
        updateRadioButtons(session, "rm_safe", selected = pl$rm_safe)
        updateSliderInput(
          session, "max_cloud_safe_perc",
          value = if (!all(is.na(nn(pl$max_cloud_safe)))) {pl$max_cloud_safe} else {100}
        )
        updateRadioButtons(session, "step_atmcorr", selected = pl$step_atmcorr)
        updateRadioButtons(
          session, "use_dem", selected = if (
            is.na(pl$sen2cor_use_dem)
          ) {"NA"} else {as.character(pl$sen2cor_use_dem)}
        )
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
        if (all(is.na(pl$list_prods))) {pl$list_prods <- character(0)}
        updateCheckboxGroupInput(
          session, "list_prods",
          selected = c(
            pl$list_prods,
            if (any(!is.na(nn(pl$list_indices)))) {"indices"},
            if (any(!is.na(nn(pl$list_rgb)))) {"rgbimages"}
          )
        )
        if (all(is.na(nn(pl$list_indices)))) {pl$list_indices <- character(0)}
        indices_rv$checked <- pl$list_indices
        # updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices) # FIXME 1 not working since it is reactive
        if (all(is.na(nn(pl$list_rgb)))) {pl$list_rgb <- character(0)}
        if (all(is.na(nn(pl$list_rgb))) && all(is.na(nn(pl$rgb_ranges)))) {pl$rgb_ranges <- character(0)}
        rv$list_rgb_ranges <- setNames(pl$rgb_ranges, pl$list_rgb)
        updateCheckboxGroupInput(session, "list_rgbimages", selected = pl$list_rgb)
        updateRadioButtons(session, "atm_mask",
                           selected = ifelse(is.na(pl$mask_type),FALSE,TRUE))
        updateSliderInput(session, "max_masked_perc",
                          value = ifelse(is.na(pl$mask_type),100,pl$max_mask))
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
        updateRadioButtons(session, "keep_tiles", selected = ifelse(is.na(nn(pl$path_tiles)),FALSE,TRUE))
        updateRadioButtons(session, "keep_merged", selected = ifelse(is.na(nn(pl$path_merged)),FALSE,TRUE))
        setProgress(0.6)
        
        
        # set directories
        updateTextInput(session, "path_l1c_textin", value = pl$path_l1c)
        updateTextInput(session, "path_l2a_textin", value = pl$path_l2a)
        updateTextInput(session, "path_tiles_textin", value = pl$path_tiles)
        updateTextInput(session, "path_merged_textin", value = pl$path_merged)
        updateTextInput(session, "path_out_textin", value = pl$path_out)
        updateTextInput(session, "path_indices_textin", value = pl$path_indices)
        updateTextInput(session, "path_rgb_textin", value = pl$path_rgb)
        updateRadioButtons(session, "path_subdirs", selected = pl$path_subdirs)
        updateRadioButtons(session, "check_thumbnails", selected = pl$thumbnails)
        
        # update logs and parallelisation
        if (!is.na(pl$log[1])) {rv$log_path <- pl$log[1]}
        parallel <- if (is.numeric(pl$parallel)) {pl$parallel>1} else {as.logical(pl$parallel)}
        updateRadioButtons(session, "parallel", selected = parallel)
        if (parallel) {updateSwitchInput(session, "n_cores_auto", value = !is.numeric(pl$parallel))}
        if (parallel & is.numeric(pl$parallel)) {updateSliderInput(session, "n_cores", value = pl$parallel)}
        updateSelectInput(session, "processing_order", selected = pl$processing_order)
        
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
          updateTextInput(
            session, "outproj", 
            value = ifelse(!is.na(pl$proj), pl$proj, character(0))
          )
          updateRadioButtons(session, "outformat", selected = pl$outformat)
        }
        updateRadioButtons(session, "index_datatype", selected = pl$index_datatype)
        updateRadioButtons(session, "resampling", selected = pl$resampling)
        updateRadioButtons(session, "resampling_scl", selected = pl$resampling_scl)
        updateRadioButtons(
          session, "compression", 
          selected = ifelse(
            pl$outformat %in% c("GTiff","BigTIFF"), 
            pl$compression, 
            character(0)
          )
        )
        updateRadioButtons(session, "overwrite", selected = pl$overwrite)
        setProgress(0.8)
        
        # update extent (at the end, not to interfer with other events
        # (the delay is required to update the map after the map is charged)
        shinyjs::delay(5E3, {
          update_extent("imported", custom_source = pl$extent)
          updatePickerInput(session, "tiles_checkbox", selected = if(length(nn(pl$s2tiles_selected))>0) {pl$s2tiles_selected} else {NA})
          rv$update_tiles_on_map <- sample(1E6, 1) # update the [un]selected tiles on the map
        })
        updatePickerInput(session, "orbits_checkbox", selected = if(length(nn(pl$s2orbits_selected))>0) {pl$s2orbits_selected} else {NA})
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
    
    # if Return is pressend:
    observeEvent(input$return_param, {
      if (
        if (any(length(nn(rv$apihub_path)) == 0, anyNA(rv$apihub_path))) {
          !file.exists(file.path(dirname(attr(load_binpaths(), "path")), "apihub.txt"))
        } else {
          !file.exists(rv$apihub_path)
        }
      ) {
        
        # alert if apihub does not exists
        sendSweetAlert(
          session,
          "Missing SciHub credentials",
          paste0(
            "Please specify your SciHub credentials using the button ",
            "\"Login to SciHub\" in the \"Product selection\" sheet."
          ),
          type = "error"
        )
        
      } else if (length(c(
        input$list_prods[!input$list_prods %in% c("indices","rgbimages")],
        input$list_indices,
        input$list_rgb
      )) == 0) {
        
        # alert if no products were specified
        sendSweetAlert(
          session, NULL,
          paste0(
            "Please select at least one product, spectral index or RGB image ",
            "before continuing."
          ),
          type = "error"
        )
        
      } else if (length(input$sel_sensor) == 0) {
        
        # alert if no sensors were specified
        sendSweetAlert(
          session, NULL,
          "Please select at least one sensor before continuing.",
          type = "error"
        )
        
      } else if (safe_req$l1c & input$path_l1c_textin == "") {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          "Please specify the directory for Level-1C SAFE products before continuing.",
          type = "error"
        )
        
      } else if (safe_req$l2a & input$path_l2a_textin == "") {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          "Please specify the directory for Level-2A SAFE products before continuing.",
          type = "error"
        )
        
      } else if (
        length(input$list_prods[!input$list_prods %in% c("indices","rgbimages")]) > 0 &
        input$path_out_textin == ""
      ) {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          "Please specify the directory for output products before continuing.",
          type = "error"
        )
        
      } else if (
        length(input$list_indices) > 0 &
        input$path_out_textin == "" &
        input$path_indices_textin == ""
      ) {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          span("Please specify at least one among the directories for spectral",
               "indices and for output products before continuing."),
          type = "error"
        )
        
      } else if (
        length(input$list_rgb) > 0 &
        input$path_out_textin == "" &
        input$path_rgb_textin == ""
      ) {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          span("Please specify at least one among the directories for RGB",
               "images and for output products before continuing."),
          type = "error"
        )
        
      } else if (all(
        input$online == TRUE,
        length(nn(rv$extent)) == 0,
        length(nn(input$tiles_checkbox)) == 0
      )) {
        
        # directory missing
        sendSweetAlert(
          session, NULL,
          "Please specify the extent.",
          type = "error"
        )
        
      } else {
        
        # if all checks passed, exit from GUI and return values
        return_list <- create_return_list() # run creation of return_list
        # add parameters not modified by the GUI
        return_list <- c(return_list, param_list[!names(param_list) %in% names(return_list)])
        check_param_result <- check_param(return_list)
        if (check_param_result) {
          shinyjs::js$closeWindow()
          stopApp(return_list)
        }
        
      }
    })
    
    # if Exit is pressend, exit from GUI
    observeEvent(input$exit_gui, {
      shinyjs::js$closeWindow()
      stopApp()
    })
    
    # if Export is pressed, export the values (using server-side button)
    observeEvent(input$export_param, {
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
      import_param_path <- parseFilePaths(volumes,input$import_param)
      rv$imported_param <- if (nrow(import_param_path)>0) {
        check_param_list(
          fromJSON(readLines(as.character(import_param_path$datapath))),
          type = "warning", correct = TRUE
        )
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
        
        # disable log button if logging was already doing
        if (all(!is.na(param_list$log))) {
          disable("save_log")
        }
        
        # if Create log is pressed, set the paramtere to sink the log
        observeEvent(input$save_log, {
          shinyFileSave(input, "save_log", roots=volumes, session=session)
          rv$log_path <- parseSavePath(volumes, input$save_log)$datapath
        })
        
        observeEvent(param_list, {
          req(param_list)
          import_param_list(param_list)
        })
        
        ## end of path module ##
        
        # Closing the connection when window is closed
        session$onSessionEnded(function() {
          stopApp()
        })
        
        # Remove waiting modal dialog
        removeModal()
        
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

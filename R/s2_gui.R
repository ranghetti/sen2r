

s2_gui <- function(param_list=NULL) {

  require(shinydashboard)
  require(mapview)
  require(mapedit)
  require(shiny)
  library(leaflet)
  library(leaflet.extras)


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
          menuItem("Spatial-temporal selection", tabName = "tab_query", icon = icon("image"))
        )
      ),

      dashboardBody(
        tabItems(

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


            )), # end of fluidRow/box "Spatial extent"

            # Save button
            tags$head(tags$script(src = "message-handler.js")),
            actionButton("save_param", "Save parameters")

          ) # end of tabItem tab_query

        ) # end of tabItems

      ) # end of dashboardBody

    ) # end of s2_gui.ui dashboardPage

  s2_gui.server <- function(input, output, session) {

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
      browser()
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







    ## Exit and save ##
    observeEvent(input$save_param, {
      return_list <- list()
browser()
      return_list$drawn_extent <- rv$drawn_extent
      return_list$s2tiles_intersect <- rv$s2tiles_selected
      stopApp(return_list)
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




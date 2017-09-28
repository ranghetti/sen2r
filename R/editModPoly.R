#' Shiny Module Server for Geo Create, Edit, Delete
#'
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param leafmap leaflet map to use for Selection
#' @param targetLayerId \code{character} identifier of layer to edit, delete
#' @param sf \code{logical} to return simple features.  \code{sf=FALSE} will return
#'          \code{GeoJSON}.
#' @param record \code{logical} to record all edits for future playback.
#' @param crs CRS (EPSG) to be used
#' @return server function for Shiny module
#' @importFrom shiny observeEvent reactive reactiveValues
#' @importFrom leaflet.extras addDrawToolbar drawPolygonOptions drawRectangleOptions editToolbarOptions
#' @importFrom leaflet renderLeaflet
# #' @importFrom mapedit combine_list_of_sf st_as_sfc.geo_list
#' @import mapedit
#' @note Slightly edited from [mapedit::editMod] in order to allow drawing only polygons.
editModPoly <- function(
  input, output, session,
  leafmap,
  targetLayerId = NULL,
  sf = TRUE,
  record = FALSE,
  crs = 4326
) {
  # check to see if addDrawToolbar has been already added to the map
  if(is.null(
    Find(
      function(cl) {
        cl$method == "addDrawToolbar"
      },
      leafmap$x$calls
    )
  )) {
    # add draw toolbar if not found
    leafmap <- leaflet.extras::addDrawToolbar(
      leafmap,
      targetGroup = targetLayerId,
      polylineOptions = FALSE,
      polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
      circleOptions = FALSE,
      rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
      markerOptions = FALSE,
      editOptions = leaflet.extras::editToolbarOptions()
    )
  }

  output$map <- leaflet::renderLeaflet({leafmap})

  featurelist <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list()
  )

  recorder <- list()

  EVT_DRAW <- "map_draw_new_feature"
  EVT_EDIT <- "map_draw_edited_features"
  EVT_DELETE <- "map_draw_deleted_features"

  shiny::observeEvent(input[[EVT_DRAW]], {
    featurelist$drawn <- c(featurelist$drawn, list(input[[EVT_DRAW]]))
    featurelist$finished <- c(featurelist$finished, list(input[[EVT_DRAW]]))
  })

  shiny::observeEvent(input[[EVT_EDIT]], {
    edited <- input[[EVT_EDIT]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist$finished[loc] <<- list(x)
      }
    })

    featurelist$edited_all <- c(featurelist$edited_all, list(edited))
  })

  shiny::observeEvent(input[[EVT_DELETE]], {
    deleted <- input[[EVT_DELETE]]
    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify finished to match edited
    lapply(deleted$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist$finished[loc] <<- NULL
      }
    })

    featurelist$deleted_all <- c(featurelist$deleted_all, list(deleted))
  })

  # record events if record = TRUE
  if(record == TRUE) {
    lapply(
      c(EVT_DRAW, EVT_EDIT, EVT_DELETE),
      function(evt) {
        observeEvent(input[[evt]], {
          recorder <<- c(
            recorder,
            list(
              list(
                event = evt,
                timestamp = Sys.time(),
                feature = input[[evt]]
              )
            )
          )
        })
      }
    )
  }


  # collect all of the the features into a list
  #  by action
  returnlist <- reactive({
    workinglist <- list(
      drawn = featurelist$drawn,
      edited = featurelist$edited_all,
      deleted = featurelist$deleted_all,
      finished = featurelist$finished
    )
    # if sf argument is TRUE then convert to simple features
    if(sf) {
      workinglist <- lapply(
        workinglist,
        function(action) {
          # ignore empty action types to prevent error
          #   handle in the helper functions?
          if(length(action) == 0) { return() }

          # FeatureCollection requires special treatment
          #  and we need to extract features
          features <- Reduce(
            function(left,right) {
              if(right$type == "FeatureCollection") {
                right <- lapply(right$features, identity)
              } else {
                right <- list(right)
              }
              c(left,right)
            },
            action,
            init = NULL
          )

          mapedit:::combine_list_of_sf(
            lapply(features, mapedit:::st_as_sf.geo_list, crs = crs)
          )
        }
      )

      recorder <- lapply(
        recorder,
        function(evt) {
          feature = mapedit:::st_as_sfc.geo_list(evt$feature, crs = crs)
          list(evt = evt$event, timestamp = evt$timestamp, feature = feature)
        }
      )
    }
    # return merged features
    if(record==TRUE) {
      attr(workinglist, "recorder") <- recorder
    }
    return(workinglist)
  })

  return(returnlist)
}

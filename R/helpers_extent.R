#' @title Insert an extent
#' @description Internal functions and modal dialogs to specify an extent
#'  in the GUI.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom shiny actionButton div fileInput htmlOutput icon modalButton modalDialog
#'  numericInput span tagList textInput
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom mapedit editModUI

#' @name load_extent_bbox
#' @rdname load_extent
load_extent_bbox <- function() {
  modalDialog(
    title = "Specify a bounding box",
    size = "l",
    fluidRow(
      column(
        width=4,
        strong("Insert the coordinates of the bounding box:"),
        div(
          style="width:200px;position:relative;margin-left:40px;top:0px;margin-top:5px;",
          numericInput(
            "bbox_ymax", 
            label = span(style="font-weight:normal;color:grey", "upper northing"),
            value = NULL, width = "110px"
          )
        ),
        div(
          style="width:200px;",
          div(
            style="display:inline-block;position:relative;top:-5px;",
            numericInput(
              "bbox_xmin", 
              label = span(style="font-weight:normal;color:grey", "left easting"), 
              value = NULL, width = "90px"
            )
          ),
          div(
            style="display:inline-block;position:relative;margin-left:10px;top:-5px;",
            numericInput(
              "bbox_xmax", 
              label = span(style="font-weight:normal;color:grey", "right easting"), 
              value = NULL, width = "90px"
            )
          )
        ),
        div(
          style="width:200px;position:relative;margin-left:40px;top:-10px;",
          numericInput(
            "bbox_ymin", 
            label = span(style="font-weight:normal;color:grey", "lower northing"), 
            value = NULL, 
            width = "110px"
          )
        ),
        strong("Specify the projection of the coordinates:"),
        div(
          style="display:inline-block;position:relative;margin-top:5px;",
          textInput("bboxproj", NULL,
                    value="4326", width="190px")
        ),
        p(style = "font-size:small;color:grey;",
          "(type an EPSG code, a WKT text or the path of a spatial file /",
          "of a text file containing a WKT)"),
        div(
          style="display:inline-block;position:relative;bottom:0;margin-left:10px;",
          htmlOutput("bboxproj_message")
        )
      ),
      column(
        width=8,
        leafletOutput("view_map_bbox", height=500, width="100%")
      )
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_bbox", strong("\u2000Ok"), icon=icon("check")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}


#' @name load_extent_vectfile
#' @rdname load_extent
load_extent_vectfile <- function() {
  modalDialog(
    title = "Select vector file",
    size = "m",
    div(
      style="display:inline-block;vertical-align:top;margin-bottom:10px;",
      shinyFilesButton(
        "path_vectfile_sel", "Select",
        "Specify the file to be used as extent",
        multiple = FALSE
      )
    ),
    div(
      style="display:inline-block;vertical-align:middle;padding-left:10px;",
      helpText(em(
        p("Chose the vector file to be used as extent.")
      ))
    ),
    leafletOutput("view_map_vectfile", height=500, width="100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_vectfile", strong("\u2000Ok"), icon=icon("check")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}


#' @name load_extent_draw
#' @rdname load_extent
#' @param extent_ns_name Name of the namespace to be used
load_extent_draw <- function(extent_ns_name) {
  modalDialog(
    title = "Draw the extent",
    size = "l",
    helpText(em("Use the tools on the left to draw the extent of your products.")),
    editModUI(extent_ns_name, height=500, width="100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_draw", strong("\u2000Ok"), icon=icon("check")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}

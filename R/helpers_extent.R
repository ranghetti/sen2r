#' @title Insert an extent
#' @description Internal functions and modal dialogs to specify an extent
#'  in the GUI.
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom shiny actionButton div fileInput htmlOutput icon modalButton modalDialog
#'  numericInput span tagList textInput

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
          style="width:200px;position:relative;margin-left:40px;top:0px;",
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
          div(
            style="display:inline-block;position:relative;",
            textInput("bboxproj", NULL,
                      value="4326", width="190px")
          ),
          div(
            style="display:inline-block;position:relative;bottom:0;margin-left:10px;",
            htmlOutput("bboxproj_message")
          )
        )
      ),
      column(
        width=8,
        leafletOutput("view_map_bbox", height=400, width="100%")
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
    helpText(em(
      p("Chose the vector file to be used as extent."),
      p("To upload a shapefile, select all the related files",
        "(at most the .shp, .shx, .dbf and .prj ones must be present).")
    )),
    # div(div(style="display:inline-block;vertical-align:top;width:50pt;", # FIXME 2 choosing file with the button the extent is not drawn yet, and the toolbar is not added/removed changing extent_type
    #         shinyFilesButton("path_vectfile_sel",
    #                          "Select",
    #                          "Specify the file to be used as extent",
    #                          multiple = FALSE)),
    #     div(style="display:inline-block;vertical-align:top;",
    #         textInput("path_vectfile_textin", NULL, ""))),
    fileInput("path_vectfile_sel",
              "Select",
              multiple = TRUE),
    div(style="display:inline-block;vertical-align:top;",
        htmlOutput("path_vectfile_errormess")),
    leafletOutput("view_map_vectfile", height=400, width="100%"),
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

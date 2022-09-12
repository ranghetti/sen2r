#' @title Add RGB product
#' @description Modal dialog to define an RGB image.
#' @param s2_bands 2-length list (one for TOA, one for BOA),
#'  each element being a list of S2 bands, as defined in [s2_gui].
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. 
#'  \doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @keywords internal

add_rgb_image <- function(s2_bands) {
  
  # Define internal functions as aliases of shiny* - leaflet* ones,
  # so to avoid using "shiny::" every time
  actionButton <- shiny::actionButton
  column <- shiny::column
  fluidRow <- shiny::fluidRow
  HTML <- shiny::HTML
  icon <- shiny::icon
  modalButton <- shiny::modalButton
  modalDialog <- shiny::modalDialog
  radioButtons <- shiny::radioButtons
  sliderInput <- shiny::sliderInput
  strong <- shiny::strong
  tagList <- shiny::tagList
  pickerInput <- shinyWidgets::pickerInput
  
  modalDialog(
    title = "Set a new RGB image",
    size = "m",
    
    # setSliderColor(c("", "red", "darkgreen", "blue"), c(2, 3, 4)),
    # # this is 2, 3, 4 becaluse sliderInput 1 is "max_masked_perc".
    # # in case other sliderInput are added, check this!
    
    radioButtons(
      "newrgb_source",
      label = "RGB source:",
      choices = list("BOA" = "BOA", "TOA" = "TOA"),
      selected = "BOA",
      inline = TRUE
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          "band_r", 
          label = HTML("<span style='color:red;'>Red band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band4"#,
          # options = list(style = "btn-danger")
        )
      ),
      column(
        width = 8,
        sliderInput(
          "band_r_range", 
          label = HTML("<span style='color:red;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          "band_g", 
          label = HTML("<span style='color:darkgreen;'>Green band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band3"#,
          # options = list(style = "btn-danger")
        )
      ),
      column(
        width = 8,
        sliderInput(
          "band_g_range", 
          label = HTML("<span style='color:darkgreen;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          "band_b", 
          label = HTML("<span style='color:blue;'>Blue band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band2"#,
          # options = list(style = "btn-primary")
        )
      ),
      column(
        width = 8,
        sliderInput(
          "band_b_range", 
          label = HTML("<span style='color:blue;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),

    easyClose = FALSE,
    footer = tagList(
      actionButton("add_new_rgb", strong("\u2000Add"), icon=icon("check")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}

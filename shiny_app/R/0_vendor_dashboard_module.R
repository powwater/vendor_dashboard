vendor_dashboard_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
        h5("Map:"),
        uiOutput(ns("global_map"))
      )
    ),
    fluidRow(
      column(
        12,
        h5("Region:"),
        uiOutput(ns("vendor_region"))
      )
    )
  )
}

vendor_dashboard <- function(input, output, session, vendor_info, is_mobile) {

  output$vendor_region <- renderUI({
    HTML(
      paste0(
        '<iframe width="100%" height="450" style="border:0" loading="lazy" allowfullscreen
      src="https://www.google.com/maps/embed/v1/place?q=place_id:',
      vendor_info()$region_id, '&key=', key, '"></iframe>'
      )
    )
  })

  output$global_map <- renderUI({
    HTML(
      '<iframe src="https://www.google.com/maps/d/embed?mid=1wAlwYO6JtA_JZlmcrnfLjIySRFzVU0yF&hl=en" width="100%" height="550"></iframe>'
      )
  })

  # top customers


}

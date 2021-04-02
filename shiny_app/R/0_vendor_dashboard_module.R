vendor_dashboard_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        6,
        h5("Region:"),
        uiOutput(ns("vendor_region"))
      ),
      column(
        6,
        h5("Vendor Details:") #,
        # googleway::google_mapOutput(ns("customer_locations"))
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

  # top customers


}

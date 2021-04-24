user_module_ui <- function(id) {

  ns <- NS(id)

  div(
    br(),
    hr(),
    uiOutput(ns("user_panel")),
    div(
      id = ns("edit_user_div"),
      style = "text-align: center;",
      actionLink(ns("edit_user"), "Edit", icon = icon("edit"))
    ),
    hr(),
    br()
  )
}

user_module <- function(input, output, session, vendor_info) {

  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFileChoose(input,
                  "document",
                  roots = volumes,
                  session = session,
                  defaultRoot = "Home")

  observe({
    cat("\ninput$file value:\n\n")
    print(input$file)
  })

  output$filepaths <- renderPrint({
    if (is.integer(input$document)) {
      cat("No files have been selected")
    } else {
      parseFilePaths(volumes, input$document)
    }
  })

  image_path <- reactive({
    vend <- vendor_info()$vendor_name
    out <- switch(tolower(vend),
                  "dutch water" = "images/profiles/dutch_water.png",
                  "images/profiles/no_picture.png")
    out
  })

  is_open <- reactive({
    TRUE
    # is_vendor_open(conn, vendor_info()$vendor_uid)
  })

  output$user_panel <- renderUI({

    img <- image_path()

    if (is_open()) {
      online_badge <- shinydashboardPlus::dashboardBadge("Open", color = "green")
      wifi_color <- paste0("color: ", pow_colors$darkgreen)
    } else {
      online_badge <- shinydashboardPlus::dashboardBadge("Closed", color = "red")
      wifi_color <- paste0("color: ", pow_colors$grey)
    }

    div(
      id = session$ns("user_panel"),
      sidebarUserPanel(
        name = paste0("Vendor: ", vendor_info()$vendor_name),
        subtitle = tags$span(
          icon("wifi", style = wifi_color),
          ": ",
          online_badge
        ),
        image = img
      )
    )
  })

  observeEvent(input$edit_user, {
    shiny::showModal(
      shiny::modalDialog(
        title = icon_text("id-badge", "Vendor Profile:"),
        size = 'm',
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            session$ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        boxProfile(
          src = image_path(),
          title = vendor_info()$vendor_name,
          subtitle = "Vendor Profile",
          boxProfileItemList(
            bordered = TRUE,
            boxProfileItem(
              title = span(icon("box"), "Total Historical Orders"),
              description = "44 orders"
            ),
            boxProfileItem(
              title = span(icon("money"),"2021 Earnings"),
              description = "100,000 KES"
            ),
            boxProfileItem(
              title = span(icon("clock-o"), "Working Hours"),
              description = "Monday-Friday 8AM to 8PM"
            ),
            boxProfileItem(
              title = span(icon("info"), "Operation Description"),
              description = "Dutch Water"
            )
          )
        ),
        span(
          shinyFiles::shinyFilesButton(session$ns("document"),
                                       "Upload Documents",
                                       "Please select a document to upload.",
                                       multiple = TRUE,
                                       viewtype = "detail"),

          verbatimTextOutput(session$ns("filepaths"))

        )
      )
    )
  })

  observeEvent(input$submit, {
    removeModal()
  })

  ## print to browser
  output$filepaths <- renderPrint({
    if (is.integer(input$document)) {
      cat("")
    } else {
      hold <- parseFilePaths(volumes, input$document)
      paste0("Selected: ", hold$datapath)
    }
  })

}

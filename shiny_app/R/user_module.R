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

  image_path <- reactive({

    # hold <- paste0(
    #   "images/profiles/",
    #   snakecase::to_snake_case(tolower(vendor_info()$vendor_name)),
    #   ".png"
    # )
    #
    # out <- if (file.exists(hold)) hold else "images/profiles/no_picture.png"

    "images/profiles/dutch_water.png"

    # out
  })

  output$user_panel <- renderUI({

    img <- image_path()

    div(
      id = session$ns("user_panel"),
      sidebarUserPanel(
        name = paste0("Vendor: ", vendor_info()$vendor_name),
        subtitle = tags$span(
          icon("wifi"),
          ": ",
          shinydashboardPlus::dashboardBadge("Online", color = "green")
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
          ),

          shinyFiles::shinyFilesButton(session$ns("document"),
                                       "Upload Documents",
                                       "Please select a document to upload.",
                                       FALSE)
        )
      )
    )
  })

  observeEvent(input$submit, {
    removeModal()
  })

  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes(exclude = "wd")())

  shinyFileChoose(input, session$ns("document"), roots = volumes, session = session, defaultRoot = "Home")

  ## print to browser
  output$filepaths <- renderPrint({
    if (is.integer(input$document)) {
      cat("No files have been selected (shinyFileChoose)")
    } else {
      parseFilePaths(volumes, input$document)
    }
  })

}

server <- function(input, output, session) {

  logged_in_vendor_info <- reactive({

    list(
      vendor_uid = "c401b531-719d-4cad-82e7-71db3ffba166",
      vendor_location_uid = "702e0732-db29-479a-a2a0-2595392e7280",
      vendor_name = "Dutch Water",
      place_id = "ChIJrToahpQJQBgRZ6ukvDc1LO4",
      region_id = "ChIJ5b0LA6wOQBgRe0sIruEoRCc"
    )

  })

  callModule(
    powpolished::profile_module,
    'polished_profile'
  )

  output$user_panel <- renderUI({
    tagList(
      br(),
      hr(),
      div(
        id = "user_panel",
        sidebarUserPanel(
          name = paste0("Vendor: ", logged_in_vendor_info()$vendor_name),
          subtitle = tags$span("Current Status: ", shinydashboardPlus::dashboardBadge("Online", color = "green")),
          image = "images/no_picture.png"
        ),
        div(
          id = "edit_user_div",
          style = "text-align: center;",
          actionLink("edit_user", "Edit", icon = icon("edit"))
        ) %>% shinyjs::hidden()
      ),
      hr(),
      br()
    )
  })

  observeEvent(input$waiter_trigger == '1', {
    req(input$waiter_trigger == '1')
    waiter_hide()
  })

  shinyjs::onevent("mouseleave", "user_panel", shinyjs::hide("edit_user_div", anim = TRUE))
  shinyjs::onevent("mouseenter", "user_panel", shinyjs::show("edit_user_div", anim = TRUE))

  observeEvent(input$edit_user, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Edit Vendor Profile:",
        size = 'm',
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            'submit',
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        boxProfile(
          src = "images/no_picture.png",
          title = logged_in_vendor_info()$vendor_name,
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
        )
      )
    )
  })

  observeEvent(input$submit, {
    removeModal()
  })

  callModule(
    customers_module,
    "customers_module",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    orders_module,
    "orders_module",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    tests_module,
    "tests_module",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    inventory_module,
    "inventory_module",
    vendor_info = logged_in_vendor_info
  )

  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes(exclude = "wd")())

  shinyFileChoose(input, "document", roots = volumes, session = session, defaultRoot = "Home")

  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  observe({
    cat("\ninput$document value:\n\n")
    print(input$document)
  })

  ## print to browser
  output$filepaths <- renderPrint({
    if (is.integer(input$document)) {
      cat("No files have been selected (shinyFileChoose)")
    } else {
      parseFilePaths(volumes, input$document)
    }
  })
}

powpolished::secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

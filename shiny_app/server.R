server <- function(input, output, session) {

  logged_in_vendor_info <- reactive({

    list(
      vendor_uid = "c401b531-719d-4cad-82e7-71db3ffba166",
      vendor_name = "Dutch Water",
      place_id = "ChIJrToahpQJQBgRZ6ukvDc1LO4",
      region_id = "ChIJ5b0LA6wOQBgRe0sIruEoRCc"
    )

  })

  callModule(
    profile_module,
    'polished_profile'
  )

  output$user_panel <- renderUI({
    sidebarUserPanel(name = paste0("Vendor: ", logged_in_vendor_info()$vendor_name), #session$userData$user()$email,
                     subtitle = tags$span("Current Status: ", shinydashboardPlus::dashboardBadge("Online", color = "green")),
                     image = NULL)
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

polished::secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

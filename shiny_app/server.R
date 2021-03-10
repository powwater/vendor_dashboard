server <- function(input, output, session) {

  logged_in_vendor_info <- reactive({

    list(
      vendor_uid = "c401b531-719d-4cad-82e7-71db3ffba166",
      vendor_name = "Dutch Water"
    )

  })

  callModule(
    profile_module,
    'polished_profile'
  )

  output$user_panel <- renderUI({
    sidebarUserPanel(name = session$userData$user()$email,
                     subtitle = Sys.Date(),
                     image = NULL)
  })

  callModule(
    customers_module,
    "customers_module",
    vendor_info = logged_in_vendor_info
  )
  #
  # callModule(
  #   orders_module,
  #   "orders_module"
  # )

}

polished::secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

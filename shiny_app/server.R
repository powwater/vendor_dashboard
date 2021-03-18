server <- function(input, output, session) {

  observeEvent(input$waiter_trigger == '1', {
    req(input$waiter_trigger == '1')
    waiter_hide()
  })

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

  callModule(
    right_sidebar_module,
    "rightbar",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    user_module,
    "user_panel",
    vendor_info = logged_in_vendor_info
  )

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
}

powpolished::secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

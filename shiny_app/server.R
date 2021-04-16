server <- function(input, output, session) {

  logger::log_layout(layout_glue_colors)
  logger::log_shiny_input_changes(input = input, level = INFO)

  observeEvent(input$waiter_trigger == '1', {
    req(input$waiter_trigger == '1')
    waiter_hide()
  })

  is_mobile_device_rv <- reactiveVal(NULL)

  observeEvent(input$is_mobile_device, {
    print(list("Mobile Device Detected: " = isTRUE(input$is_mobile_device)))
    if (isTRUE(input$is_mobile_device)) {
      is_mobile_device_rv(TRUE)
      shinyjs::addCssClass(selector = "nav > div:nth-child(3)", class = "mobile-hide")
    } else {
      is_mobile_device_rv(FALSE)
    }
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
    profile_module,
    'polished_profile'
  )

  configs <- callModule(
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
    vendor_dashboard,
    "vendor_dashboard_module",
    vendor_info = logged_in_vendor_info,
    is_mobile = is_mobile_device_rv
  )

  callModule(
    customers_module,
    "customers_module",
    vendor_info = logged_in_vendor_info,
    configs = configs,
    is_mobile = is_mobile_device_rv
  )

  callModule(
    orders_module,
    "orders_module",
    vendor_info = logged_in_vendor_info,
    is_mobile = is_mobile_device_rv
  )

  callModule(
    tests_module,
    "tests_module",
    vendor_info = logged_in_vendor_info,
    is_mobile = is_mobile_device_rv
  )

  callModule(
    inventory_module,
    "inventory_module",
    vendor_info = logged_in_vendor_info,
    is_mobile = is_mobile_device_rv
  )

  callModule(
    riders_module,
    "riders_module",
    vendor_info = logged_in_vendor_info,
    is_mobile = is_mobile_device_rv
  )
}

secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

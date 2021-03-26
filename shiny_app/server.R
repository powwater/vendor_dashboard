server <- function(input, output, session) {

  observeEvent(input$waiter_trigger == '1', {
    req(input$waiter_trigger == '1')
    waiter_hide()
  })

  callModule(
    polished::profile_module,
    'polished_profile'
  )

  logged_in_vendor_info <- reactive({
    req(input$vendor)
    vendor_uid <- input$vendor
    vendors_info[[vendor_uid]]
  })

  configs <- callModule(
    right_sidebar_module,
    "rightbar",
    vendor_info = logged_in_vendor_info
  )

  observeEvent(configs(), print(list(configs = configs())))

  callModule(
    user_module,
    "user_panel",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    vendor_dashboard,
    "vendor_dashboard_module",
    vendor_info = logged_in_vendor_info
  )

  callModule(
    customers_module,
    "customers_module",
    vendor_info = logged_in_vendor_info,
    configs = configs
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

  callModule(
    riders_module,
    "riders_module",
    vendor_info = logged_in_vendor_info
  )
}

polished::secure_server(
  server,
  custom_sign_in_server = sign_in_module_2
)

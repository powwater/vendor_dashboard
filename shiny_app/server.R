server <- function(input, output, session) {

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

  user_data <- reactive({
    user_data <- session$userData$user()
    user_uid <- user_data$user_uid
    is_user_admin <- user_data$is_admin
    list(uid = user_uid, is_admin = is_user_admin, data = user_data)
  })

  session$userData$vendor <- reactiveVal(NULL)

  observeEvent(session$userData$user(), {
    hold_user <- session$userData$user()


    if (is_powwater_admin_user(hold_user$email)) {
      out <- NULL

      hold_vendors <- tbl(conn, "vendors") %>%
        select(uid, vendor_name) %>%
        collect() %>%
        arrange(vendor_name)

      vendor_choices <-  hold_vendors$uid
      names(vendor_choices) <- hold_vendors$vendor_name

      shiny::showModal(
        modalDialog(
          size = "s",
          selectInput(
            "select_vendor",
            label = "Vendor",
            choices = vendor_choices
          ),
          title = "Select Vendor",
          footer = shiny::actionButton(
            "submit_select_vendor",
            "Submit"
          )
        ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999")
      )

      observeEvent(input$submit_select_vendor, {
        out <- list(vendor_uid = input$select_vendor)
        session$userData$vendor(out)
        shiny::removeModal()
      })

    } else {
      out <- tbl(conn, "vendor_users") %>%
        filter(
          .data$user_uid == local(hold_user$user_uid)
        ) %>%
        select(vendor_uid) %>%
        collect()

      if (identical(nrow(out), 1L)) {

        session$userData$vendor(list(vendor_uid = out$vendor_uid))

      } else {

        # sign the user out.  They are not linked up to any particular vendor.
        polished::sign_out_from_shiny()
        session$reload()

      }



    }
  })

  logged_in_vendor_info <- reactive({
    req(session$userData$vendor())

    user_vendor <- session$userData$vendor()$vendor_uid
    get_vendor_info(conn = conn, user_vendor)
  })

  callModule(
    profile_module,
    'polished_profile'
  )

  observeEvent(logged_in_vendor_info(), {
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
  })

}

secure_server(
  server,
  custom_sign_in_server = pow_sign_in_module_2
)


# deprecated --------------------------------------------------------------

# logger::log_layout(layout_glue_colors)
# logger::log_shiny_input_changes(input = input, level = INFO)

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

  vendor_users <- reactiveVal(NULL)

  observeEvent(session$userData$user(), {
    user_email <- session$userData$user()$email

    if (is_powwater_admin_user(user_email)) {
      out <- NULL

      hold_vendors <- tbl(conn, "vendors") %>%
        select(uid, vendor_name)

      available_vendor_users <- tbl(conn, "vendor_users") %>%
        window_order(created_at) %>%
        select(user_uid, vendor_uid) %>%
        left_join(hold_vendors, by = c("vendor_uid" = "uid")) %>%
        collect()

      # IFF no Vendor Users, send Admin to Admin Panel to Add first vendor user
      #   Will only occur for EMPTY app (No vendors OR users)

      # if (nrow(available_vendor_users) == 0) {
      #   vendor_choices <-  list(hold_vendors$uid)
      #   names(vendor_choices) <- available_vendor_users$vendor_name
      #
      #   shiny::showModal(
      #     modalDialog(
      #       shinyWidgets::pickerInput(
      #         "select_vendor",
      #         choices = vendor_choices
      #       ),
      #       title = "Select Vendor",
      #       footer = shiny::actionButton(
      #         "submit_select_vendor",
      #         "Submit"
      #       )
      #     ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999"),
      #     session
      #   ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999")
      #
      #   observeEvent(input$submit_select_vendor, {
      #     shiny::updateQueryString(
      #       queryString = paste0("?page=admin_panel"),
      #       session = session,
      #       mode = "push"
      #     )
      #
      #     session$reload()
      #   })
      #
      #
      # } else {
      #   vendor_choices <-  list(available_vendor_users$vendor_uid)
      #   names(vendor_choices) <- available_vendor_users$vendor_name
      #
      #   shiny::showModal(
      #     modalDialog(
      #       shinyWidgets::pickerInput(
      #         "select_vendor",
      #         choices = vendor_choices
      #       ),
      #       title = "Select Vendor",
      #       footer = shiny::actionButton(
      #         "submit_select_vendor",
      #         "Submit"
      #       )
      #     ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999"),
      #     session
      #   ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999")
      #
      #   observeEvent(input$submit_select_vendor, {
      #     out <- list(vendor_uid = input$select_vendor)
      #     vendor_users(out)
      #     shiny::removeModal()
      #   })
      # }

      vendor_choices <-  list(available_vendor_users$vendor_uid)
      names(vendor_choices) <- available_vendor_users$vendor_name

      shiny::showModal(
        modalDialog(
          shinyWidgets::pickerInput(
            "select_vendor",
            choices = vendor_choices
          ),
          title = "Select Vendor",
          footer = shiny::actionButton(
            "submit_select_vendor",
            "Submit"
          )
        ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999"),
        session
      ) %>% shiny::tagAppendAttributes(style = "z-index: 9999999")

      observeEvent(input$submit_select_vendor, {
        out <- list(vendor_uid = input$select_vendor)
        vendor_users(out)
        shiny::removeModal()
      })

    } else {
      out <- tbl(conn, "vendor_users") %>%
        filter(.data$user_uid %in% .env$usr) %>%
        collect()

      vendor_users(out)
    }
  })

  logged_in_vendor_info <- reactive({
    req(vendor_users())
    user_vendor <- unique(vendor_users()$vendor_uid) # [match(user_uid, vendor_users()$user_uid)]
    get_vendor_info(conn = conn, user_vendor)
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


# deprecated --------------------------------------------------------------

# logger::log_layout(layout_glue_colors)
# logger::log_shiny_input_changes(input = input, level = INFO)

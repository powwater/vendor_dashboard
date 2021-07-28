menu_info <- function(title, description) {
  htmlTemplate(filename = "html/menu_info.html",
               title = title,
               description = description)
}

right_sidebar_module_ui <- function(id) {

  ns <- NS(id)

  shinydashboardPlus::rightSidebar(
    title = "Configurations and Logs",
    width = 350,
    rightSidebarTabContent(
      id = ns("config"),
      icon = "cogs",
      title = tags$div(style = "text-align: center;", icon_text("cogs", "Configuration")),
      active = TRUE,
      div(
        style = "text-align: center;",
        br(),
        br(),
        shinyWidgets::pickerInput(
          ns("currency"),
          label = icon_text("wallet", "Select Currency:"),
          choices = choices$currency,
          selected = "kes"
        ),
        br(),
        shinyWidgets::pickerInput(
          ns("timezone"),
          icon_text("clock-o", "Select Timezone: "),
          choices = choices$timezone,
          selected = "auto"
        ),
        br(),
        awesomeRadio(
          inputId = ns("phone_number_format"),
          label = "Select Phone Number Format:",
          choices = choices$phone_number_format,
          selected = choices$phone_number_format[1],
          inline = TRUE,
          checkbox = TRUE
        ),
        shiny::helpText("Example National: 712121974"),
        shiny::helpText("Example International: +254712121974"),
        br(),
        shinyWidgets::awesomeCheckboxGroup(
          ns("payment_types"),
          icon_text("wallet", "Select Available Payment Types: "),
          choices = choices$payment_types,
          selected = choices$payment_types,
          inline = TRUE
        ),
        br(),
        shinyWidgets::numericInputIcon(
          ns("vendor_commission"),
          icon_text("percent", "Default Vendor Commission:"),
          value = 10,
          min = 0,
          max = 100,
          step = 1,
          icon = icon("percent"),
          help_text = "Please enter a value between 1 and 100"
        ),
        br(),
        shinyWidgets::numericInputIcon(
          ns("rider_commission"),
          icon_text("percent", "Default Rider Commission:"),
          value = 10,
          min = 0,
          max = 100,
          step = 1,
          icon = icon("percent"),
          help_text = "Please enter a value between 1 and 100"
        ),
        br(),
        shinyWidgets::prettyCheckbox(
          ns("provide_discount"),
          icon_text("check-circle", "Discount?"),
          value = FALSE
        ),
        br(),
        div(
          id = ns("discount_inputs"),
          shinyWidgets::numericInputIcon(
            ns("discount_minimum"),
            icon_text("user-tag", "Discount Minimum:"),
            value = 0,
            min = 0,
            max = Inf,
            step = 100,
            icon = icon("money-bill-alt")
          ),
          br(),
          shinyWidgets::numericInputIcon(
            ns("discount_percentage"),
            icon_text("percentage", "Discount Percentage:"),
            value = 10,
            min = 0,
            max = 100,
            step = 1,
            icon = icon("percent"),
            help_text = "Please enter a value between 1 and 100"
          )
        ) %>% shinyjs::hidden()
      )
    ),

    rightSidebarTabContent(
      id = ns("logs"),
      title = tags$div(style = "text-align: center;", icon_text("list", "Activity Logs")),
      icon = "list",
      uiOutput(ns("logsUI"))
    ),

    rightSidebarTabContent(
      id = ns("usermgmt"),
      title = tags$div(style = "text-align: center;", icon_text("users", "User Management")),
      icon = "users"#,
      # uiOutput(ns("usersUI"))
    )
  )

}

right_sidebar_module <- function(input, output, session, vendor_info) {


  # output$phone_number_example <- renderText({
  #   req(input$phone_number_format)
  #   switch(input$phone_number_format,
  #          )
  # })

  image_path <- reactive({
    req(vendor_info())
    vend <- vendor_info()$vendor_name
    out <- switch(tolower(vend),
                  "dutch water" = "images/profiles/dutch_water.png",
                  "images/profiles/no_picture.png")
    out
  })

  output$logsUI <- renderUI({
    shinydashboardPlus::userMessages(
      width = 12,
      status = "info",
      userMessage(
        author = "User A",
        date = "2021-03-10 13:45",
        icon_text("user", "Example Log 1"),
        src = image_path()
      ),
      userMessage(
        author = "User C",
        date = "2021-03-09 09:15",
        icon_text("database", "Example Log 2"),
        src = image_path()
      ),
      userMessage(
        author = "User A",
        date = "2021-03-07 18:20",
        icon_text("money-bill-alt", "Example Log 3"),
        src = image_path()
      )
    )
  })

  observeEvent(input$provide_discount, {
    if (input$provide_discount == TRUE) {
      shinyjs::show("discount_inputs")
    } else {
      shinyjs::hide("discount_inputs")
    }
  })

  configs <- reactive({
    list(
      currency = input$currency,
      timezone = input$timezone,
      phone_number_format = input$phone_number_format,
      payment_types = input$payment_types,
      vendor_commision = input$vendor_commission,
      rider_commision = input$rider_commission,
      provide_discount = input$provide_discount,
      discount_minimum = input$discount_minimum,
      discount_percentage = input$discount_percentage
    )
  })

  configs
}

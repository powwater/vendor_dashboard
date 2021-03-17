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
            icon = icon("money")
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
      title = "Activity Logs",
      icon = "list",
      shinydashboardPlus::userMessages(
        width = 12,
        status = "info",
        userMessage(
          author = "User A",
          date = "2021-03-10 13:45",
          icon_text("user", "Example Log 1")
        ),
        userMessage(
          author = "User C",
          date = "2021-03-09 09:15",
          icon_text("database", "Example Log 2")
        ),
        userMessage(
          author = "User A",
          date = "2021-03-07 18:20",
          icon_text("money", "Example Log 3")
        )
      )
    )
  )

}

right_sidebar_module <- function(input, output, session, vendor_info) {




  observeEvent(input$provide_discount, {
    if (input$provide_discount == TRUE) {
      shinyjs::show("discount_inputs")
    } else {
      shinyjs::hide("discount_inputs")
    }
  })

}

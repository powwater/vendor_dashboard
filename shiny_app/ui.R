header <- dashboardHeaderPlus(

  title = tagList(
    tags$span(
      class = "logo-lg",
      tags$a(
        htmltools::tags$img(
          style = "margin-top: -5px",
          src = "images/primary_logo.png",
          width = 200
        ),
        href = "https://powwater.com",
        target = "_blank"
      )
    ),
    tags$img(
      style = "height: 38px; width: 25px;",
      src = "images/logo_droplet.png"
    )
  ),
  left_menu = tagList(
    htmltools::h4(
      style = "margin-top: 5px;",
      "Vendor Dashboard | Version: ",
      shinydashboardPlus::dashboardBadge("Alpha"),
      "| Last Updated on: ",
      shinydashboardPlus::dashboardBadge(get_last_updated_date(), color = "green")
    )
  ),
  # about_bttn(),
  contact_menu(c(
    contact_item("Andy Merlino", "Developer", "404-514-6417", "andy.merlino@tychobra.com"),
    contact_item("Jimmy Briggs", "Developer", "678-491-4856", "jimmy.briggs@tychobra.com"),
    contact_item("Patrick Howard", "Developer", "404-408-2500", "patrick.howard@tychobra.com")
  )),
  polished::profile_module_ui("polished_profile")
)

sidebar <- dashboardSidebar(

  tagList(

    div(
      class = "text-center",
      uiOutput("user_panel")
    ),
    sidebarMenu(
      id = 'sidebar_menu',
      menuItem(
        text = 'Customers',
        tabName = 'customers_tab',
        icon = icon('users')
      ),
      menuItem(
        text = 'Orders',
        tabName = 'orders_tab',
        icon = icon('money')
      )
    )
  )
)

body <- dashboardBody(
  tags$head(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    includeCSS("www/stars.css"),
    includeCSS("www/styles.css"),
    intlTelInputDependencies()
    # shinylogs::use_tracking()
  ),
  tabItems(
    tabItem(
      tabName = 'customers_tab',
      customers_module_ui("customers_module")
    ),
    tabItem(
      tabName = 'orders_tab',
      orders_module_ui('orders_module')
    )
  )
)

ui <- shinydashboardPlus::dashboardPagePlus(
  header,
  sidebar,
  body,
  title = 'Powwater Vendor Dashboard',
  skin = 'black',
  enable_preloader = TRUE,
  loading_duration = 2
)

polished::secure_ui(
  ui,
  sign_in_ui_default(
    sign_in_module = sign_in_module_2_ui('sign_in'),
    color = '#2F3474',
    company_name = 'POWWATER',
    logo_top = tags$div(
      style = 'width: 100%; margin-top: 25px; margin-bottom: 40px;',
      tags$img(
        src = 'images/logo_home.png',
        alt = 'POWWATER Logo',
        width = '30%'
      )
    ),
    icon_href = 'images/logo_droplet.png',
    background_image = "images/power_of_water.png"
  )
)

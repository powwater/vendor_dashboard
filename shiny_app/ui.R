waiting_screen <- tagList(
  tags$div(
    style = 'width: 100%;', # margin-top: 25px; margin-bottom: 40px;',
    tags$img(
      src = "spinner.gif",
      alt = 'POWWATER Logo',
      width = '100%'
    )
  ),
  br(),
  spin_chasing_dots(),
  br(),
  span("Loading...", style = "color:white;")
)

header <- dashboardHeaderPlus(
  title = header_title(),
  left_menu = header_left_menu("Vendor Dashboard", app_config$app_version),
  header_contact_menu(),
  profile_module_ui("polished_profile"),
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "cogs"
)

sidebar <- dashboardSidebar(
  tagList(
    user_module_ui("user_panel"),
    sidebarMenu(
      id = 'sidebar_menu',
      menuItem(
        text = "Dashboard",
        tabName = "vendor_dash",
        icon = icon("dashboard")
      ),
      menuItem(
        text = 'Customers',
        tabName = 'customers_tab',
        icon = icon('users'),
        selected = TRUE
      ),
      menuItem(
        text = 'Orders',
        tabName = 'orders_tab',
        icon = icon('money'),
        badgeLabel = "!",
        badgeColor = "red"
      ),
      menuItem(
        text = 'Tests',
        tabName = 'tests_tab',
        icon = icon("hand-holding-water")
      ),
      menuItem(
        text = 'Inventory',
        tabName = 'inventory_tab',
        icon = icon("warehouse")
      ),
      menuItem(
        text = 'Riders',
        tabName = 'riders_tab',
        icon = icon("biking")
      )
    )
  )
)

body <- dashboardBody(
  waiter::use_waiter(),
  waiter::waiter_show_on_load(waiting_screen),
  shinyscroll::use_shinyscroll(),
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  tags$head(
    htmltools::tags$script(src = "https://cdn.datatables.net/1.10.24/js/jquery.dataTables.min.js",
                           type = "text/javascript",
                           language = "JavaScript"),
    htmltools::tags$script(src = "https://cdn.datatables.net/1.10.24/js/dataTables.bootstrap.min.js",
                           type = "text/javascript",
                           language = "JavaScript"),
    includeCSS("https://cdn.datatables.net/1.10.24/css/dataTables.bootstrap.min.css"),
    htmltools::tags$script(src = "custom.js",
                           type = "text/javascript",
                           language = "JavaScript"),
    htmltools::tags$script(src = "js/is_mobile.js",
                           type = "text/javascript",
                           language = "JavaScript"),
    includeCSS("www/stars.css"),
    includeCSS("www/styles.css")
  ),
  tabItems(
    tabItem(
      tabName = "vendor_dash",
      vendor_dashboard_ui("vendor_dashboard_module")
    ),
    tabItem(
      tabName = 'customers_tab',
      customers_module_ui("customers_module")
    ),
    tabItem(
      tabName = 'orders_tab',
      orders_module_ui('orders_module')
    ),
    tabItem(
      tabName = "tests_tab",
      tests_module_ui("tests_module")
    ),
    tabItem(
      tabName = "inventory_tab",
      inventory_module_ui("inventory_module")
    ),
    tabItem(
      tabName = "riders_tab",
      riders_module_ui("riders_module")
    )
  )
)

ui <- shinydashboardPlus::dashboardPagePlus(
  header,
  sidebar,
  body,
  rightsidebar = right_sidebar_module_ui("rightbar"),
  title = 'Powwater Vendor Dashboard',
  skin = 'black'
)

secure_ui(
  ui,
  sign_in_page_ui = sign_in_ui_default(
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
  ),
  custom_admin_button_ui = admin_button_ui(align = "left", vertical_align = "bottom")
)

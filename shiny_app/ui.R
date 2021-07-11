waiting_screen <- tagList(
  tags$div(
    style = 'width: 100%;', # margin-top: 25px; margin-bottom: 40px;',
    tags$img(
      src = "images/drop_full_size.png",
      alt = 'POWWATER Logo',
      width = "230px"
    )
  ),
  br(),
  spin_chasing_dots(),
  br()
)

header <- dashboardHeaderPlus(
  title = tags$span(
    shiny::tags$img(
      style = "margin-top: -5px",
      src = "images/logo_text.png",
      width = 150
    ),
    href = "https://powwater.com",
    target = "_blank"
  ),
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
        icon = icon('money-bill-alt'),
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
      ),
      tags$div(
        style = "position: fixed; bottom: 5px; left: 15px;",
        a(
          img(
            src = "images/pow_text_white.png",
            width = 200
          ),
          href = "https://powwater.org"
        )
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
    tags$link(rel = "shortcut icon", href = "images/drop_32.png"),
    tags$script(
      src = "js/vendor.js?version=1",
      type = "text/javascript",
      language = "JavaScript"
    ),
    tags$script(
      src = "js/is_mobile.js",
      type = "text/javascript",
      language = "JavaScript"
    ),
    tags$link(rel = "stylesheet", href = "css/vendor_stars.css?version=1"),
    tags$link(rel = "stylesheet", href = "css/vendor_styles.css?version=1"),
    tags$link(rel = "stylesheet", href = "css/water_rating_icons.css?version=1")
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
        src = 'images/logo_text.png',
        alt = 'POWWATER Logo',
        style = "width = 300px; max-width: 100%;"
      )
    ),
    icon_href = 'images/drop_32.png',
    background_image = "images/power_of_water.png"
  )
)

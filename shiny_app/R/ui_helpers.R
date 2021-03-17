header_title <- function() {
  tagList(
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
  )
}

header_left_menu <- function(app_name, app_version) {
  tagList(
    htmltools::h4(
      style = "margin-top: 5px;",
      paste0(app_name, " | Version: "),
      shinydashboardPlus::dashboardBadge(app_version),
      "| Last Updated on: ",
      shinydashboardPlus::dashboardBadge(get_last_updated_date(), color = "green")
    )
  )
}

header_contact_menu <- function() {
  contact_menu(c(
    contact_item("Andy Merlino", "Developer", "404-514-6417", "andy.merlino@tychobra.com"),
    contact_item("Jimmy Briggs", "Developer", "678-491-4856", "jimmy.briggs@tychobra.com"),
    contact_item("Patrick Howard", "Developer", "404-408-2500", "patrick.howard@tychobra.com")
  ))
}

#' Contact Item
#'
#' Creates an item to be placed in a contact dropdownmenu.
#'
#' @param name Name
#' @param role Role
#' @param phone Phone
#' @param email Email
#'
#' @return contact menu item
#' @export
#' @importFrom shiny tagList icon
#' @importFrom htmltools tags a
contact_item <- function(name = "First Name, Last Name",
                         role = "Role",
                         phone = "###-###-####",
                         email = "first.last@oliverwyman.com"){

  shiny::tagList(
    htmltools::tags$li(htmltools::a(style = "pointer-events: none; cursor: default;", shiny::h4(tags$b(name)), shiny::h5(tags$i(role)))),
    htmltools::tags$li(htmltools::a(shiny::icon("envelope"), href = paste0("mailto:", email), email)),
    htmltools::tags$li(htmltools::a(shiny::icon("phone"), href = paste0("tel:+1-", phone), phone)),
    htmltools::tags$hr()
  )

}

#' Creates a dropdown menu specific for contacts
#'
#' @param ... contact items to put into dropdown
#'
#' @return menu
#' @export
#' @importFrom shiny tags div
contact_menu <- function(...){

  items <- c(list(...))

  shiny::tags$li(
    class = "dropdown",
    shiny::tags$a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      shiny::div(
        shiny::tags$i(
          class = "fa fa-phone"
        ),
        style = "display: inline"
      ),
      shiny::tags$ul(
        class = "dropdown-menu",
        items)
    )
  )
}


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

#' Icon Text
#'
#' Creates an HTML div containing the icon and text.
#'
#' @param icon fontawesome icon
#' @param text text
#'
#' @return HTML div
#' @export
#'
#' @examples
#' icon_text("table", "Table")
#'
#' @importFrom shiny icon tagList
#' @importFrom shiny icon tagList
icon_text <- function(icon, text) {

  i <- shiny::icon(icon)
  t <- paste0(" ", text)

  tags$span(i, t)

}

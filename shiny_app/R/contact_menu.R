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

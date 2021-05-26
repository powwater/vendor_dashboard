

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

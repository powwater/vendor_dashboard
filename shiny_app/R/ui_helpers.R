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

ui <- fluidPage(
  uiOutput('button'),
  verbatimTextOutput('input_button')
)
server <- function(input, output) {
  # you must crate a path in www for saving images (www/img/...)
  output$button <- renderUI({
    buttonImageInput(inputId = 'chosen_button',
                     images = basename(fs::dir_ls("www/images")),
                     ncol = 2,
                     active = basename(fs::dir_ls("www/images"))[1],
                     path = "images/")
  })
  # print input id when clicking
  output$input_button <- renderPrint({
    input$chosen_button
  })
}
shinyApp(ui, server)

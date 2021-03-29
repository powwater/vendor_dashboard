riders_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("car-side", 'Drivers'),
        footer = "Powwater | Tychobra 2021",
        status = "primary",
        solidHeader = TRUE,
        br(),
        br(),
        column(
          width = 12,
          DT::DTOutput(ns('riders_table')) %>%
            shinycustomloader::withLoader(),
          hr()
        )
      )
    )
  )
}

riders_module <- function(input, output, session, vendor_info) {

  ns <- session$ns

  riders <- reactive({

    id <- notify("Loading Riders from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({
      out <- get_riders_by_vendor(vend, conn)
    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })
    out
  })

  riders_prep <- reactiveVal(NULL)

  observeEvent(riders(), {

    hold <- riders() %>%
      select(-rider_uid, -vendor_uid, -c(created_at:modified_by))

    riders_prep(hold)

  })

  output$riders_table <- DT::renderDT({

    req(riders_prep())

    out <- riders_prep()

    cols <- snakecase::to_title_case(colnames(out))

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = 'table table-striped table-bordered table-hover nowrap table',
      extensions = c("Buttons"),
      filter = "top",
      width = "100%",
      options = list(
        scrollX = TRUE,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "riders-table")
      )
    )
  })

}

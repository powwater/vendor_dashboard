riders_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        box(
          width = 12,
          title = icon_text("car-side", 'Powwater Riders'),
          footer = "Powwater | Tychobra 2021",
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(
              width = 12,
              helpText("Powwater rides are assigned to vendors by Powwater to fulfill order deliveries to customers."),
              DT::DTOutput(ns('pow_riders_table')) %>%
                shinycustomloader::withLoader()
            )
          )
        ),
        box(
          width = 12,
          title = icon_text("car-side", 'Vendor Riders'),
          footer = "Powwater | Tychobra 2021",
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(
              12,
              helpText("Vendor specific riders are employees of the vendor and only deliver for that vendor."),
              DT::DTOutput(ns('vendor_riders_table')) %>%
                shinycustomloader::withLoader()
            )
          )
        )
      )
    )
  )
}

riders_module <- function(input, output, session, vendor_info, is_mobile) {

  riders <- reactive({

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({
      out <- get_riders_by_vendor(vend, conn)
    }, error = function(err) {
      msg <- 'Error collecting riders from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })
    out
  })

  pow_riders <- reactive({
    riders()$pow_riders
  })

  pow_riders_prep <- reactiveVal(NULL)

  observeEvent(pow_riders(), {

    hold <- pow_riders() %>%
      select(-rider_uid, -vendor_uid, -c(created_at:modified_by))

    pow_riders_prep(hold)

  })

  vendor_riders_prep <- reactiveVal(NULL)

  vendor_riders <- reactive({
    riders()$vendor_riders
  })

  observeEvent(vendor_riders(), {

    hold <- vendor_riders() %>%
      select(-rider_uid, -vendor_uid, -c(created_at:modified_by))

    vendor_riders_prep(hold)

  })

  output$pow_riders_table <- DT::renderDT({

    req(pow_riders_prep())

    out <- pow_riders_prep()

    cols <- snakecase::to_title_case(colnames(out))

    if (isTRUE(is_mobile())) {
      tbl_class <- "table table-striped table-bordered dt-center dt-responsive dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons", "Responsive")
      tbl_filt <- "none"
      tbl_scroll <- FALSE
    } else {
      tbl_class <- "table table-striped table-bordered dt-center dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons")
      tbl_filt <- "top"
      tbl_scroll <- TRUE
    }

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = tbl_class,
      extensions = tbl_exts,
      filter = tbl_filt,
      width = "100%",
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "riders-table"),
        language = list(
          emptyTable = "Selected Vendor does not have any Riders assigned from Powwater."
        )
      )
    )
  })

  output$vendor_riders_table <- DT::renderDT({

    req(vendor_riders_prep())

    out <- vendor_riders_prep()

    cols <- snakecase::to_title_case(colnames(out))

    if (isTRUE(is_mobile())) {
      tbl_class <- "table table-striped table-bordered dt-center dt-responsive dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons", "Responsive")
      tbl_filt <- "none"
      tbl_scroll <- FALSE
    } else {
      tbl_class <- "table table-striped table-bordered dt-center dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons")
      tbl_filt <- "top"
      tbl_scroll <- TRUE
    }

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = tbl_class,
      extensions = tbl_exts,
      filter = tbl_filt,
      width = "100%",
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "riders-table"),
        language = list(
          emptyTable = "Selected Vendor does not have any Vendor specific Riders."
        )
      )
    )
  })

}

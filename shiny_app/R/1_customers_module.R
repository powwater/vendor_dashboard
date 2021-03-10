customers_module_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Customers Table',
        DT::DTOutput(ns('customers_table')) %>%
          shinycustomloader::withLoader(),
        br()
      )
    )
  )


}

customers_module <- function(input, output, session, vendor_info) {
  ns <- session$ns

  # customers <- reactiveVal(NULL)

  customers <- reactive({

    # browser()

    id <- notify("Loading Customers from Database...")
    on.exit(removeNotification(id), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      init <- conn %>%
        tbl("orders") %>%
        filter(vendor_uid == vend) %>%
        distinct(customer_uid, customer_name)

      out <- conn %>%
        tbl("customers") %>%
        left_join(init, by = c("uid" = "customer_uid", "customer_name")) %>%
        collect() %>%
        rename(customer_uid = uid) %>%
        left_join(select(customer_locations, -customer_name), by = "customer_uid")

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  customers_prep <- reactive({
    req(customers())

    customers() %>%
      mutate(
        location = Vectorize(create_link)(location_url, location_name)
      ) %>%
      select(
        customer_number,
        customer_name,
        phone_number,
        location,
        address,
        location_type,
        vicinity,
        lat,
        lon
      )
  })

  output$customers_table <- DT::renderDT({

    req(customers_prep())

    out <- customers_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1 * match("location", colnames(out)))
    id <- session$ns("customers_table")

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = 'dt-center stripe cell-border display',
      # Escape the HTML
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          # list(targets = 0, orderable = FALSE),
          list(className = "dt-center dt-col nowrap", targets = "_all")
          # list(render = JS("$.fn.dataTable.render.ellipsis( 20, false )"), targets = 4)
        ),
        buttons = dt_bttns(out, "customers-table", esc_cols)
      )
    )

  })


}

orders_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Orders',
        uiOutput(ns("ratings_ui"), inline = TRUE),
        DT::DTOutput(ns('orders_table')) %>%
          shinycustomloader::withLoader(),
        hr(),
        h3("Routes"),
        fluidRow(
          column(
            6,
            h4("Delivery Route Map:"),
            pickerInput(ns("selected_order"),
                        "Select an Order to View:",
                        choices = ""),
            uiOutput(ns("directions_iframe")) %>%
              shinycustomloader::withLoader()
            # googleway::google_mapOutput(ns("deliveries"), height = "450px")%>%
            # shinycustomloader::withLoader()
          ),
          column(
            6,
            h4("Delivery Details:"),
            DT::DTOutput(ns('delivery_details')) %>%
              shinycustomloader::withLoader()
          )
        )
      )
    ),
    htmltools::tags$script(src = "orders_module.js"),
    htmltools::tags$script(paste0("orders_table_module_js('", ns(''), "')"))
  )
}

orders_module <- function(input, output, session, vendor_info){
  ns <- session$ns

  orders <- reactive({

    id <- notify("Loading Orders from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- get_orders_by_vendor(vend, conn)

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  observeEvent(orders(), {
    order_choices <- orders()$order_uid
    names(order_choices) <- paste0("Order #", orders()$order_number)
    updatePickerInput(session, "selected_order", choices = order_choices)
  }, once = TRUE)

  orders_prep <- reactive({
    req(orders())

    ids <- orders()$order_uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Order Details" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
      )
    })

    orders() %>%
      mutate(
        delivery_fee = paste0(formattable::currency(delivery_fee, "", sep = "", big.mark = ","), " KES"),
        total_price = paste0(formattable::currency(total_price, "", sep = "", big.mark = ","), " KES"),
        vendor_prep_time = paste0(vendor_prep_time, " Minutes"),
        payment_total = paste0(formattable::currency(payment_total, "", sep = "", big.mark = ","), " KES"),
        order_type = ifelse(order_type == "Refill", "Swap", order_type)
      ) %>%
      select(
        order_number,
        customer_name,
        rider_name,
        order_time,
        status,
        delivery_fee,
        total_price_of_water = total_price,
        total_transaction_payment = payment_total,
        order_type,
        payment_type,
        vendor_prep_time,
        vendor_rating
      ) %>%
      # add action bttns
      tibble::add_column(" " = actions, .before = 1)
  })

  output$orders_table <- DT::renderDT({

    req(orders_prep())

    factor_cols <- c(
      "customer_name",
      "order_type",
      "status",
      "payment_type"
    )

    rating_cols <- c("vendor_rating")

    out <- orders_prep() %>%
      # mutate_at(vars(tidyselect::all_of(factor_cols)), as.factor) %>%
      ratings_to_stars(cols = rating_cols)

    out$status[2] <- "Out for Delivery"
    out$status[3] <- "Pending"
    out$status[4] <- "Cancelled"


    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1, -13)
    id <- session$ns("orders_table")

    dt_js <- paste0(
      "function(settings, json) {
        var filters = $('#", id, " td .form-group');
        // columns that should have visible filters
        var cols = [", paste(2:n_col, collapse = ", "), "];
        // hide certain column filters
        for (var i = 1; i <= filters.length; i++) {
          if (!cols.includes(i))
            filters.eq(i - 1).css('visibility', 'hidden');
          filters.eq(i - 1).css('position', 'static');
        }
      }")

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = 'dt-center stripe cell-border display compact',
      # Escape the HTML
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        autoWidth = TRUE,
        # scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "35px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "vendors-table", esc_cols),
        initComplete = DT::JS(dt_js),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"),
        language = list(
          emptyTable = "No Orders for Selected Filters"
        )
      )
    ) %>%
      formatStyle("status",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$order_status,
                                     values = assets$order_status_colors,
                                     default = "black"))

  })

  avg_rating <- reactive({
    mean(orders()$vendor_rating, na.rm = TRUE)
  })

  num_ratings <- reactive({
    orders() %>%
      filter(vendor_rating > 0, !is.na(vendor_rating), !is.null(vendor_rating)) %>%
      nrow()
  })

  output$ratings_ui <- renderUI({
    fluidRow(
      column(width = 12,
             div(
               h3(paste0(vendor_info()$vendor_name, " Average Rating: ",
                         formattable::comma(avg_rating(), 3), " Stars ")),
               rating_stars(avg_rating()),
               h3(paste0(" (", formattable::comma(num_ratings(), 0), " Reviews)")),
               hr()))
    )
  })

  selected_order_for_view <- reactiveVal(NULL)

  order_to_info <- eventReactive(input$order_id_to_info, {
    orders() %>%
      filter(order_uid == input$order_id_to_info)
  })

  observeEvent(order_to_info(), {
    scroll(session$ns("directions_iframe"))
    sel <- order_to_info()
    updatePickerInput(session, "selected_order", selected = sel$order_uid)
    selected_order_for_view(sel)
  })

  routes <- reactive({

    id <- notify("Loading Routes from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_location_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("order_routes") %>%
        dplyr::filter(vendor_location_uid == vend) %>%
        left_join(
          conn %>%
            tbl('vendor_locations') %>%
            select(vendor_location_uid = uid,
                   vendor_uid,
                   vendor_location_lat,
                   vendor_location_lon,
                   vendor_location_name,
                   vendor_location_address,
                   vendor_location_place_id),
          by = "vendor_location_uid"
        ) %>%
        left_join(
          conn %>%
            tbl('customer_locations') %>%
            select(customer_location_uid = uid,
                   customer_uid,
                   customer_location_lat,
                   customer_location_lon,
                   customer_location_name,
                   customer_location_address,
                   customer_location_place_id),
          by = "customer_location_uid"
        ) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  routes_filt <- reactive({
    req(routes())
    routes() %>% filter(order_uid == input$selected_order)
  })

  output$directions_iframe <- renderUI({
    vendor_place_id <- vendor_info()$place_id
    customer_place_id <- routes_filt()$customer_location_place_id
    HTML(create_directions_iframe(key = key, start = vendor_place_id, stop = customer_place_id))
  })

  output$delivery_details <- DT::renderDT({
    req(routes_filt())

    hold <- routes_filt()

    out <- tibble::tibble(
      " " = c("Location", "Address", "Coordinates"),
      "Start (Vendor)" = c(hold$vendor_location_name, hold$vendor_location_address, paste0("(", round(hold$vendor_location_lat, 3), ", ", round(hold$vendor_location_lon, 3), ")")),
      "Stop (Customer)" = c(hold$customer_location_name, hold$customer_location_address, paste0("(", round(hold$customer_location_lat, 3), ", ", round(hold$customer_location_lon, 3), ")"))
    )

    cap <- paste0(
      "Order #",
      orders()$order_number[match(input$selected_order, orders()$order_uid)][1],
      " - Order placed on: ",
      paste0(orders()$date[1], " ", orders()$order_time[1])
    )

    n_row <- nrow(out)
    n_col <- ncol(out)
    id <- session$ns("delivery_details_table")

    datatable(
      out,
      caption = cap,
      class = "row-border nowrap",
      rownames = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        columnDefs = list(
          list(className = "dt-center dt-col", targets = "_all")
        )
      )
    )

  })

}

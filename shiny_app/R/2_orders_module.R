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
            h5("Order Deliveries:")#,
            # googleway::google_mapOutput(ns("deliveries"), height = "450px")%>%
            # shinycustomloader::withLoader()
          ),
          column(
            6,
            h5("Delivery Details:")#,
            # DT::DTOutput(ns('delivery_details')) %>%
            # shinycustomloader::withLoader()
          )
        )
      )
    ),
    htmltools::tags$script(src = "orders_module.js"),
    htmltools::tags$script(paste0("orders_table_module_js('", ns(''), "')")),
  )
}

orders_module <- function(input, output, session, vendor_info){
  ns <- session$ns

  orders <- reactive({

    id <- notify("Loading Orders from Database...")
    on.exit(removeNotification(id), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("orders") %>%
        dplyr::filter(vendor_uid == vend) %>%
        select(uid, order_number, customer_name, rider_name, order_time:vendor_rating, payment_total) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  orders_prep <- reactive({
    req(orders())

    ids <- orders()$uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Order Details" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
      )
    })

    orders() %>%
      select(-uid) %>%
      mutate(
        delivery_fee = paste0(formattable::currency(delivery_fee, "", sep = "", big.mark = ","), " KES"),
        total_price = paste0(formattable::currency(total_price, "", sep = "", big.mark = ","), " KES"),
        vendor_prep_time = paste0(vendor_prep_time, " Minutes"),
        payment_total = paste0(formattable::currency(payment_total, "", sep = "", big.mark = ","), " KES"),
        order_type = ifelse(order_type == "Refill", "Swap", order_type)
      ) %>%
      select(
        order_number:status, delivery_fee, total_price_of_water = total_price, total_transaction_payment = payment_total,
        payment_type, vendor_prep_time, vendor_rating
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
      mutate_at(vars(tidyselect::all_of(factor_cols)), as.factor) %>%
      ratings_to_stars(cols = rating_cols)

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
      formatStyle("status", color = "green")

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

  order_to_info <- eventReactive(input$order_id_to_info, {
    orders() %>%
      filter(uid == input$order_id_to_info)
  })

#   shiny::callModule(
#     order_info_module,
#     "edit_offering",
#     vendor_inventory_to_edit = vendor_inventory_to_edit,
#     trigger = reactive({input$vendor_inventory_id_to_edit}),
#     vendor_info = vendor_info
#   )


  # output$customer_locations <- renderGoogle_map({
  #
  #   # browser()
  #
  #   dat <- customers() %>%
  #     mutate(
  #       title = paste0(customer_name, ": ", customer_location_name),
  #       info = paste0(
  #         "<div id='bodyContent'>",
  #         "<h4>", title, "</h4><hr>",
  #         "<iframe width='450px' height='250px'",
  #         "frameborder='0' style = 'border:0'",
  #         "src=",
  #         paste0(
  #           "https://www.google.com/maps/embed/v1/place?q=place_id:",
  #           customer_location_place_id,
  #           "&key=",
  #           key
  #         ),
  #         "></iframe></div>"
  #       )
  #     )
  #
  #   # set_key(key)
  #
  #   google_map(dat,
  #              key = key,
  #              location = c(dat$customer_location_lat, dat$customer_location_lon),
  #              zoom = 12,
  #              search_box = TRUE,
  #              update_map_view = TRUE,
  #              geolocation = TRUE,
  #              map_type_control = TRUE,
  #              zoom_control = TRUE,
  #              # street_view_control = TRUE,
  #              scale_control = TRUE,
  #              rotate_control = TRUE,
  #              fullscreen_control = TRUE,
  #              event_return_type = "list"
  #   ) %>%
  #     add_markers(
  #       data = dat,
  #       id = "customer_uid",
  #       lat = "customer_location_lat",
  #       lon = "customer_location_lon",
  #       title = "title",
  #       # mouse_over_group =
  #       # label = "customer_name",
  #       # layer_id = "customer_location_name",
  #       info_window = "info",
  #       # mouse_over = "customer_location_address",
  #       # close_info_window = TRUE,
  #       # cluster = TRUE,
  #       update_map_view = TRUE
  #     )
  # })
  #
  # observeEvent(input$customer_locations_marker_click, {
  #   print(input$customer_locations_map_marker_click)
  # })
  #
  # output$vendor_region <- renderUI({
  #   HTML(
  #     paste0(
  #       '<iframe width="100%" height="450" style="border:0" loading="lazy" allowfullscreen
  #     src="https://www.google.com/maps/embed/v1/place?q=place_id:',
  #     vendor_info()$region_id, '&key=', key, '"></iframe>'
  #     )
  #   )
  # })

}


# order_info_module <- function(input, output, session,
#                               order_id_to_info,
#                               trigger = order_id_to_info,
#                               vendor_info) {
#
#   ns <- session$ns
#
#   shiny::observeEvent(trigger(), {
#
#     hold <- order_id_to_info()
#
#     details_data <- conn %>%
#
#
#     shiny::showModal(
#       shiny::modalDialog(
#
# }
# Copy in UI
#orders_module_u("orders_module_ui")

# Copy in server
#callModule(orders_module_, "orders_module_ui")


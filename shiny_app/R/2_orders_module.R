orders_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Orders',
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
    )
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
      select(-c(contains("uid"), contains("malicious"), created_at, created_by, modified_at, modified_by)) %>%
      mutate(
        delivery_fee = paste0(formattable::currency(delivery_fee, "", sep = "", big.mark = ","), " KES"),
        total_price = paste0(formattable::currency(total_price, "", sep = "", big.mark = ","), " KES"),
        vendor_prep_time = paste0(vendor_prep_time, " Minutes")
      ) %>%
      mutate_if(is.logical, formattable::formattable, formatter = true_false_formatter.malicious) %>%
      # add action bttns
      tibble::add_column(" " = actions, .before = 1)
  })

  output$orders_table <- DT::renderDT({

    req(orders_prep())

    factor_cols <- c(
      "customer_name",
      "vendor_name",
      "order_type",
      "status",
      "payment_type"
    )

    rating_cols <- c("vendor_rating", "rider_rating", "order_rating")

    out <- orders_prep() %>%
      mutate_at(vars(tidyselect::all_of(factor_cols)), as.factor) %>%
      ratings_to_stars(cols = rating_cols)

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1, -13, -14, -15)
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
      class = 'dt-center stripe cell-border display compact nowrap',
      # Escape the HTML
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE),
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

# Copy in UI
#orders_module_u("orders_module_ui")

# Copy in server
#callModule(orders_module_, "orders_module_ui")


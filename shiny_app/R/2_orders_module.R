orders_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinyFeedback::valueBoxModuleUI(
        ns("completed_orders"),
        "Completed Orders",
        icon = icon("check"),
        backgroundColor = pow_colors$green,
        width = 4
      ),
      shinyFeedback::valueBoxModuleUI(
        ns("awaiting_vendor_response"),
        "Orders Awaiting Vendor Response",
        icon = icon("hourglass"),
        backgroundColor = pow_colors$red,
        width = 4
      ),
      shinyFeedback::valueBoxModuleUI(
        ns("average_rating"),
        "Average Rating per Order",
        icon = icon("star"),
        backgroundColor = pow_colors$yellow,
        width = 4
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = icon_text("folder-open", 'Orders'),
        footer = "Powwater | Tychobra 2021",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("ratings_ui"), inline = TRUE),
        fluidRow(
          column(
            12,
            h3("Orders Awaiting Vendor Response:"),
            # div(
            #   id = ns("resolve_div"),
            #   actionButton(
            #     ns("resolve_awaiting_orders"),
            #     "Resolve Orders Pending Response",
            #     icon = icon("check"),
            #     class = "btn-danger"
            #   ),
            #   br(),
            #   br()
            # ) %>% shinyjs::hidden(),
            DT::DTOutput(ns("awaiting_orders_table")) %>%
              shinycustomloader::withLoader()
          )
        ),
        fluidRow(
          column(
            12,
            h3("Accepted Orders:"),
            DT::DTOutput(ns('orders_table')) %>%
              shinycustomloader::withLoader()
          )
        ),
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
    htmltools::tags$script(src = "orders_module.js?version=2"),
    htmltools::tags$script(paste0("orders_table_module_js('", ns(''), "')")),
    htmltools::tags$script(paste0("awaiting_orders_table_module_js('", ns(''), "')"))
  )
}

orders_module <- function(input, output, session, vendor_info){
  ns <- session$ns

  session$userData$orders_trigger <- reactiveVal(0)

  orders <- reactive({

    id <- notify("Loading Orders from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    session$userData$orders_trigger()

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

  completed_orders <- reactive({
    req(orders())

    orders() %>%
      filter(order_status == "Completed")
  })

  awaiting_orders <- reactive({
    req(orders())

    orders() %>%
      filter(vendor_response ==  "Pending" | is.na(vendor_response)) %>%
      select(
        order_uid,
        order_number,
        order_datetime,
        customer_name,
        order_type,
        order_status,
        total_payment_price,
        vendor_response,
        vendor_response_text
      )
  })

  observeEvent(nrow(awaiting_orders()) > 0, {
    shinyjs::show("resolve_div")
  })

  awaiting_orders_prep <- reactiveVal(NULL)

  observeEvent(awaiting_orders(), {
    ids <- awaiting_orders()$order_uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-success btn-sm accept_btn" data-toggle="tooltip" data-placement="top" title="Accept" id="', id_,'" style="margin: 0"><i class="fas fa-check"></i></button>
          <button class="btn btn-danger btn-sm decline_btn" data-toggle="tooltip" data-placement="top" title="Decline" id="', id_,'" style="margin: 0"><i class="fas fa-ban"></i></button>
        </div>'
      )
    })

    curr_cols <- c("total_payment_price")

    factor_cols <- c(
      "customer_name",
      "order_type",
      "order_status",
      "vendor_response"
    )

    out <- awaiting_orders() %>%
      mutate_at(vars(all_of(curr_cols)), format_currency_kes) %>%
      mutate_at(vars(all_of(factor_cols)), as.factor) %>%
      mutate_if(is.numeric, coalesce, 0L) %>%
      select(
        order_number,
        order_datetime,
        customer_name,
        order_type,
        order_status,
        total_payment_price,
        vendor_response,
        vendor_response_text
      ) %>%
      tibble::add_column(" " = actions, .before = 1)

    if (is.null(awaiting_orders_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      awaiting_orders_prep(out)
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without re-rendering the entire table
      replaceData(awaiting_orders_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })

  output$awaiting_orders_table <- DT::renderDT({

    req(awaiting_orders_prep())

    out <- awaiting_orders_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1)
    id <- session$ns("awaiting_orders_table")

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = "row-border nowrap",
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "none",
      options = list(
        dom = 't',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "45px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "awaiting-orders-table", esc_cols),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"),
        language = list(
          emptyTable = "No Orders Awaiting Vendor Review"
        )
      )
    ) %>%
      formatStyle("order_status",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$order_status,
                                     values = assets$order_status_colors,
                                     default = "black")) %>%
      formatStyle("vendor_response",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$vendor_response,
                                     values = assets$vendor_response_colors,
                                     default = "black"))

  })

  awaiting_orders_table_proxy <-  DT::dataTableProxy("awaiting_orders_table")

  order_to_accept <- eventReactive(input$order_id_to_accept, {
    orders() %>%
      filter(order_uid == input$order_id_to_accept)
  })

  observeEvent(order_to_accept(), {
    sel <- order_to_accept()

    shinyWidgets::ask_confirmation(
      session$ns("accept_order"),
      title = "Accept Order?",
      text = "Please confirm your acceptance of this order.",
    )

    edit_dat <- eventReactive(input$accept_order, {
      order_to_accept() %>%
        mutate(
          vendor_response = "Accepted",
          modified_at = tychobratools::time_now_utc(),
          modified_by = session$userData$user()$email
        )
    })

    observeEvent(edit_dat(), {

      hold <- edit_dat() %>%
        select(
          order_uid,
          vendor_response,
          modified_at,
          modified_by
        )

      dat <- list(
        "data" = hold %>% select(-order_uid),
        "uid" = hold %>% pull(order_uid)
      )

      tryCatch({
        dbExecute(
          conn,
          "UPDATE orders SET
            vendor_response=$1,
            modified_at=$2,
            modified_by=$3 WHERE uid=$4",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
        session$userData$orders_trigger(session$userData$orders_trigger() + 1)
        reloadData(awaiting_orders_table_proxy)

        accepted_dat <- orders() %>%
          filter(order_uid == dat$uid)

        ids <- accepted_dat$order_uid

        # Edit/Delete buttons
        actions <- purrr::map_chr(ids, function(id_) {
          paste0(
            '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Order Details" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
          )
        })

        curr_cols <- c("delivery_fee",
                       "price_of_water",
                       "delivery_commission",
                       "vendor_commission",
                       "discount_amount",
                       "total_payment_price")

        duration_cols <- c("time_vendor_prep",
                           "time_rider_to_vendor",
                           "time_rider_to_customer",
                           "total_delivery_time")

        factor_cols <- c(
          "customer_name",
          "rider_name",
          "order_type",
          "order_status",
          "order_delivery_status",
          "vendor_response",
          "payment_type"
        )

        rating_cols <- c("vendor_rating")

        out <- accepted_dat %>%
          mutate_at(vars(all_of(curr_cols)), format_currency_kes) %>%
          mutate_at(vars(all_of(duration_cols)), format_duration_minutes) %>%
          mutate_at(vars(all_of(factor_cols)), as.factor) %>%
          mutate_if(is.numeric, coalesce, 0L) %>%
          ratings_to_stars(cols = rating_cols) %>%
          select(
            order_number,
            order_date,
            order_time,
            customer_name,
            rider_name,
            order_type,
            order_status,
            order_delivery_status,
            vendor_response,
            vendor_response_text,
            payment_type,
            price_of_water,
            delivery_fee,
            delivery_commission,
            vendor_commission,
            discount_applied,
            discount_amount,
            total_transaction_payment = total_payment_price,
            total_delivery_time,
            time_vendor_prep,
            time_rider_to_vendor,
            time_rider_to_customer,
            vendor_rating
          ) %>%
          tibble::add_column(" " = actions, .before = 1)

        addRow(orders_table_proxy, out, resetPaging = FALSE)

        msg <- paste0("Order accepted!")
        shinyFeedback::showToast("success", msg)
      }, error = function(err) {
        msg <- 'Error updating database'
        print(msg)
        print(err)
        shinyFeedback::showToast('error', msg)
      })
    })
  })

  order_to_decline <- eventReactive(input$order_id_to_decline, {
    orders() %>%
      filter(order_uid == input$order_id_to_decline)
  })

  observeEvent(order_to_decline(), {
    sel <- order_to_decline()

    showModal(
      modalDialog(
        title = "Decline Order:",
        size = "s",
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            session$ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        fluidRow(
          column(
            12,
            helpText("Rejected Orders must be reviewed by Powwater and Vendor must provide a detailed description for the reasoning behind rejection."),
            textAreaInput(session$ns("vendor_response_text"),
                          "Response Comments:",
                          placeholder = "Add details for why you are rejecting the order here...")

          )
        )
      )
    )

    observeEvent(input$vendor_response_text, {
      if (is.null(input$vendor_response_text) || input$vendor_response_text == "") {
        shinyFeedback::showFeedbackDanger("vendor_response_text")
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("vendor_response_text")
        shinyjs::enable("submit")
      }
    })

  })

  edit_dat <- eventReactive(input$submit, {
    order_to_decline() %>%
      mutate(
        vendor_response = "Rejected",
        vendor_response_text = input$vendor_response_text,
        modified_at = tychobratools::time_now_utc(),
        modified_by = session$userData$user()$email
      ) %>%
      select(
        order_uid,
        vendor_response,
        vendor_response_text,
        modified_at,
        modified_by
      )
  })

  observeEvent(edit_dat(), {

    removeModal()

    hold <- edit_dat() %>%
      select(
        order_uid,
        vendor_response,
        vendor_response_text,
        modified_at,
        modified_by
      )

    dat <- list(
      "data" = hold %>% select(-order_uid),
      "uid" = hold %>% pull(order_uid)
    )

    tryCatch({
      dbExecute(
        conn,
        "UPDATE orders SET
            vendor_response=$1,
            vendor_response_text=$2,
            modified_at=$3,
            modified_by=$4 WHERE uid=$5",
        params = c(
          unname(dat$data),
          list(dat$uid)
        )
      )
      session$userData$orders_trigger(session$userData$orders_trigger() + 1)
      reloadData(awaiting_orders_table_proxy)

      accepted_dat <- orders() %>%
        filter(order_uid == dat$uid)

      ids <- accepted_dat$order_uid

      # Edit/Delete buttons
      actions <- purrr::map_chr(ids, function(id_) {
        paste0(
          '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Order Details" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
        )
      })

      curr_cols <- c("delivery_fee",
                     "price_of_water",
                     "delivery_commission",
                     "vendor_commission",
                     "discount_amount",
                     "total_payment_price")

      duration_cols <- c("time_vendor_prep",
                         "time_rider_to_vendor",
                         "time_rider_to_customer",
                         "total_delivery_time")

      factor_cols <- c(
        "customer_name",
        "rider_name",
        "order_type",
        "order_status",
        "order_delivery_status",
        "vendor_response",
        "payment_type"
      )

      rating_cols <- c("vendor_rating")

      out <- accepted_dat %>%
        mutate_at(vars(all_of(curr_cols)), format_currency_kes) %>%
        mutate_at(vars(all_of(duration_cols)), format_duration_minutes) %>%
        mutate_at(vars(all_of(factor_cols)), as.factor) %>%
        mutate_if(is.numeric, coalesce, 0L) %>%
        ratings_to_stars(cols = rating_cols) %>%
        select(
          order_number,
          order_date,
          order_time,
          customer_name,
          rider_name,
          order_type,
          order_status,
          order_delivery_status,
          vendor_response,
          vendor_response_text,
          payment_type,
          price_of_water,
          delivery_fee,
          delivery_commission,
          vendor_commission,
          discount_applied,
          discount_amount,
          total_transaction_payment = total_payment_price,
          total_delivery_time,
          time_vendor_prep,
          time_rider_to_vendor,
          time_rider_to_customer,
          vendor_rating
        ) %>%
        tibble::add_column(" " = actions, .before = 1)

      addRow(orders_table_proxy, out, resetPaging = FALSE)

      session$userData$orders_trigger(session$userData$orders_trigger() + 1)
      msg <- paste0("Order Submitted for Rejection..")
      shinyFeedback::showToast("success", msg)

    }, error = function(err) {
      msg <- 'Error updating database'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

  })

  observeEvent(completed_orders(), {
    req(orders(), completed_orders())
    order_choices <- completed_orders()$order_uid
    names(order_choices) <- paste0("Order #", completed_orders()$order_number)
    updatePickerInput(session, "selected_order", choices = order_choices)
  }, once = TRUE)

  orders_prep <- reactiveVal(NULL)

  observeEvent(list(orders(), awaiting_orders()), {

    hold <- orders() %>%
      filter(!(order_uid %in% awaiting_orders()$order_uid))

    ids <- hold$order_uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Order Details" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
      )
    })

    curr_cols <- c("delivery_fee",
                   "price_of_water",
                   "delivery_commission",
                   "vendor_commission",
                   "discount_amount",
                   "total_payment_price")

    duration_cols <- c("time_vendor_prep",
                       "time_rider_to_vendor",
                       "time_rider_to_customer",
                       "total_delivery_time")

    factor_cols <- c(
      "customer_name",
      "rider_name",
      "order_type",
      "order_status",
      "order_delivery_status",
      "vendor_response",
      "payment_type"
    )

    rating_cols <- c("vendor_rating")

    out <- hold %>%
      mutate_at(vars(all_of(curr_cols)), format_currency_kes) %>%
      mutate_at(vars(all_of(duration_cols)), format_duration_minutes) %>%
      mutate_at(vars(all_of(factor_cols)), as.factor) %>%
      mutate_if(is.numeric, coalesce, 0L) %>%
      ratings_to_stars(cols = rating_cols) %>%
      select(
        order_number,
        order_date,
        order_time,
        customer_name,
        rider_name,
        order_type,
        order_status,
        order_delivery_status,
        vendor_response,
        vendor_response_text,
        payment_type,
        price_of_water,
        delivery_fee,
        delivery_commission,
        vendor_commission,
        discount_applied,
        discount_amount,
        total_transaction_payment = total_payment_price,
        total_delivery_time,
        time_vendor_prep,
        time_rider_to_vendor,
        time_rider_to_customer,
        vendor_rating
      ) %>%
      tibble::add_column(" " = actions, .before = 1)

    if (is.null(orders_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      orders_prep(out)
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without re-rendering the entire table
      replaceData(orders_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }

  })

  output$orders_table <- DT::renderDT({

    req(orders_prep())

    out <- orders_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1, -ncol(out))
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
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = 'table table-striped table-bordered dt-center compact hover',
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "35px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "orders-table", esc_cols),
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
      formatStyle("order_status",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$order_status,
                                     values = assets$order_status_colors,
                                     default = "black")) %>%
      formatStyle("order_delivery_status",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$order_delivery_status,
                                     values = assets$order_delivery_status_colors,
                                     default = "black")) %>%
      formatStyle("vendor_response",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$vendor_response,
                                     values = assets$vendor_response_colors,
                                     default = "black"))

  })

  orders_table_proxy <- DT::dataTableProxy("orders_table")

  # valboxes ----------------------------------------------------------------

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

  avg_rating_valbox <- reactive({
    paste0(formattable::comma(avg_rating(), 3), " Stars")
  })

  callModule(
    shinyFeedback::valueBoxModule,
    "average_rating",
    value = avg_rating_valbox
  )

  num_awaiting_response <- reactive({
    hold <- orders() %>%
      filter(vendor_response == "Pending" | is.na(vendor_response)) %>%
      nrow()

    paste0(hold, " Orders")
  })

  callModule(
    shinyFeedback::valueBoxModule,
    "awaiting_vendor_response",
    value = num_awaiting_response
  )

  completed_orders_valbox <- reactive({
    hold <- orders() %>%
      filter(order_status == "Completed") %>%
      nrow()

    paste0(hold, " Orders")
  })

  callModule(
    shinyFeedback::valueBoxModule,
    "completed_orders",
    value = completed_orders_valbox
  )

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

      out <- get_routes_by_vendor(vend, conn = conn)

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
      paste0(orders()$order_date[1], " ", orders()$order_time[1])
    )

    n_row <- nrow(out)
    n_col <- ncol(out)
    id <- session$ns("delivery_details_table")

    datatable(
      out,
      caption = cap,
      style = "bootstrap",
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

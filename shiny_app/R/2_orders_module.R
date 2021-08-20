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
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(
            12,
            actionButton(
              ns("reload_bttn"),
              "Reload",
              icon = icon("refresh"),
              class = "btn-success pull-right",
              style = "color: #FFF;"
            ) %>%
              shinyjs::disabled(),
            h3(icon_text("hourglass", "Orders Awaiting Vendor Response:")),
            hr(),
            DT::DTOutput(ns("awaiting_orders_table"), width = "100%") %>%
              shinycssloaders::withSpinner()
          )
        ),
        hr(),
        fluidRow(
          column(
            12,
            h3(icon_text("check", "All Non Pending Orders:")),
            hr(),
            DT::DTOutput(ns('orders_table'), width = "100%") %>%
              shinycssloaders::withSpinner()
          )
        )
      ),
      hr()#,
      # box(
      #   width = 12,
      #   title = icon_text("road", " Delivery Details:"),
      #   status = "primary",
      #   solidHeader = TRUE,
      #   height = NULL,
      #   fluidRow(
      #     column(
      #       width = 6,
      #       h4(icon_text("route", "Delivery Routes:")),
      #       uiOutput(ns("directions_iframe")) %>%
      #         shinycssloaders::withSpinner()
      #     ),
      #     column(
      #       width = 6,
      #       h4(icon_text("info", "Delivery Details")),
      #       pickerInput(ns("selected_order"),
      #                   "Select an Order to View:",
      #                   choices = ""),
      #       DT::DTOutput(ns('delivery_details')) %>%
      #         shinycssloaders::withSpinner()
      #     )
      #   )
      # )
    ),
    shiny::tags$script(src = "js/orders_module.js?version=4"),
    shiny::tags$script(paste0("orders_table_module_js('", ns(''), "')"))
  )
}

orders_module <- function(input, output, session, vendor_info, is_mobile) {

  ns <- session$ns

  session$userData$orders_trigger <- reactiveVal(0)



  ring_chime <- reactiveVal(FALSE)
  check_db_change <- reactivePoll(
    # checks every minute
    intervalMillis = 60 * 1000,
    session = session,
    checkFunc = function() {
      if (is.null(vendor_info())) return(NULL)
      out <- NULL
      try({
        vend_id <- vendor_info()$vendor_uid
        out <- conn %>%
          tbl("orders") %>%
          filter(
            vendor_uid == vend_id,
            modified_at == max(modified_at, na.rm = TRUE)
          ) %>%
          select(created_at, modified_at) %>%
          collect()
      })

      if (identical(out$created_at, out$modified_at)) {
        # if created_at != modified_at, then it is not a new order, it is a changed order (e.g.
        # order status updated from "Pending" to "In Progress"), so we don't want to wr
        ring_chime(TRUE)
      } else {
        ring_chime(FALSE)
      }

      return(out)
    },
    valueFunc = function() {
      Sys.time()
    }
  )

  initial_change_check <- TRUE
  observeEvent(check_db_change(), {

    if (isFALSE(initial_change_check)) {
      if (isTRUE(ring_chime())) {
        session$sendCustomMessage("ka_ching", message = list())
        showToast("info", "New order's data detected! Reload data table to view.")
        ring_chime(FALSE)
      } else {
        showToast("info", "Order changes detected. Reload to see changes.")
      }

      shinyjs::enable("reload_bttn")
    }
    # do not show the reload data button if this is the initial data load
    initial_change_check <<- FALSE
  })

  observeEvent(input$reload_bttn, {
    #req(check_db_change())
    session$userData$orders_trigger(session$userData$orders_trigger() + 1)
    shinyjs::disable("reload_bttn")
  })

  orders <- reactive({
    req(vendor_info())
    # id <- notify("Loading Orders from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)
    session$userData$orders_trigger()
    vend <- vendor_info()$vendor_uid
    out <- NULL

    tryCatch({

      out <- get_orders_by_vendor(vend, conn) %>%
        mutate(
          order_datetime = as.character(lubridate::with_tz(order_datetime, tzone = "Africa/Nairobi")),
          # remove seconds
          order_datetime = substr(order_datetime, 1, nchar(order_datetime) - 3),
          order_datetime = paste0(order_datetime, " EAT")
        )

    }, error = function(err) {

      msg <- 'Error collecting vendor orders from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)

      invisible()
    })

    out
  })


  awaiting_orders <- reactive({
    req(orders())

    orders_out <- orders() %>%
      filter(
        order_status == "Pending"
      )

    out <- NULL
    tryCatch({

      order_items_out <- conn %>%
        tbl("order_items") %>%
        filter(order_uid %in% local(orders_out$order_uid)) %>%
        select(order_uid, volume, quantity) %>%
        collect() %>%
        group_by(order_uid) %>%
        mutate(volume = volume * quantity) %>%
        summarize(
          volume = sum(volume),
          quantity = sum(quantity)
        ) %>%
        ungroup()

      out <- orders_out %>%
        left_join(order_items_out, by = "order_uid")

    }, error = function(err) {

      msg <- "unable to get pending order items"
      print(msg)
      print(err)
      showToast("error", msg)

      invisible()
    })

    out
  })

  awaiting_orders_prep <- reactiveVal(NULL)

  observeEvent(awaiting_orders(), {
    ids <- awaiting_orders()$order_uid

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Order Buttons">
          <button class="btn btn-success btn-sm accept_btn" data-toggle="tooltip" data-placement="top" title="Accept" id="', id_,'" style="margin: 0"><i class="fas fa-check"></i></button>
          <button class="btn btn-danger btn-sm decline_btn" data-toggle="tooltip" data-placement="top" title="Decline" id="', id_,'" style="margin: 0"><i class="fas fa-ban"></i></button>
        </div>'
      )
    })

    curr_cols <- c(
      "total_payment_price",
      "price_of_water",
      "delivery_fee",
      "discount_amount"
    )

    factor_cols <- c(
      #"customer_name",
      "order_type"
    )

    out <- awaiting_orders() %>%
      select(
        order_number,
        order_datetime,
        customer_name,
        order_address,
        order_type,
        volume,
        quantity,
        total_payment_price,
        price_of_water,
        delivery_fee,
        delivery_commission,
        discount_amount
      ) %>%
      mutate(
        delivery_commission = delivery_commission / 100
      ) %>%
      mutate_at(vars(all_of(curr_cols)), Vectorize(format_currency_kes)) %>%
      mutate_at(vars(all_of(factor_cols)), as.factor) %>%
      mutate_if(is.numeric, coalesce, 0L) %>%
      tibble::add_column(" " = actions, .before = 1)

    if (is.null(awaiting_orders_prep())) {
      awaiting_orders_prep(out)
    } else {
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

    if (isTRUE(is_mobile())) {
      tbl_class <- "table table-striped table-bordered dt-center dt-responsive dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons", "Responsive")
      tbl_scroll <- FALSE
    } else {
      tbl_class <- "table table-striped table-bordered dt-center dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons")
      tbl_scroll <- TRUE
    }

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      class = tbl_class,
      escape = esc_cols,
      extensions = tbl_exts,
      filter = "none",
      width = "100%",
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
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
      formatPercentage(
        c("delivery_commission"),
        digits = 1
      )
    #%>%
      # formatStyle("order_status",
      #             target = "cell",
      #             fontWeight = "bold",
      #             color = styleEqual(levels = choices$order_status,
      #                                values = assets$order_status_colors,
      #                                default = "black")) %>%
      # formatStyle("vendor_response",
      #             target = "cell",
      #             fontWeight = "bold",
      #             color = styleEqual(levels = choices$vendor_response,
      #                                values = assets$vendor_response_colors,
      #                                default = "black"))

  })

  awaiting_orders_table_proxy <-  DT::dataTableProxy("awaiting_orders_table")

  order_to_accept <- eventReactive(input$order_id_to_accept, {
    orders() %>%
      filter(order_uid == input$order_id_to_accept)
  })

  observeEvent(order_to_accept(), {

    showModal(
      modalDialog(
        title = "Accept Order:",
        size = "s",
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            session$ns('submit'),
            'Accept Order',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        div(
          style = "padding: 15px;",
          class = "text-center",
          h3(
            style = "line-height: 1.5;",
            "Confirm order acceptance"
          )
        )

      )
    )
  })

  edit_dat <- eventReactive(input$submit, {
    hold <- order_to_accept()
    removeModal()

    list(
      order_status = "In Progress",
      vendor_response = "Accepted",
      vendor_response_time = time_now_utc(),
      modified_at = time_now_utc(),
      modified_by = session$userData$user()$user_uid,
      uid = hold$order_uid
    )
  })

  observeEvent(edit_dat(), {
    out <- edit_dat()


    tryCatch({
      dbExecute(
        conn,
        "UPDATE orders SET
            order_status=$1,
            vendor_response=$2,
            vendor_response_time=$3,
            modified_at=$4,
            modified_by=$5 WHERE uid=$6",
        params = c(
          unname(out)
        )
      )

      initial_change_check <<- TRUE
      session$userData$orders_trigger(session$userData$orders_trigger() + 1)
      shinyFeedback::showToast("success", "Order Accepted")

    }, error = function(err) {
      msg <- "Error accepting order"
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })
  })

  order_to_decline <- eventReactive(input$order_id_to_decline, {
    orders() %>%
      filter(order_uid == input$order_id_to_decline)
  })

  observeEvent(order_to_decline(), {
    showModal(
      modalDialog(
        title = "Decline Order:",
        size = "s",
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            session$ns('submit_decline'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        tagList(
          helpText("Rejected Orders must be reviewed by Powwater and Vendor must
                   provide a detailed description for the reasoning behind rejection."),
          div(
            style = "width = 100%;",
            textAreaInput(session$ns("vendor_response_text"),
                          "Response Text:",
                          placeholder = "Add details for why you are rejecting the order here...",
                          height = "200px",
                          width = "100%",
                          resize = "vertical")
          )
        )
      )
    )

    observeEvent(input$vendor_response_text, {
      if (is.null(input$vendor_response_text) || input$vendor_response_text == "") {
        shinyFeedback::showFeedbackDanger("vendor_response_text", text = "Cannot be left blank.", icon = NULL)
        shinyjs::disable("submit_decline")
      } else {
        shinyFeedback::hideFeedback("vendor_response_text")
        shinyjs::enable("submit_decline")
      }
    })

  })

  edit_dat_ <- eventReactive(input$submit_decline, {

    removeModal()

    time_now <- time_now_utc()

    list(
      order_status = "Rejected",
      vendor_response = "Rejected",
      vendor_response_text = input$vendor_response_text,
      vendor_response_time = time_now,
      modified_at = time_now_utc(),
      modified_by = session$userData$user()$user_uid,
      order_uid = order_to_decline()$order_uid
    )
  })

  observeEvent(edit_dat_(), {

    hold <- edit_dat_()

    dat <-

    tryCatch({
      dbExecute(
        conn,
        "UPDATE orders SET
          order_status=$1,
          vendor_response=$2,
          vendor_response_text=$3,
          vendor_response_time=$4,
          modified_at=$5,
          modified_by=$6
        WHERE uid=$7",
        params = unname(hold)
      )

      initial_change_check <<- TRUE
      session$userData$orders_trigger(session$userData$orders_trigger() + 1)
      shinyFeedback::showToast("success", "Order Rejected.")
    }, error = function(err) {
      msg <- 'Error rejecting order'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })
  })

  observeEvent(accepted_orders(), {
    req(accepted_orders())
    hold <- accepted_orders()
    order_choices <- hold$order_uid

    if (length(order_choices) > 0) {
      names(order_choices) <- paste0("Order #", hold$order_number)
    }

    updatePickerInput(
      session,
      "selected_order",
      choices = order_choices
    )

  }, once = TRUE)

  accepted_orders <- reactive({
    req(orders(), awaiting_orders())

    hold <- orders() %>%
      filter(!(order_uid %in% awaiting_orders()$order_uid))
  })


  orders_prep <- reactiveVal(NULL)

  observeEvent(accepted_orders(), {
    hold <- accepted_orders()

    ids <- hold$order_uid

    #disable_ids <- hold_disable$order_uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {

      class <- NULL#if (id_ %in% disable_ids) "shinyjs-disabled " else NULL

      paste0(
        '<button class="',
        paste0(class, 'btn btn-info btn-sm info_btn" '),
        'data-toggle="tooltip" data-placement="top" title="Details" id="', id_,
        '" style="margin: 0" disabled><i class="fas fa-id-card"></i></button></div>'
      )
    })

    curr_cols <- c("delivery_fee",
                   "price_of_water",
                   "delivery_commission",
                   "discount_amount",
                   "total_payment_price")

    duration_cols <- c("time_vendor_prep",
                       "time_rider_to_vendor",
                       "time_rider_to_customer",
                       "total_delivery_time")

    factor_cols <- c(
      #"customer_first_name",
      #"rider_name",
      "order_type",
      "order_status"#,
      #"vendor_response"
    )

    logical_cols <- c("discount_applied")

    rating_cols <- c("vendor_rating")

    out <- hold %>%
      mutate_at(vars(all_of(curr_cols)), Vectorize(format_currency_kes)) %>%
      mutate_at(vars(all_of(duration_cols)), Vectorize(format_duration_minutes)) %>%
      mutate_at(vars(all_of(factor_cols)), as.factor) %>%
      mutate_at(vars(all_of(logical_cols)), ~ stringr::str_to_title(format_true_false(., "yes", "no"))) %>%
      # mutate_if(is.numeric, coalesce, 0L) %>%
      ratings_to_stars(cols = rating_cols) %>%
      tibble::add_column(" " = actions, .before = 1) %>%
      arrange(desc(modified_at)) %>%
      select(
        " ",
        order_number,
        order_datetime,
        customer_name,
        rider_name,
        order_address,
        order_type,
        order_status,
        # order_delivery_status,
        vendor_response,
        vendor_response_text,
        price_of_water,
        delivery_fee,
        delivery_commission,
        discount_applied,
        discount_amount,
        total_transaction_payment = total_payment_price,
        total_delivery_time,
        time_vendor_prep,
        time_rider_to_vendor,
        time_rider_to_customer,
        rider_rating,
        vendor_rating
      )

    if (is.null(orders_prep())) {
      orders_prep(out)
    } else {
      replaceData(orders_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }

  })

  output$orders_table <- DT::renderDT({

    req(orders_prep())

    out <- orders_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1, -1 * match("discount_applied", colnames(out)), -ncol(out))
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
      filter = tbl_filt,
      escape = esc_cols,
      extensions = tbl_exts,
      width = "100%",
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>", # '<Bf>tip',
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
      # formatStyle("order_delivery_status",
      #             target = "cell",
      #             fontWeight = "bold",
      #             color = styleEqual(levels = choices$order_delivery_status,
      #                                values = assets$order_delivery_status_colors,
      #                                default = "black")) %>%
      formatStyle("vendor_response",
                  target = "cell",
                  fontWeight = "bold",
                  color = styleEqual(levels = choices$vendor_response,
                                     values = assets$vendor_response_colors,
                                     default = "black"))

  })

  orders_table_proxy <- DT::dataTableProxy("orders_table")





  # valboxes ----------------------------------------------------------------

  avg_rating_valbox <- reactive({
    req(orders())

    # only have rider rating, so using that for now
    order_rating <- orders() %>%
      filter(order_status == "Completed") %>%
      pull("rider_rating") %>%
      mean(na.rm = TRUE)

    paste0(formattable::comma(order_rating, 2), " Rating")
  })

  callModule(
    shinyFeedback::valueBoxModule,
    "average_rating",
    value = avg_rating_valbox
  )

  num_awaiting_response <- reactive({
    hold <- orders() %>%
      filter(order_status == "Pending") %>%
      nrow()

    paste0(hold, " Awaiting")
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

    paste0(hold, " Completed")
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


    updatePickerInput(
      session,
      "selected_order",
      selected = sel$order_uid
    )
    selected_order_for_view(sel)
  })


  # routes ------------------------------------------------------------------


  route <- reactive({
    req(vendor_info()$vendor_location_uid, input$selected_order)
    # id <- notify("Loading Routes from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)

    hold <- orders() %>%
      filter(uid == input$selected_order)

    out <- NULL

    tryCatch({

      # TODO: query Google API for the route


    }, error = function(err) {
      msg <- 'Error collecting vendor routes from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })


  output$directions_iframe <- renderUI({
    req(route())
    vendor_place_id <- vendor_info()$place_id
    customer_place_id <- routes_filt()$customer_location_place_id
    HTML(create_directions_iframe(key = key, start = vendor_place_id, stop = customer_place_id))
  })

  # output$delivery_details <- DT::renderDT({
  #   req(routes_filt())
  #
  #   hold <- routes_filt()
  #
  #   out <- tibble::tibble(
  #     " " = c("Location", "Address", "Coordinates"),
  #     "Start (Vendor)" = c(hold$vendor_location_name, hold$vendor_location_address, paste0("(", round(hold$vendor_location_lat, 3), ", ", round(hold$vendor_location_lon, 3), ")")),
  #     "Stop (Customer)" = c(hold$customer_location_name, hold$customer_location_address, paste0("(", round(hold$customer_location_lat, 3), ", ", round(hold$customer_location_lon, 3), ")"))
  #   )
  #
  #   cap <- paste0(
  #     "Order #",
  #     orders()$order_number[match(input$selected_order, orders()$order_uid)][1],
  #     " - Order placed on: ",
  #     paste0(orders()$order_date[1], " ", orders()$order_time[1])
  #   )
  #
  #   n_row <- nrow(out)
  #   n_col <- ncol(out)
  #   id <- session$ns("delivery_details_table")
  #
  #   if (isTRUE(is_mobile())) {
  #     tbl_class <- "table dt-center dt-responsive nowrap table"
  #     tbl_exts <- c("Responsive")
  #     tbl_filt <- "none"
  #     tbl_scroll <- FALSE
  #   } else {
  #     tbl_class <- "table dt-center nowrap table"
  #     tbl_exts <- list()
  #     tbl_filt <- "none"
  #     tbl_scroll <- TRUE
  #   }
  #
  #   datatable(
  #     out,
  #     caption = cap,
  #     style = "bootstrap",
  #     class = tbl_class,
  #     rownames = FALSE,
  #     selection = "none",
  #     extensions = tbl_exts,
  #     options = list(
  #       scrollX = tbl_scroll,
  #       dom = "t",
  #       columnDefs = list(
  #         list(className = "dt-center dt-col", targets = "_all")
  #       )
  #     )
  #   )
  #
  # })

}

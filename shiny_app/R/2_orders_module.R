orders_module_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        12,
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
      )
    ),
    fluidRow(
      column(
        12,
        box(
          width = 12,
          title = icon_text("folder-open", 'Orders'),
          footer = "Powwater | Tychobra 2021",
          status = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(
              12,
              actionButton(
                ns("reload_bttn"),
                "Reload",
                icon = icon("refresh"),
                class = "btn-success",
                style = "float: right;"
              ) %>%
                shinyjs::disabled(),
              h3(icon_text("hourglass", "Orders Awaiting Vendor Response:")),
              hr(),
              DT::DTOutput(ns("awaiting_orders_table"), width = "100%") %>%
                shinycustomloader::withLoader()
            )
          ),
          hr(),
          fluidRow(
            column(
              12,
              h3(icon_text("check", "All Orders:")),
              hr(),
              DT::DTOutput(ns('orders_table'), width = "100%") %>%
                shinycustomloader::withLoader()
            )
          )
        ),
        hr(),
        box(
          width = 12,
          title = icon_text("road", " Delivery Details:"),
          footer = "Powwater | Tychobra 2021",
          status = "primary",
          solidHeader = TRUE,
          height = NULL,
          fluidRow(
            column(
              width = 6,
              h4(icon_text("route", "Delivery Routes:")),
              uiOutput(ns("directions_iframe")) %>%
                shinycustomloader::withLoader()
            ),
            column(
              width = 6,
              h4(icon_text("info", "Delivery Details")),
              pickerInput(ns("selected_order"),
                          "Select an Order to View:",
                          choices = ""),
              DT::DTOutput(ns('delivery_details')) %>%
                shinycustomloader::withLoader()
            )
          )
        ),
        hr(),
        box(
          width = 12,
          title = icon_text("star", " Ratings"),
          footer = "Powwater | Tychobra 2021",
          status = "primary",
          solidHeader = TRUE,
          height = NULL,
          fluidRow(
            column(
              12,
              uiOutput(ns("ratings_ui")) %>%
                shinycustomloader::withLoader()
            )
          )
        )
      )
    ),
    htmltools::tags$script(src = "orders_module.js?version=2"),
    htmltools::tags$script(paste0("orders_table_module_js('", ns(''), "')")),
    htmltools::tags$script(paste0("awaiting_orders_table_module_js('", ns(''), "')"))
  )
}

orders_module <- function(input, output, session, vendor_info, is_mobile) {

  ns <- session$ns

  session$userData$orders_trigger <- reactiveVal(0)

  # get row counts for tables used in module
  check_db_change <- reactivePoll(
    intervalMillis = 0.25 * 60 * 1000,
    session = session,
    checkFunc = function() {
      # usethis::ui_info("Checking database..")
      # mod_stamp <- RPostgres::dbGetQuery(conn, "SELECT timestamp FROM pg_last_committed_xact()")

      vend_id <- vendor_info()$vendor_uid
      hold <- conn %>% tbl("orders") %>% filter(vendor_uid == vend_id)
      mod_stamp <- hold %>% pull(modified_at) %>% max(na.rm = TRUE)

      # message(paste0("Latest modified timestamp: ", mod_stamp))

      return(mod_stamp)
    },
    valueFunc = function() {
      Sys.time()
    }
  )

  initial_change_check <- TRUE
  observeEvent(check_db_change(), {

    if (isFALSE(initial_change_check)) {
      id <- notify("New order's data detected! Reload data table to view.")
      shinyjs::enable("reload_bttn")
    }
    # do not show the reload data button if this is the initial data load
    initial_change_check <<- FALSE
  })

  observeEvent(input$reload_bttn, {
    # req(check_db_change() > 1)
    session$userData$orders_trigger(session$userData$orders_trigger() + 1)
    shinyjs::disable("reload_bttn")
  })

  orders <- reactive({

    # id <- notify("Loading Orders from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)
    session$userData$orders_trigger()
    vend <- vendor_info()$vendor_uid
    out <- NULL

    tryCatch({
      out <- get_orders_by_vendor(vend, conn)
    }, error = function(err) {
      msg <- 'Error collecting vendor orders from database.'
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

    curr_cols <- c("total_payment_price")

    factor_cols <- c(
      "customer_name",
      "order_type",
      "order_status",
      "vendor_response"
    )

    out <- awaiting_orders() %>%
      mutate_at(vars(all_of(curr_cols)), Vectorize(format_currency_kes)) %>%
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
        dom = "<'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i>>",
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

    showModal(
      modalDialog(
        title = "Accept Order:",
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
        h5("Please confirm your acceptance of this order.")
      )
    )
  })

  edit_dat <- eventReactive(input$submit, {

    removeModal()

    order_to_accept() %>%
      mutate(
        order_status = "In Progress",
        vendor_response = "Accepted",
        vendor_response_time = tychobratools::time_now_utc(),
        modified_at = tychobratools::time_now_utc(),
        modified_by = session$userData$user()$user_uid
      )
  })

  observeEvent(edit_dat(), {

    hold <- edit_dat() %>%
      select(
        order_uid,
        order_status,
        vendor_response,
        vendor_response_time,
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
            order_status=$1,
            vendor_response=$2,
            vendor_response_time=$3,
            modified_at=$4,
            modified_by=$5 WHERE uid=$6",
        params = c(
          unname(dat$data),
          list(dat$uid)
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
            session$ns('submit_'),
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
        shinyFeedback::showFeedbackDanger("vendor_response_text", text = "Cannot be left blank.", icon = shiny::icon("ban", lib = "font-awesome"))
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("vendor_response_text")
        shinyjs::enable("submit")
      }
    })

  })

  edit_dat_ <- eventReactive(input$submit_, {

    removeModal()

    order_to_decline() %>%
      mutate(
        order_status = "Rejected",
        vendor_response = "Rejected",
        vendor_response_text = input$vendor_response_text,
        vendor_response_time = tychobratools::time_now_utc(),
        modified_at = tychobratools::time_now_utc(),
        modified_by = session$userData$user()$user_uid
      ) %>%
      select(
        order_uid,
        order_status,
        vendor_response,
        vendor_response_text,
        vendor_response_time,
        modified_at,
        modified_by
      )
  })

  observeEvent(edit_dat_(), {

    hold <- edit_dat_() %>%
      select(
        order_uid,
        order_status,
        vendor_response,
        vendor_response_text,
        vendor_response_time,
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
            order_status=$1,
            vendor_response=$2,
            vendor_response_text=$3,
            vendor_response_time=$4,
            modified_at=$5,
            modified_by=$6 WHERE uid=$7",
        params = c(
          unname(dat$data),
          list(dat$uid)
        )
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

  observeEvent(completed_orders(), {
    req(orders(), completed_orders())
    order_choices <- completed_orders()$order_uid
    names(order_choices) <- paste0("Order #", completed_orders()$order_number)
    updatePickerInput(session, "selected_order", choices = order_choices)
  }, once = TRUE)

  orders_prep <- reactiveVal(NULL)

  observeEvent(orders(), {

    hold <- orders() %>%
      filter(!(order_uid %in% awaiting_orders()$order_uid))

    hold_disable <- hold %>%
      filter(order_status != "Completed")

    ids <- hold$order_uid

    disable_ids <- hold_disable$order_uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {

      class <- if (id_ %in% disable_ids) "shinyjs-disabled " else NULL

      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Order Buttons"><button class="',
        paste0(class, 'btn btn-info btn-sm info_btn" '),
        'data-toggle="tooltip" data-placement="top" title="Details" id="', id_,
        '" style="margin: 0"><i class="fas fa-id-card"></i></button></div>'
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
        order_date,
        order_time,
        customer_name,
        rider_name,
        order_type,
        order_status,
        # order_delivery_status,
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

  # ratings -----------------------------------------------------------------

  ratings_prep <- reactive({
    avg_rating <- mean(orders()$vendor_rating, na.rm = TRUE)

    num_ratings <- orders() %>%
      filter(vendor_rating > 0, !is.na(vendor_rating), !is.null(vendor_rating)) %>%
      nrow()

    num_five_star <- orders() %>%
      filter(vendor_rating > 4) %>%
      nrow()

    num_four_star <- orders() %>%
      filter(vendor_rating > 3 & vendor_rating <= 4) %>%
      nrow()

    num_three_star <- orders() %>%
      filter(vendor_rating > 2 & vendor_rating <= 3) %>% # >= 3, vendor_rating <= 4) %>%
      nrow()

    num_two_star <- orders() %>%
      filter(vendor_rating > 1 & vendor_rating <= 2) %>%
      nrow()

    num_one_star <- orders() %>%
      filter(vendor_rating > 0 & vendor_rating <= 1) %>%
      nrow()

    unrated <- orders() %>%
      filter(is.na(vendor_rating) | is.null(vendor_rating) | vendor_rating == 0) %>%
      nrow()

    list(
      avg_rating = avg_rating,
      num_ratings = num_ratings,
      num_orders = num_ratings + unrated,
      num_five_star = num_five_star,
      num_four_star = num_four_star,
      num_three_star = num_three_star,
      num_two_star = num_two_star,
      num_one_star = num_one_star,
      num_unrated = unrated
    )
  })

  output$ratings_ui <- renderUI({

    # browser()

    avg <- formattable::comma(ratings_prep()$avg_rating, 3)
    tot <- paste0(formattable::comma(ratings_prep()$num_orders, 0), " Total Orders")
    tot_ratings <- paste0(formattable::comma(ratings_prep()$num_ratings, 0), " total reviewed orders")
    five <- paste0(formattable::comma(ratings_prep()$num_five_star, 0), " Reviews")
    four <- paste0(formattable::comma(ratings_prep()$num_four_star, 0), " Reviews")
    three <- paste0(formattable::comma(ratings_prep()$num_three_star, 0), " Reviews")
    two <- paste0(formattable::comma(ratings_prep()$num_two_star, 0), " Reviews")
    one <- paste0(formattable::comma(ratings_prep()$num_one_star, 0), " Reviews")
    unrated = paste0(formattable::comma(ratings_prep()$num_unrated, 0), " Unrated Orders")

    percents <- c("one_pct", "two_pct", "three_pct", "four_pct", "five_pct", "unrated_pct")

    pcts <- purrr::map_dbl(percents,
                           function(x) {

                             hold <- switch(x,
                                            "five_pct" = ratings_prep()$num_five_star,
                                            "four_pct" = ratings_prep()$num_four_star,
                                            "three_pct" = ratings_prep()$num_three_star,
                                            "two_pct" = ratings_prep()$num_two_star,
                                            "one_pct" = ratings_prep()$num_one_star,
                                            "unrated_pct" = ratings_prep()$num_unrated)

                             out <- hold / ratings_prep()$num_orders

                             out
                           }) %>% set_names(percents)

    pct_widths <- map_chr(pcts, function(x) {
      paste0(as.character(round(x * 100, 2)), "%")
    }) %>%
      set_names(percents)

    fluidRow(
      column(
        width = 8,
        offset = 2,
        div(
          htmlTemplate(
            filename = "www/html/ratings_card.html",
            header = paste(vendor_info()$vendor_name, ": Average Vendor Rating"),
            header_stars = rating_stars(ratings_prep()$avg_rating, span = TRUE, class = "ratings-fa"),
            average_rating = avg,
            number_of_reviews = tot_ratings,
            num_five_star = five,
            num_four_star = four,
            num_three_star = three,
            num_two_star = two,
            num_one_star = one,
            num_unrated = unrated,
            five_pct = pct_widths["five_pct"],
            four_pct = pct_widths["four_pct"],
            three_pct = pct_widths["three_pct"],
            two_pct = pct_widths["two_pct"],
            one_pct = pct_widths["one_pct"],
            unrated_pct = pct_widths["unrated_pct"]
          )
        )
      )
    )

  })

  # valboxes ----------------------------------------------------------------

  avg_rating_valbox <- reactive({
    paste0(formattable::comma(ratings_prep()$avg_rating, 3), " Stars")
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


  # routes ------------------------------------------------------------------


  routes <- reactive({

    # id <- notify("Loading Routes from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_location_uid

    out <- NULL

    tryCatch({

      out <- get_routes_by_vendor(vend, conn = conn)

    }, error = function(err) {
      msg <- 'Error collecting vendor routes from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  routes_filt <- reactive({
    req(routes(), input$selected_order)
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

    if (isTRUE(is_mobile())) {
      tbl_class <- "table dt-center dt-responsive nowrap table"
      tbl_exts <- c("Responsive")
      tbl_filt <- "none"
      tbl_scroll <- FALSE
    } else {
      tbl_class <- "table dt-center nowrap table"
      tbl_exts <- list()
      tbl_filt <- "none"
      tbl_scroll <- TRUE
    }

    datatable(
      out,
      caption = cap,
      style = "bootstrap",
      class = tbl_class,
      rownames = FALSE,
      selection = "none",
      extensions = tbl_exts,
      options = list(
        scrollX = tbl_scroll,
        dom = "t",
        columnDefs = list(
          list(className = "dt-center dt-col", targets = "_all")
        )
      )
    )

  })

}
